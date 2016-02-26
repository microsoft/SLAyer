(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Reachability via pointers in symbolic heaps *)

open Library

open Type
open Variable
open Expression
module E = Exp
open SYMBOLIC_HEAP
open SymbolicHeap
open TransRel
module Ends = BiEdge.Poly

module L = (val Log.std Config.vRch : Log.LOG)


(* Timing =================================================================== *)

let reachability_graphs_tmr =
  Timer.create "Reachability.reachability_graphs"


(* Tracing ================================================================== *)

let fmt ff rgm =
  let fmt_lbl_rg ff (lbl,rg) =
    Format.fprintf ff "@[%2i: %a@]" lbl ExpTransRel.fmt rg
  in
  Format.fprintf ff "@[%a@]" (List.fmt "@\n" fmt_lbl_rg) (IntMap.to_list rgm)


(*============================================================================
                             Reachability Graphs
  ============================================================================*)

exception FoundPath of Typ.t * Fld.t list

module ShFrm = struct
  include SH
  module Exp = Exp
  module Exps = Exps
  module ExpMap = ExpMap

  let is_leaf q = DjS.is_empty q

  let fold_rels add_scc add_edg q z =
    let rec concretize_lists xs sh =
      SH.LsS.fold (fun ({Ls.pat} as ls) concretized_lss ->
        let vs, fnt, bck = Ls.split_on_fresh_point ls in
        let vxs = Vars.union vs xs in
        let fnt = Patn.instantiate pat fnt in
        let bck = Patn.instantiate pat bck in
        let fs, fnt = XSH.exists_bind vxs fnt in
        let bs, bck = XSH.exists_bind vxs bck in
        let vfbs = Vars.union (Vars.union vs fs) bs in
        let vfbxs = Vars.union vfbs xs in
        let fnt = concretize_lists vfbxs fnt in
        let bck = concretize_lists vfbxs bck in
        let fnt = XSH.exists_intro fs fnt in
        let bck = XSH.exists_intro bs bck in
        XSH.exists_intro vs (XSH.star [fnt; bck] concretized_lss)
      ) sh (SH.exists_intro Vars.empty (SH.LsS.empty sh))
    in
    let _,q = XSH.exists_bind Vars.empty (concretize_lists Vars.empty q)
    in
    let add_edg loc cnt z =
      if Exp.sort_of cnt <> Var.PointerSort then z else
      add_edg loc cnt z
    in
    let base_to_type =
      SH.PtS.fold (fun {Pt.loc; off} base_to_type ->
        let obj_typ, fld_path =
          match Off.desc off with
          | Off.Path(ty,fs) ->
              (ty,fs)
          | Off.Var(v) ->
              try
                Exps.iter (fun e ->
                  match Off.desc (Off.mk e) with
                  | Off.Path(ty,fs) -> raise (FoundPath(ty,fs))
                  | _ -> ()
                ) (SH.Pf.class_of q (E.mkVar v)) ;
                failwithf "unexpected Var offset: %a" Var.fmt v
              with FoundPath(ty,fs) ->
                (ty,fs)
        in
        let obj_bas = SH.Pf.normalize q (E.mkSubs loc fld_path)
        in
        ExpMap.add obj_bas obj_typ base_to_type
      ) q ExpMap.empty
    in
    z
    |>
    SH.Pf.fold_classes (fun e _ z ->
      if Exp.is_pointer e then
        add_scc [e] z
      else
        z
    ) q
    |>
    ExpMap.fold (fun bas typ z ->
      let locs =
        List.fold (fun (_,fs,_ty) locs ->
          let loc = SH.Pf.normalize q (E.mkAdds bas fs) in
          if SH.Pf.mem_carrier loc q
          then loc :: locs
          else locs
        ) (Typ.all_paths typ) []
      in
      add_scc locs z
    ) base_to_type
    |>
    SH.PtS.fold (fun {Pt.loc; cnt} z ->
      Option.fold (add_edg loc) cnt z
    ) q

  let fold_nrels _ _ z = z

end

module CngRel = struct
  include CngRel
  type exp = Exp.t
  type exps = Exps.t
end

module HG = ExpTransRel


module DTC = DisjTransClos.Make (ShFrm) (CngRel) (SH) (HG)


(** [reachability_graphs sh] computes a map from each label of [sh] to a heap
    graph representing the reachability relation for that branch. *)
let reachability_graphs sh =
  assert(true$>
    L.incf 5 "( reachability_graphs:@ %a" SH.fmt sh );
    Timer.start reachability_graphs_tmr ;
  (fun rgm ->
    Timer.stop reachability_graphs_tmr ; assert(true$>
    L.decf 5 ") reachability_graphs:@ %a" fmt rgm ))
  <&
  DTC.dtc sh

(* let reachability_graphs sh = debug_wrap1 Config.vRch 5 reachability_graphs sh *)


(*============================================================================
                                 Is-Reachable
  ============================================================================*)

(** [is_reachable root sh dt loc] holds if the branch of [sh] for [dt] proves
    that [loc] is reachable from an expression satisfying [root]. *)
let is_reachable root sh =
  let rgm = reachability_graphs sh in
  fun dt loc ->
    let rg = IntMap.find (SH.lbl dt) rgm in
    let loc' = SH.Pf.normalize dt loc in
    let root = (fun e -> Exps.exists root (SH.Pf.class_of dt e)) in
    root loc || (HG.is_reachable root rg loc')
(*
    let preds = Exps.add loc (HG.predecessors rg loc') in
    Exps.exists (fun e -> Exps.exists root (SH.Pf.class_of dt e)) preds
*)

(*============================================================================
                                 Abstraction
  ============================================================================*)

(* holds only if intermediate allocs are existential *)
let intermediate_allocs_existential xs edg edg' =
  L.incf 2 "( intermediate_allocs_existential:@ @[%a@ %a@]" HeapGraph.Edge.fmt edg HeapGraph.Edge.fmt edg' ;
  L.decf 2 ") intermediate_allocs_existential:@ %b"
  <&
  let {back= back} = edg
  and {frnt= frnt'} = edg'
  in
  let allocs_are_existential links =
    List.for_all (function
      | Some(a) -> Vars.intersect xs (E.fv a)
      | _ -> false
    ) links
  in
     allocs_are_existential back
  && allocs_are_existential frnt'


(* holds only if edges not pointing back into themselves *)
let acyclic edg edg' =
  L.incf 2 "( acyclic:@ @[%a@ %a@]" HeapGraph.Edge.fmt edg HeapGraph.Edge.fmt edg' ;
  L.decf 2 ") acyclic:@ %b"
  <&
  let {prev= prev; frnt= frnt; back= back} = edg
  and {frnt= frnt'; back= back'; next= next'} = edg'
  in
  let add_dangles = List.fold (Option.fold Exps.add) in
  let dangles = add_dangles prev (add_dangles next' Exps.empty)
  in
  let allocs_disjoint_dangles links =
    List.for_all (function
      | Some(a) -> not (Exps.mem a dangles)
      | _ -> true
    ) links
  in
     allocs_disjoint_dangles frnt
  && allocs_disjoint_dangles back
  && allocs_disjoint_dangles frnt'
  && allocs_disjoint_dangles back'


(* holds only if e and f have same reachability predecessors *)
let same_predecessors rgm xs sh e f =
  L.incf 2 "( same_predecessors:@ @[%a@ %a@]" E.fmt e E.fmt f ;
  L.decf 2 ") same_predecessors:@ %b"
  <&
  let rg = IntMap.find (SH.lbl sh) rgm
  in
  let us = Vars.diff (SH.fv sh) xs
  in
  let univ_preds b =
    Exps.fold (fun a preds ->
      Exps.union
        (Exps.filter (fun a' -> Vars.subset (E.fv a') us)
           (SH.Pf.class_of sh a))
        preds
    ) (HG.predecessors rg (SH.Pf.normalize sh b)) Exps.empty
  in
  Exps.equal (univ_preds e) (univ_preds f)


(* holds only if allocs of edges have same reachability predecessors *)
let allocs_same_predecessors rgm xs sh edg edg' =
  L.incf 2 "( allocs_same_predecessors:@ @[%a@ %a@]" HeapGraph.Edge.fmt edg HeapGraph.Edge.fmt edg' ;
  L.decf 2 ") allocs_same_predecessors:@ %b"
  <&
  let {frnt= frnt; back= back} = edg
  and {frnt= frnt'; back= back'} = edg'
  in
  let rec allocs_same_predecessors_ es fs =
    match es, fs with
    | Some(e)::es, Some(f)::fs ->
           same_predecessors rgm xs sh e f
        && allocs_same_predecessors_ es fs
    | [], [] ->
        true
    | _ ->
        false
  in
     allocs_same_predecessors_ frnt frnt'
  && allocs_same_predecessors_ back back'


(** Determine whether to abstract two edges into one, based on whether the
    intermediate allocs are existential, the same universal expressions reach
    to the allocs of the edges, and the edges are not cyclic. *)
let should_append (xs,sh) =
  let rgm = reachability_graphs sh in
  fun edg edg' dt ->
    L.incf 2 "( should_append:@ @[%a@ %a@]" HeapGraph.Edge.fmt edg HeapGraph.Edge.fmt edg' ;
    L.decf 2 ") should_append:@ %b"
    <& let()=()in
       intermediate_allocs_existential xs edg edg'
    && acyclic edg edg'
    && allocs_same_predecessors rgm xs dt edg edg'
