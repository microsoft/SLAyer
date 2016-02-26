(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Graph representation of pointer structure of symbolic heaps *)

open Library

open Variable
open Expression
module E = Exp
module S = Substitution
open SymbolicHeap

module L = (val Log.std Config.vHG : Log.LOG)


(* Timing =================================================================== *)

let add_with_closure_tmr = Timer.create "HeapGraph.add_with_closure"



(*============================================================================
                                 Heap Graphs
  ============================================================================*)


(** "Lifted" value expressions. *)
module LE = struct

  type t = E.t option

  let fv eo = Option.option Vars.empty E.fv eo
  let map_exps s eo = Option.map s eo
  let fold_exps fn eo z = Option.option z (fun e -> E.fold fn e z) eo
  let equal = Option.equal E.equal
  let compare = Option.compare E.compare
  let fmtp fxt ff eo = Option.fmt "-" (E.fmtp fxt) ff eo
  let fmt ff eo = fmtp (Vars.empty,Vars.empty) ff eo
  let fmt_caml ff eo =
    Option.fmt "None" (fun ff -> Format.fprintf ff "Some(%a)" E.fmt_caml) ff eo

end


module Edge = struct

  include BiEdge.Make (LE)

  let fmt ff e = Format.fprintf ff "@[(%a)@]" fmt e


  let meet x y =
    (* greatest lower bound in the lattice where Undef < Some < None *)
    let meet_ lx ly =
      match lx, ly with
      | None  , None                       -> None
      | None  , Some _                     -> ly
      | Some _, None                       -> lx
      | Some _, Some _ when LE.equal lx ly -> lx
      | Some _, Some _                     -> raise Undef
    in
    try map2 meet_ x y
    with Invalid_argument _ -> raise Undef


  let drop none x = map (Option.optionk none id) x


  let append_unchecked = append

  let append edg edg' =
    let are_adjacent =
      try
        (* only the common meeting point must be present *)
        let btwn = drop (fun () -> raise Undef) (Args.between edg edg') in
        Args.fold_links (fun (a,d) so_far -> so_far && E.equal a d) btwn true
      with Undef -> false
    in
    if are_adjacent
    then Some (append edg edg')
    else None

  let to_ls pat edg =
(*     L.incf 0 "( to_ls: %a %a" Patn.fmt pat fmt edg ; *)
(*     L.decf 0 ") to_ls: %a" SH.fmt <& *)
    let none () = E.mkVar (Var.gensym "drop" Var.PointerSort) in
    let ls = {Ls.
      pat= pat;
      len= E.mkVar (Var.gensym "len" Var.IntegerSort);
      arg= drop none edg
    } in
    SH.LsS.star [ls] SH.emp

  let fmt ff edg = Format.fprintf ff "@[(%a)@]" fmt edg

end


module EdgeSet = Set.Make(Edge)


let fmt ff pg =
  Format.fprintf ff "@[<hov 1>[%a]@]"
    (List.fmt ";@ " Edge.fmt) (EdgeSet.to_list pg)


let add_with_closure edg g =
  Timer.start add_with_closure_tmr ;
  (fun _ -> Timer.stop add_with_closure_tmr)
  <&
  let hds =
    EdgeSet.fold (fun g_edg h ->
      match Edge.append g_edg edg with
      | None -> h
      | Some(g_edg_edg) -> EdgeSet.add g_edg_edg h
    ) g EdgeSet.empty
  in
  let tls =
    EdgeSet.fold (fun g_edg h ->
      match Edge.append edg g_edg with
      | None -> h
      | Some(edg_g_edg) -> EdgeSet.add edg_g_edg h
    ) g EdgeSet.empty
  in
  let spn =
    EdgeSet.fold_product (fun hd_edg tl_edg h ->
      (* Note: It shouldn't be necessary to call append_unchecked here,
         something is wrong with Args.adjacent and normalization. *)
      EdgeSet.add (Edge.append_unchecked hd_edg tl_edg) h
    ) hds tls EdgeSet.empty
  in
  EdgeSet.union spn (EdgeSet.union tls (EdgeSet.union hds (EdgeSet.add edg g)))


(** [union_with_closure g g'] is the transitive closure of [g] union [g'],
    assuming [g'] is closed. *)
let union_with_closure g g' = EdgeSet.fold add_with_closure g g'


let transitive_closure g = union_with_closure g EdgeSet.empty


include EdgeSet
