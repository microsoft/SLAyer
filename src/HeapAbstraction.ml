(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Abstraction of the heap part of symbolic heaps *)

(* Notes:
   - The definition of Pg.of_pattern only works for patterns where only a
     single points-to predicate is needed to follow the list spine.  For
     instance, the pattern for lists of even length using two points-tos in
     the body will not work.
*)

open Library

open Variable
open Expression
module E = Exp
module S = Substitution
open SymbolicHeap

module L = (val Log.std Config.vAbsH : Log.LOG)
module LAbs = (val Log.std Config.vAbs : Log.LOG)


(* Support ================================================================== *)

(* let smaller_by len q x = (SH.sizeof q) - (XSH.sizeof x) >= len *)



(*============================================================================
                                Pattern Graphs
  ============================================================================*)

module PatternGraph = struct
  include HeapGraph

  (* the unit of meet on pattern graphs *)
  let top pat =
    singleton (Edge.map (const None) pat.Patn.params)

  exception Meet_None
  let meet sh pat x y =
    fold_product (fun x_edg y_edg i ->
      try
        let xy_edg = Edge.meet x_edg y_edg in
        try
          let none () = raise Meet_None in
          let arg = Edge.map (Option.optionk none id) xy_edg in
          let _,body = XSH.exists_bind Vars.empty (Patn.instantiate pat arg) in
          let bex = E.map (SH.Pf.normalize sh) (SH.pure_sf body) in
          (* only add edge if pure subformula of pat simplifies to true *)
          if E.equal E.tt bex then
            add xy_edg i
          else
            i
        with Meet_None ->
          add xy_edg i
      with Undef ->
        i
    ) x y empty


(*   let remove_path _len pat edg q = *)
(*     let ls = Edge.to_ls pat edg in *)
(*     let xs = Vars.diff (SH.fv ls) (SH.fv q) in *)
(*     (* remainder from subtract should have at least len fewer *-conjuncts *) *)
(* (* Note: investigate dropping the proviso *) *)
(*     match Prover.subtract(* _with_proviso (smaller_by len q) *) q xs ls with *)
(*     | Prover.Success(q',_) -> Some(xs, q') *)
(*     | Prover.Failure       -> None *)


  (** [of_pattern pat fg q] is the pattern graph corresponding to a template
      of the pattern [pat] filled in with the field graph [fg].  It is the
      meet of pattern graphs for each points-to record in [pat] *)
  let of_pattern pat sh =
(*     L.incf 6 "( Pg.of_pattern: %a" Patn.fmt pat ; *)
(*     L.decf 6 ") Pg.of_pattern: %a" fmt *)
(*     <& *)
    let {Patn.params; body} = pat
    in
    let bv = Params.fv params
    in
    let leq e f =
(*       L.printf 0 "leq %a %a = %b" E.fmt e E.fmt f <& *)
      Vars.intersect bv (E.fv f) || Vars.disjoint bv (E.fv e)
    in
    (* Note: review ignoring these existentials *)
    let _, body = XSH.exists_bind Vars.empty body
    in
    let sh' = SH.Pf.union leq sh body
(*     &> L.printf 0 "sh': %a" SH.fmt *)
    in
    let params' = Params.map (fun v -> SH.Pf.normalize body (E.mkVar v)) params
    in
    let pg = top pat
    in
    let pg =
      (* consider every edge x -o-> y in pat body *)
      SH.PtS.fold (fun {Pt.loc= x; off= o; cnt= yo} pg ->
(*      L.incf 7 "( considering pattern edge  : %a" *)
(*        Pt.fmt {Pt.loc= x; off= o; cnt= yo} ; *)
(*      L.decf 7 ") pg: %a" fmt *)
(*      <& *)
        let pg' =
          (* consider every edge l -o-> e in sh *)
          SH.PtS.fold (fun {Pt.loc= l; off= o'; cnt= eo} pg ->
            if not (Off.equal o o') then pg
            else
            let edg =
(*            L.incf 7 "( considering sh edge     : %a" *)
(*              Pt.fmt {Pt.loc= l; off= o'; cnt= eo} ; *)
(*            L.decf 7 ") found pattern graph edge: %a" Edge.fmt *)
(*            <& *)
              let sh' = SH.Pf.merge leq sh' x l in
              let sh' =
                match yo, eo with
                | Some(y), Some(e) -> SH.Pf.merge leq sh' y e
                | _ -> sh' in
(*            L.printf 0 "sh': %a" SH.fmt sh' ; *)
              Edge.map (fun p ->
                try
                  let p' = SH.Pf.normalize sh' p in
(*                L.printf 0 "instantiating %a to %a" E.fmt p E.fmt p' ; *)
                  if Vars.disjoint bv (E.fv p') then Some(p') else None
                with Not_found -> None
              ) params'
            in
            add edg pg
          ) sh empty
        in
(*      L.printf 8 "pg of pat edge: %a" fmt pg' ; *)
        if is_empty pg' then pg
        else
        meet sh pat pg pg'
      ) body pg
    in
(*     L.printf 6 "pre-filtered pattern graph:@ %a" fmt pg; *)
(*     let pg = filter (fun edg -> None <> remove_path 1 pat edg sh) pg *)
(*     in *)
    (* add edges for existing lists of the current pattern *)
    SH.LsS.fold (fun {Ls.pat= pat'; arg} pg ->
      if not (Patn.equal pat pat') then pg
      else
      let edg = Args.map Option.some arg in
      add edg pg
    ) sh pg

end

module Pg = PatternGraph



(*============================================================================
                                Generalization
  ============================================================================*)

let append_segments should_append pat (xs,sh) edg edg' =
  assert(true$>(
    if Pg.Edge.append edg edg' <> None then
    L.incf 5 "( append_segments: @[%a@ %a@]@ %a"
      Pg.Edge.fmt edg Pg.Edge.fmt edg' SH.fmt_xs (xs,sh) ));
  (fun a -> assert(true$>(
    if Pg.Edge.append edg edg' <> None then
    L.decf 5 ") append_segments:@ %a" (Option.fmt "failed" (fun ff (_,_,_,xs,sh) ->
     SH.fmt_xs ff (xs,sh))) a )))
  <&
  (* check that edges are adjacent if append should be done *)
  match Pg.Edge.append edg edg' with
  | Some(cat) when should_append edg edg' sh ->
      (* subtract a matching path from the SH *)
      (* Note: Refactor: below is almost an inlining of Pg.remove_path. *)
      let ls = Pg.Edge.to_ls pat cat in
      let ys = Vars.diff (SH.fv ls) (SH.fv sh) in
(* Note: investigate dropping the proviso *)
      (match Prover.subtract(* _with_proviso (smaller_by 2 sh) *) sh ys ls with
      | Prover.Unknown -> None
      | Prover.Success(sh_m_cat,_) ->
          (* add a corresponding ls to the SH *)
          let xsh' = XSH.star [SH.exists_intro Vars.empty ls] sh_m_cat in
          (* Note: Can the pattern graph be incrementally updated here? *)
          let zs, sh' = XSH.exists_bind ys xsh' in
          if 2 <= !Config.vAbsH then (
            let fmt_o, _fmt_i, fmt_n = SH.fmt_did_xs ((xs,sh), (zs,sh')) in
            LAbs.printf 2 "@[<hov 2>abs_ls: creating list %a, replace:@ %t@]@ @[<hov 2>with:@ %t@]"
              SH.fmt ls fmt_o fmt_n
          );
          Some(edg, edg', cat, Vars.unions [xs; ys; zs], sh')
      )
  | _ ->
      None


(** [generalize q] attempts to produce a formula [q'] logically weaker than
    [q] by appending list-segments. *)
let generalize should_append pat (xs,sh) pgm pgs =
  assert(true$>
    L.incf 5 "( generalize:@ %a" SH.fmt_xs (xs,sh) );
  (fun (sh,(xs,_,_)) -> assert(true$>
    L.decf 5 ") generalize:@ %a" SH.fmt_xs (xs,sh)))
  <&
  (* append edges of pattern graph until no more can be *)
  let rec append_edges pg xs' sh' pgs =
    match
      Pg.take_first_pair (append_segments should_append pat (xs',sh')) pg
    with
    | Some(edg, edg', cat, xs', sh') ->
        (* update the pattern graph *)
        let pg' = Pg.add cat (Pg.remove edg (Pg.remove edg' pg)) in
(*      assert( Pg.equal pg' (Pg.of_pattern pat sh') || *)
(*        (L.warnf "incremental update of pattern graph incorrect"; true )); *)
        append_edges pg' xs' sh' true
    | None ->
        (pg, xs', sh', pgs)
  in
  (* build initial pattern graph *)
  let pg = Pg.of_pattern pat sh
  in
  (* append edges of stem *)
  let pg, xs, sh, pgs = append_edges pg xs sh pgs
  in
  let pg, xs, sh, pgs =
    if SH.DjS.is_empty sh then (pg, xs, sh, pgs)
    else
    let pg =
      (* for each disjunction... *)
      SH.DjS.fold (fun dj pg ->
        (* compute set of all edges in any disjunct *)
        let dt_edges =
          Dj.fold (fun dt edgs ->
            let pg_dt =
              try IntMap.find (SH.lbl dt) pgm with Not_found -> Pg.empty in
            Pg.union pg_dt edgs
          ) dj Pg.empty in
        (* add edges common to all disjuncts *)
        let pg_dj =
          Dj.fold (fun dt pg_dj ->
            let pg_dt =
              try IntMap.find (SH.lbl dt) pgm with Not_found -> Pg.empty in
            (* add 0-length edges of dt_edges based on dt's equalities *)
            let pg_dt =
              Pg.fold (fun edg pg_dt ->
                if
                  HeapGraph.Edge.fold_links (fun link so_far -> so_far &&
                    match link with
                    | (Some(a), Some(d)) ->
                        E.equal (SH.Pf.normalize dt a) (SH.Pf.normalize dt d)
                    | _ -> false
                  ) edg true
                then
                  Pg.add edg pg_dt
                else
                  pg_dt
              ) dt_edges pg_dt in
            Pg.meet dt pat pg_dt pg_dj
          ) dj (Pg.top pat) in
        Pg.union pg_dj pg
      ) sh pg
    in
    (* append edges of stem plus added edges from disjunctions *)
    append_edges pg xs sh pgs
  in
  (* save updated and transitively closed pattern graph *)
  let pgm = IntMap.add (SH.lbl sh) (Pg.transitive_closure pg) pgm in
  (sh, (xs, pgm, pgs))



(*============================================================================
                                 Entry Point
  ============================================================================*)

(** [abstract q] attemps to apply predicate-based generalization and
    disjunction factorization to [q], returning [Some q'] if such a [q'] was
    obtained, and [None] otherwise. *)
let abstract (xs,sh) =
  let should_append = Reachability.should_append (xs,sh) in
  let pgm = IntMap.empty in
  let sh',(xs',_,pgs) =
    (* try each pattern *)
    Discovery.fold (fun pat (sh,(xs,pgm,pgs)) ->
      let sh, (xs, pgm, pgs) =
        SH.map_fold_distrib (fun dt (xs, pgm, pgs) ->
          generalize should_append pat (xs,dt) pgm pgs
        ) sh (xs,pgm,pgs) in
      let xs, sh = SH.normalize (xs,sh) in
      (sh, (xs, pgm, pgs))
    ) (SH.exists_intro xs sh) (sh,(xs,pgm,false))
  in
  if pgs then Some(xs',sh') else None
