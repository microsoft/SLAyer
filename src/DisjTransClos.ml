(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Disjunctive Transitive Closure *)

open Library

open FORMULA
open CONGRUENCE_RELATION
open TRANSITIVE_RELATION

module LblMap = IntMap


module Make
  (Frm: FORMULA)
  (CngRel: CONGRUENCE_RELATION
           with type exp = Frm.Exp.t
            and type exps = Frm.Exps.t )
  (DisjCngClos: sig
    val dcc : Frm.t -> (CngRel.t * CngRel.t) LblMap.t end)
  (TransRel: TRANSITIVE_RELATION
             with type exp = Frm.Exp.t
              and type exps = Frm.Exps.t ) :
sig

  val dtc : Frm.t -> TransRel.t LblMap.t

end = struct

  module Exp = Frm.Exp
  module Exps = Frm.Exps
  module ExpMap = Frm.ExpMap


  let dtc f =
    (* Compute disjunctive congruence closure *)
    let cm = DisjCngClos.dcc f
    in
    (* Compute strongest transitive and trimmed congruence closed relation forced by each leaf branch *)
    let m =
      Frm.fold_dnf
        (fun f ((cube,t),m) ->
          let l = Frm.lbl f in
          let _, r_trim = LblMap.find l cm in
          let t =
            Frm.fold_rels
              (fun es ->
                TransRel.add_scc
                  (List.fold (fun e scc ->
                     Exps.union (CngRel.class_of r_trim e) scc
                   ) es Exps.empty)
              )
              (fun e f t ->
                t
                |> TransRel.add_scc (CngRel.class_of r_trim e)
                |> TransRel.add_scc (CngRel.class_of r_trim f)
                |> TransRel.add (e,f)
              )
              f t in
          ((f::cube, t), m)
        )
        (fun ((cube,t),m) ->
          (* For all the clauses that mention a particular sub formula,
             calculate the intersection of the relations *)
          List.fold (fun f m ->
            if not (Frm.is_leaf f) then
              m
            else
              LblMap.modify_add (fun t0 -> TransRel.inter t0 t) (Frm.lbl f) t m
          ) cube m
        )
        f ([], TransRel.empty) LblMap.empty
    in
    (* Compute relations for internal labels from those for leaves *)
    Frm.fold_sp
      (fun f lbls -> Frm.lbl f :: lbls)
      (fun f lbls m ->
        match lbls with
        | _ :: l_parent :: _ ->
            let l = Frm.lbl f in
            let t = LblMap.find l m in
            LblMap.modify_add (fun t0 -> TransRel.inter t t0) l_parent t m
        | _ ->
            m
      )
      f [] m

end
