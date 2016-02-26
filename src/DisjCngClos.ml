(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Disjunctive Congruence Closure *)

open Library

open FORMULA
open CONGRUENCE_RELATION

module LblMap = IntMap

module L = (val Log.std Config.vDCC : Log.LOG)



(* Timing =================================================================== *)

let cc_tmr = Timer.create "DCC leaves by congruence closure"
let gie_tmr = Timer.create "DCC leaves by get_implied_equalities"



module Make
  (Frm: FORMULA)
  (CngRel: CONGRUENCE_RELATION
           with type exp := Frm.Exp.t
            and type exps := Frm.Exps.t) :
sig

  val dcc : (Frm.Exp.t->Frm.Exp.t->bool) -> ?dnf:bool -> ?init:CngRel.t -> Frm.t -> (CngRel.t * CngRel.t) LblMap.t

end = struct

  open Expression
(*   module Exp = Frm.Exp *)
(*   module Exps = Frm.Exps *)
(*   module ExpMap = Frm.ExpMap *)

  module Weights = struct
    include ExpMap
    let find e w = try find e w with Not_found -> 0
  end


  let cc_leaves leq dnf init f =
    Timer.start cc_tmr ; (fun _ -> Timer.stop cc_tmr) <&
    Frm.fold_dnf ~dnf
      (fun f ((cube,r),m) ->
(*         L.incf 0 "( map: %a" Frm.fmt f ; (fun _ -> L.decf 0 ") map") <& *)
(*         L.incf 0 "( map: %a" Frm.fmt f ; (fun ((_cube,r),(_m,_w)) -> L.decf 0 ") map: %a" CngRel.fmt r) <& *)
        let r =
          Frm.fold_rels
            (fun es r ->
              match es with
              | e::es -> List.fold (fun e' r -> CngRel.merge leq r e e') es r
              | _ -> r
            )
            (fun e f r -> CngRel.merge leq r e f)
            f r in
        let r =
          Frm.fold_nrels (fun e f r ->
            CngRel.split r e f
          ) f r in
        ((f::cube,r),m)
      )
      (fun ((cube,r),m) ->
        List.fold (fun f m ->
          if not (Frm.is_leaf f) then m
          else
            let l = Frm.lbl f in
            let s' = try CngRel.inter (fun _ _ -> true) (LblMap.find l m) r with Not_found -> r in
(*             L.printf 0 "red: %i" l ; *)
(*             L.printf 0 "red: %i@ %a" l CngRel.fmt s' ; *)
            LblMap.add l s' m
        ) cube m
      )
      f ([], init) LblMap.empty


  let ctx = Pure.mk ()

  let gie_leaves leq init f =
    Timer.start gie_tmr ; (fun _ -> Timer.stop gie_tmr) <&
    let init_eqs = CngRel.fold (fun e' e b -> Exp.mkEq e' e :: b) init []
    in
    let pure_f, lbl_to_prop = Frm.labeled_pure_consequences f
    in
    Pure.conjoin_weak ctx (Exp.mkAnd (Array.of_list (pure_f :: init_eqs)))
    ;
    let m =
      Frm.fold_sp
        (fun f (lbls, carrier) ->
(*        L.printf 0 "dn: %a" Frm.fmt f ; *)
          let carrier =
            Frm.fold_rels
              (fun es z -> List.fold Exps.add es z)
              (fun e' e z -> Exps.add e' (Exps.add e z))
              f carrier in
          let lbls = Frm.lbl f :: lbls in
          (lbls, carrier)
        )
        (fun f (lbls, carrier) m ->
(*        L.printf 0 "up: @[[@[%a@]]@ %a@]" (List.fmt ";@ " Format.pp_print_int) lbls Frm.fmt f ; *)
          if not (Frm.is_leaf f) then m
          else
            let lbl = Frm.lbl f in
            let assumptions = Array.of_list (List.map (fun lbl -> IntMap.find lbl lbl_to_prop) lbls) in
            let cng = CngRel.implied_by leq init ctx assumptions carrier in
            LblMap.add lbl cng m
        )
        f ([], Exps.empty) LblMap.empty
    in
    Pure.clear ctx
    ;
    m


  let dcc leq ?(dnf=true) ?(init=CngRel.empty) f =
(*     L.incf 0 "( dcc: %a" Frm.fmt f ; (fun _ -> L.decf 0 ") dcc") <& *)
    (* Refine preorder [cmp] to prefer w-heavier exps *)
    let init_reps = CngRel.representatives init
    in
    let leqr e f =
      match Exps.mem e init_reps, Exps.mem f init_reps with
      | true, false -> true
      | false, true -> false
      | _           -> leq e f
    in
    (* Compute strongest congruence relation forced by each leaf branch *)
    let m = cc_leaves leqr dnf init f
    in
    (* Re-express congruence relations for leaves using final weights *)
    let w =
      LblMap.fold (fun _lbl cng w ->
        Exps.fold (fun e' w ->
          let cls = CngRel.class_of cng e' in
          let cls_size = Exps.cardinal cls - 1 in
          Exps.fold (fun e w ->
            Weights.modify_add (fun old -> old + cls_size) e cls_size w
          ) cls w
        ) (CngRel.representatives cng) w
      ) m Weights.empty
    in
    let leqw w e f =
(*       (fun o -> L.printf 0 "%a %s %a" Exp.fmt e (if o then "<=" else ">") Exp.fmt f) <& *)
      match Exps.mem e init_reps, Exps.mem f init_reps with
      | true, false -> true
      | false, true -> false
      | _           ->
          (Weights.find e w) > (Weights.find f w) || leq e f
    in
    let m = LblMap.map (fun r -> CngRel.union (leqw w) r r) m
    in
    assert(true$>(
      if not Config.dcc_gie || not Config.check_gie_vs_cc then () else
      let some_inconsis = ref false
      in
      let m0 =
        if Config.dcc_gie then
          LblMap.map (fun r -> CngRel.union (leqw w) r r) (gie_leaves leqr init f)
        else
          LblMap.empty
      in
      LblMap.iter (fun lbl cng1 ->
        let cng0 = LblMap.find lbl m0 in
        if CngRel.is_empty cng0 then
          some_inconsis := true
        else
          let missed_eqs cng0 cng1 =
            CngRel.fold (fun e' e eqs ->
              try
                if not (CngRel.mem_carrier e' cng1) || not (CngRel.mem_carrier e' cng1)
                  || (Exp.sort_of e' <> Exp.sort_of e)
                  || (CngRel.mem cng1 e' e)
                then eqs
                else Exp.mkEq e' e :: eqs
              with exn ->
                L.printf 0 "comparing get_implied_equalities vs congruence closure:@ @[%a@ %a@]"
                  CngRel.fmt cng0 CngRel.fmt cng1 ;
                L.printf 0 "%a = %a" Exp.fmt e' Exp.fmt e ;
                raise exn
            ) cng0 [] in
          let cc_missed = missed_eqs cng0 cng1 in
          let ie_missed = missed_eqs cng1 cng0 in
          if cc_missed <> [] || ie_missed <> [] then
            L.printf 0 "comparing get_implied_equalities vs congruence closure:@ @[%a@ %a@]"
              CngRel.fmt cng0 CngRel.fmt cng1 ;
          if cc_missed <> [] then L.printf 0 "cc missed: @[%a@]" (List.fmt ";@ " Exp.fmt) cc_missed ;
          if ie_missed <> [] then L.printf 0 "ie missed: @[%a@]" (List.fmt ";@ " Exp.fmt) ie_missed ;
          assert( cc_missed = [] && ie_missed = [] );
      ) m ;
    ));
    (* Compute congruence relations for internal labels from those for leaves *)
    let m =
      Frm.fold_sp
        (fun f lbls ->
(*        L.printf 0 "dn: %a" Frm.fmt f ; *)
          Frm.lbl f :: lbls)
        (fun f lbls m ->
(*        L.printf 0 "up: @[[@[%a@]]@ %a@]" (List.fmt ";@ " Format.pp_print_int) lbls Frm.fmt f ; *)
          match lbls with
          | _ :: l_parent :: _ ->
              let l = Frm.lbl f in
              let m_l = LblMap.find l m in
              let r' =
                try
                  let m_p = LblMap.find l_parent m in
                  (* Choose reps from deeper labels if possible *)
                  let reps = Exps.union (CngRel.representatives m_l) (CngRel.representatives m_p) in
                  let leq e f = (Exps.mem e reps) || not (Exps.mem f reps) in
                  CngRel.inter leq m_l m_p
                with Not_found ->
                  m_l
              in
(*            L.printf 0 "add: %i" l_parent ; *)
(*            L.printf 0 "add: %i@ %a" l_parent CngRel.fmt r' ; *)
              LblMap.add l_parent r' m
          | _ ->
              m
        )
        f [] m
    in
    (* Compute set of exps that are representatives in some relation *)
    let reps =
      LblMap.fold (fun _ r reps ->
        Exps.union (CngRel.representatives r) reps
      ) m Exps.empty
    in
    (* Compute trimmed relation for each label *)
    LblMap.fold (fun l r m ->
      LblMap.add l (r, CngRel.restrict r reps) m
    ) m LblMap.empty

end
