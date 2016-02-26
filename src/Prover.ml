(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Theorem prover for subtraction and entailment of symbolic heaps *)

open Library

open Variable
open Expression
module E = Exp
module S = Substitution
open SymbolicHeap

module SSet = Set.Make(Substitution)

module L = (val Log.std Config.vPrv : Log.LOG)



(* Timing ====================================================================*)

let subtract_tmr = Timer.create "Prover.subtract"
let entails_tmr = Timer.create "Prover.entails"
let inconsistent_tmr = Timer.create "Prover.inconsistent.Pure.inconsistent"
let ent_pure_tmr = Timer.create "Prover.ent_pure.Pure.impliesx"
let pure_valid_tmr = Timer.create "Prover.pure_valid"
let sub_inconsis_m_tmr = Timer.create "Prover.sub_inconsis_m.Pure.inconsistent"
let sub_inconsis_s_tmr = Timer.create "Prover.sub_inconsis_s.Pure.inconsistent"
let pure_normalize_tmr = Timer.create "Prover.Pure.normalize"
let sh_normalize_tmr = Timer.create "Prover.SH.normalize"

module Pure = struct include Pure

  let inconsistent tmr ?parts x =
    Timer.start tmr ;
    let res = inconsistent ?parts x in
    Timer.stop tmr ;
    res

  let impliesx tmr ?parts x xb =
    Timer.start tmr ;
    let res = impliesx ?parts x xb in
    Timer.stop tmr ;
    res

end

module SH = struct include SH

  let normalize ?dnf ?init xsh =
    Timer.start sh_normalize_tmr ;
    let res = normalize ?dnf ?init xsh in
    Timer.stop sh_normalize_tmr ;
    res

end

module XSH = struct include XSH

  let normalize ?dnf ?init xsh =
    Timer.start sh_normalize_tmr ;
    let res = normalize ?dnf ?init xsh in
    Timer.stop sh_normalize_tmr ;
    res

end



(*============================================================================
                              Initialization
  ============================================================================*)

let sub_count = ref 0
let saved_count = ref 0

let cxt        = Pure.mk ()
let _cxt_ent_ls = Pure.mk ()
let cxt_chk    = Pure.mk ()



(*============================================================================
                  Subtraction: Support types and combinators
  ============================================================================*)

(** Entailment judgments
        ! UniversalS. Minuend |- ? eXistentialS. Subtrahend *> Remainder
    and subtraction judgments
        ! UniversalS. Minuend \- ? eXistentialS. Subtrahend *> Remainder
    produce Remainder given the other components, which are represented by:  *)
type judgment = {
  ent: bool;            (** form: true = entailment, false = subtraction     *)

  us : Vars.t;          (** (universal) variable context                     *)
  mm : SH.t;            (** minuend                                          *)
  xs : Vars.t;          (** existential context                              *)
  ss : SH.t;            (** subtrahend                                       *)

  pm : Pure.t;          (** pure context for approximation of minuend        *)
  ps : Pure.partition;  (** partition of pm for approximation of subtrahend  *)

  nm : SH.t;            (** minuend conjunct not yet conjoined to pm         *)
  ns : SH.t;            (** subtrahend conjunct not yet conjoined to ps      *)

  cc : SH.t;            (** common conjunct that has already been subtracted *)
  pv : SH.t;            (** subtrahend conjunct to be checked by pure_valid  *)
}


(* Formatting =============================================================== *)

let fmt_us = Vars.fmt_embrace "@[<hov 2>! " " .@]@ "
let fmt_xs = Vars.fmt_embrace "@[<hov 2>? " " .@]@ "
(* let fmt_pure ff q = E.fmt ff (SH.pure_sf q) *)

let fmt lab ff {us; mm; xs; ss} =
  let (_,uniques) as fxt = SH.mk_fxt (xs,ss) in
  let xs = if !Config.vVar > 0 then xs else Vars.diff xs uniques in
  Format.fprintf ff "%s@[:  @[%a%a@]@\n\\- @[%a%a@]@]" lab
    (if !Config.vPrv < 9 then fun _ _ -> () else fmt_us) us
    SH.fmt mm
    (Vars.fmtp_embrace "@[<hov 2>? " " .@]@ " fxt) xs
    (SH.fmtsp (if !Config.vPrv > 100 then SH.emp else mm) fxt) ss

let fmt_sub ff ({ent} as goal) = fmt (if ent then "( ent" else "( sub") ff goal



(* Proof Search Combinators ================================================= *)

type result = Unknown | Success of XSH.t * back_cont
(* It would be preferable to use:
     type result = (XSH.t * back_cont) option
   but it requires -rectypes *)

(* backtrack continuation, invoked when goal is unprovable *)
and  back_cont = unit -> result

(* success continuation, invoked when rule applies *)
type succ_cont = XSH.t -> back_cont -> result

(* failure continuation, invoked when rule does not apply *)
type fail_cont = succ_cont -> back_cont -> result

type sub_query = succ_cont -> fail_cont -> back_cont -> result

type sub_rule = judgment -> sub_query


(* sub_query combinators *)
let succ_ r sk _  bk = sk r bk
let fail_   sk fk bk = fk sk bk
let back_   _  _  bk = bk ()

(* sub_rule combinators *)
let fail__ : sub_rule = fun _g -> fail_

(* search logging *)
let trying    msg pp v fn g =
  assert(true$> L.printf 7 ("trying   "^^msg) pp v ); fn g msg pp v
let applying  msg pp v      =
  assert(true$> L.printf 3 ("applying "^^msg) pp v )
let failing   msg pp v z    =
  assert(true$> L.printf 9 ("failing  "^^msg) pp v ); z
let backtracking msg pp v z =
  assert(true$> L.printf 3 ("backtracking "^^msg) pp v ); z

(* sub_query combinators with logging *)
let succ r msg pp v : sub_query = fun sk fk bk ->
  applying msg pp v ;
  succ_ r sk fk (fun()-> backtracking msg pp v bk ())

let fail msg pp v : sub_query = failing msg pp v fail_

let back msg pp v : sub_query = backtracking msg pp v back_

(* try qry and apply fn to the remainder *)
let appl (qry: sub_query) fn msg pp v : sub_query = fun sk fk bk ->
  applying msg pp v ;
  qry (fun r -> sk (fn r)) fk (fun()-> backtracking msg pp v bk ())


(* try qry and backtrack if it fails *)
let commit (qry: sub_query) sk _ bk : result =
  qry sk (fun _ _ -> bk()) bk

(* try to apply rule0 and if it fails try rule1 *)
let ( $+ ) (rule0: sub_rule) (rule1: sub_rule) (goal: judgment)
    (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result =
  rule0 goal sk (fun _ _ -> rule1 goal sk fk bk) bk

(* try to apply rule0 to goal and if it fails try rule1 on (fn goal) *)
let seq_xfrm_on_fail fn (rule0: sub_rule) (rule1: sub_rule) (goal: judgment)
    (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result =
  rule0 goal sk (fun _ _ -> rule1 (fn goal) sk fk bk) bk

(* try to apply rule0 and if it backtracks try rule1 *)
(* let ( $& ) (rule0: sub_rule) (rule1: sub_rule) (goal: judgment) *)
(*     (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result = *)
(*   rule0 goal sk fk (fun()-> rule1 goal sk fk bk) *)

(* try qry0 and if it fails or backtracks try qry1 *)
(* let or_else (qry0: sub_query) (qry1: sub_query) *)
(*     (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result = *)
(*   qry0 sk (fun _ _ -> qry1 sk fk bk) (fun()-> qry1 sk fk bk) *)

(* try to apply rule0 and if it fails or backtracks try rule1 *)
(* let ( $+& ) (rule0: sub_rule) (rule1: sub_rule) (goal: judgment) = *)
(*   or_else (rule0 goal) (rule1 goal) *)

(* try to apply rule0 and if it succeeds apply fn to the remainder *)
let try_then (qry: sub_query) fn msg pp v
    (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result =
  qry (fun x bk -> fn x msg pp v sk fk bk) fk bk
(* let ( $>> ) = try_then *)

(* try to apply rule0 and if it succeeds apply fn to the remainder,
   if fn backtracks, do no backtrack rule0 *)
let try_commit_then (qry: sub_query) fn msg pp v
    (sk:succ_cont) (fk:fail_cont) (bk:back_cont) : result =
  qry (fun x _ -> fn x msg pp v sk fk bk) fk bk
let ( $!> ) = try_commit_then

type xsk = succ_cont -> succ_cont

(* Note: Here, if [fn x ... next fk0 bk] backtracks, then the [bk] of the
   previous [x] is invoked.  This is suboptimal, it would be better to pass
   [bk0] to [fn x ...] and accumulate the [bk]s passed to [next], composing
   them to form the [bk] finally passed to [sk0].  But this would require
   [bk]s to be composable, which they aren't. *)
let try_forall (kfold: ('x->xsk)->xsk) fn (z:XSH.t) msg pp v
    (sk0:succ_cont) (fk0:fail_cont) (bk0:back_cont) : result =
(*   let sk0 a = L.printf 10 "$>>> sk0"; sk0 a in *)
(*   let fk0 a = L.printf 10 "$>>> fk0"; fk0 a in *)
(*   let bk0 a = L.printf 10 "$>>> bk0"; bk0 a in *)
  kfold (fun x next -> fun z bk ->
(*     let next a = L.printf 10 "$>>> next"; next a in *)
(*     let bk a = L.printf 10 "$>>> bk"; bk a in *)
    fn x z msg pp v next fk0 bk
  ) sk0 z bk0

type xfk = fail_cont -> fail_cont

let try_exists (kfold: ('x->xfk)->xfk) fn msg pp v
    (sk0:succ_cont) (_fk0:fail_cont) (bk0:back_cont) : result =
(*   let sk0 a = L.printf 10 "$++& sk0"; sk0 a in *)
(*   let bk0 a = L.printf 10 "$++& bk0"; bk0 a in *)
  kfold (fun x next -> fun sk bk ->
(*     let next a = L.printf 10 "$++& next"; next a in *)
(*     let sk a = L.printf 10 "$++& sk"; sk a in *)
(*     let bk a = L.printf 10 "$++& bk"; bk a in *)
    fn x msg pp v sk (fun _ _ -> next sk bk) (fun()-> next sk bk)
  ) (fun _ bk -> bk()) sk0 bk0


(* Specializations for particular rules used below *)

let pp_nop _ff () = ()
(* let trying_pure_valid   = trying "pure_valid%a"     pp_nop () *)
let trying_s_inconsis_m = trying "sub_inconsis_m%a" pp_nop ()
let trying_s_inconsis_s = trying "sub_inconsis_s%a" pp_nop ()
let trying_s_emp        = trying "sub_emp%a"        pp_nop ()
let trying_s_true       = trying "sub_true%a"       pp_nop ()
(* let trying_s_did        = trying "sub_did%a"        pp_nop () *)
let trying_s_distrib_m  = trying "sub_distrib_m%a"  pp_nop ()
let trying_s_distrib_s  = trying "sub_distrib_s%a"  pp_nop ()

let trying_s_split_ls = trying "sub_split_ls: @[%a@]" Ls.fmt

let trying_s_pt_pt =
  let func =
    trying "sub_pt_pt:%a" (fun ff (m_pt,s_pt) ->
      Format.fprintf ff "@[   %a@ |- %a@]" Pt.fmt m_pt Pt.fmt s_pt) in
  fun m_pt s_pt -> func (m_pt,s_pt)

let trying_s_ls_pt =
  let func =
    trying "sub_ls_pt %a" (fun ff (dir,ls,pt) ->
      Format.fprintf ff "(%s):@[   %a@ |- %a@]"
        (if dir then "front" else "back") Ls.fmt ls Pt.fmt pt) in
  fun dir ls pt -> func (dir,ls,pt)

(* let trying_s_emp_ls  = trying "sub_emp_ls: @[%a@]" Ls.fmt *)
let trying_s_emp_lss = trying "sub_emp_lss%a" pp_nop ()

let trying_s_pt_ls =
  let func =
    trying "sub_pt_ls %a" (fun ff (dir,pt,ls) ->
      Format.fprintf ff "(%s):@[   %a@ |- %a@]"
        (if dir then "front" else "back") Pt.fmt pt Ls.fmt ls) in
  fun dir pt ls -> func (dir,pt,ls)

let trying_s_ls_ls =
  let func =
    trying "sub_ls_ls %a" (fun ff (dir,m_ls,s_ls) ->
      Format.fprintf ff "(%s):@[   %a@ |- %a@]"
        (if dir then "front" else "back") Ls.fmt m_ls Ls.fmt s_ls) in
  fun dir m_ls s_ls -> func (dir,m_ls,s_ls)



(*============================================================================
                            First-Order Entailment
  ============================================================================*)

(** [ent_pure pm ps xs p] holds only if [pm] implies the pure subformula
    of [?xs. p]. *)
let ent_pure {pm; mm; xs; ss= p} =
  assert(true$>
    L.incf 5 "( ent_pur@[<hv>e: @[%a%a@]@]" fmt_xs xs SH.fmt p );
  (fun b -> assert(true$>
    L.decf 5 ") ent_pure: %B" b ))
  <&
  let try_ent_pure xs p =
    (* convert to a boolean expression *)
    let b = SH.pure_sf p in
    let b = E.map (SH.Pf.normalize mm) b in
    let xs = Vars.inter xs (E.fv b) in
    Pure.impliesx ent_pure_tmr pm (xs, b)
  in
  let xs, p = SH.exists_elim (xs, p)
  in
  match try_ent_pure xs p with
  | Some(b) -> b
  | None ->
      let xs, p = SH.normalize (xs, p) in
      match try_ent_pure xs p with
      | Some(b) -> b
      | None -> false



(*============================================================================
                            Entailment of Patterns
  ============================================================================*)

let rec ent_patn m_patn xs s_patn =
  assert(true$>
    L.incf 18 "( ent_patn: %a |- %a%a"
      Patn.fmt m_patn fmt_xs xs Patn.fmt s_patn );
  (fun b -> assert(true$>
    L.decf 18 ") ent_patn: %B" b ))
  <&
  if Patn.equal m_patn s_patn then
    true
  else
    false
(* Note: Reenable to treat lists monotonically in patterns.
    let {Patn.params= m_params} = m_patn in
    let args = Ends.map E.var (fun v -> (Type.Top, E.ovar v)) m_params in
    let m_body = Patn.instantiate m_patn args in
    let s_body = Patn.instantiate s_patn args in
    let us = Vars.union (XSH.fv m_body) (XSH.fv s_body) in
    let ys, m_body = XSH.exists_bind (Vars.union us xs) m_body in
    let us = Vars.union us ys in
    let zs, s_body = XSH.exists_bind (Vars.union us xs) s_body in
    let xs = Vars.union xs zs in
    let pm = !!cxt_ent_ls in
    Pure.clear pm;
    let ps = Pure.mk_partition pm in
    let goal = {ent=true; us; pm; mm= m_body; xs; ps; ss= s_body;
                nm= m_body; ns= s_body} in
    match ent goal with
    | Success(r,_) -> Some(XSH.exists_intro (zs, r))
    | Unknown -> None
*)



(*ent=========================================================================
                                  Entailment
  ============================================================================*)

and ent (goal: judgment) =
  sub {goal with ent= true}
    (fun rr bk ->
      (* is_intuitionistic and is_empty are stronger on normalized formulas *)
(* Note: need to do something in lieu of normalization? *)
(*       let rr = XSH.normalize rr in *)
           (* M \- S *> R  &  R => emp  ==>  M |- S *)
      if   XSH.is_empty (XSH.normalize rr)
(* Note: sub_true should make this unnecessary
           (* M \- S * tt *> R  ==>  M |- S * tt *)
        || SH.Jnk.mem () goal.r
*)
      then Success(rr,bk)
      else bk())
    (fun _sk bk -> bk())
    (fun()-> Unknown)



(*sub=========================================================================
     Subtraction: Judgment Normalization and Well-Formedness, and Tracing
  ============================================================================*)

and sub (goal: judgment) sk fk bk : result =

  (* express subtrahend in terms of representatives of minuend *)
  assert(true$> L.printf 20 "pre-simplify:@\n%a)@\n" fmt_sub goal );
  let goal =
    if not Config.prv_simplify then goal else
    let {mm; xs; ss; nm; ns} = goal in
    let init =
      SH.fold_exps (fun e init ->
        match E.sort_of e with
        | (Var.PointerSort | Var.OffsetSort) when Vars.disjoint xs (E.fv e) ->
            SH.Pf.extend xs init e
        | _ ->
            init
      ) ss mm in
    let (_, mm), meqs' = SH.normalize_stem ~init (Vars.empty, mm) in
    let (xs, ss), seqs' = SH.normalize_stem ~init:mm (xs, ss) in
    let nm = SH.Pf.star (Exps.to_list meqs') nm in
    let ns = SH.Pf.star (Exps.to_list seqs') ns in
    {goal with mm; xs; ss; nm; ns} in

  (* extend pm and pv if there are new constraints *)
  let {nm= nm0; ns= ns0} = goal in
  let goal =
    if nm0 = SH.emp && ns0 = SH.emp then
      goal
    else
      let pm = Pure.extend goal.pm in
      let pv = SH.star [ns0] goal.pv in
      {goal with pm; nm= SH.emp; ns= SH.emp; pv} in

  (* conjoin consequences of the new minuend and subtrahend constraints *)
  let goal =
    if nm0 = SH.emp then goal
    else
      let ps, c, d = SH.pure_consequences (SH.star [nm0] goal.cc) in
      let us = Vars.union goal.us ps in
(*       L.printf 0 "nm: %a" SH.fmt nm0 ; *)
(*       assert ( not (Pure.inconsistent ent_pure_tmr goal.pm) ) ; *)
(*       L.printf 0 "conjoin m: %a" E.fmt (E.mkAnd [|c; d|]) ; *)
      Pure.conjoin goal.pm (E.mkAnd [|c; d|]) ;
(*       if Pure.inconsistent ent_pure_tmr goal.pm then *)
(*      L.printf 0 "Inconsistent" ; *)
      {goal with us}
  in
  let goal =
    if ns0 = SH.emp then goal
    else
      let ps, c, d = SH.pure_consequences ns0 in
      let xs = Vars.union goal.xs ps in
      let cd = E.mkAnd [|c; d|] in
(*       L.printf 0 "conjoin s: %a" E.fmt cd ; *)
      Pure.conjoin ~parts:[goal.ps] goal.pm cd ;
(*       if Pure.inconsistent ~parts:[goal.ps] ent_pure_tmr goal.pm then *)
(*      L.printf 0 "Inconsistent" ; *)
      {goal with xs}
  in
  (* Note: Delay conjoining nm to mm and ns to ss in callers of sub and wait
     to conjoin here, after using the old mm and ss to refine the above calls
     to pure_consequences? *)

  (* trace printing *)
  incr sub_count ;

  let rsk = ref sk and rfk = ref fk and rbk = ref bk in
  assert(true$>
    let indent = L.latch_incf 3 "%a" fmt_sub goal in
    let reset x = L.resetf 3 indent x in
    let name = if goal.ent then "ent" else "sub" in
    rsk := (fun r -> reset ") %s: %a" name XSH.fmt r ; sk r) ;
    rfk := (fun k -> reset ") %s: failed" name       ; fk k) ;
    rbk := (fun() -> reset ") %s: backtracked" name  ; bk())
  );
  let sk = !rsk and fk = !rfk and bk = !rbk in

  (* variable occurrence conditions on judgments *)
  assert( Vars.disjoint goal.us goal.xs
    || failwithf "contexts intersect: @[%a@]" Vars.fmt
         (Vars.inter goal.us goal.xs) );
  assert( Vars.subset (SH.fv goal.mm) goal.us
    || failwithf "unbound minuend vars: @[%a@]" Vars.fmt
         (Vars.diff (SH.fv goal.mm) goal.us) );
  assert( Vars.subset (SH.fv goal.ss) (Vars.union goal.us goal.xs)
    || failwithf "unbound subtrahend vars: @[%a@]" Vars.fmt
         (Vars.diff (SH.fv goal.ss) (Vars.union goal.us goal.xs)) );

  (* variable occurrence conditions on remainder *)
  let sk r =
    assert( Vars.subset (XSH.fv r) (Vars.union goal.us goal.xs)
      || failwithf "unbound remainder vars: @[%a@] in@ @[%a@]"
           Vars.fmt (Vars.diff (XSH.fv r) (Vars.union goal.us goal.xs))
           (fmt "goal") goal );
    sk r in

  try
    sub_ goal sk fk bk
  with Exp.IllSorted _ ->
    fail_ sk fk bk


and sub_ (goal: judgment) =


(*============================================================================
                         Subtraction: Logical Axioms
  ============================================================================*)

  let check_pure_valid goal =
    if not Config.prv_valid_check then fail_ else
    let {pm; xs; pv} = goal in
(*     L.printf 0 "pv : %a" SH.fmt pv ; *)
    if SH.equal SH.emp pv then
      fail_
(*     else if Pure.inconsistent sub_inconsis_m_tmr pm then *)
(*       succ_ XSH.ff *)
    else (
      let xs, pv' = SH.exists_elim (xs, pv) in
(*       L.printf 0 "pv': %a" SH.fmt pv' ; *)
      let ps, c, d = SH.pure_consequences pv' in
      let bex = E.mkAnd [|c; d|] in
      let bex =
(*      (L.incf 0 "( remove:@ {@[%a@]}@ %a" Vars.fmt xs E.fmt bex ; *)
        E.remove (function
          | E.Var(x) -> Vars.mem x xs
          | _ -> false
        ) bex
(*      &> L.decf 0 ") remove: %a" E.fmt) *)
      in
(*       L.printf 0 "bex: %a" E.fmt bex ; *)
      (* Note: Is this too strong, causing Pure.impliesx trouble with
         quantification of ps? *)
      let xs = Vars.union xs ps in
      let xs = Vars.inter xs (E.fv bex) in
      if Pure.impliesx pure_valid_tmr pm (xs, bex) = Some(false) then (
        assert(true$> L.printf 3 "backtracking pure_valid" );
        back_
      ) else
        fail_
    )
  in
  let pure_valid =
    seq_xfrm_on_fail (fun g -> {g with pv= SH.emp}) check_pure_valid
  in


  (*  M |- ff
     ------------------
      M \- xs. S *> ff
  *)
  let sub_inconsis_m logical : sub_rule = trying_s_inconsis_m (fun goal ->
    let {mm; pm} = goal in
    if (if logical
        then Pure.inconsistent sub_inconsis_m_tmr pm
        else SH.inconsistent mm)
    then succ XSH.ff
    else fail
  )
  in


  (*  S |- ff
     ----------------
      M \- xs. S ~/>
  *)
  let sub_inconsis_s logical : sub_rule = trying_s_inconsis_s (fun goal ->
    let {ss; pm; ps} = goal in
    if logical then
      if Pure.inconsistent sub_inconsis_s_tmr pm ~parts:[ps]
      then back
      else fail
    else
      if SH.inconsistent ss
      then appl (sub_inconsis_m true goal) (fun r -> r)
      else fail
  )
  in


  (*  S |- emp   [M] |- ?xs. S
     --------------------------
      M \- xs. S *> M * S

      S |- emp   [M] |/- ?xs. S
     ---------------------------
      M \- xs. S ~/>
  *)
  let sub_emp = trying_s_emp (fun goal ->
    let {mm; ss} = goal in
    if not (SH.is_empty ss)
    then fail
    else if ent_pure goal
    then succ (SH.exists_intro Vars.empty (SH.star [ss] mm))
    else back
  )
  in


  (*  is_pure(S)   [M] |- ?xs. S
     --------------------------------------
      M \- xs. S *> M    M |- xs. S *> [M]

      is_pure(S)   [M] |/- ?xs. S
     -----------------------------
      M \- xs. S ~/>

      This assumes, for completeness, that no cut formulas are intuitionistic.
      If cut formulas are intuitionistic, then in the entailment case
      remainders with footprints between [M] and M may be necessary.
  *)
  let sub_true = trying_s_true (fun goal ->
    let {ent; mm; ss} = goal in
    if not (SH.is_pure ss)
    then fail
    else if ent_pure goal
    then if ent then succ (XSH.Pf.star [SH.pure_sf mm] XSH.emp)
                else succ (SH.exists_intro Vars.empty mm)
    else back
  )
  in


(*============================================================================
                          Subtraction: Logical Rules
  ============================================================================*)

  (*  us. M \- xs. S *> R
     ----------------------------- Q != _ * tt
      us. Q * M \- xs. Q * S *> R
  *)
  let sub_did goal =
    let {mm= q_m; ss= q_s; cc} = goal in
    let mm, q, ss as mqs = SH.diff_inter_diff ~pas:false q_m q_s in
    let mm, q, ss =
      if SH.Jnk.is_empty q then
        mqs
      else
        (SH.Jnk.star mm, SH.Jnk.remove q, SH.Jnk.star ss) in
    if SH.is_empty q then
      fail_
    else
      let cc = SH.star [q] cc in
      appl (sub {goal with mm; ss; cc}) (fun r -> r) "s_did: %a" SH.fmt q
  in


  (*  us. Q0 * M \- xs. S *> R0  …  us. QN * M \- xs. S *> RN
     ---------------------------------------------------------
      us. (Q0 v…v QN) * M \- xs. S *> R0 v…v RN
  *)
  (* Note: Choose which disjunction to distribute based on proving equations
     between may-allocs, similar to sub_split_ls.  Also simultaneously
     distribute the subtrahend disjunction containing the alloc. *)
  let sub_distrib_m q0_qN = trying_s_distrib_m (fun goal ->
    let {mm= q0_qN_m; nm} = goal in
    let m = SH.DjS.remove q0_qN q0_qN_m in
    try_forall (Dj.kfold q0_qN) (fun qI r ->
      assert(true$> L.printf 6 "s_distrib_m considering:@ %a" SH.fmt qI );
      let qI_m = SH.star [qI] m in
      let nm = SH.star [qI_m] nm in
      appl (sub {goal with mm= qI_m; nm}) (fun rI ->
        XSH.disj [rI] r
      )
    ) XSH.ff
  )
  in


  (*  us. M \- xs. QI * S *> R
     -----------------------------------
      us. M \- xs. (Q0 v…v QN) * S *> R
  *)
  (* Note: Choose which disjunction to distribute based on proving equations
     between may-allocs, similar to sub_split_ls. *)
  let sub_distrib_s q0_qN = trying_s_distrib_s (fun goal ->
    let {ss= q0_qN_s; ns} = goal in
    let s = SH.DjS.remove q0_qN q0_qN_s in
    try_exists (Dj.kfold q0_qN) (fun qI ->
      assert(true$> L.printf 6 "s_distrib_s considering:@ %a" SH.fmt qI );
      let qI_s = SH.star [qI] s in
      let ns = SH.star [qI_s] ns in
      appl (sub {goal with ss= qI_s; ns}) (fun r -> r)
    )
  )
  in


(*============================================================================
                      Subtraction: Theory-specific Rules
  ============================================================================*)

  (* us. 0=k ^ f=n ^ b=p * M \- xs. S *> R0
     us. 0<k * ls(L,k,p,f,b,n) * M \- xs. S *> R1
    ----------------------------------------------
     us. ls(L,k,p,f,b,n) * M \- xs. S *> R0 \/ R1
  *)
  let sub_split_ls ({Ls.len} as ls) = trying_s_split_ls ls (fun goal ->
    let {ent; mm= ls_m; nm} = goal in
    (* check if len has already been split on *)
    if SH.Pf.mem (E.mkZLt E.zero len) ls_m then
      fail
    else
      let m = SH.LsS.remove ls ls_m in
      let eqs = Ls.empty_eqs ls in
(*       L.printf 0 "eqs: %a" E.fmt_b (E.band eqs) ; *)
      let eqs_m = SH.Pf.star eqs m in
(*       L.printf 0 "eqs_m: %a" SH.fmt eqs_m ; *)
      let nm = SH.Pf.star eqs nm in
      assert(true$> L.printf 3 "sub_split_ls empty:@ %a" Ls.fmt ls );
      (if ent then try_then else try_commit_then)
        (commit (sub {goal with mm= eqs_m; nm})) (fun r0 ->
          assert(true$> L.printf 6 "sub_split_ls non-empty:@ %a" Ls.fmt ls );
          let ineq = SH.Pf.star [E.mkZLt E.zero len] SH.emp in
          let ineq_ls_m = SH.star [ineq] ls_m in
          appl (commit (sub {goal with mm= ineq_ls_m; nm= ineq})) (fun r1 ->
            XSH.disj [r0] r1
          )
        )
  )
  in


  (*  us. M \- xs. r0==r1 * S *> R
     -------------------------------------
      us. l->r0 * M \- xs. l->r1 * S *> R
  *)
  let sub_pt_pt m_pt s_pt = trying_s_pt_pt m_pt s_pt (fun goal ->
    let {Pt.off= o0; cnt= r0} = m_pt in
    let {Pt.off= o1; cnt= r1} = s_pt in
    let {mm= pt_m; ss= pt_s; ns; cc} = goal in
    let m = SH.PtS.remove m_pt pt_m in
    let s = SH.PtS.remove s_pt pt_s in
    let oeq = E.mkEq (o0 :>Exp.t) (o1 :>Exp.t) in
    let req = Option.to_list (Option.map2 E.mkEq r0 r1) in
    let eqs = oeq :: req in
    let eq_s = SH.Pf.star eqs s in
    let ns = SH.Pf.star eqs ns in
    let cc = SH.PtS.star [m_pt] cc in
    appl (sub {goal with mm= m; ss= eq_s; ns; cc}) (fun r -> r)
  )
  in


  (* [empty_approx Q (xs,ys)] returns a formula [P] such that [Q |- P * tt]
     and [P |- emp], and where every boolean subformula of [P] must mention one
     of [xs] and none of [ys].  *)
  let empty_approx q =
    let pure_sf_q = SH.pure_sf q in
    fun (xs,ys) ->
(*       L.incf 0 "( empty_approx: {%a} %a" Vars.fmt xs SH.fmt q ; *)
(*       L.printf 0 "[q]: %a" E.fmt_b pure_sf_q ; *)
      E.remove (fun d ->
        let b = E.name d in
           E.is_boolean b
        && (let vs = E.fv b in Vars.disjoint vs xs || Vars.intersect vs ys)
      ) pure_sf_q
(*       &> L.decf 0 ") empty_approx: %a" E.fmt_b *)
  in


  (* us. k=0 ^ f=n ^ b=p * M0 \- xs. P0 * e->r *> R0
     us,ys. M1 * M0 \- xs. P0 * e->r *> R1
     us,ws,zs. R01' \- xs-ws. S0 *> R
    -----------------------------------------------------
     us. ls(L,k,p,f,b,n) * M0 \- xs. e->r * S0 *> ?zs. R

    where
     e->r * S0 |= P0 * tt  and  P0 |= emp
     ?ys. M1 = ?i,j. L(p,f,i,j) * ls(L,k-1,i,j,b,n)
     ws = xs ∩ fv(e->r)   (xs-ws = xs - fv(e->r))
     ?zs. R01' = ?xs-ws. R0 \/ ?ys. R1

    applied when
       ls(L,k,p,f,b,n) * M |- ?xs. e=f  when dir = true
    or ls(L,k,p,f,b,n) * M |- ?xs. e=b  when dir = false
  *)
  (* Note: Is this premiss order optimal? *)
  let sub_ls_pt dir ls0 pt0 = trying_s_ls_pt dir ls0 pt0 (fun goal ->
    let {us; mm= ls_m0; xs; ss= pt_s0; nm; cc} = goal in
    let m0 = SH.LsS.remove ls0 ls_m0 in
    let s0 = SH.PtS.remove pt0 pt_s0 in
    let mk_p0 = empty_approx pt_s0 in
    let eqs = Ls.empty_eqs ls0 in
    let eqs_m0 = SH.Pf.star eqs m0 in
    let ws, xs_m_ws = Vars.inter_diff xs (Pt.fv pt0) in
    let p0_pt = SH.Pf.star [mk_p0 (ws, xs_m_ws)] (SH.PtS.star [pt0] SH.emp) in
    let nm = SH.Pf.star eqs nm in
    let goal' = {goal with mm= eqs_m0; ss= p0_pt; nm; pv= SH.emp} in
    assert(true$> L.printf 3 "s_ls_pt: empty case" );
    commit (sub goal') $!> (fun r0 ->
      let {Ls.pat; len} = ls0 in
      let _, pre, suf = Ls.split_on_fresh_point ls0 in
      let link_args = if dir then pre else suf in
      let ls1_args  = if dir then suf else pre in
      let us_xs = Vars.union us xs in
      let _, link = XSH.exists_bind us_xs (Patn.instantiate pat link_args) in
      let ls1 = {ls0 with Ls.len= E.mkZSub [|len; E.one|]; arg= ls1_args} in
      let link_ls1 = SH.LsS.star [ls1] link in
      let link_ls1_m0 = SH.star [link_ls1] m0 in
      let ys = Vars.diff (SH.fv link_ls1) us_xs in
      let us_ys = Vars.union us ys in
      let goal' = {goal with us= us_ys; mm= link_ls1_m0; ss= p0_pt;
                             nm= link_ls1; pv= SH.emp} in
      assert(true$> L.printf 3 "s_ls_pt: nonempty case" );
      commit (sub goal') $!> (fun r1 ->
        let r01 = XSH.disj [r0] (XSH.exists_intro ys r1) in
        let zs, r01' = XSH.exists_bind us_xs (XSH.exists_intro xs_m_ws r01) in
        let us_ws_zs = Vars.union (Vars.union us ws) zs in
        (* Note: This adds a subtrahend subformula to cc, is this ok, might it
           contain existentials that then get pulled out of scope? *)
        let cc = SH.PtS.star [pt0] cc in
        let goal' =
          {goal with us= us_ws_zs; mm= r01'; xs= xs_m_ws; ss= s0; nm= r01'; cc}
        in
        appl (commit (sub goal')) (fun r ->
          XSH.exists_intro zs r
        )
      )
    )
  )
  in


  (* M |- ?xs. k=0 ^ f=n ^ b=p ^ P0
     us. M \- xs. k=0 ^ f=n ^ b=p * S *> R
    ---------------------------------------
     us. M \- xs. ls(L,k,p,f,b,n) * S *> R

    where
     ls(L,k,p,f,b,n) * S |= P0 * tt  and  P0 |= emp
  *)
  (* Note: Assumes that ls has already been removed from goal.ss. *)
  let sub_emp_ls_ goal ls p0 min_fk min_bk m p v sk maj_fk maj_bk =
    let {ss= s0; ns} = goal in
    let ls0 = SH.Pf.star (Ls.empty_eqs ls) SH.emp in
    let ls0_p0 = SH.Pf.star [p0] ls0 in
    if ent_pure {goal with ss= SH.Pf.star [SH.pure_sf ls0_p0] SH.emp} then
      let ls0_s0 = SH.star [ls0] s0 in
      let ns = SH.star [ls0] ns in
      appl (sub {goal with ss= ls0_s0; ns}) (fun r ->
        r
      ) m p v sk maj_fk maj_bk
    else
      min_fk sk min_bk
  in

  (* us. M \- xs. k0=0 ^ f0=n0 ^ b0=p0 ^…^ kN=0 ^ fN=nN ^ bN=pN * S *> R
    -----------------------------------------------------------------------
     us. M \- xs. ls(L0,k0,p0,f0,b0,n0) *…* ls(LN,kN,pN,fN,bN,nN) * S *> R
  *)
  let sub_emp_lss : sub_rule = trying_s_emp_lss (fun goal ->
    let {ss= lss_s; ns} = goal in
    if SH.LsS.is_empty lss_s then fail else
    let eqs, s =
      SH.LsS.fold (fun ls (eql,s) ->
        (List.rev_append (Ls.empty_eqs ls) eql, SH.LsS.remove ls s)
      ) lss_s ([], lss_s) in
    let eqs_s = SH.Pf.star eqs s in
    let ns = SH.Pf.star eqs ns in
    appl (sub {goal with ss= eqs_s; ns}) (fun r -> r)
  )
  in


  (* us. e->r * M \- xs,ys. P0 * S1 *> R0
     us,ws,zs. R0' \- xs-ws. S *> R
    ---------------------------------------------------
     us. e->r * M \- xs. ls(L,k,p,f,b,n) * S *> ?zs. R

    instantiated with each of
     ?ys. S1 = k=1 ^ L(p,f,b,n)
     ?ys. S1 = ?i,j. L(p,f,i,j) * ls(L,k-1,i,j,b,n)  when dir = true
     ?ys. S1 = ?i,j. ls(L,k-1,p,f,i,j) * L(i,j,b,n)  when dir = false

    where
     ls(L,k,p,f,b,n) * S |= P0 * tt  and  P0 |= emp
     ws = xs ∩ fv(P0 * S1)      (xs-ws = xs - fv(P0 * S1))
     ?zs. R0' = ?(xs-ws),ys. R0

    applied when
     e->r * M |- ?xs. e=f  when dir = true
     e->r * M |- ?xs. e=b  when dir = false

    preceded by sub_emp_ls
  *)
  (* Note: It may seem to be equivalent and provide tighter control on the
     search space to change
       ?ys. S1 = ?i,j. L(p,f,i,j) * ls(L,k-1,i,j,b,n)
     to
       ?ys. S1 = ?i,j. k>1 * L(p,f,i,j) * ls(L,k-1,i,j,b,n)
     but this introduces incompleteness.  The models of the minuend may not
     all fall into one of the empty, single, and so modified multiple cases.
  *)
  (* Note: Is distinguishing between k=1 and k>1 cases worthwhile?  No, at
     least unless the major premiss can be committed in the empty case. *)
  let sub_pt_ls dir pt ls = trying_s_pt_ls dir pt ls (fun goal m p v sk fk bk ->
    let {Ls.pat; len} = ls in
    let {ent; us; xs; ss= ls_s0; nm; ns; cc} = goal in
    let us_xs = Vars.union us xs in
    let s0 = SH.LsS.remove ls ls_s0 in
    let p0 = empty_approx ls_s0 (Vars.inter_diff xs (Ls.fv ls)) in

    let major ws xs_m_ws ys sk maj_fk maj_bk = fun r0 bk ->
      let xs = xs_m_ws in
      let xys = Vars.union xs ys in
      let zs, r0' = XSH.exists_bind us_xs (XSH.exists_intro xys r0) in
(*       L.printf 0 "xs-ws:@ {%a}@\n ys:@ {%a}@\n r0:@ %a@\nr0':@ %a" *)
(*      Vars.fmt xs_m_ws Vars.fmt ys XSH.fmt r0 SH.fmt r0' ; *)
      let us = Vars.union (Vars.union us ws) zs in
      let nm = SH.star [r0'] nm in
      let cc = SH.LsS.star [ls] cc in
      appl (sub {goal with us; mm= r0'; xs; ss= s0; nm; cc}) (fun r ->
        XSH.exists_intro zs r
      ) m p v sk (maj_fk bk) (maj_bk bk)
    in

    let multiple_case min_fk min_bk maj_fk maj_bk =
      let _, pre, suf = Ls.split_on_fresh_point ls in
      let link_args = if dir then pre else suf in
      let ls2_args  = if dir then suf else pre in
      let _, link = XSH.exists_bind us_xs (Patn.instantiate pat link_args) in
      let ls2 = {ls with Ls.len= E.mkZSub [|len; E.one|]; arg= ls2_args} in
      let lsM = SH.LsS.star [ls2] link in
      let p0_lsM = SH.Pf.star [p0] lsM in
      let fv_s1 = SH.fv lsM in
      let ys = Vars.diff fv_s1 us_xs in
      let xys = Vars.union xs ys in
      let fv_p0_s1 = SH.fv p0_lsM in
      let ws, xs_m_ws = Vars.inter_diff xs fv_p0_s1 in
      let ns = SH.star [p0_lsM] ns in
      assert(true$> L.printf 3 "s_pt_ls: multiple case" );
      sub {goal with xs= xys; ss= p0_lsM; ns; pv= SH.emp}
        (major ws xs_m_ws ys sk maj_fk maj_bk) min_fk min_bk
    in
(*
    let single_case min_fk min_bk maj_fk maj_bk =
      let ys, link = XSH.exists_bind us_xs (Patn.instantiate pat arg) in
      let ls1 = SH.Pf.star [E.eq_v len E.one] link in
      let xys = Vars.union xs ys in
      let fv_s1 = SH.fv ls1 in
      let ws, xs_m_ws = Vars.inter_diff xs fv_s1 in
      let p0_ls1 = SH.Pf.star [p0] ls1 in
      let ns = SH.star [p0_ls1] ns in
      L.printf 3 "s_pt_ls: single case" ;
      sub {goal with xs= xys; ss= p0_ls1; ns; pv= SH.emp}
        (major ws xs_m_ws ys sk maj_fk maj_bk) min_fk min_bk
    in
*)
    let empty_case min_fk min_bk fk bk =
      assert(true$> L.printf 3 "s_pt_ls: empty case" );
      let goal = {goal with ss= s0} in
      sub_emp_ls_ goal ls p0 min_fk min_bk m p v sk fk bk
    in

    if ent then
      let multiple_case bk () =
        multiple_case
          (* continue after rule if minor premiss fails or backtracks *)
          fk bk
          (* continue after rule if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _bk -> fk) (fun _bk -> bk)
      in
      let empty_case bk () =
        empty_case
          (* continue with multiple case if minor premiss fails or backs *)
          (fun _sk _bk -> multiple_case bk ()) (multiple_case bk)
          (* continue with multiple case if major premiss fails or backs *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _sk _bk -> multiple_case bk ()) (fun _bk -> multiple_case bk ())
      in
      empty_case bk ()
(* This premiss order is uniformly slower experimentally:
      let empty_case bk () =
        empty_case
          (* continue after rule if minor premiss fails or backtracks *)
          fk bk
          (* continue after rule if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          fk bk
      in
      let multiple_case bk () =
        multiple_case
          (* continue with empty case if minor premiss fails or backs *)
          (fun _sk _bk -> empty_case bk ()) (empty_case bk)
          (* continue with empty case if major premiss fails or backs *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _sk _bk bk -> empty_case bk ()) (fun bk -> empty_case bk)
      in
      multiple_case bk ()
*)
    else
      let multiple_case() =
        multiple_case
          (* continue after rule if minor premiss fails or backtracks *)
          fk bk
          (* backtrack rule if major premiss fails, continue if it backtracks *)
          (* Note: verify conjecture that this is complete *)
          (fun _ _ _ -> bk()) (fun bk -> bk)
      in
      let empty_case() =
        empty_case
          (* continue with multiple case if minor premiss fails or backtracks *)
          (fun _ _ -> multiple_case()) multiple_case
          (* continue with multiple case if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _ _ -> multiple_case()) multiple_case
      in
      empty_case()
  )
  in


  (* K |- L
     us,vs. M1 * M \- xs-vs,ys. P0 * S1 *> R0
     us. 0=i ^ e=m ^ a=o * M \- xs. P0 * ls(L,j,p,f,b,n) *> R1
     us,ws,zs. R01' \- xs-ws. S *> R
    --------------------------------------------------------------
     us. ls(K,i,o,e,a,m) * M \- xs. ls(L,j,p,f,b,n) * S *> ?zs. R

    instantiated with:
        M1 = 0<i                            S1 = i=j ^ o=p ^ e=f ^ a=b ^ m=n
    and when dir = true with:
        M1 = 0<j' * ls(K,i-j',b',n',a,m)    S1 = j=j' ^ o=p ^ e=f ^ b'=b ^ n'=n
    and M1 = 0<i                            S1 = e=f ^ o=p * ls(L,j-i,a,m,b,n)
    and when dir = false with:
        M1 = 0<j' * ls(K,i-j',o,e,p',f')    S1 = j=j' ^ p'=p ^ f'=f ^ a=b ^ m=n
    and M1 = 0<i                            S1 = a=b ^ m=n * ls(L,j-i,p,f,o,e)

    where
     ls(L,j,p,f,b,n) * S |= P0 * tt  and  P0 |= emp
     vs = fv(M1) - us
     ws = xs ∩ fv(P0 * S1)      (xs-ws = xs - fv(P0 * S1))
     ?zs. R01' = ?xs-ws. (?vs,ys. R0) \/ R1

    applied when
       ls(K,i,o,e,a,m) * M |- ?xs. e=f  when dir = true
    or ls(K,i,o,e,a,m) * M |- ?xs. a=b  when dir = false

    succeeded by sub_emp_ls
  *)
  (* Note: It may seem equivalent and more direct to use
        M1 = 0<j * ls(K,i-j,b,n,a,m)        S1 = e=f ^ o=p
     instead of
        M1 = 0<j' * ls(K,i-j',b',n',a,m)    S1 = j=j' ^ o=p ^ e=f ^ b'=b ^ n'=n
     but doing so is unsound, since it assumes that, under the assumption that
     j≤i, the minuend ls may go through p,f, but this is not necessarily so.
  *)
  (* Note: It may seem benign to change the last (forward) instantiation to:
        M1 = 0<i                        S1 = e=f ^ o=p ^ i<j * ls(L,j-i,a,m,b,n)
     thereby forcing the case when the i=j to be covered by the preceding
     instantiation, but this results in remainders with possibly-empty but
     cyclic lists that the check for emptiness in ent doesn't understand.
  *)
  (* Note: The set of variables passed to empty_approx is unexplained.  For
     instance, it is unsound to pass only the existentials that appear in S1.
  *)
  let sub_ls_ls dir m_ls s_ls = trying_s_ls_ls dir m_ls s_ls (fun goal m p v sk fk bk ->
    let {Ls.len= m_len; arg= m_arg; pat= m_patn} = m_ls in
    let {Ls.len= s_len; arg= s_arg; pat= s_patn} = s_ls in
    let {ent; us; mm= ls_m0; xs; ss= ls_s0; nm; ns; cc} = goal in
    let m0 = SH.LsS.remove m_ls ls_m0 in
    let s0 = SH.LsS.remove s_ls ls_s0 in
    let p0 = empty_approx ls_s0 (Vars.inter_diff xs (Ls.fv s_ls)) in

    let equal_eqs m_arg s_arg =
      Args.fold_links2 (fun (e,m) (f,n) eqs ->
        E.mkEq e f :: E.mkEq m n :: eqs
      ) m_arg s_arg []
    in

    let major_premiss vs ws xs_m_ws r0 maj_fk maj_bk = fun r1 bk ->
      let us_xs = Vars.union us xs in
      let r01 = XSH.set_lbl (SH.lbl m0) (XSH.disj [XSH.exists_intro vs r0] r1) in
      let zs, r01' = XSH.exists_bind us_xs (XSH.exists_intro xs_m_ws r01) in
      let us_ws_zs = Vars.union (Vars.union us ws) zs in
      let nm = SH.star [r01'] nm in
      let cc = SH.LsS.star [s_ls] cc in
      let goal = {goal with us= us_ws_zs; mm= r01'; xs= xs_m_ws; ss= s0; nm; cc} in
      appl (sub goal) (fun r ->
        XSH.exists_intro zs r
      ) m p v sk (maj_fk bk) (maj_bk bk)
    in

    let empty_minuend_ls_premiss vs ws xs_m_ws p0 maj_fk maj_bk = fun r0 bk ->
      assert(true$> L.printf 3 "s_ls_ls: empty minuend ls premiss" );
      let m2 = SH.Pf.star (Ls.empty_eqs m_ls) SH.emp in
      let m2_m0 = SH.star [m2] m0 in
      let s2 = SH.LsS.star [s_ls] p0 in
      let nm = SH.star [m2] nm in
      let goal = {goal with mm= m2_m0; ss= s2; nm; pv= SH.emp} in
      sub goal (major_premiss vs ws xs_m_ws r0 maj_fk maj_bk) fk bk
    in

    let nonempty_minuend_ls_premiss m1 xs s1 min_fk min_bk maj_fk maj_bk =
      assert(true$> L.printf 3 "s_ls_ls: nonempty minuend ls premiss" );
      let vs = Vars.diff (SH.fv m1) us in
      let xs_m_vs = Vars.diff xs vs in
      let us_vs = Vars.union us vs in
      let m1_m0 = SH.star [m1] m0 in
      let p0 = SH.Pf.star [p0] SH.emp in
      let p0_s1 = SH.star [p0] s1 in
      let ws, xs_m_ws = Vars.inter_diff xs (SH.fv p0_s1) in
      let nm = SH.star [m1] nm in
      let ns = SH.star [p0_s1] ns in
      let goal = {goal with us= us_vs; mm= m1_m0; xs= xs_m_vs; ss= p0_s1; nm; ns; pv= SH.emp} in
      sub goal (empty_minuend_ls_premiss vs ws xs_m_ws p0 maj_fk maj_bk) min_fk min_bk
    in

    let long_case min_fk min_bk maj_fk maj_bk =
      assert(true$> L.printf 3 "s_ls_ls: longer subtrahend ls case" );
      let presuffix, arg = Args.remove dir m_arg s_arg in
      let len = E.mkZSub [|s_len; m_len|] in
      let s_m_ls = {s_ls with Ls.len; arg} in
      let m1 = SH.Pf.star [E.mkZLt E.zero m_len] SH.emp in
      let s1 = SH.Pf.star (Args.cycle_eqs presuffix)
              (SH.LsS.star [s_m_ls] SH.emp) in
      nonempty_minuend_ls_premiss m1 xs s1 min_fk min_bk maj_fk maj_bk
    in

    let short_case min_fk min_bk maj_fk maj_bk =
      assert(true$> L.printf 3 "s_ls_ls: shorter subtrahend ls case" );
      let s_len' = E.mkVar (Var.gensym "k" Var.IntegerSort) in
      let len = E.mkZSub [|m_len; s_len'|] in
      let _, pre, suf = Ls.split_on_fresh_point m_ls in
      let m_s_ls = {m_ls with Ls.len; arg= if dir then suf else pre} in
      let m1 = SH.Pf.star [E.mkZLt E.zero s_len'] (SH.LsS.star [m_s_ls] SH.emp) in
      let s1 = SH.Pf.star (E.mkEq s_len s_len'
                        :: equal_eqs (if dir then pre else suf) s_arg) SH.emp in
      nonempty_minuend_ls_premiss m1 xs s1 min_fk min_bk maj_fk maj_bk
    in

    let equal_case min_fk min_bk maj_fk maj_bk =
      assert(true$> L.printf 3 "s_ls_ls: equal length ls case" );
      let m1 = SH.Pf.star [E.mkZLt E.zero m_len] SH.emp in
      let s1 = SH.Pf.star (E.mkEq m_len s_len
                        :: equal_eqs m_arg s_arg) SH.emp in
      nonempty_minuend_ls_premiss m1 xs s1 min_fk min_bk maj_fk maj_bk
    in

    let empty_case min_fk min_bk maj_fk maj_bk =
      assert(true$> L.printf 3 "s_ls_ls: empty subtrahend ls case" );
      let goal = {goal with ss= s0} in
      sub_emp_ls_ goal s_ls p0 min_fk min_bk m p v sk maj_fk maj_bk
    in

    if not (ent_patn m_patn xs s_patn) then fail_ sk fk bk else

    (* Note: Is this premiss order optimal? *)
    if ent then
      (* maximize footprint of subtrahend when proving entailment *)
      let empty_case bk () =
        empty_case
          (* continue after rule if minor premiss fails or backtracks *)
          fk bk (* (fun () -> fk sk bk) *)
          (* continue after rule if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          fk bk
      in
      let short_case bk () =
        short_case
          (* continue with long case if minor premisses fail or backtrack *)
          (fun _sk _bk -> empty_case bk ()) (empty_case bk)
          (* continue with long case if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun bk _sk _bk -> empty_case bk ()) (fun _bk -> empty_case bk)
      in
      let long_case () =
        long_case
          (* continue with short case if minor premisses fail or backtrack *)
          (fun _sk bk -> short_case bk ()) (short_case bk)
          (* continue with short case if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun bk _sk _bk -> short_case bk ()) (fun _bk -> short_case bk)
      in
      long_case()
    else
      let long_case() =
        long_case
          (* continue after rule if minor premisses fail or backtrack *)
          fk bk (* (fun () -> fk sk bk) *)
          (* backtrack whole rule if major premiss fails or backtracks *)
          (* Note: verify conjecture that this is complete *)
          (fun _ _ _ -> bk()) (fun bk -> bk)
      in
      let short_case() =
        short_case
          (* continue with long case if minor premiss fails or backtracks *)
          (fun _ _ -> long_case()) long_case
          (* continue with long case if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _ _ _ -> long_case()) (fun _ -> long_case)
      in
      let equal_case() =
        equal_case
          (* continue with short case if minor premisses fail or backtrack *)
          (fun _ _ -> short_case()) short_case
          (* continue with short case if major premisses fail or backtrack *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _ _ _ -> short_case()) (fun _ -> short_case)
      in
      let empty_case() =
        empty_case
          (* continue with equal case if minor premiss fails or backtracks *)
          (fun _ _ -> equal_case()) equal_case
          (* continue with equal case if major premiss fails or backtracks *)
          (* Note: verify conjecture that committing here is incomplete *)
          (fun _ _ -> equal_case()) equal_case
      in
      empty_case()
  )
  in



(*============================================================================
                     Subtraction: Proof Search Algorithm
  ============================================================================*)

  (* stub *)
  let find_witnesses _ _ = S.empty
(*
  let find_witnesses {pm; mm; xs; ps; ss} s_loc =
    let xs_s_loc = Vars.inter xs (E.fv s_loc) in
    if Vars.is_empty xs_s_loc then
      S.empty
    else if not !prv_wbn then
      let es =
        Array.of_list (Vars.fold (fun x es -> E.mkVar x :: es) xs_s_loc []) in
      let car = Exps.union (SH.carrier mm) (SH.carrier ss) in
      let car =
        Exps.filter (fun e ->
          match E.sort_of e with
          | Var.OffsetSort | Var.ValueSort -> Vars.disjoint (E.fv e) xs
          | _                              -> false
        ) car in
      Pure.forced_equalities ~parts:[ps] pm E.compare es car
      &> L.printf 6 "found witnesses: %a" S.fmt
    else
      (* Note: Avoid this call to Pure.normalize by extracting witnesses from
         the proof from the preceding Pure.implies call? *)
      (* Note: Refactor: this is mostly copied from SymbolicHeap.choose_rep *)
      let cmp e f =
        let num_xs e = Vars.cardinal (Vars.inter xs (E.fv e)) in
        let ord = Pervasives.compare (num_xs e) (num_xs f) in
        if ord <> 0 then ord else E.compare e f in
      let choose_rep cls =
        Exps.fold (fun e m ->
          if cmp e m < 0 then e else m
        ) cls (Exps.choose cls)
      in
      let car = Exps.union (SH.carrier mm) (SH.carrier ss) in
      let car =
        Exps.filter (fun e ->
          match E.sort_of e with
          | Var.OffsetSort | Var.ValueSort -> Vars.disjoint (E.fv e) xs
          | _                              -> false
        ) car in
      let car =
        Vars.fold (fun x car -> Exps.add (E.mkVar x) car) xs_s_loc car in
      match Pure.normalize ~parts:[ps] pm choose_rep car with
      | None -> S.empty
      | Some(rep, _) ->
          Vars.fold (fun x ws ->
            let xexp = E.mkVar x in
            match S.tryfind xexp rep with
            | Some(xrep) when Vars.disjoint (E.fv xrep) xs -> S.add xexp xrep ws
            | _                                            -> ws
          ) xs_s_loc S.empty
          &> L.printf 6 "found witnesses: %a" S.fmt
*)
  in

  (* M ^ |S| |- e==f
     us,vs. Ws=ys * M \- xs-vs. e==f ^ S[e,Ws/f,ys] *> R
    -----------------------------------------------------
     us. M \- xs. S *> R

    where
     ys ⊆ xs
     vs = xs ∩ fv(Ws=ys)   (xs-vs = xs - fv(Ws=ys))
  *)
  let _find_provable_equality m_locs s_locs keep goal =
    let {pm; us; mm; xs; ps; ss; nm} = goal
    and keep = Lazy.force keep
    in
    match Pure.find_provable_equality ~parts:[ps] pm m_locs s_locs keep with
    | Pure.Equality(m_loc, s_loc) as eq ->
(*         L.printf 0 "proved: %a = %a" E.fmt_v m_loc E.fmt_v s_loc ; *)
        (* find witnesses of existentials in s_loc *)
        let witnesses = find_witnesses goal s_loc in
        (* conjoin equalities witnesses to minuend *)
        let goal =
          if S.is_empty witnesses then
            goal
          else
            let witness_b = S.to_exp witnesses in
            let vs = E.fv witness_b in
            let us = Vars.union us vs in
            let xs = Vars.diff xs vs in
            let mm = SH.Pf.star [witness_b] mm in
            let nm = SH.Pf.star [witness_b] nm in
            {goal with us; mm; xs; nm} in
        (* express subtrahend ito witnesses and m_loc for s_loc *)
        let witnesses = S.add s_loc m_loc witnesses in
        let ss = SH.Pf.star [E.mkEq s_loc m_loc] (SH.subst witnesses ss) in
        (eq, {goal with ss})
    | res ->
        (res, goal)
  in


  let find_syntactic_equality m_locs s_locs _ goal =
    try
      let loc = Exps.min_elt (Exps.inter m_locs s_locs) in
      (Pure.Equality(loc, loc), goal)
    with Not_found ->
      (Pure.Disjunctions([]), goal)
  in


  (* choose a rule instance to apply to subtract a pt *)
  let chs_pts find_equiv_and_witnesses ({mm; xs; ss} as goal) =
    assert(true$> L.printf 8 "chs_pts" );
    let m_locs = SH.may_allocs_stem mm in
    let s_locs = SH.PtS.may_allocs ss in
    let keep = lazy (
      let s_djs_xs =
        Vars.inter xs
          (SH.DjS.fold (fun dj vs ->
             Vars.union (Dj.fv dj) vs
           ) ss Vars.empty) in
      fun m_loc s_loc ->
        (* don't try trivial equalities, they have been handled already *)
           not (E.equal m_loc s_loc)
        (* don't try to match a subtrahend pt with existentials in common with
           the subtrahend djs with a minuend ls *)
        && (Vars.disjoint (Pt.fv (SH.PtS.find s_loc ss)) s_djs_xs
         || (match SH.find m_loc mm with SH.Pt _ -> true | _ -> false) )
    )
    in
    match find_equiv_and_witnesses m_locs s_locs keep goal with
    | Pure.Equality(loc,_), ({mm; ss} as goal) ->
        let s_pt = SH.PtS.find loc ss in
        (match SH.find loc mm with
        | SH.Pt(m_pt) ->
            assert(true$>
              L.printf 6 "chs_pts match m_pt:@[   %a@ |- %a@]"
                Pt.fmt m_pt Pt.fmt s_pt );
            commit (sub_pt_pt m_pt s_pt goal)
        | SH.Ls(m_ls) ->
            assert(true$>
              L.printf 6 "chs_pts match m_ls:@[   %a@ |- %a@]"
                Ls.fmt m_ls Pt.fmt s_pt );
            (* since sub_ls_pt considers the case where the minuend ls is
               empty, even if multiple minuend may-allocs are equal to loc,
               committing does not introduce incompleteness *)
            commit (sub_ls_pt (Ls.direction m_ls loc) m_ls s_pt goal)
        | _ -> failwith "chs_pts: case split needed?"
        )
    | Pure.Disjunctions(djs), _ ->
        assert(true$> if djs <> [] then
          L.printf 6 "chs_pts: case split: [%a]"
            (List.fmt ";@ " (fun ff efl ->
              Format.fprintf ff "[%a]"
                (List.fmt ";@ " (fun ff (e,f) ->
                  Format.fprintf ff "%a = %a" E.fmt e E.fmt f)) efl)) djs );
        let djs = List.fast_sort (List.compare_lex (fun _ _ -> 0)) djs in
        List.kfold (fun dj (next_dj : sub_rule) ->
          let locs =
            List.fold (fun (e,f) locs -> Exps.add e (Exps.add f locs))
              dj Exps.empty in
          Exps.kfold locs (fun loc (next_loc : sub_rule) ->
            try sub_split_ls (SH.LsS.find loc mm) $+ next_loc
            with Not_found -> next_loc
          ) next_dj
        ) djs fail__ goal
    | Pure.Inconsistent, _ ->
        assert(true$> L.printf 6 "chs_pts: inconsistent" );
        commit ((sub_inconsis_m true $+ sub_inconsis_s true) goal)
  in


  (* choose a rule instance to apply to subtract a ls *)
  (* Note: try sub_pt_ls with lists with universal lengths first *)
  let chs_lss find_equiv_and_witnesses ({mm; ss} as goal) =
    assert(true$> L.printf 8 "chs_lss" );
    let m_pt_locs = SH.PtS.may_allocs mm in
    let m_ls_locs = SH.LsS.may_allocs mm in
    let s_locs = SH.LsS.may_allocs ss in
    let keep = lazy (fun m_loc s_loc -> not (E.equal m_loc s_loc))
    in
    (* first try to find a matching pt *)
    match find_equiv_and_witnesses m_pt_locs s_locs keep goal with
    | Pure.Equality(loc,_), ({mm; ss} as goal) ->
        let s_ls = SH.LsS.find loc ss in
        let m_pt = SH.PtS.find loc mm in
        assert(true$>
          L.printf 6 "chs_lss match m_pt:@[   %a@ |- %a@]"
            Pt.fmt m_pt Ls.fmt s_ls );
        (* since sub_pt_ls considers the case where minuend lists are
           empty, even if multiple minuend may-allocs are equal to loc,
           committing does not introduce incompleteness *)
        commit (sub_pt_ls (Ls.direction s_ls loc) m_pt s_ls goal)
    | Pure.Inconsistent, _ ->
        assert(true$> L.printf 6 "chs_lss: Inconsistent" );
        (* chs_lss only called after sub_inconsis_m, so just backtrack here *)
        commit back_
    | Pure.Disjunctions([]), _ ->
        (* second try to find a matching ls *)
        (match find_equiv_and_witnesses m_ls_locs s_locs keep goal with
        | Pure.Equality(loc,_), ({mm; ss} as goal) ->
            let s_ls = SH.LsS.find loc ss in
            let m_ls = SH.LsS.find loc mm in
            assert(true$>
              L.printf 6 "chs_lss match m_ls:@[   %a@ |- %a@]"
                Ls.fmt m_ls Ls.fmt s_ls );
            (* sub_ls_ls is not complete if m_ls is longer than s_ls *)
            sub_ls_ls (Ls.direction s_ls loc) m_ls s_ls goal
        | Pure.Disjunctions([]), _ ->
            fail_
        | Pure.Disjunctions(_), _ ->
            failwith "chs_lss: case split needed"
        | Pure.Inconsistent, _ ->
            assert( L.warnf "chs_lss: spontaneous inconsistency" );
            commit back_
        )
    | Pure.Disjunctions(_), _ ->
        failwith "chs_lss: case split needed"
  in


  let distrib_pt goal =
    let {xs; ss} = goal in
    try
      let s_pt = SH.PtS.choose ss in
      assert(true$> L.printf 8 "distrib_pt considering:@ %a" Pt.fmt s_pt );
      let pt_xs = Vars.inter xs (Pt.fv s_pt) in
      SH.DjS.kfold ss (fun s_dj next_s_dj ->
        assert(true$> L.printf 8 "distrib_pt considering:@ %a" Dj.fmt s_dj );
        if Vars.intersect pt_xs (Dj.fv s_dj) then
          let s_dj' = Dj.map (fun dt -> SH.PtS.star [s_pt] dt) s_dj in
          let ss =
            SH.DjS.add s_dj' (SH.DjS.remove s_dj (SH.PtS.remove s_pt ss)) in
          assert(true$> L.printf 3 "applying distrib_pt:@ %a" Pt.fmt s_pt );
          commit (sub {goal with ss})
        else
          next_s_dj
      ) fail_
    with
      Not_found -> fail_
  in


  let distrib_ls goal =
    let {xs; ss} = goal in
    try
      let s_ls = SH.LsS.choose ss in
      assert(true$> L.printf 8 "distrib_ls considering:@ %a" Ls.fmt s_ls );
      let ls_xs = Vars.inter xs (Ls.fv s_ls) in
      SH.DjS.kfold ss (fun s_dj next_s_dj ->
        assert(true$> L.printf 8 "distrib_ls considering:@ %a" Dj.fmt s_dj );
        if Vars.intersect ls_xs (Dj.fv s_dj) then
          let s_dj' = Dj.map (fun dt -> SH.LsS.star [s_ls] dt) s_dj in
          let ss =
            SH.DjS.add s_dj' (SH.DjS.remove s_dj (SH.LsS.remove s_ls ss)) in
          assert(true$> L.printf 6 "distrib_ls:@ %a" Ls.fmt s_ls );
          commit (sub {goal with ss})
        else
          next_s_dj
      ) fail_
    with
      Not_found -> fail_
  in


  let chs_split_ls goal =
    let {mm; ss} = goal in
    if SH.DjS.is_empty ss then fail_ else
    SH.LsS.kfold mm (fun m_ls (next_m_ls : sub_rule) ->
      sub_split_ls m_ls $+ next_m_ls
    ) fail__ goal
  in


  let chs_distrib_m goal =
    let {mm} = goal in
    try
      let dj = SH.DjS.choose mm in
      assert(true$> L.printf 7 "chs_distrib_m considering:@ %a" Dj.fmt dj );
      commit (sub_distrib_m dj {goal with mm= SH.DjS.remove dj mm})
    with
      Not_found -> fail_
  in


  let chs_distrib_s goal =
    let {ss} = goal in
    try
      let dj = SH.DjS.choose ss in
      assert(true$> L.printf 7 "chs_distrib_s considering:@ %a" Dj.fmt dj );
      commit (sub_distrib_s dj {goal with ss= SH.DjS.remove dj ss})
    with
      Not_found -> fail_
  in


  (sub_inconsis_m false $+ sub_inconsis_s false $+
   sub_did $+ sub_emp $+ sub_true $+
   chs_pts find_syntactic_equality $+
   pure_valid (
   sub_inconsis_m true $+ (* sub_inconsis_s true $+ *)
   chs_lss find_syntactic_equality $+
(*    chs_pts find_provable_equality $+ *)
(*    chs_lss find_provable_equality $+ *)
   distrib_pt $+ distrib_ls $+
   chs_distrib_m $+ chs_split_ls $+ chs_distrib_s $+
   sub_emp_lss $+
   (* sub_inconsis_m true $+ *) sub_inconsis_s true
   )
  )

  goal



(*============================================================================
                                 Entry Points
  ============================================================================*)

(* Backtrack continuations are invalidated by making a new toplevel query.  If
   we e.g. created new Pure contexts for each query, we could support this.
   Deleting such contexts would get tricky. *)
let new_query, is_valid =
  let query_count = ref 0               (* counter for toplevel queries *)
  in
  let new_query kind ((us, mm, xs, ss) as umxs) =
    let pm = cxt in
    Pure.clear pm ;
    let ps = Pure.mk_partition pm in
    incr query_count ;
(*     assert(true$>( *)
      if !query_count = Config.prv_gen_test then
        (TestGenProver.gen_query (!query_count, kind, umxs); exit 2) ;
      if Config.prv_gen_test < 0 then
        (TestGenProver.gen_query (!query_count, kind, umxs)) ;
(*     )); *)
    let fmt_query = fmt ("( " ^ kind ^ " " ^ (string_of_int !query_count)) in
    let goal = {ent= false; us; mm; xs; ss; pm; ps;
                nm= mm; ns= ss; cc= SH.emp; pv= SH.emp}
    in
    (goal, (!query_count, kind, umxs), fmt_query)
  in
  let is_valid (query,_,_) =
    !query_count = query
  in
  (new_query, is_valid)


let subtract mm xs ss : result =
  Timer.start subtract_tmr ;
  let module ResMset = ImperativeSet.Make(struct
    type t = XSH.t

    (* Note: Which comparison should be used? *)
    let equal = XSH.equal
    let compare = XSH.compare
(*
    let equal = XSH.equal_coarse
    let compare = XSH.compare_coarse

    (* [r0 = r1] if [q * r0 -||- q * r1] *)
    let compare r0 r1 =
      let q1 = ss in
      let ord = XSH.compare_coarse r0 r1 in
      if ord = 0 then ord else
      let r0_q1 = XSH.star [SH.exists_intro Vars.empty q1] r0
      and r1_q1 = XSH.star [SH.exists_intro Vars.empty q1] r1 in
      let ord = XSH.compare_coarse r0_q1 r1_q1 in
      if ord = 0 then ord else
      let us = Vars.union (SH.fv mm) (Vars.diff (SH.fv ss) xs) in
      let r0_q1_xs, r0_q1 = XSH.exists_bind us r0_q1 in
      let r1_q1_xs, r1_q1 = XSH.exists_bind (Vars.union us r0_q1_xs) r1_q1 in
      (* guess the common existentials are their own witnesses *)
      let ws, xs, ys = Vars.diff_inter_diff r0_q1_xs r1_q1_xs in
      let us' = Vars.union us ys in
      match
        sub {us=us' ; mm= r0_q1; xs; ss= r1_q1}
          (fun r bk ->
             if XSH.is_empty r then
               let us' = Vars.union us ws in
               sub {us=us' ; mm= r1_q1; xs; ss= r0_q1}
                 (fun r bk -> if XSH.is_empty r then Some(r,bk) else bk ())
                 (fun _sk bk -> bk ())
                 (fun()-> None)
             else
               bk ())
          (fun _sk bk -> bk ())
          (fun()-> None)
      with
      | Some(_) -> 0
      | None -> ord

    let equal r0 r1 = 0 = compare r0 r1
*)
  end)
  in
  let mset = ResMset.create ()
  in
  sub_count := 0; saved_count := 0
  ;
  let xs, us_ss = Vars.inter_diff (SH.fv ss) xs in
  let us = Vars.union (SH.fv mm) us_ss in
  let goal, query, fmt_query = new_query "subtract" (us, mm, xs, ss) in
  let indent = L.latch_incf 1 "%a" fmt_query goal in
  (try
    sub goal
      (fun remainder bk ->
         if ResMset.mem mset remainder then (
           (* filter out duplicate remainders, proofs need not be unique *)
           assert(true$> L.printf 3 "subtract: duplicate remainder" );
           bk ()
         ) else (
           saved_count := !sub_count ;
           assert( not Config.check_prv || (
             (* Note that when points-tos have None contents (as the correct
                sort is unknown), valid proofs can fail to be checked if the
                contents of the points-to is the unique pointer to an allocated
                location. *)
             L.printf 8 "self-checking soundness (%i)" !sub_count ;
             let s_r = XSH.star [SH.exists_intro Vars.empty ss] remainder in
             (* intro and bind xs in order to eliminate them if possible *)
             let xs_s_r = XSH.exists_intro xs s_r in
             let xs_zs, s_r = XSH.exists_bind us xs_s_r in
             let pm = cxt_chk in
             Pure.clear pm;
             let ps = Pure.mk_partition pm in
             let goal =
               {goal with pm; xs= xs_zs; ps; ss= s_r; nm= mm; ns= s_r} in
             L.shift_verb 5 (fun()->
               ent goal) <> Unknown
               || L.warnf "Prover failed to self-check soundness"
           ));
           sub_count := !saved_count ;
           ResMset.add mset remainder ;
           let bk() =
             Timer.start subtract_tmr ;
             if not( is_valid query ) then
               failwith "invalid backtrack continuation" ;
             sub_count := !saved_count ;
             bk()
           in
           Success(remainder, bk)
         ))
      (fun _sk bk -> bk())
      (fun()-> Unknown)
  with exc -> TestGenProver.gen_query query ; raise exc)
  |>
  (let rec reset res =
    Timer.stop_report subtract_tmr
      (if Config.subtract_time >= 0. then Config.subtract_time else subtract_tmr.Timer.max)
      (L.printf 0 "subtract %4i (%3i) time:@ %12.6f  %12.6f sec"
         (fst3 query) !sub_count) ;
    match res with
    | Unknown ->
        L.resetf 1 indent ") subtract: (%i) failed" !sub_count ;
        Unknown
    | Success(r,k) ->
        L.resetf 1 indent ") subtract: (%i) %a" !sub_count XSH.fmt r ;
        let k() =
          (try k() with exc -> TestGenProver.gen_query query ; raise exc)
          |> reset in
        Success(r,k)
  in reset)


let subtract_with_proviso pred mm xs ss =
  let rec find_fst k =
    match k() with
    | Unknown as f                  -> f
    | Success(r,_) as s when pred r -> s
    | Success(_,k')                 -> find_fst k'
  in
  find_fst (fun()-> subtract mm xs ss)


let entails mm xs ss =
  Timer.start entails_tmr ;
  sub_count := 0 ;
  let xs, us_ss = Vars.inter_diff (SH.fv ss) xs in
  let us = Vars.union (SH.fv mm) us_ss in
  let goal, query, fmt_query = new_query "entails" (us, mm, xs, ss) in
  L.incf 1 "%a" fmt_query goal ;
  (try
    match
      ent {goal with ent=true}
(* Version that only tries ent if sub first succeeds:
      sub goal
        (fun rr bk ->
          if XSH.is_empty rr then
            Success(rr,bk)
          else (
            Pure.clear pm ;
            ent {goal with ent=true; ps = Pure.mk_partition pm}
          ))
        (fun _sk bk -> bk())
        (fun()->
          assert (
            Pure.clear pm ;
            ent {goal with ent=true; ps= Pure.mk_partition pm} = Unknown
          ) ;
          Unknown) *)
    with
    | Success(rr,_) -> Some(rr)
    | Unknown -> None
  with exc -> TestGenProver.gen_query query ; raise exc)
  &> fun r ->
  Timer.stop_report entails_tmr
    (if Config.entails_time >= 0. then Config.entails_time else entails_tmr.Timer.max)
    (L.printf 0 "entails  %4i (%3i) time:@ %12.6f  %12.6f sec"
       (fst3 query) !sub_count) ;
  let sub_count = !sub_count in
(*   assert ( *)
(*     (* check subtraction is at least as strong as entailment *) *)
(*     r = None || *)
(*     ( Pure.clear goal.pm ; *)
(*       sub {goal with ps= Pure.mk_partition goal.pm} *)
(*      (fun rr bk -> Success(rr,bk)) (fun _sk bk -> bk()) (fun()-> Unknown) *)
(*       <> Unknown ) *)
(*   ) ; *)
  L.decf 1 ") entails: (%i) %a" sub_count (Option.fmt "failed" XSH.fmt) r


let entailsx p q =
  let xs, p' = XSH.exists_bind (XSH.fv q) p in
  let ys, q' = XSH.exists_bind (SH.fv p') q in
  match entails p' ys q' with
  | Some(r) -> Some(XSH.exists_intro (Vars.union xs ys) r)
  | None -> None


let inconsistent sh =
  let pm = cxt in
  Pure.clear pm;
  assert(
    let us = SH.fv sh in
    let xs = Vars.empty in
    let goal, _, fmt_query = new_query "inconsistent" (us, sh, xs, SH.ff) in
    let goal = {goal with ent= false} in
    L.incf 2 "%a" fmt_query goal ;
    true
  );
  let pm = Pure.extend pm in
  let _, c, d = SH.pure_consequences sh in
  Pure.conjoin pm (E.mkAnd [|c; d|]) ;
  let inconsis = Pure.inconsistent inconsistent_tmr pm in
  assert(
    L.decf 2 ") inconsistent: %B" inconsis ;
    true
  );
  inconsis


let inconsistentx xsh =
  let _, sh = XSH.exists_bind Vars.empty xsh in
  inconsistent sh


(*============================================================================
                              Debugging Wrappers
  ============================================================================*)

let subtract_count mm xs ss =
  let count = ref 0 in
  let rec drain (res : result) =
    match res with
    | Success(_,k) -> incr count; drain (k())
    | Unknown      -> !count in
  drain (subtract mm xs ss)
