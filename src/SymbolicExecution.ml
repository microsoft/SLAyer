(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Symbolic Execution of commands on symbolic heaps *)

open Library

open Type
open Variable
module HC = HashCons
open Expression
module S = Substitution
open SymbolicHeap
open Program
module I = Inst

include Interproc_sig

module L = (val Log.std Config.vSE : Log.LOG)


(* Timing =================================================================== *)

let normalize_tmr = Timer.create "SymbolicExecution.SH.normalize"

module SH = struct include SH
  let normalize xsh =
    Timer.start normalize_tmr ;
    let res = normalize xsh in
    Timer.stop normalize_tmr ;
    res
end

module XSH = struct include XSH
  let normalize xsh =
    Timer.start normalize_tmr ;
    let res = normalize xsh in
    Timer.stop normalize_tmr ;
    res
end


(* Formatting =============================================================== *)

let xs_sh_fmt ff xs_sh =
  let norm_fmt ff xs_sh =
    let xs, sh = (* SH.normalize *) xs_sh in
    Format.fprintf ff "%a%a"
      (Vars.fmt_embrace "@[<hov 2>? " " .@]@ ") xs SH.fmt sh in
  Format.fprintf ff "@[<hv>%a@]" norm_fmt xs_sh



(*============================================================================
                               Generic Commands
  ============================================================================*)

(*   call[xs'/xs][gs'/gs] \ gs,ys'. pre[ys'/ys] ~> frame
   ----------------------------------------------------------------------------
    {?xs.call} !gs.{?ys.pre}{post} ~> {(?gs.post * ?xs',ys',ms.frame)[gs/gs']}

   If global is set, then frame must be empty and
     ?gs,ys'.pre[ys'/ys] * frame |- abs( ?xs'.call[xs'/xs][gs'/gs] )
*)
let exec_gc global cxt call gs ys_pre ms =
  (* freshen existentials of pre wrt call *)
  let ys', pre = XSH.exists_bind (Vars.union cxt (XSH.fv call)) ys_pre
  in
  L.printf 6 "@[<hov 2>pre freshened:@ %a@]" xs_sh_fmt (ys',pre)
  ;
  (* freshen existentials of call wrt gs and ys' *)
  let gs_ys' = Vars.union gs ys' in
  let xs', call = XSH.exists_bind (Vars.union cxt gs_ys') call
  in
  L.printf 6 "@[<hov 2>call freshened:@ %a@]" xs_sh_fmt (xs',call)
  ;
  (* rename ghosts appearing in pre (which includes ys') *)
  let call, _, gs_to_gs', gs'_to_gs =
    SH.rename_vs (Vars.inter gs (SH.fv pre)) call
  in
  L.printf 6 "@[<hov 2>call freshened ghosts:@ %a@ %a@]" S.fmt gs_to_gs' SH.fmt call
  ;
  (* now have input well-formed arguments for subtract, try to find a frame *)
  let tryfind_frame call zs pre =
    if global then
      Prover.entails call zs pre
    else
      match Prover.subtract call zs pre with
      | Prover.Unknown -> None
      | Prover.Success(frame,_) -> Some(frame)
  in
  match tryfind_frame call gs_ys' pre with
  | None -> None
  | Some(frame) ->
      L.printf 6 "@[<hov 2>frame:@ %a@]" XSH.fmt frame
      ;
      if global &&
        let pre_frame = XSH.star [ys_pre] frame
        in
        let abs_call,_ = Abstraction.abstract (SH.exists_intro xs' call)
        in
        None = Prover.entailsx pre_frame abs_call
      then None
      else
      (* existentially quantify the modified vars out of frame, lift star
         between pre and frame above existential on ys', and eliminate the
         existential on xs' *)
      let frame = XSH.exists_intro (Vars.union (Vars.union xs' ys') ms) frame
      in
      let post_to_retn post =
        (* reconjoin frame *)
        let retn = XSH.star [post] frame
        in
        L.printf 6 "@[<hov 2>post:@ %a@]" XSH.fmt post ;
        L.printf 6 "@[<hov 2>frame:@ %a@]" XSH.fmt frame ;
        L.printf 6 "@[<hov 2>post * frame:@ %a@]" XSH.fmt retn
        ;
        (* existentially quantify the ghosts *)
        let retn' = XSH.exists_intro gs retn
        in
        L.printf 6 "@[<hov 2>w/o ghosts:@ %a%a@]" (Vars.fmt_embrace "" "@ ") gs XSH.fmt retn'
        ;
        (* reinstate the shadowed ghosts *)
        let retn = XSH.subst gs'_to_gs retn'
        in
        let retn = if Prover.inconsistentx retn then XSH.ff else retn
        in
        L.printf 6 "@[<hov 2>return:@ %a@]" XSH.fmt retn
        ;
        retn
      in
      Some(post_to_retn)



(*============================================================================
                              Primitive Commands
  ============================================================================*)

exception Error


(** [subtractx us q xs f] returns [(ys,r)] such that [q |- ?ys. f * r]. *)
let subtractx cxt q xs footprint =
  let vs, q' = XSH.exists_bind (Vars.union cxt xs) q in
  match L.shift_verb 3 (fun()->
    Prover.subtract q' xs footprint
  ) with
  | Prover.Success(remainder, k) ->
      assert( L.shift_verb 10 k = Prover.Unknown
              || failwithf "ERROR: multiple frames for concretization" );
      let ys = Vars.union vs xs in
      (ys, remainder)
  | Prover.Unknown ->
      raise Error


let subtract_field cxt q ys loc cnt =
  let off_v = Var.gensym "off" Var.OffsetSort in
  let off = Off.mkVar off_v in
  let ys = Vars.add off_v ys in
  let pt = {Pt.loc; off; cnt} in
  let xs, rem = subtractx cxt q ys (SH.PtS.star [pt] SH.emp) in
  (xs, pt, rem)


(** [subtract_object cxt q loc] checks that [loc] is a pointer to the base of
    an object and removes the object:

     q |- ?zs,o. l(@o)->_ * p
     p |- o==(t)0
     q |- ?xs. *_{f in t} l(@f)->_ * r
   ------------------------------------
    { q } free(l) ~> { ?xs. r }         *)

exception Found of Typ.t

let subtract_base_field cxt q loc =
  let zs, {Pt.off}, rem = subtract_field cxt q Vars.empty loc None in
  let xs, rem = XSH.exists_bind (Vars.union zs cxt) rem in
  let xs = Vars.union xs zs in
  let xs, rem = SH.normalize (xs, rem) in
  let off_eqc = SH.Pf.class_of rem (off :>Exp.t) in
  let typ =
    try
      Exps.iter (fun o ->
        match Off.is_base (Off.mk o) with
        | Some(typ) -> raise (Found(typ))
        | None -> ()
      ) off_eqc ;
      raise Error
    with
      Found(typ) -> typ in
  (typ, xs, rem)

let subtract_nonbase_fields cxt q loc new_size typ xs rem =
  let paths = Typ.all_offsets typ in
  if paths = [] then
    (Vars.empty, SH.exists_intro xs rem)
  else (
    (match new_size with
    | Some(n) when n > Typ.sizeof(typ) ->
        L.printf 0 "attempted cast from size %Li to %Li" (Typ.sizeof(typ)) n ;
        if not Config.trust_casts then raise Error
    | _ -> ()
    );
    let obj =
      SH.PtS.star
        (List.map (fun (_,path, _fld_typ) ->
          {Pt.loc= Exp.mkAdds loc path; off= Off.mkPath typ path; cnt= None}
         ) paths) SH.emp in
    (* The postcondition computed for free is not strongest, so
       normalize to propagate pure consequences of l->_ before
       removing it.  This produces stronger, but still not strongest,
       postconditions. *)
    (* Note: revisit this question *)
    let q = XSH.normalize q in
    subtractx cxt q Vars.empty obj
  )


let subtract_object cxt q loc new_size =
  let typ, xs, rem = subtract_base_field cxt q loc in
  subtract_nonbase_fields cxt q loc new_size typ xs rem


(* Trim access paths to the longest array-indexing-free prefix. *)
let rec remove_idx e =
  let open Exp in let open HC in
  match desc e with
  | App({desc= App({desc= Idx},_)},e) ->
      remove_idx e
  | App(f,e) ->
      let e' = remove_idx e in
      if Exp.equal e e' then
        mkApp f e'
      else
        e'
  | _ ->
      e


let rec exec_inst cxt i q =
  match i with
  | I.Assume(b) ->
      (* --------------------------
          {q} assume(b) ~> {b * q}  *)
      let q' = XSH.Pf.star [b] q in
      if Prover.inconsistentx q'
      then XSH.ff
      else q'

  | I.Assert(b) ->
      (*   q |- b
         ----------------------
          {q} assert(b) ~> {q}  *)
      if Prover.entailsx q (XSH.Pf.star [b] XSH.tt) = None
      then raise Error
      else q

  | I.Kill(vs) ->
      (* ------------------------------
          {q} v := nondet() ~> {?v. q}  *)
      XSH.exists_intro vs q

  | I.Move(v,e) ->
      (* ------------------------------------------
          {q} v := e ~> {?v'. v=e[v'/v] * q[v'/v]}  *)
      let q', v', v_to_v', _ = XSH.rename_vs (Vars.singleton v) q in
      let e' = S.subst v_to_v' e in
      XSH.exists_intro v' (XSH.Pf.star [Exp.mkEq (Exp.mkVar v) e'] q')

  | I.Load(v,loc) ->
      (*   q |- ?xs. l(@o)->c * r
         -------------------------------------------------------------
          {q} v := [l] ~> {?v',xs. v==c[v'/v] * (l(@o)->c * r)[v'/v]}  *)
      let loc' = remove_idx loc in
      if Exp.equal loc loc' then
        let cnt = Var.gensym "cnt" (Var.sort v) in
        let ys = Vars.singleton cnt in
        let cnt = Exp.mkVar cnt in
        let xs, pt, rem = subtract_field cxt q ys loc' (Some(cnt)) in
        let pt_rem = XSH.PtS.star [pt] rem in
        let pt_rem, v', v_to_v', _ = XSH.rename_vs (Vars.singleton v) pt_rem in
        let xs = Vars.union (Vars.remove v xs) v' in
        let cnt = S.subst v_to_v' cnt in
        let eq_pt_rem = XSH.Pf.star [Exp.mkEq (Exp.mkVar v) cnt] pt_rem in
        XSH.exists_intro xs eq_pt_rem
      else
        let _ = subtract_field cxt q Vars.empty loc' None in
        XSH.exists_intro (Vars.singleton v) q

  | I.Store(loc,cnt) ->
      (*   q |- ?xs. l(@o)->_ * r
         -------------------------------------
          {q} [l] := c ~> {?xs. l(@o)->c * r}  *)
      let loc' = remove_idx loc in
      let xs, pt, rem = subtract_field cxt q Vars.empty loc' None in
      let cnt = if Exp.equal loc loc' then Some(cnt) else None in
      let pt_rem = XSH.PtS.star [{pt with Pt.cnt}] rem in
      XSH.exists_intro xs pt_rem

  | I.Alloc(v,e) ->
      (* ---------------------------------------
          {q} alloc(v) ~> {?v'. v->_ * q[v'/v]}  *)
      let q', v', _, _ = XSH.rename_vs (Vars.singleton v) q in
      let loc = Exp.mkVar v in
      let off =
        match Exp.desc e with
        | Exp.Num(n) -> Off.mkPath (Typ.mkArray (Typ.mkInt false 1) (Some(n)) n) []
        | _          -> Off.mkPath Typ.mkTop [] in
      let cnt = None in
      XSH.exists_intro v' (XSH.PtS.star [{Pt.loc; off; cnt}] q')

  | I.Free(loc) ->
      (try
        let xs, rem = subtract_object cxt q loc None in
        XSH.exists_intro xs rem
      with Error ->
        let loc_eq_nil = XSH.Pf.star [Exp.mkEq loc Exp.nil] XSH.tt in
        if Prover.entailsx q loc_eq_nil <> None then
          (*   q |- l==0
             --------------------
              {q} free(l) ~> {q}  *)
          q
        else
          raise Error
      )
  | I.Cast(v,ty,loc) ->
      let c, q' =
        match Exp.convert (Var.sort v) loc with
        | None ->
            (I.Kill(Vars.singleton v), q)
        | Some(loc) ->
            let q' =
              match Typ.desc ty with
              | Typ.Pointer({HC.desc= Typ.Top}) ->
                  q
              | Typ.Pointer(typ) ->
                  (try
                    let old_typ, xs, rem = subtract_base_field cxt q loc in
                    if Typ.equal old_typ typ then
                      q
                    else
                      let xs, rem = subtract_nonbase_fields cxt q loc (Some(Typ.sizeof typ)) old_typ xs rem in
                      let paths = Typ.all_offsets typ in
                      let xs, pts =
                        if paths = [] then
                          let off = Off.mkPath Typ.mkTop [] in
                          let cnt = None in
                          (xs, [{Pt.loc; off; cnt}])
                        else
                          List.fold (fun (_,path, fld_typ) (xs, pts) ->
                            let loc = Exp.mkAdds loc path in
                            let off = Off.mkPath typ path in
                            let cnt_sort =
                              match Typ.desc fld_typ with
                              | Typ.Pointer _ -> Var.PointerSort
                              | _             -> Var.IntegerSort in
                            let cnt_v = Var.gensym "cnt" cnt_sort in
                            let cnt = Some(Exp.mkVar cnt_v) in
                            (Vars.add cnt_v xs, {Pt.loc; off; cnt} :: pts)
                          ) paths (xs, []) in
                      XSH.exists_intro xs (XSH.PtS.star pts rem)
                  with Error -> q)   (* attempted to cast an invalid/NULL pointer   *)
              | _ -> q               (* attempted cast to a non-pointer type *)
            in
            (I.Move(v, loc), q')
      in
      exec_inst cxt c q'

  | I.Nop ->
      q

  | I.Generic({I.ghosts; pre; insts=[]; post}) ->
      (* PS#297: ms would be the modified vars if generic commands had them *)
      let ms = Vars.empty in
      (match exec_gc false cxt q ghosts pre ms with
      | Some(post_to_retn) -> post_to_retn post
      | None -> raise Error
      )

  | I.Generic({I.ghosts=_ghosts; pre=_pre; insts=_cc; post=_post}) ->
      (* Implement the proof rule:

         q |- pre*R
         post(pre,cc) = post'
         post' |- post
         ------------------------------
         {{q}} {pre}cc{post} {{post*R}}

         That is,
         1. Assert pre. That is, check that q |- pre*R.
         2. Compute the post {pre}cc{post'}.
         3. Check that post' |- post.
         4. Assume post. That is, return post*R.

         All subject to: avoiding name clashes, framing, etc.
      *)
      failwith "SymExec(Generic({p}c{q})) not implemented"


let exec_inst cxt {I.desc} = function
  | None ->
      None
  | Some(q) ->
      try Some(exec_inst cxt desc q)
      with Error -> None



(*============================================================================
                                  Procedures
  ============================================================================*)

(** Ghosts encapsulates the heuristic for choosing which ghost variables get
    existentially quantified in procedure preconditions. *)
module Ghosts : sig
  type t
  val create : Prog.t -> t
  val add_for : t -> Vars.t -> S.t * S.t * Vars.t
end = struct

  type t = Vars.t list ref

  let create _ = ref []

  (** [add_for t vs = (s,os)] such that [s] renames [vs] to fresh variables
      and [os] are variables from [t] that now should be existential. *)
  let add_for vsl vs =
    let vs_to_gs, gs_to_vs, gs =
      Vars.fold (fun v (vs_to_gs, gs_to_vs, gs) ->
         let g = Var.gensym (Var.name v) (Var.sort v) in
         let g' = Exp.mkVar g in
         let v' = Exp.mkVar v in
         (S.add v' g' vs_to_gs, S.add g' v' gs_to_vs, Vars.add g gs)
      ) vs (S.empty, S.empty, Vars.empty)
    in
    (* keep the limit_ghosts most recent sets of ghosts *)
    let vsl', to_project =
      if 0 > Config.limit_ghosts || List.length !vsl < Config.limit_ghosts then
        (gs :: !vsl, Vars.empty)
      else match List.rev !vsl with
      | [] ->
          ([], gs)
      | last :: vsl_rev ->
          (gs :: List.rev vsl_rev, last)
    in
    if not (Vars.is_empty to_project) then
      L.printf 6 "@[<hov 2>losing ghosts: %a@]" Vars.fmt to_project
    ;
    vsl := vsl'
    ;
    (vs_to_gs, gs_to_vs, to_project)

end


(* representation state for the symbolic heaps domain *)

type t = XSH.t option

let compare = Option.compare XSH.compare
let equal = Option.equal XSH.equal

let fmt ff q = Option.fmt "ERROR" XSH.fmt ff q


type r = Prog.t * Ghosts.t
let create pgm = (pgm, Ghosts.create pgm)



(** For a procedure call \{call\} p(A), [footprint call p A] returns a
    subheap of [call] that over-approximates the footprint of the call. *)
let footprint call callee actuals =
  if not Config.optimize_frame then (call, XSH.emp) else
  Frame.footprint call callee actuals



let call_to_entry (_prog, ghosts) call ({Call.proc; actuals} as pcall) =
  let {Proc.formals; freturn; locals} = proc in
  match call with
  | None -> (None, fun _post -> None)
  | Some(call)
  ->
  (* over-approximate the call's footprint *)
  let pre, frame = footprint call proc actuals
  in
  L.printf 6 "@[<hov 2>footprint:@ %a@]" XSH.fmt pre
  ;
  (* add ghosts for the shadowed formals and locals *)
  let frmls_lcls =
    Vars.union (Vars.of_list formals) locals
  in
  let shdws =
    Vars.inter frmls_lcls (Vars.union (Vars.of_list actuals) (XSH.fv pre))
  in
  let shdws_to_ghsts, ghsts_to_shdws, olds = Ghosts.add_for ghosts shdws
  in
  (* rename the shadowed formals and locals *)
  let actuals =
    List.map (fun a ->
      match Exp.desc (S.subst shdws_to_ghsts (Exp.mkVar a)) with
      | Exp.Var(v) -> v
      | _ -> failwith "frmls_to_actls must be a variable renaming"
    ) actuals
  in
  let pre = XSH.subst shdws_to_ghsts pre
  in
  L.printf 6 "@[<hov 2>actuals, pre renamed shadowed formals & locals:@ %a@ %a@ %a@]"
    S.fmt shdws_to_ghsts (List.fmt ",@ " Var.fmt) actuals XSH.fmt pre
  ;
  (* existentially quantify the old ghosts *)
  let pre = XSH.exists_intro olds pre
  in
  (* generalize *)
  let pre, junk = Abstraction.abstract pre
  in
  (* add any junk to the frame *)
  let frame = if not junk then frame else XSH.Jnk.star frame
  in
  L.printf 6 "@[<hov 2>pre generalized:@ %a@]" XSH.fmt pre
  ;
  let frmls_to_actls, _actuals = Call.args pcall in
  let actls_to_frmls = S.fold (fun f a s -> S.add a f s) frmls_to_actls S.empty
  in
  (* express pre in terms of formals *)
  let pre = XSH.subst actls_to_frmls pre
  in
  L.printf 6 "@[<hov 2>pre ito formals:@ %a@ %a@]" S.fmt actls_to_frmls XSH.fmt pre
  ;
  let frmls_to_actls_n_ghsts_to_shdws =
    S.compose frmls_to_actls ghsts_to_shdws
  in
  let post_to_retn post =
    L.printf 5 "@[<hov 2>post:@ %a@]" XSH.fmt post
    ;
    (* express in terms of actuals and reinstate the shadowed formals and locals *)
    L.printf 6 "@[<hv>frmls_to_actls: %a@ ghsts_to_shdws: %a@ composition: %a@]"
      S.fmt frmls_to_actls S.fmt ghsts_to_shdws S.fmt frmls_to_actls_n_ghsts_to_shdws
    ;
    let retn = XSH.subst frmls_to_actls_n_ghsts_to_shdws post
    in
    let retn = XSH.exists_intro (Option.fold Vars.add freturn Vars.empty) retn
    in
    L.printf 5 "@[<hov 2>post ito actuals & shadows:@ %a@]" XSH.fmt retn
    ;
    (* reconjoin frame *)
    L.printf 6 "@[<hov 2>frame:@ %a@]" XSH.fmt frame
    ;
    let retn = XSH.star [retn] frame
    in
    let retn = if Prover.inconsistentx retn then XSH.ff else retn
    in
    L.printf 5 "@[<hov 2>return:@ %a@\n@]" XSH.fmt retn
    ;
    retn
  in
  (Some(pre), Option.map post_to_retn)



let exit_to_retn {Proc.locals} exit =
  match exit with
  | None -> None
  | Some(exit)
  ->
  L.printf 6 "@[<hov 2>out-scoping locals: @[%a@]@]" Vars.fmt locals;
  (* quantify the locals *)
  Some(XSH.exists_intro locals exit)



let adapted_pre_substate_call_syntactic _ _cxt pre call ({Call.proc; actuals} as pcall) =
(* Notes: *)
  (* - Here we should introduce ghosts just like in _logical, and factor
     out common code between _syntactic and _logical. *)
  (* - There is redundancy here, call will be abstracted again for every
     pre.  Reorder the arguments and partially apply in interproc?  Also,
     after calling abstract here, ATS.widen will also call it on the same
     formula. *)
  let {Proc.freturn} = proc in
  match pre, call with
  | None     , _          -> Some(fun _post -> None)
  | _        , None       -> None
  | Some(pre), Some(call)
  ->
  let call, junk = Abstraction.abstract call
  in
  assert( not junk )
  ;
  let foot, frame = footprint call proc actuals
  in
  L.printf 5 "@[<hov 2>footprint:@ %a@]" XSH.fmt foot
  ;
(*   let frame = lazy ( *)
(*     XSH.emp *)
(*     (* Note: For stronger instrumentation, either extend footprint to also *)
(*        return the frame, or use subtraction as follows: *)
(*     let zs, foot = XSH.exists_bind Vars.empty foot in *)
(*     let ws, call = XSH.exists_bind (Vars.diff (SH.fv foot) zs) call in *)
(*     (* below we fix zs in foot, so above zs were permitted to appear in call *) *)
(*     match Prover.subtract call Vars.empty foot with *)
(*     | None -> failwith "SymbolicExecution: footprint unsound" *)
(*     | Some(frame,_) -> *)
(*         L.printf 5 "@[<hov 2>frame:@ %a@]" XSH.fmt frame *)
(*         ; *)
(*         XSH.exists_intro ws frame *) *)
(*   ) in *)
  (* express pre in terms of actuals *)
  let frmls_to_actls, _actuals = Call.args pcall in
  let pre = XSH.subst frmls_to_actls pre
  in
  L.printf 6 "@[<hov 2>pre substituted actuals/formals:@ %a@ %a@]"
    S.fmt frmls_to_actls XSH.fmt pre
  ;
  match XSH.equivalent foot pre with
  | None -> None
  | Some(witnesses) ->
      L.printf 6 "@[<hov 2>witnesses:@ %a@]" S.fmt witnesses
      ;
      (* conjoin witnesses to frame *)
      let frame = lazy (
        let frame = XSH.Pf.star [S.to_exp witnesses] frame
        in
        L.printf 6 "@[<hov 2>frame and witnesses:@ %a@]" XSH.fmt frame
        ;
        frame
      ) in
      let post_to_retn post =
        L.printf 5 "@[<hov 2>post:@ %a@]" XSH.fmt post
        ;
        (* express in terms of actuals *)
        let post = XSH.subst frmls_to_actls post
        in
        let post = XSH.exists_intro (Option.fold Vars.add freturn Vars.empty) post
        in
        L.printf 6 "@[<hov 2>ito actuals:@ %a@ %a@]" S.fmt frmls_to_actls XSH.fmt post
        ;
        (* reconjoin frame *)
        let retn = XSH.star [post] (Lazy.force frame)
        in
        let retn = if Prover.inconsistentx retn then XSH.ff else retn
        in
        L.printf 5 "@[<hov 2>post star frame:@ %a@]" XSH.fmt retn
        ;
        retn
      in
      Some(Option.map post_to_retn)



let adapted_pre_substate_call_logical ({Prog.globals}, _ghosts) cxt pre call ({Call.proc; actuals} as pcall) =
  let {Proc.formals; freturn; modifs} = proc in
  match pre, call with
  | None     , _          -> Some(fun _post -> None)
  | _        , None       -> None
  | Some(pre), Some(call)
  ->
  L.printf 6 "@[<hov 2>pre:@ %a@]" XSH.fmt pre
  ;
  let fv_pre = XSH.fv pre in
  let fs = Vars.of_list formals
  in
  let gs =
    if Config.optimize_frame then
      Vars.diff fv_pre
        (Vars.union (Vars.union fs modifs) globals)
    else
      (* Only vars in the footprint are given ghosts, whether or not procedure
         calls are localized, since the prover isn't supposed to be able to
         handle the existentials that arise from trying to instantiate ghosts
         from outside the footprint. *)
      let footprint, _ = Frame.footprint pre proc actuals in
      Vars.diff (XSH.fv footprint)
        (Vars.union (Vars.union fs modifs) globals)
  in
  L.printf 6 "@[<hov 2>ghosts:@ [%a]@]" Vars.fmt gs
  ;
  let post_image vs subst =
    Vars.fold (fun v -> Vars.union (Exp.fv (S.subst subst (Exp.mkVar v))))
      vs Vars.empty
  in
  (* rename ghosts in call and actuals that appear in actuals *)
  let frmls_to_actls, _actuals = Call.args pcall in
  let fs_appear = Vars.inter fv_pre fs in
  let fv_actls = post_image fs_appear frmls_to_actls
  in
  let call, _, gas_to_gas', gas'_to_gas =
    XSH.rename_vs (Vars.inter gs fv_actls) call
  in
  let frmls_to_actls = S.compose frmls_to_actls gas_to_gas'
  in
  L.printf 6 "@[<hov 2>call renamed actuals ghosts:@ %a@ %a@]" S.fmt gas_to_gas' XSH.fmt call
  ;
  (* express pre in terms of actuals *)
  let pre = XSH.subst frmls_to_actls pre
  in
  L.printf 6 "@[<hov 2>pre substituted actuals/formals:@ %a@ %a@]"
    S.fmt frmls_to_actls XSH.fmt pre
  ;
  let global = Config.precondition_order <> Config.WeakerSubheap
  in
  match exec_gc global cxt call gs pre modifs with
  | None -> None
  | Some(post_to_retn) ->
      let post_to_retn post =
        L.printf 5 "@[<hov 2>post:@ %a@]" XSH.fmt post
        ;
        (* express in terms of actuals *)
        let post = XSH.subst frmls_to_actls post
        in
        let post = XSH.exists_intro (Option.fold Vars.add freturn Vars.empty) post
        in
        L.printf 6 "@[<hov 2>ito actuals:@ %a@ %a@]" S.fmt frmls_to_actls XSH.fmt post
        ;
        let retn' = post_to_retn post
        in
        (* reinstate the shadowed ghosts in actuals *)
        let retn = XSH.subst gas'_to_gas retn'
        in
        L.printf 5 "@[<hov 2>return:@ %a@]" XSH.fmt retn
        ;
        retn
      in
      Some(Option.map post_to_retn)



let adapted_pre_substate_call r =
  match Config.precondition_order with
  | Config.Syntactic -> adapted_pre_substate_call_syntactic r
  | _ -> adapted_pre_substate_call_logical r



let rec value_of_ sh exp =
  L.incf 100 "( value_of_: %a@ %a" Exp.fmt exp SH.fmt sh ;
  L.decf 100 ") value_of_: @[[%a]@]" (fun ff -> function
    | None -> ()
    | Some(is) -> List.fmt ";@ " Format.pp_print_int ff (IntSet.to_list is))
  <&
  (* find a value to which sh equates exp *)
  match
    Exps.fold (fun e value ->
      if value <> None then
        value
      else
        match Exp.desc e with
        | Exp.Num(i) -> Some(Int64.to_int i)
        | _ -> value
    ) (SH.Pf.class_of sh exp) None
  with
  | Some(value) ->
      Some(IntSet.singleton value)
  | None ->
      (* find a disjunction in which each disjunct equates exp *)
      SH.DjS.fold (fun dj opt_value ->
        if opt_value <> None then
          opt_value
        else
          Dj.fold (fun dt opt_values ->
            match opt_values with
            | None -> None
            | Some(values) ->
                match value_of_ dt exp with
                | None -> None
                | Some(vals) -> Some(IntSet.union vals values)
         ) dj (Some(IntSet.empty))
      ) sh None


let value_of xsh exp =
  let xsh = XSH.normalize xsh in
  let _xs, sh = XSH.exists_bind Vars.empty xsh in
  value_of_ sh exp


let resolve_indirect_call ({Prog.procs}, _ghosts) q loc _ftyp =
  L.incf 5 "( resolve_indirect_call: %a@ %a" Exp.fmt loc fmt q ;
  L.decf 5 ") resolve_indirect_call: @[[%a]@]" (List.fmt ";@ " Proc.Id.fmt)
  <&
  match q with
  | None ->
      []
  | Some(q) ->
      (* Note: review using Vars.empty here *)
      let cxt = Vars.empty in
      let cnt = Var.gensym "cnt" Var.IntegerSort in
      let ys = Vars.singleton cnt in
      let cnt = Exp.mkVar cnt in
      let cxt = Vars.union ys cxt in
      try
        let _xs, _pt, rem = subtract_field cxt q ys loc (Some(cnt)) in
        match value_of rem cnt with
        | Some(callee_nums) ->
            IntSet.fold (fun callee_num callees ->
              let dummy_id = Proc.Id.unsafe_create callee_num "dummy" in
              let {Proc.id} = Proc.IdHMap.find procs dummy_id in
              id :: callees
            ) callee_nums []
        | None ->
            []
      with Error ->
          []
