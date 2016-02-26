(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Heuristic for localization of analysis of procedure calls *)

(*
  Calculate footprint and frame for [{ call } proc(actuals)].

  1. Let RG(p) = set of globals "relevant" to proc. A global is relevant to
  proc if it appears in proc's body or in the body of any procedure that is in
  the static call-graph beginning at proc.

  2. Then the principal *-conjunction of call is split into footprint * frame
  such that frame includes every spatial subformula that contains a may-alloc
  which is not reachable from RG(p) \cup actuals.
*)


(**/**)
open Library

open Variable
open Expression
module E = Exp
open Program
open SymbolicHeap
(**/**)

module L = (val Log.std Config.vFrame : Log.LOG)
let frame_tmr = Timer.create "Frame.footprint"


(*============================================================================
                                Entry points.
  ============================================================================*)

let footprint call callee actuals =
    Timer.start frame_tmr; assert(true$>(
    L.incf 1 "( footprint:@ %a@ %a(%a)" XSH.fmt call Proc.Id.fmt callee.Proc.id (List.fmt ",@ " Var.fmt) actuals ));
  (fun (foot,frame) ->
    Timer.stop frame_tmr ; assert(true$>(
    L.decf 1 ") footprint:@[%a@],@ frame:@[%a@]" XSH.fmt foot XSH.fmt frame )))
  <&
  let exps_of_vars vv =
    Vars.fold (fun v ee -> Exps.add (E.mkVar v) ee) vv Exps.empty
  in
  let actuals = List.fold (fun a actls -> Exps.add (Exp.mkVar a) actls) actuals Exps.empty
  in
  let xs, call = XSH.exists_bind (Vars.union (XSH.fv call) (Exps.fv actuals)) call
  in
  let callee_globals = exps_of_vars callee.Proc.accessed
  in
  let roots = Exps.union callee_globals actuals
  in
  let is_reachable = Reachability.is_reachable (fun e -> Exps.mem e roots) call call
  in
  (* frame away any spatial subformula that includes an unreachable may-alloc *)
  let footprint, frame =
    SH.partition (function
      | SH.Pt({Pt.loc}) ->
          is_reachable loc
      | SH.Ls(ls) ->
(*        Format.printf "List %a reachable?@\n" Ls.fmt ls; *)
          let res = List.exists is_reachable (Ls.may_allocs ls) in
(*        Format.printf "List %a reachable %b@\n" Ls.fmt ls res; *)
          res
      | SH.Dj(dj) ->
          Dj.for_all (fun dt -> Exps.exists is_reachable (SH.may_allocs dt)) dj
    ) call
  in
  (SH.exists_intro xs footprint, SH.exists_intro xs frame)
