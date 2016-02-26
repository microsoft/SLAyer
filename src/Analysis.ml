(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Link the abstract domain and analysis algorithm together *)

(**/**)
open Library

open Variable
open Expression
module E = Exp
module S = Substitution
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint
open SymbolicHeap
(**/**)

module L = (val Log.std Config.vATS : Log.LOG)


module SymbolicHeapsDomain = struct

  include SymbolicExecution

  let fmt_pre ff (sh,proc) =
    let ghosts =
      Vars.diff
        (Option.option Vars.empty XSH.fv sh)
        (Vars.union (Vars.of_list proc.Proc.formals) proc.Proc.modifs) in
    Format.fprintf ff "@[<hv>%a%a@]"
      (Vars.fmt_embrace "@[<hov 2>! " " .@]@ ") ghosts fmt sh

  let error = None

  let tt = Some(XSH.tt)

  let is_error x = x = None

  let is_false = function
    | Some(q) when XSH.equal q XSH.ff -> true
    | _ -> false

  let join p q =
    match p, q with
    | Some(p), Some(q) -> Some(XSH.disj [p] q)
    | _ -> None

  let generalize = function
    | None ->
        (None, false)
    | Some(q) ->
        let p, j = Abstraction.abstract q in
        (Some(p), j)

  let below p q =
    match p, q with
    | _, None -> true
    | None, _ -> false
    | Some(p), Some(q) -> None <> Prover.entailsx p q

end

module Pair = Interproc.Pair(SymbolicHeapsDomain)
module InterprocDomain = AbstractTransitionSystem.Domain(Pair)
module RD = InterprocDomain.RD

include Interproc.Make (InterprocDomain)

module VertexSet = Set.Make(InterprocDomain.I_D_cp)

let init _prog = Some(XSH.emp)

let safe results = results.safe

let errors results = InterprocDomain.errors results.invariants

let leaks results =
  let {Prog.main; procs} = results.program in
  let {Proc.exit} = Proc.IdHMap.find procs main
  in
  VertexSet.of_list (InterprocDomain.leaks results.invariants)
  |>
  List.fold (fun v leaks ->
    match RD.project (fst (InterprocDomain.I_D_cp.project v)) with
    | Some(xsh) when not (XSH.is_empty (XSH.Jnk.remove xsh)) -> VertexSet.add v leaks
    | _ -> leaks
  ) (InterprocDomain.states_for results.invariants exit)
  |>
  VertexSet.to_list

let must_diverge results =
  results.safe
  &&
  let {Prog.main; procs} = results.program in
  let {Proc.exit} = Proc.IdHMap.find procs main in
  (InterprocDomain.states_for results.invariants exit) = []

let dead results = InterprocDomain.dead results.invariants

let hit_limit results = results.hit_limit || InterprocDomain.hit_limit results.invariants
