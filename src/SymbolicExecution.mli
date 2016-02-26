(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Symbolic Execution of commands on symbolic heaps *)

open Library

open Type
open Variable
open Expression
open SymbolicHeap
open Program

val normalize_tmr : Timer.t


(*============================================================================
                              SymbolicExecution
  ============================================================================*)

type t = XSH.t option

val compare : t -> t -> int
val equal : t -> t -> bool
val fmt : t formatter


val exec_inst : Vars.t -> Inst.t -> t -> t

type r

val create : Prog.t -> r

val adapted_pre_substate_call : r -> Vars.t -> t -> t -> Proc.t Call.t -> (t -> t) option

val call_to_entry : r -> t -> Proc.t Call.t -> t * (t -> t)

val exit_to_retn : Proc.t -> t -> t

val resolve_indirect_call : r -> t -> Exp.t -> Typ.t -> Proc.Id.t list
