(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Link the abstract domain and analysis algorithm together *)

(**/**)
open Program
open SymbolicHeap
open Interproc_sig
(**/**)


module SymbolicHeapsDomain : (INTRAPROC_DOMAIN with type t = XSH.t option)

module Pair : (module type of Interproc.Pair(SymbolicHeapsDomain))

module InterprocDomain : (module type of AbstractTransitionSystem.Domain(Pair))

(* Analyze *)
include (module type of Interproc.Make(InterprocDomain))

val init : Prog.t -> SymbolicHeapsDomain.t
val exec_prog : Prog.t -> SymbolicHeapsDomain.t -> t

(* Query analysis result *)
val safe : t -> bool
val errors : t -> InterprocDomain.I_D_cp.t list
val leaks : t -> InterprocDomain.I_D_cp.t list
val must_diverge : t -> bool
val dead : t -> Position.t list
val hit_limit : t -> bool
