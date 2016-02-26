(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Abstraction of spatial part of symbolic heaps *)

open Variable
open SymbolicHeap


(*============================================================================
                               HeapAbstraction
  ============================================================================*)

val abstract : Vars.t * SH.t -> (Vars.t * SH.t) option
