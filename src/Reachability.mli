(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Reachability via pointers in symbolic heaps *)

open Variable
open Expression
open SymbolicHeap
open SYMBOLIC_HEAP


val reachability_graphs_tmr : Timer.t


(** [is_reachable root sh dt loc] holds if the branch of [sh] for [dt] proves
    that [loc] is reachable from an expression satisfying [root]. *)
val is_reachable : (Exp.t -> bool) -> SH.t -> SH.t -> Exp.t -> bool


(** Determine whether to abstract two edges into one, based on whether the
    intermediate allocs are existential, the same universal expressions reach
    to the allocs of the edges, and the edges are not cyclic. *)
val should_append :
  (Vars.t * SH.t) -> (Exp.t option) edg -> (Exp.t option) edg -> SH.t -> bool
