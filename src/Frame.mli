(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Heuristic for localization of analysis of procedure calls *)

open Variable
open Program
open SymbolicHeap


(** For a procedure call [{ call } proc(actuals)],
    [footprint call proc actuals] is a pair ([footprint],[frame]) such that
    1. [footprint] is a sub-heap of [call] that over-approximates the
       footprint of the call; and
    2. ([footprint] * [frame]) = [call].
*)
val footprint : XSH.t -> Proc.t -> Var.t list -> XSH.t * XSH.t

val frame_tmr : Timer.t
