(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Abstraction of symbolic heaps *)

open SymbolicHeap


val abstract_tmr : Timer.t
val abs_junk_tmr : Timer.t
val abs_ls_tmr : Timer.t
val abs_arith_tmr : Timer.t
val abs_pure_tmr : Timer.t
val normalize_tmr : Timer.t


(*============================================================================
                                 Abstraction
  ============================================================================*)

(** [abstract xsh] applies heuristics to abstract un-needed
    pure predicate, arithmetic, ls, dis-equality and points-to
    expressions. *)
val abstract : XSH.t -> XSH.t * bool
