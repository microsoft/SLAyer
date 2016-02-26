(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Theorem prover for subtraction and entailment of symbolic heaps *)

open Variable
open SymbolicHeap


val subtract_tmr : Timer.t
val entails_tmr : Timer.t
val inconsistent_tmr : Timer.t
val ent_pure_tmr : Timer.t
val sub_inconsis_m_tmr : Timer.t
val sub_inconsis_s_tmr : Timer.t
val pure_normalize_tmr : Timer.t
val sh_normalize_tmr : Timer.t


(*============================================================================
                                    Prover
  ============================================================================*)

type result = Unknown | Success of XSH.t * (unit -> result)

val subtract : SH.t -> Vars.t -> SH.t -> result

val subtract_with_proviso :
  (XSH.t -> bool) -> SH.t -> Vars.t -> SH.t -> result

val entails : SH.t -> Vars.t -> SH.t -> XSH.t option
val entailsx : XSH.t -> XSH.t -> XSH.t option

(** [inconsistent sh] holds only if [sh] is inconsistent, and may not hold
    even for logically inconsistent formulae. *)
val inconsistent : SH.t -> bool

val inconsistentx : XSH.t -> bool



(*============================================================================
                              Debugging Wrappers
  ============================================================================*)

val subtract_count : SH.t -> Vars.t -> SH.t -> int
