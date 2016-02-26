(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Pure formulas and theorem prover *)

open Variable
open Expression


val z3_assert_tmr : Timer.t
val z3_push_tmr : Timer.t
val z3_pop_tmr : Timer.t
val z3_check_tmr : Timer.t
val z3_check_assumptions_tmr : Timer.t
val z3_eval_tmr : Timer.t
val z3_get_implied_equalities_tmr : Timer.t
val get_implied_equalities_tmr : Timer.t
val find_provable_equality_tmr : Timer.t
val conjoin_tmr : Timer.t
val inconsistent_tmr : Timer.t
val implies_tmr : Timer.t


(*============================================================================
                                     Pure
  ============================================================================*)

(** A logical context. *)
type t

(** A vertical (orthogonal to extension) partition of a tree of contexts. *)
type partition


(** [mk_partition x] creates a new partition of [x]. *)
val mk_partition : t -> partition

(** [mk ()] creates a new tree of contexts. *)
val mk : unit -> t

(** [clear x] retracts (and deallocates) all constraints and partitions from
    [x]. *)
val clear : t -> unit

(** [extend x] adds and returns an empty context that extends [x]. *)
val extend : t -> t

(** [conjoin ps x b] conjoins [b] to the intersection of the [ps] partitions
    of [x]. *)
val conjoin : ?parts:partition list ->
  t -> Exp.t -> unit

(** [implies ps x b] holds only if the union of the [ps] partitions of [x]
    implies [b]. *)
val implies : ?parts:partition list ->
  t -> Exp.t -> bool option

(** [impliesx ps x (vs,b)] holds only if the union of the [ps] partitions
    of [x] impliex [? vs. b]. *)
val impliesx : ?parts:partition list ->
  t -> Vars.t * Exp.t -> bool option

(** [inconsistent ps x] holds if and only if [implies ps x E.ff]. *)
val inconsistent : ?parts:partition list ->
  t -> bool


type find_provable_equality_t =
  | Inconsistent
  | Equality of Exp.t * Exp.t
  | Disjunctions of (Exp.t * Exp.t) list list

val find_provable_equality : ?parts:(partition list) ->
  t -> Exps.t -> Exps.t -> (Exp.t -> Exp.t -> bool) -> find_provable_equality_t


val conjoin_weak : ?parts:partition list -> t -> Exp.t -> unit

val get_implied_equalities : t -> Exp.t array -> Exp.t array -> int array option
