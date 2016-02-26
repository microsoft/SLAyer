(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Extensions of the standard library. *)


(*============================================================================
                                 Combinators
  ============================================================================*)

(** {3 Combinators } *)


(** {4 Function combinators } *)

val id : 'a -> 'a
val const : 'a -> 'b -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val ( &> ) : 'a -> ('a -> unit) -> 'a
(** [x &> f] applies [f] to [x] and returns [x], left associative. *)

val ( <& ) : ('a -> unit) -> 'a -> 'a
(** [f <& x] applies [f] to [x] and returns [x], left associative. *)

val ( $> ) : 'a -> unit -> 'a
(** Reverse sequential composition, left associative *)



(** {4 Tuple combinators } *)

val pair : 'a -> 'b -> 'a * 'b
val swap : 'a * 'b -> 'b * 'a

val fst3 : ('a * 'b * 'c) -> 'a
val snd3 : ('a * 'b * 'c) -> 'b
val thd3 : ('a * 'b * 'c) -> 'c

val fst4 : ('a * 'b * 'c * 'd) -> 'a
val snd4 : ('a * 'b * 'c * 'd) -> 'b
val thd4 : ('a * 'b * 'c * 'd) -> 'c
val fth4 : ('a * 'b * 'c * 'd) -> 'd

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd


(** {4 Predicate combinators } *)

val ( &&& ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** Short-circuit conjunction lifted to predicates, left associative. *)

val ( ||| ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** Short-circuit disjunction lifted to predicates, left associative. *)



(** {4 Equality combinators } *)

val equal_tup2 : ('a->'b->bool)->('c->'d->bool)->'a*'c->'b*'d->bool
val equal_tup3 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->'a*'c*'e->'b*'d*'f->bool
val equal_tup4 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->'a*'c*'e*'g->'b*'d*'f*'h->bool
val equal_tup5 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->('i->'j->bool)->'a*'c*'e*'g*'i->'b*'d*'f*'h*'j->bool
val equal_tup6 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->('i->'j->bool)->('k->'l->bool)->'a*'c*'e*'g*'i*'k->'b*'d*'f*'h*'j*'l->bool
val equal_tup7 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->('i->'j->bool)->('k->'l->bool)->('m->'n->bool)->'a*'c*'e*'g*'i*'k*'m->'b*'d*'f*'h*'j*'l*'n->bool
val equal_tup8 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->('i->'j->bool)->('k->'l->bool)->('m->'n->bool)->('o->'p->bool)->'a*'c*'e*'g*'i*'k*'m*'o->'b*'d*'f*'h*'j*'l*'n*'p->bool
val equal_tup9 : ('a->'b->bool)->('c->'d->bool)->('e->'f->bool)->('g->'h->bool)->('i->'j->bool)->('k->'l->bool)->('m->'n->bool)->('o->'p->bool)->('q->'r->bool)->'a*'c*'e*'g*'i*'k*'m*'o*'q->'b*'d*'f*'h*'j*'l*'n*'p*'r->bool


(** {4 Comparison combinators } *)

val compare_tup2 : ('a->'b->int)->('c->'d->int)->'a*'c->'b*'d->int
val compare_tup3 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->'a*'c*'e->'b*'d*'f->int
val compare_tup4 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->'a*'c*'e*'g->'b*'d*'f*'h->int
val compare_tup5 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->('i->'j->int)->'a*'c*'e*'g*'i->'b*'d*'f*'h*'j->int
val compare_tup6 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->('i->'j->int)->('k->'l->int)->'a*'c*'e*'g*'i*'k->'b*'d*'f*'h*'j*'l->int
val compare_tup7 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->('i->'j->int)->('k->'l->int)->('m->'n->int)->'a*'c*'e*'g*'i*'k*'m->'b*'d*'f*'h*'j*'l*'n->int
val compare_tup8 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->('i->'j->int)->('k->'l->int)->('m->'n->int)->('o->'p->int)->'a*'c*'e*'g*'i*'k*'m*'o->'b*'d*'f*'h*'j*'l*'n*'p->int
val compare_tup9 : ('a->'b->int)->('c->'d->int)->('e->'f->int)->('g->'h->int)->('i->'j->int)->('k->'l->int)->('m->'n->int)->('o->'p->int)->('q->'r->int)->'a*'c*'e*'g*'i*'k*'m*'o*'q->'b*'d*'f*'h*'j*'l*'n*'p*'r->int


(** {3 File handling } *)

val with_in_bin : string -> (in_channel -> 'a) -> 'a
val with_out_bin : string -> (out_channel -> 'a -> 'b) -> 'a -> 'b
val with_out : string -> (Buffer.t -> 'a) -> 'a


(** {3 Exception handling } *)

exception Undef

val try_finally : (unit -> 'a) -> (unit -> 'b) -> 'a
val finally_try : (unit -> 'b) -> (unit -> 'a) -> 'a
val debug_wrap1 : int ref -> int -> ('a->'b) -> 'a->'b
val debug_wrap2 : int ref -> int -> ('a->'b->'c) -> 'a->'b->'c
val debug_wrap3 : int ref -> int -> ('a->'b->'c->'d) -> 'a->'b->'c->'d
val debug_wrap4 : int ref -> int -> ('a->'b->'c->'d->'e) -> 'a->'b->'c->'d->'e
val debug_wrap5 : int ref -> int -> ('a->'b->'c->'d->'e->'f) -> 'a->'b->'c->'d->'e->'f



(*============================================================================
                                  Formatting
  ============================================================================*)

(** {3 Formatting } *)

exception Nothing_to_fmt

(** Type of functions for formatting ['a] values. *)
type 'a formatter = Format.formatter -> 'a -> unit

(** Type of format strings that make a single call to an ['a formatter]. *)
type 'a format_str = ('a formatter -> 'a -> unit, Format.formatter, unit) format

val ifbreakf : ('a, Format.formatter, unit) format -> Format.formatter -> 'a

val failwithf : ('a, Format.formatter, unit, 'b) format4 -> 'a

val invalid_argf : ('a, Format.formatter, unit, 'b) format4 -> 'a



(*============================================================================
                                 Collections
  ============================================================================*)

(** {2 Collections } *)

(** Types equipped with an equivalence relation. *)
module type EqualityType = sig
  type t
  val equal: t -> t -> bool
end

(** Types equipped with a total order. *)
module type OrderedType = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

(** Types equipped with a hash function. *)
module type HashedType = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

(** Pairs of types equipped with a hash function. *)
module HashedTypeTup2 (H0: HashedType) (H1: HashedType)
  : (HashedType with type t = H0.t * H1.t)

(** Sets of unordered values. *)
module type Set0 = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map_fold : (elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
  val kfold : t -> (elt -> ('a->'b) -> 'a->'b) -> ('a->'b) -> 'a->'b
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val exists_unique : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val cardinal : t -> int
  val of_list : elt list -> t
  val to_list : t -> elt list
  val choose : t -> elt
  val union : t -> t -> t
  val diff : t -> t -> t
end

module type Set1 = sig
  include Set0
  include OrderedType with type t := t
  val remove : elt -> t -> t
  val diff_inter_diff : t -> t -> t * t * t
end
