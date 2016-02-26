(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Operations on ['a option]. *)
module Option : sig

  val option : 'b -> ('a -> 'b) -> 'a option -> 'b
  (** [option d f o] tranforms [o] with [f] using [d] as a default value. *)

  val optionk : (unit -> 'b) -> ('a -> 'b) -> 'a option -> 'b
  (** [optionk k f o] tranforms [o] with [f] using [k()] as a default value. *)

  val is_some : 'a option -> bool
  (** Test for existence of a carried value. *)

  val is_none : 'a option -> bool
  (** Test for absence of a carried value. *)

  val get : 'a option -> 'a
  (** Extract carried value, raises [Invalid_argument] if [None]. *)

  val from_some : 'a option -> 'a
  (** Extract carried value, raises [Invalid_argument] if [None]. *)

  val get_or : 'a option -> 'a -> 'a
  (** Extract carried value, using given value as a default. *)

  val or_get : 'a -> 'a option -> 'a
  (** Extract carried value, using given value as a default. *)

  val from_option : 'a -> 'a option -> 'a
  (** Extract carried value, using given value as a default. *)

  val some : 'a -> 'a option
  (** Monadic unit for options. *)

  val bind : 'a option -> ('a -> 'b option) -> 'b option
  (** Monadic bind for options. *)

  val flatten : 'a option option -> 'a option
  (** Monadic join for options. *)

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** Monadic lift for options. *)

  val map2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
  (** Monadic binary lift for options. *)

  val map3 : ('a->'b->'c->'d) -> 'a option -> 'b option -> 'c option -> 'd option
  (** Monadic ternary lift for options. *)

  val mapN : ('a array -> 'b) -> 'a option array -> 'b option
  (** Monadic n-ary lift for options. *)

  val fold : ('a -> 'z -> 'z) -> 'a option -> 'z -> 'z

  val iter : ('a -> unit) -> 'a option -> unit

  val to_list : 'a option -> 'a list
  (** Convert option to list. *)

  val of_list : 'a list -> 'a option
  (** Convert (head of) list to option. *)

  val meet : 'a option -> 'b option -> ('a*'b) option
  (** Smash product. *)

  val meetN : 'a option list -> 'a list option
  (** Smash product, n-ary. *)

  val concat : 'a option list -> 'a list
  (** Return list of carried values. *)

  val until_none : ('a -> 'a option) -> 'a -> 'a
  (** Apply given function to given value repeatedly until [None] results,
      then return the value that the function mapped to [None]. *)

  val equal : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
  (** Equivalence relation on options. *)

  val compare : ('a -> 'b -> int) -> 'a option -> 'b option -> int
  (** Total order on options, [None] is smaller than [Some]. *)

  val fmt :
    (unit,Format.formatter,unit)format -> 'a formatter -> 'a option formatter
  (** Formatter for options, accepting a format string to use for [None] and a
      formatter for carried values. *)

end



(** {4 Option combinators } *)

val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
(** Monadic bind for options, left associative. *)

val ( =<< ) : ('a -> 'b option) -> 'a option -> 'b option
(** Monadic bind for options with arguments flipped, left associative. *)

val ( >>== ) : 'a option -> ('a -> 'b) -> 'b option
(** Monadic lift for options with arguments flipped, left associative. *)

val ( >=> ) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option
(** Left-to-right Kleisli composition. *)

val ( <=< ) : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option
(** Right-to-left Kleisli composition. *)

val ( |+| ) : 'a option -> 'b option -> ('a * 'b) option
(** Smash product. *)

val ( !! ) : 'a option ref -> 'a
(** Dereference possibly-NULL pointer. *)
