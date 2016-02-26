(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib

(** Operations on ['a array].  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/Array.html}Array}. *)

module Array : sig

  include module type of Array

  val swap : 'a array -> int -> int -> unit

  val reverse : 'a array -> int -> int -> unit

(*   val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool *)
  val equal : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
  val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int

  val fmt :
    (unit,Format.formatter,unit)format -> 'a formatter -> 'a array formatter

  val for_all : ('a -> bool) -> 'a array -> bool

end
