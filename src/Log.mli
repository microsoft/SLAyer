(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Debug message logging *)

open Library


module type LOG = sig
  val printf : int -> ('a,Format.formatter,unit)format -> 'a
  val incf : int -> ('a,Format.formatter,unit)format -> 'a
  val decf : int -> ('a,Format.formatter,unit)format -> 'a

  val warnf  : ('a,Format.formatter,unit,bool)format4 -> 'a

  val latch : unit -> int
  val latch_incf : int -> 'a format_str -> 'a formatter -> 'a -> int
  val resetf : int -> int -> ('a,Format.formatter,unit)format -> 'a

  val shift_verb : int -> (unit -> 'a) -> 'a
end

val mk_raw : out_channel -> int ref -> (module LOG)

val raw : int ref -> (module LOG)

val mk : out_channel -> int ref -> (module LOG)

val std : int ref -> (module LOG)
