(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Operations on tuples. *)
module Tuple : sig
  val map2 : ('a->'b) -> ('c->'d) -> 'a*'c -> 'b*'d
  val map3 : ('a->'b) -> ('c->'d) -> ('e->'f) -> 'a*'c*'e -> 'b*'d*'f
  val fmt : 'a formatter -> 'a list formatter
end
