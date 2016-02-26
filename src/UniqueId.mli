(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Unique Identifiers *)


module type S = sig
  type data
  type uniq

  val compare : uniq -> uniq -> int
  val equal : uniq -> uniq -> bool
  val hash : uniq -> int

  val id : uniq -> int
  val gensym : data -> uniq

  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit

  val unsafe_create : int -> data -> uniq
end

module Make (M: sig
  type data
  type uniq
  val get : uniq -> int
  val set : int -> data -> uniq
end) :
(S with type uniq = M.uniq and type data = M.data)
