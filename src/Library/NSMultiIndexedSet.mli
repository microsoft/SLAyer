(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSSet


module type MultiIndexedType = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  type idx
  val index : t -> idx
  val indices : t -> idx list
  val equal_idx : idx -> idx -> bool
  val compare_idx : idx -> idx -> int
end

module MultiIndexedSet : sig
  module type S = sig
    include Set.S
    type idx
    val memi : idx -> t -> bool
    val find : idx -> t -> elt
    val tryfind : idx -> t -> elt option
    val keys : t -> idx list
  end

  module Make(Idx: MultiIndexedType) :
    (S with type elt = Idx.t and type idx = Idx.idx)
end
