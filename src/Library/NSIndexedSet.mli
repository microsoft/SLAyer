(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSSet


module type IndexedType = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  type idx
  val index : t -> idx
  val equal_idx : idx -> idx -> bool
  val compare_idx : idx -> idx -> int
end

module IndexedSet : sig
  module type S = sig
    include Set.S
    type idx
    val memi : idx -> t -> bool
    val find : idx -> t -> elt
    val tryfind : idx -> t -> elt option
    val keys : t -> idx list
    val fold_keys : (idx -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Make(Idx: IndexedType) :
    (S with type elt = Idx.t and type idx = Idx.idx)
end
