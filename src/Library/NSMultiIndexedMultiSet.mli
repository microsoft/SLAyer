(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSMultiSet
open NSMultiIndexedSet


module MultiIndexedMultiSet : sig
  module type S = sig
    include MultiSet.S
    type idx
    type elts
    val memi : idx -> t -> bool
    val find : idx -> t -> elts
    val keys : t -> idx list
  end

  module Make (Idx: MultiIndexedType) (EltSet: Set0 with type elt = Idx.t) :
    (S with type idx = Idx.idx and type elt = Idx.t and type elts = EltSet.t)
end
