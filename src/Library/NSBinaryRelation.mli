(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSSet


(** Binary Relations of ordered values. *)
module BinaryRelation : sig
  module Make (Ord : OrderedType) : sig
    include Set.S with type elt = Ord.t * Ord.t
    val inverse : t -> t
    val fold_product : (elt -> elt -> 'z -> 'z) -> t -> t -> 'z -> 'z
    val close : t -> t
    val choose_maximal_path : t -> elt list
  end
end
