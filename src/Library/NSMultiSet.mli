(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Multisets of ordered values. *)
module MultiSet : sig

  module type S = sig
    type elt
      (** The type of multiset elements. *)

    type t
      (** The type of multisets of elt. *)

    val empty : t
      (** The empty multiset. *)

    val is_empty : t -> bool

    val add : elt -> t -> t
      (** [add e s] increases the multiplicity of [e] by one. *)

    val singleton : elt -> t

    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'z -> 'z) -> t -> 'z -> 'z
    val map_fold : (elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
    val kfold : t -> (elt -> ('a->'b) -> 'a->'b) -> ('a->'b) -> 'a->'b

    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val exists_unique : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val cardinal : t -> int

    val of_list : elt list -> t
    val to_list : t -> elt list
      (** [to_list s] is a list of the elements of [s], in increasing order,
          where elements are repeated according to their multiplicities. *)

    val choose : t -> elt

    val union : t -> t -> t
      (** [union s t] sums the multiplicities of elements of [s] and [t]. *)

    val diff : t -> t -> t

    val equal: t -> t -> bool
    val compare: t -> t -> int

    val remove : elt -> t -> t

    val foldm : (elt -> int -> 'z -> 'z) -> t -> 'z -> 'z
      (** [foldm f s z] computes [(f eN mN … (f e2 m2 (f e1 m1 z))…)], where
          [e1…eN] are the distinct elements of [s], in increasing order, and
          [m1…mN] are their multiplicities. *)

    val fold_pairs : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Make (Ord: OrderedType) : (S with type elt = Ord.t)
end
