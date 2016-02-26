(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Sets of ordered values.  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/Set.html}Set}. *)
module Set : sig
  module type Q = sig
    type elt
    type t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val disjoint : t -> t -> bool
    val intersect : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val foldr : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val foldi : (int -> elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_pairs : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_product : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    val fold2 : ('a -> elt -> elt -> 'a) -> 'a -> t -> t -> 'a
    val kfold : t -> (elt -> ('a->'b) -> 'a->'b) -> ('a->'b) -> 'a->'b
    val reduce : (elt -> 'a) -> (elt -> 'a -> 'a) -> t -> 'a option
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val exists_unique : (elt -> bool) -> t -> bool
    val cardinal : t -> int
    val to_list : t -> elt list
    val to_array : t -> elt array
    val min_elt : t -> elt
    val max_elt : t -> elt
    val next : elt -> t -> elt
    val choose : t -> elt
    val trychoose : t -> elt option
    val extract : t -> elt * t
    val tryextract : t -> (elt * t) option
    val take : (elt -> bool) -> t -> elt
    val trytake : (elt -> bool) -> t -> elt option
    val take_first_pair : (elt -> elt -> 'a option) -> t -> 'a option
    val the_only : (elt -> bool) -> t -> elt option
    val classify : (elt -> elt -> bool) -> t -> elt list list
  end
  module type R = sig
    include Q
    val add : elt -> t -> t
    val adds : elt list -> t -> t
    val singleton : elt -> t
    val of_list : elt list -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val unions : t list -> t
    val inter : t -> t -> t
    val inters : t list -> t
    val diff : t -> t -> t
    val diff_inter_diff : t -> t -> t * t * t
    val diff_diff : t -> t -> t * t
    val inter_diff : t -> t -> t * t
    val map : (elt -> elt) -> t -> t
    val map_fold : (elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
    val map_foldi : (int -> elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
  end
  module type S = sig
    include R
    val empty : t
  end

  module Make (Ord : OrderedType) : (S with type elt = Ord.t)

  module type EmbedProject = sig
    type t
    type s
    val embed : s -> t -> t
    val project : t -> s
  end

  module Lift (S: S) (EP: EmbedProject with type s = S.t) : sig
    include S with type elt = S.elt and type t = EP.t
    val empty : t -> t
  end

end


module IntSet : Set.S with type elt = int
