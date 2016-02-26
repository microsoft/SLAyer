(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Sets of ordered values.  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/Set.html}Set}. *)
module PolySet : sig
  module type S = sig
    type 'a elt
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : 'a elt -> 'a t -> 'a t
    val singleton : 'a elt -> 'a t
    val iter : ('a elt -> unit) -> 'a t -> unit
    val map : ('a elt -> 'a elt) -> 'a t -> 'a t
    val fold : ('a elt -> 'z -> 'z) -> 'a t -> 'z -> 'z
    val exists : ('a elt -> bool) -> 'a t -> bool
    val filter : ('a elt -> bool) -> 'a t -> 'a t
    val mem : 'a elt -> 'a t -> bool
    val remove : 'a elt -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val unions : 'a t list -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val inters : 'a t list -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val diff_diff : 'a t -> 'a t -> 'a t * 'a t
    val inter_diff : 'a t -> 'a t -> 'a t * 'a t
    val diff_inter_diff : 'a t -> 'a t -> 'a t * 'a t * 'a t
    val subset : 'a t -> 'a t -> bool
    val disjoint : 'a t -> 'a t -> bool
    val intersect : 'a t -> 'a t -> bool
    val kfold : 'a t -> ('a elt -> ('y->'z) -> 'y->'z) -> ('y->'z) -> 'y->'z
    val fold2 : ('z -> 'a elt -> 'a elt -> 'z) -> 'z -> 'a t -> 'a t -> 'z
    val for_all : ('a elt -> bool) -> 'a t -> bool
    val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val to_list : 'a t -> 'a elt list
    val of_list : 'a elt list -> 'a t
    val to_array : 'a t -> 'a elt array
    val min_elt : 'a t -> 'a elt
    val max_elt : 'a t -> 'a elt
    val choose : 'a t -> 'a elt
    val trychoose : 'a t -> 'a elt option
    val split : 'a elt -> 'a t -> 'a t * bool * 'a t
    val next : 'a elt -> 'a t -> 'a elt
    val fold_pairs : ('a elt -> 'a elt -> 'z -> 'z) -> 'a t -> 'z -> 'z
    val fold_product : ('a elt -> 'a elt -> 'z->'z) -> 'a t -> 'a t -> 'z->'z
    val the_only : ('a elt -> bool) -> 'a t -> 'a elt option
(*     val take : ('a elt -> bool) -> 'a t -> 'a elt option *)
    val take_first_pair : ('a elt -> 'a elt -> 'z option) -> 'a t -> 'z option
    val equal : 'a t -> 'a t -> bool
    val compare : 'a t -> 'a t -> int
  end

  module Make (Ord : PolySet.OrderedType) : (S with type 'a elt = 'a Ord.t)
end
