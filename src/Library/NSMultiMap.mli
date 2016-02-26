(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Maps from ordered keys to multiple values. *)
module MultiMap : sig
  module type S = sig
    type k
    type v
    type vs
    type t

    val empty : t
    val add : k -> v -> t -> t
    val union : k -> vs -> t -> t
    val diff : k -> vs -> t -> t
    val replace : k -> vs -> t -> t
    val remove : k -> t -> t

    val is_empty : t -> bool
    val length : t -> int
    val mem : k -> t -> bool
    val find : k -> t -> vs

    val iter : (k -> v -> unit) -> t -> unit
    val iter_keys : (k -> vs -> unit) -> t -> unit
    val exists : (k -> v -> bool) -> t -> bool
    val existsi : k -> (v -> bool) -> t -> bool
    val filter : (k -> v -> bool) -> t -> t
    val filteri : k -> (v -> bool) -> t -> t
    val map : (v -> v) -> t -> t
    val mapi : (k -> v -> v) -> t -> t
    val fold : (k -> v -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_keys : (k -> vs -> 'a -> 'a) -> t -> 'a -> 'a
    val to_list : t -> (k * v) list

(*     val equal : t -> t -> bool *)
(*     val compare : t -> t -> int *)
  end

  module Make (Key: OrderedType) (ValSet: Set0) :
    (S with type k = Key.t and type v = ValSet.elt and type vs = ValSet.t)
end
