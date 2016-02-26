(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Imperative maps from ordered keys to multiple values. *)
module ImperativeMultiMap : sig
  module type S = sig
    type k
    type v
    type vs
    type t

    val create : unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add : t -> k -> v -> unit
    val replace : t -> k -> vs -> unit
    val remove : t -> k -> unit

    val is_empty : t -> bool
    val length : t -> int
    val mem : t -> k -> bool
    val find : t -> k -> vs

    val iter : (k -> v -> unit) -> t -> unit
    val iter_keys : (k -> vs -> unit) -> t -> unit
    val exists : (k -> v -> bool) -> t -> bool
    val existsi : k -> (v -> bool) -> t -> bool
    val filter : (k -> v -> bool) -> t -> unit
    val filteri : k -> (v -> bool) -> t -> unit
    val map : (v -> v) -> t -> unit
    val mapi : (k -> v -> v) -> t -> unit
    val fold : (k -> v -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_keys : (k -> vs -> 'a -> 'a) -> t -> 'a -> 'a
    val to_list : t -> (k * v) list
  end

  module Make(Key: OrderedType)(ValSet: Set0) :
    (S with type k = Key.t and type v = ValSet.elt and type vs = ValSet.t)
end
