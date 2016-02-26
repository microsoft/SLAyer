(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Imperative maps from ordered keys to single values. *)
module ImperativeMap : sig
  module type S = sig
    type key
    type 'a t

    val create : unit -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit

    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val mem : 'a t -> key -> bool
    val find : 'a t -> key -> 'a
    val tryfind : 'a t -> key -> 'a option

    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> unit
    val map : ('a -> 'a) -> 'a t -> unit
    val mapi : (key -> 'a -> 'a) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val to_list : 'a t -> (key * 'a) list
  end

  module Make (Key: OrderedType) : (S with type key = Key.t)
end


module IntIMap : ImperativeMap.S with type key = int
