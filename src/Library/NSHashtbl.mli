(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(** @deprecated Make the standard [Hashtbl] read-only since it confuses maps
    and multi-maps.  Use [HashMap] or [HashMultiMap] instead. *)
module Hashtbl : sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val hash : 'a -> int
  val hash_param : int -> int -> 'a -> int
  val create : int -> ('a, 'b) Hashtbl.t
  val clear : ('a, 'b) Hashtbl.t -> unit
  val copy : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t
  val add : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
  val replace : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
  val remove : ('a, 'b) Hashtbl.t -> 'a -> unit
  val length : ('a, 'b) Hashtbl.t -> int
  val mem : ('a, 'b) Hashtbl.t -> 'a -> bool
  val find : ('a, 'b) Hashtbl.t -> 'a -> 'b
  val find_all : ('a, 'b) Hashtbl.t -> 'a -> 'b list
  val iter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Hashtbl.t -> 'c -> 'c
  val is_empty : ('a, 'b) Hashtbl.t -> bool
  val tryfind : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  val exists : ('a -> 'b -> bool) -> ('a, 'b) Hashtbl.t -> bool
  val existsi : 'a -> ('b -> bool) -> ('a, 'b) Hashtbl.t -> bool
  val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
  val filter : ('a -> 'b -> bool) -> ('a, 'b) Hashtbl.t -> unit
  val map : ('v -> 'w) -> ('k, 'v) Hashtbl.t -> ('k, 'w) Hashtbl.t
  val mapi : ('k -> 'v -> 'w) -> ('k, 'v) Hashtbl.t -> ('k, 'w) Hashtbl.t
  module Make(H : NSLib.HashedType) : sig
    type key = H.t
    type 'a t = 'a Hashtbl.Make(H).t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val is_empty : 'a t -> bool
    val tryfind : 'a t -> key -> 'a option
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val existsi : key -> ('a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val filter : (key -> 'a -> bool) -> 'a t -> unit
    val map : ('v -> 'w) -> 'v t -> 'w t
    val mapi : (key -> 'v -> 'w) -> 'v t -> 'w t
  end
end
