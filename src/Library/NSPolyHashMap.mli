(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(** Imperative maps from keys to single values using polymorphic equality and
    hashing.  @deprecated Polymorphic equality is usually wrong, use [HashMap]
    instead. *)
module PolyHMap : sig
  type ('a, 'b) t

  val create : int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit

  val is_empty : ('a, 'b) t -> bool
  val length : ('a, 'b) t -> int
  val mem : ('a, 'b) t -> 'a -> bool
  val find : ('a, 'b) t -> 'a -> 'b
  val tryfind : ('a, 'b) t -> 'a -> 'b option

  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
  val filter : ('a -> 'b -> bool) -> ('a,'b) t -> unit
  val map : ('v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val mapi : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val to_list : ('a, 'b) t -> ('a * 'b) list
end
