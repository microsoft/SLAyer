(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(** Imperative sets of hashed values. *)
module HashSet : sig
  type 'a t

  val create : int -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit

  val cardinal : 'a t -> int
  val mem : 'a t -> 'a -> bool

  val iter : ('a -> unit) -> 'a t -> unit
  val exists : ('a -> bool) -> 'a t -> bool
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_list : 'a t -> 'a list
end
