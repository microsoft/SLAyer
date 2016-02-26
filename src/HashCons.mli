(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Hash-consing construction based on weak hash tables *)

open Library


type 'a hc = private {
  desc : 'a;
  id   : int;
  hash : int;
}


module Make (H : sig include HashedType val fmt: t formatter end) : sig

  type t

  val create : int -> t

  val intern : t -> H.t -> H.t hc

  val find : t -> H.t hc -> H.t hc

  val find_all : t -> H.t hc -> H.t hc list

  val iter : (H.t hc -> unit) -> t -> unit

  val fold : (H.t hc -> 'z -> 'z) -> t -> 'z -> 'z

  val stats : t -> int * int * int * int * int * int

end
