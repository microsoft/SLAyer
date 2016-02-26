(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


(** Imperative sets of ordered values. *)
module ImperativeSet : sig
  module type S = sig
    type v
    type t

    val create : unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add : t -> v -> unit
    val remove : t -> v -> unit

    val is_empty : t -> bool
    val mem : t -> v -> bool

    val iter : (v -> unit) -> t -> unit
    val exists : (v -> bool) -> t -> bool
    val fold : (v -> 'a -> 'a) -> t -> 'a -> 'a
    val to_list : t -> v list
  end

  module Make (Val : OrderedType) : (S with type v = Val.t)
end
