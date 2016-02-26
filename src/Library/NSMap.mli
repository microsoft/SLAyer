(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSSet


(** Maps from ordered keys to single values.  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/Map.html}Map}. *)
module Map : sig
  module type S = sig
    include Map.S
    val tryfind : key -> 'a t -> 'a option
    val exists_unique : (key -> 'a -> bool) -> 'a t -> bool
    val extract : key -> 'a t -> 'a * 'a t
    val modify : ('a -> 'a) -> key -> 'a t -> 'a t
    val modify_add : ('a -> 'a) -> key -> 'a -> 'a t -> 'a t
    val pop : 'a t -> (key * 'a) * 'a t
    val to_list : 'a t -> (key * 'a) list
    val map_to_array : (key -> 'a -> 'b) -> 'a t -> 'b array

(*============================================================================
                                   Map.Set
  ============================================================================*)

    (** Sets of Key-Val pairs where the keys of all elements are distinct. *)
    module Set (Val: OrderedType) : (Set.S with type elt = key * Val.t
                                            and type t = Val.t t)
  end

  module Make (Key: OrderedType) : S with type key = Key.t
end


module IntMap : Map.S with type key = int
