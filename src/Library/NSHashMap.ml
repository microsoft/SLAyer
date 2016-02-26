(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSHashtbl


module HashMap = struct
  module type S = sig
    type key
    type 'a t

    val create : int -> 'a t
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
    val map : ('v -> 'w) -> 'v t -> 'w t
    val mapi : (key -> 'v -> 'w) -> 'v t -> 'w t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val to_list : 'a t -> (key * 'a) list
  end

  module Make(H: HashedType) = struct
    include Hashtbl.Make(H)
    let add = replace
  end
end

module StringHMap = HashMap.Make(struct
  type t = string
  let equal = (Pervasives.( = ) : string -> string -> bool)
  let hash = (Hashtbl.hash : string -> int)
end)

module IntHMap = HashMap.Make(struct
  type t = int
  let equal = (Pervasives.( = ) : int -> int -> bool)
  let hash = (Hashtbl.hash : int -> int)
end)

module Int64HMap = HashMap.Make(struct
  type t = int64
  let equal = (Pervasives.( = ) : int64 -> int64 -> bool)
  let hash = (Hashtbl.hash : int64 -> int)
end)
