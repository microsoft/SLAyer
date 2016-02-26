(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList
open NSImperativeMap


module ImperativeSet = struct
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

  module Make(Val: OrderedType) = struct
    module M = ImperativeMap.Make(Val)

    type v = Val.t
    type t = unit M.t

    let create = M.create
    let clear = M.clear
    let copy = M.copy
    let add s x = M.add s x ()
    let remove = M.remove

    let is_empty = M.is_empty
    let mem = M.mem

    let iter f = M.iter (fun x () -> f x)
    let exists p = M.exists (fun x () -> p x)
    let fold f = M.fold (fun x () -> f x)
    let to_list s = fold List.cons s []
  end
end
