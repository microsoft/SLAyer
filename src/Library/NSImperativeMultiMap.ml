(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSMultiMap


module ImperativeMultiMap = struct
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

  module Make (Key: OrderedType) (ValSet: Set0) = struct
    module M = MultiMap.Make(Key)(ValSet)
    type k = M.k
    type v = M.v
    type vs = M.vs
    type t = M.t ref

    let create () = ref M.empty
    let clear t = t := M.empty
    let copy t = ref !t
    let add t k v = t := M.add k v !t
    let replace t k vs = t := M.replace k vs !t
    let remove t k = t := M.remove k !t

    let is_empty t = M.is_empty !t
    let length t = M.length !t
    let mem t k = M.mem k !t
    let find t k = M.find k !t

    let iter f t = M.iter f !t
    let iter_keys f t = M.iter_keys f !t
    let exists p t = M.exists p !t
    let existsi k p t = M.existsi k p !t
    let filter p t = t := M.filter p !t
    let filteri k p t = t := M.filteri k p !t
    let map f t = t := M.map f !t
    let mapi f t = t := M.mapi f !t
    let fold f t = M.fold f !t
    let fold_keys f t = M.fold_keys f !t
    let to_list t = M.to_list !t
  end
end
