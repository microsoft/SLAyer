(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSMap


module ImperativeMap = struct
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

  module Make(Key: OrderedType) = struct
    module M = Map.Make(Key)
    type key = Key.t
    type 'a t = 'a M.t ref

    let create () = ref M.empty
    let clear t = t := M.empty
    let copy t = ref !t
    let add t k v = t := M.add k v !t
    let remove t k = t := M.remove k !t

    let is_empty t = M.is_empty !t
    let length t = M.cardinal !t
    let mem t k = M.mem k !t
    let find t k = M.find k !t
    let tryfind t k = M.tryfind k !t

    let iter f t = M.iter f !t
    let exists f t = M.exists f !t
    let filter f t = t := M.filter f !t
    let map f t = t := M.map f !t
    let mapi f t = t := M.mapi f !t
    let fold f t a = M.fold f !t a
    let to_list t = M.to_list !t
  end
end


module IntIMap = ImperativeMap.Make(struct
  type t = int
  let compare = (Pervasives.compare : int -> int -> int)
  let equal = (Pervasives.( = ) : int -> int -> bool)
end)
