(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


module Hashtbl = struct
  type ('a,'b) t = ('a,'b) Hashtbl.t

  let hash = Hashtbl.hash
  let hash_param = Hashtbl.hash_param

  let create n = Hashtbl.create n
  let clear = Hashtbl.clear
  let copy = Hashtbl.copy
  let add = Hashtbl.add
  let replace = Hashtbl.replace
  let remove = Hashtbl.remove

  let length = Hashtbl.length
  let mem = Hashtbl.mem
  let find = Hashtbl.find
  let find_all = Hashtbl.find_all

  let iter = Hashtbl.iter
  let fold = Hashtbl.fold

  exception Found_is_empty
  let is_empty t =
    try iter (fun _ -> raise Found_is_empty) t ; true with Found_is_empty -> false

  let tryfind t x = try Some(find t x) with Not_found -> None

  exception Found_exists
  let exists p t =
    try iter (fun k v -> if p k v then raise Found_exists) t ; false
    with Found_exists -> true

  let existsi k p t = List.exists p (find_all t k)

  let to_list t = fold (fun k v es -> (k,v) :: es) t []

  let filter p t =
    List.iter (fun (k,v) -> if not (p k v) then remove t k) (to_list t)

  let map f t =
    let t' = create (length t) in
    iter (fun k v -> add t' k (f v) ) t ;
    t'

  let mapi f t =
    let t' = create (length t) in
    iter (fun k v -> add t' k (f k v) ) t ;
    t'

  module Make(H: HashedType) = struct
    include Hashtbl.Make(H)

    let is_empty t =
      try iter (fun _ -> raise Found_is_empty) t ; true with Found_is_empty -> false

    let tryfind t x = try Some(find t x) with Not_found -> None

    let exists p t =
      try iter (fun k v -> if p k v then raise Found_exists) t ; false
      with Found_exists -> true

    let existsi k p t = List.exists p (find_all t k)

    let to_list t = fold (fun k v es -> (k,v) :: es) t []

    let filter p t =
      List.iter (fun (k,v) -> if not (p k v) then remove t k) (to_list t)

    let map f t =
      let t' = create (length t) in
      iter (fun k v -> add t' k (f v) ) t ;
      t'

    let mapi f t =
      let t' = create (length t) in
      iter (fun k v -> add t' k (f k v) ) t ;
      t'
  end
end
