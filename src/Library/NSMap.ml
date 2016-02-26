(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList
open NSSet


module Map = struct
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

    (** Sets of Key-Val pairs where the keys of all elements are distinct. *)
    module Set (Val: OrderedType) : (Set.S with type elt = key * Val.t
                                            and type t = Val.t t)
  end

  module Make (Key: OrderedType) = struct
    include Map.Make(Key)

    let tryfind k m = try Some(find k m) with Not_found -> None

    let exists_unique _ = failwith "Map.exists_unique unimplemented"

    (* Note: extract, modify*, pop should be implemented with one search *)
    let extract k m = (find k m, remove k m)

    let modify fn k m = add k (fn (find k m)) m

    let modify_add fn k v m = try modify fn k m with Not_found -> add k v m

    let pop m =
      let k,v = choose m in
      ((k,v), remove k m)

    let to_list = bindings

    let map_to_array fn m =
      let len = cardinal m in
      if len = 0 then [||]
      else
        let (k,v),m = pop m in
        let a = Array.make len (fn k v) in
        let i = ref 0 in
        iter (fun k v ->  incr i ; Array.set a !i (fn k v)) m ;
        a


(*============================================================================
                                   Map.Set
  ============================================================================*)

    module Set (Val: OrderedType) = struct

      type elt = Key.t * Val.t
      type _t = Val.t t
      type t = _t

      (* constructors *)

      let empty = empty

      let add (k,v) s =
        match tryfind k s with
        | None -> add k v s
        | Some(w) when Val.equal v w -> s
        | Some _ ->
            invalid_arg "Map.Set.add: element with same key already exists"

      let adds l s = List.fold add l s

      let singleton kv = add kv empty

      let of_list l = List.fold_right add l empty

      let remove (k,v) s =
        match tryfind k s with
        | Some(w) when Val.equal v w -> remove k s
        | _ -> s

      let fold fn s a = fold (fun x y -> fn (x,y)) s a

      (* Warning: union is ill-defined *)
      let union = fold add
      let unions _ = failwith "Map.unions unimplemented"
      let inter _ = failwith "Map.inter unimplemented"
      let inters _ = failwith "Map.inters unimplemented"
      let diff _ = failwith "Map.diff unimplemented"

      let mem (k,v) s =
        match tryfind k s with
        | Some(w) -> Val.equal v w
        | None -> false

      let diff_inter_diff s t =
        fold (fun a ((s_m_i,i,t_m_i) as acc) ->
          if mem a t
          then (remove a s_m_i, add a i, remove a t_m_i)
          else acc
        ) s (s,empty,t)

      let diff_diff _ = failwith "Map.diff_diff unimplemented"
      let inter_diff _ = failwith "Map.inter_diff unimplemented"

      let map fn s = fold (fun kv t -> add (fn kv) t) s empty

      let map_fold _ = failwith "Map.map_fold unimplemented"
      let map_foldi _ = failwith "Map.map_foldi unimplemented"
      let filter _ = failwith "Map.filter unimplemented"
      let partition _ = failwith "Map.partition unimplemented"
      let split _ = failwith "Map.split unimplemented"

      (* queries / destructors *)

      let is_empty = is_empty

      let compare = compare Val.compare
      let equal = equal Val.equal

      let subset s t = fold (fun kv a -> a && mem kv t) s true

      let disjoint _ = failwith "Map.disjoint unimplemented"
      let intersect _ = failwith "Map.intersect unimplemented"

      let iter fn s = iter (fun k v -> fn (k,v)) s

      let foldr _ = failwith "Map.foldr unimplemented"
      let foldi _ = failwith "Map.foldi unimplemented"
      let fold_pairs _ = failwith "Map.fold_pairs unimplemented"

      let fold_product fn xs ys =
        fold (fun x -> fold (fun y -> fn x y) ys) xs

      let fold2 _ = failwith "Map.fold2 unimplemented"

      let kfold x fn k = List.kfold fn (to_list x) k

      let for_all _ = failwith "Map.for_all unimplemented"
      let exists _ = failwith "Map.exists unimplemented"
      let exists_unique _ = failwith "Map.exists_unique unimplemented"

      let cardinal = cardinal

      let to_list = bindings

      let to_array _ = failwith "Map.to_array unimplemented"

      let min_elt _ = failwith "Map.min_elt unimplemented"
      let max_elt _ = failwith "Map.max_elt unimplemented"
      let next _ = failwith "Map.next unimplemented"

      let choose = choose

      exception Found of elt
      let trychoose s =
        try iter (fun kv -> raise (Found(kv))) s ; None
        with Found(kv) -> Some(kv)

      let extract _ = failwith "NSMap.Lift.extract unimplemented"
      let tryextract _ = failwith "NSMap.Lift.tryextract unimplemented"

      let take _ _ = failwith "Map.take unimplemented"

      let trytake p s =
        try Some(take p s) with Not_found -> None

      let take_first_pair _ = failwith "Map.take_first_pair unimplemented"
      let the_only _ = failwith "Map.the_only unimplemented"
      let reduce _ = failwith "NSMap.Lift.reduce unimplemented"
      let classify _ = failwith "NSMap.Lift.classify unimplemented"

    end
  end
end


module IntMap = Map.Make(struct
  type t = int
  let compare = (Pervasives.compare : int -> int -> int)
  let equal = (Pervasives.( = ) : int -> int -> bool)
end)
