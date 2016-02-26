(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList
open NSOption


module Set = struct
  module type Q = sig
    type elt
    type t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val disjoint : t -> t -> bool
    val intersect : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val foldr : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val foldi : (int -> elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_pairs : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_product : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    val fold2 : ('a -> elt -> elt -> 'a) -> 'a -> t -> t -> 'a
    val kfold : t -> (elt -> ('a->'b) -> 'a->'b) -> ('a->'b) -> 'a->'b
    val reduce : (elt -> 'a) -> (elt -> 'a -> 'a) -> t -> 'a option
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val exists_unique : (elt -> bool) -> t -> bool
    val cardinal : t -> int
    val to_list : t -> elt list
    val to_array : t -> elt array
    val min_elt : t -> elt
    val max_elt : t -> elt
    val next : elt -> t -> elt
    val choose : t -> elt
    val trychoose : t -> elt option
    val extract : t -> elt * t
    val tryextract : t -> (elt * t) option
    val take : (elt -> bool) -> t -> elt
    val trytake : (elt -> bool) -> t -> elt option
    val take_first_pair : (elt -> elt -> 'a option) -> t -> 'a option
    val the_only : (elt -> bool) -> t -> elt option
    val classify : (elt -> elt -> bool) -> t -> elt list list
  end
  module type R = sig
    include Q
    val add : elt -> t -> t
    val adds : elt list -> t -> t
    val singleton : elt -> t
    val of_list : elt list -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val unions : t list -> t
    val inter : t -> t -> t
    val inters : t list -> t
    val diff : t -> t -> t
    val diff_inter_diff : t -> t -> t * t * t
    val diff_diff : t -> t -> t * t
    val inter_diff : t -> t -> t * t
    val map : (elt -> elt) -> t -> t
    val map_fold : (elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
    val map_foldi : (int -> elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
  end
  module type S = sig
    include R
    val empty : t
  end

  module Make (Ord: OrderedType) = struct
    include Set.Make(Ord)

    let equal x y = if x == y then true else 0 = compare x y

    let union x y = if x == y then x else union x y

    let unions = List.fold_left union empty
    let inters = function
      | [] -> empty
      | xs::xss -> List.fold_left inter xs xss

    let diff_inter_diff s t = let i = inter s t in (diff s i, i, diff t i)
(*       fold (fun a ((s_m_i,i,t_m_i) as acc) -> *)
(*               if mem a t *)
(*            then (remove a s_m_i, add a i, remove a t_m_i) *)
(*            else acc) *)
(*      s (s,empty,t) *)

    let inter_diff x y = (inter x y, diff x y)
    let diff_diff x y = let (x_i, _, y_i) = diff_inter_diff x y in (x_i, y_i)

    let disjoint x y = is_empty (inter x y)

    (* Within the implementation of Set, the following implementation (due to
       Christophe Raffalli [christophe.raffalli@univ-savoie.fr]) should be
       faster:
    let rec disjoint t1 t2 =
      match (t1, t2) with
        (Empty, _) | (_, Empty) ->
          true
      | (Node (l1, v1, r1, _), Node (l2, v2, r2, _)) ->
          let c = Ord.compare v1 v2 in
          if c = 0 then
            false
          else if c < 0 then
            disjoint (Node (l1, v1, Empty, 0)) l2 && disjoint r1 t2
          else
            disjoint l1 (Node (l2, v2, Empty, 0)) && disjoint t1 r2
    *)

    let intersect x y = not (disjoint x y)

    let to_list = elements

    let map fn s = fold (fun kv t -> add (fn kv) t) s empty

    let foldr fn s = List.fold_right fn (to_list s)

    let foldi fn s z = snd (fold (fun x (i,z) -> (i+1, fn i x z)) s (0, z))

    let kfold x fn k = List.kfold fn (to_list x) k

    let map_fold fn (s,z) =
      fold (fun x (s',z) ->
        let x', z' = fn (x, z) in
        (add x' s', z')
      ) s (empty, z)

    let map_foldi fn (s,z) =
      foldi (fun i x (s',z) ->
        let x', z' = fn i (x, z) in
        (add x' s', z')
      ) s (empty, z)

    let of_list l = List.fold add l empty

    let adds l s = List.fold add l s

    let to_array s = Array.of_list (to_list s)

    let trychoose s = try Some (choose s) with Not_found -> None

    let next elt s =
      let _below, present, above = split elt s in
      assert present ;
      min_elt above

    let rec fold_pairs fn xs a =
      try
        let x = choose xs in
        let xs = remove x xs in
        fold (fun a -> fn x a) xs (fold_pairs fn xs a)
      with
        Not_found -> a

    let fold_product fn xs ys z =
      fold (fun x z -> fold (fun y z -> fn x y z) ys z) xs z

    let the_only p s =
      match filter p s with
      | s' when cardinal s' = 1 -> Some (choose s')
      | _ -> None

    (* Note: should be implemented with one search *)
    let extract xs =
      let x = choose xs in
      (x, remove x xs)

    let tryextract xs =
      try Some(extract xs) with Not_found -> None

    exception TakeFound of elt

    let take p s =
      try iter (fun x -> if p x then raise (TakeFound(x))) s ; raise Not_found
      with TakeFound(x) -> x

    let trytake p s =
      try Some(take p s) with Not_found -> None

    let rec take_first_pair fn s =
      try
        let x = choose s in
        let s = remove x s in
        match
          fold (fun y found ->
            match found with
            | None ->
                (match fn x y with
                | None -> fn y x
                | z -> z)
            | some -> some
          ) s None
        with
        | None -> take_first_pair fn s
        | a -> a
      with
        Not_found -> None

    let fold2 fn a s t = List.fold_left2 fn a (to_list s) (to_list t)

    let exists_unique p xs =
      let module M = struct exception Found end in
      try
        fold (fun x found ->
          if found
          then not (p x) || raise M.Found
          else p x
        ) xs false
      with M.Found -> false

    let reduce init fn xs =
      tryextract xs
      >>== fun (x, xs) -> fold fn xs (init x)

    let classify fn xs =
      let rec classify_one x = function
        | xs :: xss when fn x (List.hd xs) -> (x :: xs) :: xss
        | xs :: xss -> xs :: (classify_one x xss)
        | [] -> [[x]]
      in
      fold classify_one xs []

  end


  module type EmbedProject = sig
    type t
    type s
    val embed : s -> t -> t
    val project : t -> s
  end

  module Lift (S: S) (EP: EmbedProject with type s = S.t) : sig
    include S with type elt = S.elt and type t = EP.t
    val empty : t -> t
  end = struct

    type elt = S.elt
    type t = EP.t

    let is_empty x = S.is_empty (EP.project x)
    let mem e x = S.mem e (EP.project x)
    let compare _ = failwith "ToDo: NSSet.Lift.compare"
    let equal _ = failwith "ToDo: NSSet.Lift.equal"
    let subset _ = failwith "ToDo: NSSet.Lift.subset"
    let disjoint _ = failwith "ToDo: NSSet.Lift.disjoint"
    let intersect _ = failwith "ToDo: NSSet.Lift.intersect"
    let iter _ = failwith "ToDo: NSSet.Lift.iter"
    let fold f x z = S.fold f (EP.project x) z
    let foldr _ = failwith "ToDo: NSSet.Lift.foldr"
    let foldi f x z = S.foldi f (EP.project x) z
    let fold_pairs _ = failwith "ToDo: NSSet.Lift.fold_pairs"
    let fold_product _ = failwith "ToDo: NSSet.Lift.fold_product"
    let fold2 _ = failwith "ToDo: NSSet.Lift.fold2"
    let kfold x f k z = S.kfold (EP.project x) f k z
    let for_all _ = failwith "ToDo: NSSet.Lift.for_all"
    let exists _ = failwith "ToDo: NSSet.Lift.exists"
    let exists_unique _ = failwith "ToDo: NSSet.Lift.exists_unique"
    let cardinal x = S.cardinal (EP.project x)
    let to_list _ = failwith "ToDo: NSSet.Lift.to_list"
    let to_array _ = failwith "ToDo: NSSet.Lift.to_array"
    let min_elt _ = failwith "ToDo: NSSet.Lift.min_elt"
    let max_elt _ = failwith "ToDo: NSSet.Lift.max_elt"
    let next _ = failwith "ToDo: NSSet.Lift.next"
    let choose x = S.choose (EP.project x)
    let trychoose x = S.trychoose (EP.project x)
    let extract _ = failwith "ToDo: NSSet.Lift.extract"
    let tryextract _ = failwith "ToDo: NSSet.Lift.tryextract"
    let take _ _ = failwith "ToDo: NSSet.Lift.take"

    let trytake p s =
      try Some(take p s) with Not_found -> None

    let take_first_pair _ = failwith "ToDo: NSSet.Lift.take_first_pair"
    let the_only _ = failwith "ToDo: NSSet.Lift.the_only"
    let reduce _ = failwith "ToDo: NSSet.Lift.reduce"
    let classify _ = failwith "ToDo: NSSet.Lift.classify"
    let add e x = EP.embed (S.add e (EP.project x)) x
    let adds l x = EP.embed (S.adds l (EP.project x)) x
    let singleton _ = failwith "ToDo: NSSet.Lift.singleton"
    let of_list _ = failwith "ToDo: NSSet.Lift.of_list"
    let remove e x = EP.embed (S.remove e (EP.project x)) x
    let union _ = failwith "ToDo: NSSet.Lift.union"
    let unions _ = failwith "ToDo: NSSet.Lift.unions"
    let inter _ = failwith "ToDo: NSSet.Lift.inter"
    let inters _ = failwith "ToDo: NSSet.Lift.inters"
    let diff _ = failwith "ToDo: NSSet.Lift.diff"
    let diff_inter_diff _ = failwith "ToDo: NSSet.Lift.diff_inter_diff"
    let diff_diff _ = failwith "ToDo: NSSet.Lift.diff_diff"
    let inter_diff _ = failwith "ToDo: NSSet.Lift.inter_diff"
    let map f x = EP.embed (S.map f (EP.project x)) x
    let map_fold _ = failwith "ToDo: NSSet.Lift.map_fold"
    let map_foldi _ = failwith "ToDo: NSSet.Lift.map_foldi"
    let filter p x = EP.embed (S.filter p (EP.project x)) x
    let partition _ = failwith "ToDo: NSSet.Lift.partition"
    let split _ = failwith "ToDo: NSSet.Lift.split"
    let empty x = EP.embed S.empty x

  end

end


module IntSet = Set.Make(struct
  type t = int
  let compare = (Pervasives.compare : int -> int -> int)
  let equal = (Pervasives.( = ) : int -> int -> bool)
end)
