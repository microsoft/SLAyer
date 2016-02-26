(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList


module PolySet = struct
  module type S = sig
    type 'a elt
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : 'a elt -> 'a t -> 'a t
    val singleton : 'a elt -> 'a t
    val iter : ('a elt -> unit) -> 'a t -> unit
    val map : ('a elt -> 'a elt) -> 'a t -> 'a t
    val fold : ('a elt -> 'z -> 'z) -> 'a t -> 'z -> 'z
    val exists : ('a elt -> bool) -> 'a t -> bool
    val filter : ('a elt -> bool) -> 'a t -> 'a t
    val mem : 'a elt -> 'a t -> bool
    val remove : 'a elt -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val unions : 'a t list -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val inters : 'a t list -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val diff_diff : 'a t -> 'a t -> 'a t * 'a t
    val inter_diff : 'a t -> 'a t -> 'a t * 'a t
    val diff_inter_diff : 'a t -> 'a t -> 'a t * 'a t * 'a t
    val subset : 'a t -> 'a t -> bool
    val disjoint : 'a t -> 'a t -> bool
    val intersect : 'a t -> 'a t -> bool
    val kfold : 'a t -> ('a elt -> ('y->'z) -> 'y->'z) -> ('y->'z) -> 'y->'z
    val fold2 : ('z -> 'a elt -> 'a elt -> 'z) -> 'z -> 'a t -> 'a t -> 'z
    val for_all : ('a elt -> bool) -> 'a t -> bool
    val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val to_list : 'a t -> 'a elt list
    val of_list : 'a elt list -> 'a t
    val to_array : 'a t -> 'a elt array
    val min_elt : 'a t -> 'a elt
    val max_elt : 'a t -> 'a elt
    val choose : 'a t -> 'a elt
    val trychoose : 'a t -> 'a elt option
    val split : 'a elt -> 'a t -> 'a t * bool * 'a t
    val next : 'a elt -> 'a t -> 'a elt
    val fold_pairs : ('a elt -> 'a elt -> 'z -> 'z) -> 'a t -> 'z -> 'z
    val fold_product : ('a elt->'a elt -> 'z->'z) -> 'a t->'a t -> 'z->'z
    val the_only : ('a elt -> bool) -> 'a t -> 'a elt option
(*     val take : ('a elt -> bool) -> 'a t -> 'a elt option *)
    val take_first_pair : ('a elt -> 'a elt -> 'z option) -> 'a t -> 'z option
    val equal : 'a t -> 'a t -> bool
    val compare : 'a t -> 'a t -> int
  end

  module Make(Ord: PolySet.OrderedType) = struct
    include PolySet.Make(Ord)

    let equal x y = if x == y then true else 0 = compare x y

    let union x y = if x == y then x else union x y

    let unions x = List.fold_left union empty x
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

    let map fn s = fold (fun kv t -> add (fn kv) t) s empty

    let to_list = elements

    let kfold x fn k = List.kfold fn (to_list x) k

    let of_list l = List.fold add l empty

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
        fold (fn x) xs (fold_pairs fn xs a)
      with
        Not_found -> a

    let fold_product fn xs ys =
      fold (fun x -> fold (fun y -> fn x y) ys) xs

    let the_only p s =
      match filter p s with
      | s' when cardinal s' = 1 -> Some (choose s')
      | _ -> None

(*     exception TakeFound of elt *)

(*     let take p s = *)
(*       try iter (fun x -> if p x then raise (TakeFound(x))) s ; None *)
(*       with TakeFound(x) -> Some(x) *)

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

  end
end
