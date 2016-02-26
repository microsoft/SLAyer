(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList
open NSSet
open NSMap


module type IndexedType = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  type idx
  val index : t -> idx
  val equal_idx : idx -> idx -> bool
  val compare_idx : idx -> idx -> int
end

(** Sets of [IndexedType]s, where an [indexedType] is an [OrderedType] where
    each value [v] is associated with an index [index v].  The indices of all
    values in an [IndexedSet] are required to be disjoint. *)
module IndexedSet = struct
  module type S = sig
    include Set.S
    type idx
    val memi : idx -> t -> bool
    val find : idx -> t -> elt
    val tryfind : idx -> t -> elt option
    val keys : t -> idx list
    val fold_keys : (idx -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Make(Val: IndexedType) = struct

    module Key = struct
      type t = Val.idx
      let equal = Val.equal_idx
      let compare = Val.compare_idx
    end

    module M = Map.Make(Key)

    type elt = Val.t
    type t = Val.t M.t


    (* constructors *)

    let empty = M.empty

    let add v m =
      assert(
        (try Val.equal v (M.find (Val.index v) m) with Not_found -> true)
        || invalid_arg "IndexedSet: indices must be disjoint"
      );
      M.add (Val.index v) v m

    let adds l m = List.fold add l m

    let singleton v = add v empty

    let of_list vl = List.fold add vl empty

    let remove v s =
      let i = Val.index v in
      try if Val.equal v (M.find i s) then M.remove i s else s
      with Not_found -> s

    let union m n =
      M.merge (fun _ vo0 vo1 ->
        match vo0, vo1 with
        | None    , None     -> None
        | Some _  , None     -> vo0
        | None    , Some _   -> vo1
        | Some(v0), Some(v1) -> vo0
            $> assert(
              Val.equal v0 v1
              || invalid_arg "IndexedSet: indices must be disjoint"
            )
      ) m n

    let unions = function
      | [] -> empty
      | m :: [] -> m
      | m :: ml -> List.fold union ml m

    let inter m n =
      M.merge (fun _ vo0 vo1 ->
        match vo0, vo1 with
        | None    , _        -> None
        | _       , None     -> None
        | Some(v0), Some(v1) -> vo0
            $> assert(
              Val.equal v0 v1
              || invalid_arg "IndexedSet: indices must be disjoint"
            )
      ) m n

    let inters = function
      | [] -> invalid_arg "IndexedSet.inters: must be non-nil"
      | m :: [] -> m
      | m :: ml -> List.fold inter ml m

    let diff m n =
      M.merge (fun _ vo0 vo1 ->
        match vo0, vo1 with
        | None    , _        -> None
        | _       , None     -> vo0
        | Some(v0), Some(v1) -> None
            $> assert(
              Val.equal v0 v1
              || invalid_arg "IndexedSet: indices must be disjoint"
            )
      ) m n

    let fold f m z = M.fold (fun _ v z -> f v z) m z

    let fold_keys f m z = M.fold f m z

    let mem v s =
      try Val.equal v (M.find (Val.index v) s)
      with Not_found -> false

    let diff_inter_diff s t =
      fold (fun a ((s_m_i, i, t_m_i) as acc) ->
        if mem a t
        then (remove a s_m_i, add a i, remove a t_m_i)
        else acc
      ) s (s, empty, t)

    let diff_diff s t =
      let s_m_i, _, t_m_i = diff_inter_diff s t in
      (s_m_i, t_m_i)

    let inter_diff s t =
      let _, i, t_m_i = diff_inter_diff s t in
      (i, t_m_i)

    let map fn s = fold (fun kv t -> add (fn kv) t) s empty

    let map_fold _ = failwith "ToDo: IndexedSet.map_fold"
    let map_foldi _ = failwith "ToDo: IndexedSet.map_foldi"

    let filter p m = M.filter (fun _ v -> p v) m

    let partition p m = M.partition (fun _ v -> p v) m

    let split v m =
      let lt, eq, gt = M.split (Val.index v) m in
      (lt, eq <> None, gt)

    (* queries / destructors *)

    let is_empty = M.is_empty

    let compare m n = M.compare Val.compare m n

    let equal m n = M.equal Val.equal m n

    let subset m n = is_empty (diff m n)

    let disjoint _ = failwith "ToDo: IndexedSet.disjoint"
    let intersect _ = failwith "ToDo: IndexedSet.intersect"

    let iter f m = M.iter (fun _ v -> f v) m

    let foldr _ = failwith "ToDo: IndexedSet.foldr"
    let foldi _ = failwith "ToDo: IndexedSet.foldi"
    let fold_pairs _ = failwith "ToDo: IndexedSet.fold_pairs"

    let fold_product fn xs ys =
      fold (fun x -> fold (fun y -> fn x y) ys) xs

    let fold2 _ = failwith "ToDo: IndexedSet.fold2"

    let to_list m = M.fold (fun _ v z -> v :: z) m []

    let kfold x fn k = List.kfold fn (to_list x) k

    let for_all p m = M.for_all (fun _ v -> p v) m

    let exists p m = M.exists (fun _ v -> p v) m

    let exists_unique p m = M.exists_unique (fun _ v -> p v) m

    let cardinal m = M.cardinal m

    let to_array _ = failwith "ToDo: IndexedSet.to_array"

    let keys m = M.fold (fun i _ z -> i :: z) m []

    let min_elt m = snd (M.min_binding m)

    let max_elt m = snd (M.max_binding m)

    let next _ = failwith "ToDo: IndexedSet.next"

    let choose m = snd (M.choose m)

    exception Found of elt
    let trychoose s =
      try iter (fun kv -> raise (Found(kv))) s ; None
      with Found(kv) -> Some(kv)

    let extract _ = failwith "ToDo: NSIndexedSet.Lift.extract"
    let tryextract _ = failwith "ToDo: NSIndexedSet.Lift.tryextract"

    let take _ _ = failwith "ToDo: IndexedSet.take"

    let trytake p s =
      try Some(take p s) with Not_found -> None

    let take_first_pair _ = failwith "ToDo: IndexedSet.take_first_pair"
    let the_only _ = failwith "ToDo: IndexedSet.the_only"

    type idx = Val.idx

    let find i m = M.find i m

    let tryfind i m = M.tryfind i m

    let memi i m = M.mem i m

    let reduce _ = failwith "ToDo: NSIndexedSet.Lift.reduce"

    let classify _ = failwith "ToDo: NSIndexedSet.Lift.classify"

  end
end
