(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSMultiSet
open NSMultiIndexedSet
open NSMultiMap
open NSList


module MultiIndexedMultiSet = struct
  module type S = sig
    include MultiSet.S
    type elts
    type idx
    val memi : idx -> t -> bool
    val find : idx -> t -> elts
    val keys : t -> idx list
  end

  module Make (Idx: MultiIndexedType) (EltSet: Set0 with type elt = Idx.t) = struct
    module Key = struct
      type t = Idx.idx
      let equal = Idx.equal_idx
      let compare = Idx.compare_idx
    end

    module M = MultiMap.Make (Key) (EltSet)

    type idx = Idx.idx
    type elt = Idx.t
    type elts = EltSet.t
    type t = M.t

    let empty = M.empty
    let is_empty s = M.is_empty s
    let add e s = M.add (Idx.index e) e s
    let singleton e = add e empty
    let iter fn s = M.iter (fun _ e -> fn e) s
    let map fn s = M.map fn s
    let fold fn s z = M.fold (fun _ e z -> fn e z) s z
    let map_fold _ _ = failwith "ToDo: MultiIndexedMultiSet.map_fold"
    let kfold _ _ _ _ = failwith "ToDo: MultiIndexedMultiSet.kfold"
    let for_all _ _ = failwith "ToDo: MultiIndexedMultiSet.for_all"
    let exists _ _ = failwith "ToDo: MultiIndexedMultiSet.exists"
    let exists_unique _ _ = failwith "ToDo: MultiIndexedMultiSet.exists_unique"
    let filter fn s = M.filter (fun _ e -> fn e) s
    let cardinal s = M.length s
    let of_list es = List.fold add es empty
    let to_list s = fold List.cons s []
    let choose _ = failwith "ToDo: MultiIndexedMultiSet.choose"
    let union _ _ = failwith "ToDo: MultiIndexedMultiSet.union"
    let diff _ _ = failwith "ToDo: MultiIndexedMultiSet.diff"
    let equal _ _ = failwith "ToDo: MultiIndexedMultiSet.equal"
    let compare _ _ = failwith "ToDo: MultiIndexedMultiSet.compare"
    let remove _ _ = failwith "ToDo: MultiIndexedMultiSet.remove"
    let foldm fn s z = M.fold_keys (fun _ es z -> fn (EltSet.choose es) (EltSet.cardinal es) z) s z
    let fold_pairs _ _ _ = failwith "ToDo: MultiIndexedMultiSet.fold_pairs"
    let memi i s = M.mem i s
    let find i s = M.find i s
    let keys s = M.fold_keys (fun i _ is -> i :: is) s []

  end
end
