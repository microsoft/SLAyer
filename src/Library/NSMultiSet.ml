(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSList
open NSMap


module MultiSet = struct
  module type S = sig
    include Set0
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val remove : elt -> t -> t
    val foldm : (elt -> int -> 'z -> 'z) -> t -> 'z -> 'z
    val fold_pairs : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Make (Ord: OrderedType) = struct
    module M = Map.Make(Ord)

    type elt = Ord.t
    type t = int M.t

    let empty = M.empty

    let is_empty = M.is_empty

    let addn x n s = M.add x (try n + M.find x s with Not_found -> n) s

    let removen x n s = M.add x (try let x = M.find x s - n in if x<0 then 0 else x with Not_found -> 0) s

    let add x s = addn x 1 s

    let singleton x = add x empty

    let union s t = M.fold addn s t

    let choose s = fst (M.choose s)

    let remove x s =
      try
        let m = M.find x s in
        if m = 1
        then M.remove x s
        else M.add x (m - 1) s
      with Not_found -> s

    let diff s t = M.fold removen s t

    let iter f s = M.iter (fun x m -> for _i = 1 to m do f x done) s

    let map f s = M.fold (fun x m t -> addn (f x) m t) s empty

    let fold f s z =
      M.fold (fun x m z ->
        let rec loop n z = if n = 0 then z else loop (n-1) (f x z) in
        loop m z
      ) s z

    let foldm = M.fold

    let map_fold _ = failwith "ToDo: MultiSet.map_fold"

    let kfold _ = failwith "ToDo: MultiSet.kfold"

    let rec fold_pairs fn xs a =
      try
        let x = choose xs in
        let xs = remove x xs in
        fold (fun a -> fn x a) xs (fold_pairs fn xs a)
      with
        Not_found -> a

    let for_all p s = M.for_all (fun x _ -> p x) s

    let exists p s = M.exists (fun x _ -> p x) s

    let exists_unique p s = M.exists_unique (fun x _ -> p x) s

    let filter p s = M.filter (fun x _ -> p x) s

    let cardinal s = M.fold (fun _ m c -> c + m) s 0

    let of_list l = List.fold add l empty

    let to_list s =
      M.fold (fun x n l ->
        let rec loop l = function
          | 0 -> l
          | n -> loop (x::l) (n-1) in
        loop l n
      ) s []

    let equal s t = M.equal Pervasives.( = ) s t
    let compare s t = M.compare Pervasives.compare s t

  end
end
