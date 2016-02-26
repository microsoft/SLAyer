(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSSet


module BinaryRelation = struct
  module Make(Ord: OrderedType) = struct

    module OrdOrd = struct
      type t = Ord.t * Ord.t
      let equal = equal_tup2 Ord.equal Ord.equal
      let compare = compare_tup2 Ord.compare Ord.compare
    end

    include Set.Make (OrdOrd)

    let inverse xs =
      map (fun (x,y) -> (y,x)) xs

    let map_prod fn xs ys =
      fold (fun x -> fold (fun y -> add (fn x y)) ys) xs empty

    let fold_product fn xs ys =
      fold (fun x -> fold (fun y -> fn x y) ys) xs

    (* incremental transitive closure *)
    let add_with_closure (x,y) us =
      let ws = filter (fun (_,y) -> Ord.equal x y) us
      and zs = filter (fun (x,_) -> Ord.equal x y) us in
      us
      |> add (x,y)
      |> union (map (fun (w,_) -> (w,y)) ws)
      |> union (map (fun (_,z) -> (x,z)) zs)
      |> union (map_prod (fun (w,_) (_,z) -> (w,z)) ws zs)

    let close xs =
      fold add_with_closure xs empty

    let choose_maximal_path us =
      let rec segment x y seg =
        try
          (* search backward *)
          let w,_ =
            choose (filter (fun (w,z) ->
              Ord.equal x z && not (List.mem w seg)
            ) us)
          in
          (segment w y (w :: seg))
        with Not_found ->
        try
          (* search forward *)
          let _,z =
            choose (filter (fun (w,z) ->
              Ord.equal y w && not (List.mem z seg)
            ) us)
          in
          (segment x z (seg @ [z]))
        with Not_found ->
          []
      in
      try
        let (x,y) = choose us in
        segment x y [x; y]
      with Not_found ->
        []

  end
end
