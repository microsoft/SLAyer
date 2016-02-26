(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib


module List = struct
  include List

  let cons x xs = x :: xs

  let tryfind pred lst = try Some(find pred lst) with Not_found -> None

  let rec no_duplicates = function
    | [] -> true
    | x::xs -> if (List.mem x xs) then false else (no_duplicates xs)

  let rec iteri_aux fn i = function
    | [] -> ()
    | x::xs -> fn i x; iteri_aux fn (i+1) xs
  let iteri fn xs = iteri_aux fn 0 xs

  let fold fn l a = fold_left (fun a x -> fn x a) a l

  let foldi fn s z = snd (fold (fun x (i,z) -> (i+1, fn i x z)) s (0, z))

  let fold2 fn xs ys a = fold_left2 (fun a x y -> fn x y a) a xs ys

  let fold3 fn =
    let rec fold3_fn xs ys zs a =
      match xs, ys, zs with
      | [], [], [] -> a
      | x::xs, y::ys, z::zs -> fold3_fn xs ys zs (fn x y z a)
      | _ -> invalid_arg "List.fold3: expected equal-length lists"
    in
    fold3_fn

  let mapi fn xs = rev (snd (fold (fun x (i,z) -> (i+1, fn i x :: z)) xs (0,[])))

  let map3 fn =
    let rec map3 xs ys zs =
      match xs, ys, zs with
      | [], [], [] -> []
      | x::xs, y::ys, z::zs -> fn x y z :: map3 xs ys zs
      | _ -> invalid_arg "List.map3: expected equal-length lists"
    in
    map3

  let map_append fn xs ys = fold (fun x ys -> fn x :: ys) xs ys

  let map_to_array fn xs =
    match xs with
    | [] -> [||]
    | hd::tl as xs ->
        let a = Array.make (List.length xs) (fn hd) in
        let rec set i = function
          | [] -> a
          | hd::tl -> Array.set a i (fn hd); set (i+1) tl
        in
        set 1 tl

  let map_fold fn (xs,z) =
    fold_right (fun x (ys,z) ->
      let y, z = fn (x,z) in
      (y::ys, z)
    ) xs ([],z)

  let reduce fn = function
    | [] -> invalid_arg "List.reduce"
    | x::xs -> fold fn xs x

  let rec kfold fn xs k a =
    match xs with
    | [] -> k a
    | x::xs -> fn x (kfold fn xs k) a

  let rec kfold2 fn xs ys k a =
    match xs, ys with
    | [], [] -> k a
    | x::xs, y::ys -> fn x y (kfold2 fn xs ys k) a
    | _ -> invalid_arg "List.kfold2: expected equal-length lists"

  let rec kfold3 fn xs ys zs k a =
    match xs, ys, zs with
    | [], [], [] -> k a
    | x::xs, y::ys, z::zs -> fn x y z (kfold3 fn xs ys zs k) a
    | _ -> invalid_arg "List.kfold3: expected equal-length lists"

  let rec fold_pairs fn xs a =
    match xs with
    | [] -> a
    | x::xs -> fold (fn x) xs (fold_pairs fn xs a)

  let rec kfold_pairs fn k xs a =
    match xs with
    | [] -> k a
    | x::xs -> kfold (fn x) xs (kfold_pairs fn k xs) a

  let rec fold_product fn xs ys a =
    match xs with
    | [] -> a
    | x::xs -> fold (fn x) ys (fold_product fn xs ys a)

  let rec kfold_product xs ys fn k a =
    match xs with
    | [] -> k a
    | x::xs -> kfold (fn x) ys (kfold_product xs ys fn k) a

  let rec prefixes = function
    | [] -> [[]]
    | x :: xs -> [] :: map (fun l -> x :: l) (prefixes xs)

  let rec infixes = function
    | [] -> [[]]
    | x :: xs -> fold (fun l z -> (x :: l) :: z) (prefixes xs) (infixes xs)

  let rec powerlist = function
    | [] -> [[]]
    | x :: xs ->
        let pow_xs = powerlist xs in
        fold (fun l z -> (x :: l) :: z) pow_xs pow_xs

  let combs fn xs ys =
    let rec loop1 zs xs = function
      | [] -> [[]]
      | y :: ys ->
          let rec loop2 zs a = function
            | [] -> a
            | x :: xs ->
                loop2
                  (x :: zs)
                  (fold (fun comb a -> (fn x y comb) :: a) (loop1 zs xs ys) a)
                  xs
          in loop2 [] (loop2 [] [] zs) xs
    in loop1 [] xs ys

  let rec permutations fn xs =
    let rec loop zs xs ps =
      match xs with
      | [] -> ps
      | x :: xs ->
          loop (x :: zs) xs
            (List.fold_left (fn x) ps (permutations fn (zs @ xs)))
    in
    if xs = [] then [[]] else loop [] xs []

  let fin_funs xs ys =
    List.map (List.combine xs) (permutations (fun x ps p -> (x :: p) :: ps) ys)

  let map_partial fn xs =
    List.fold_right (fun x maybe_ys ->
      match maybe_ys with
      | None -> None
      | Some ys ->
          match fn x with
          | None -> None
          | Some y -> Some (y::ys)
    ) xs (Some [])

  let classify fn xs =
    let rec classify_one x = function
      | xs :: xss when fn x (List.hd xs) -> (x :: xs) :: xss
      | xs :: xss -> xs :: (classify_one x xss)
      | [] -> [[x]]
    in
    fold classify_one xs []

  let divide fn ys =
    let rec divide_ xss ys =
      match xss, ys with
      | xs :: xss, y :: ys when fn y (List.hd xs) -> divide_ ((y :: xs) :: xss) ys
      | xss, y :: ys -> divide_ ([y] :: xss) ys
      | xss, [] -> List.rev_map List.rev xss
    in
    divide_ [] ys

  let rec range i j = if i <= j then i::(range (i+1) j) else []

  let rec replicate n x =
    if n <= 0 then [] else x :: replicate (n-1) x

  let inter xs ys = List.filter (fun x -> List.mem x ys) xs

  let union xs ys = fold (fun x us -> if List.mem x us then us else x::us) xs ys

  let diff xs rs = find_all (fun x -> not (List.mem x rs)) xs

  let rec take_ p ys xs =
    match xs with
    | x :: xs ->
        if p x then
          (x, rev_append ys xs)
        else
          take_ p (x :: ys) xs
    | [] ->
        raise Not_found

  let take p xs =
    take_ p [] xs

  let exists_unique p xs =
    let module M = struct exception Found end in
    try
      fold (fun x found ->
        if found
        then not (p x) || raise M.Found
        else p x
      ) xs false
    with M.Found -> false

  let rec equal fn xs ys =
    if xs == ys then true else
    match xs,ys with
    | [], [] -> true
    | [], _::_
    | _::_, [] -> false
    | x::xs, y::ys -> fn x y && equal fn xs ys

  let rec compare fn xs ys =
    if xs == ys then 0 else
    match xs,ys with
    | [], _::_ -> -1
    | [], [] -> 0
    | _::_, [] -> 1
    | x::xs, y::ys -> compare_tup2 fn (compare fn) (x,xs) (y,ys)

  let rec compare_lex fn xs ys =
    if xs == ys then 0 else
    match xs,ys with
    | [], _::_ -> -1
    | [], [] -> 0
    | _::_, [] -> 1
    | x::xs, y::ys -> compare_tup2 (compare_lex fn) fn (xs,x) (ys,y)

  let compare_sorted cmp xs ys =
    if xs == ys then 0 else
    let rec loop xs ys = match xs,ys with
      | [], _::_ -> -1
      | [], [] -> 0
      | _::_, [] -> 1
      | x::xs, y::ys -> compare_tup2 cmp loop (x,xs) (y,ys)
    in
    loop (fast_sort cmp xs) (fast_sort cmp ys)

  let fmt sep fn ff xs =
    let rec aux ff = function
      | [] -> ()
      | [x] -> (try fn ff x with Nothing_to_fmt -> ())
      | x::xs ->
          try Format.fprintf ff "%a%( fmt %)%a" fn x sep aux xs
          with Nothing_to_fmt -> aux ff xs
    in
    aux ff xs

  let fmtt sep ff xs =
    let rec aux ff = function
      | [] -> ()
      | [x] -> (try x ff with Nothing_to_fmt -> ())
      | x::xs ->
          try Format.fprintf ff "%t%( fmt %)%a" x sep aux xs
          with Nothing_to_fmt -> aux ff xs
    in
    aux ff xs

  module Set (Elt: sig type t end) = struct
    type elt = Elt.t
    type t = elt list

    let empty = []
    let is_empty s = s = []
    let add e s = e :: s
    let singleton e = [e]
    let iter = iter
    let map = map
    let fold = fold
    let map_fold = map_fold
    let kfold fn s z = kfold s fn z
    let for_all = for_all
    let exists = exists
    let exists_unique = exists_unique
    let filter = filter
    let cardinal = length
    let of_list s = s
    let to_list s = s
    let choose = function x::_ -> x | [] -> raise Not_found
    let union = rev_append
    let diff = diff
  end

  module SetOrd (Elt: OrderedType) = struct
    include Set(Elt)
    let equal x y = equal Elt.equal x y
    let compare x y = compare Elt.compare x y

    let rec remove x = function
      | [] -> []
      | y::ys when Elt.equal x y -> ys
      | y::ys -> y :: remove x ys

    let diff_inter_diff xs ys =
      let xs = fast_sort Elt.compare xs
      and ys = fast_sort Elt.compare ys in
      let rec did xxs yys =
        match xxs, yys with
        | [], ys -> ([], [], ys)
        | xs, [] -> (xs, [], [])
        | x::xs, y::ys ->
            let o = Elt.compare x y in
            if o = 0 then
              let xs_ys, i, ys_xs = did xs ys in
              (xs_ys, x::i, ys_xs)
            else if o < 0 then
              let xs_yys, i, yys_xs = did xs yys in
              (x::xs_yys, i, yys_xs)
            else
              let xxs_ys, i, ys_xxs = did xxs ys in
              (xxs_ys, i, y::ys_xxs)
      in
      did xs ys
  end

end
