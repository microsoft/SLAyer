(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(*============================================================================
                                    Option
  ============================================================================*)

module Option = struct

  let option d f = function
    | Some(x) -> f x
    | None -> d

  let optionk dk f = function
    | Some(x) -> f x
    | None -> dk()

  let is_some = function
    | Some(_) -> true
    | None -> false

  let is_none o = not (is_some o)

  let get = function
    | Some(x) -> x
    | None -> raise Not_found

  let from_some = get

  let or_get y = function
    | Some(x) -> x
    | None -> y

  let from_option = or_get

  let get_or o y =
    match o with
    | Some(x) -> x
    | None -> y

  let some x = Some(x)

  let bind o fn =
    match o with
    | Some(x) -> fn x
    | None -> None

  let flatten = function
    | Some(x) -> x
    | None -> None

  let map fn = function
    | Some(x) -> Some(fn x)
    | None -> None

  let map2 fn o p =
    match o,p with
    | Some(x), Some(y) -> Some(fn x y)
    | _ -> None

  let map3 fn o p q =
    match o,p,q with
    | Some(x), Some(y), Some(z) -> Some(fn x y z)
    | _ -> None

  let mapN fn os =
    try Some(fn (Array.map from_some os))
    with Not_found -> None

  let fold fn o z =
    match o with
    | Some(x) -> fn x z
    | None -> z

  let iter fn o =
    match o with
    | Some(x) -> fn x
    | None -> ()

  (* list operations *)

  let to_list = function
    | None -> []
    | Some(x) -> [x]

  let of_list = function
    | [] -> None
    | x :: _ -> Some(x)

  let meet o p =
    match o,p with
    | Some(x), Some(y) -> Some(x,y)
    | _ -> None

  let meetN os =
    try Some(List.map from_some os)
    with Not_found -> None

  let rec concat = function
    | [] -> []
    | Some(x) :: os -> x :: concat os
    | None :: os -> concat os

  let rec until_none fn a =
    match fn a with
    | None -> a
    | Some(b) -> until_none fn b

  let equal eq_fn o p =
    match o,p with
    | None, None -> true
    | Some(x), Some(y) -> eq_fn x y
    | _ -> false

  let compare fn o p =
    match o,p with
    | Some(x), Some(y) -> fn x y
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1

  let fmt none fn ff = function
    | None -> Format.fprintf ff none
    | Some(x) -> Format.fprintf ff "@[%a@]" fn x

end

let ( >>= ) o fn = Option.bind o fn

let ( =<< ) fn o = Option.bind o fn

let ( >>== ) o fn = Option.map fn o

let ( >=> ) f g x = f x >>= g

let ( <=< ) g f x = f x >>= g

let ( |+| ) = Option.meet

let ( !! ) x = Option.from_some(!x)
