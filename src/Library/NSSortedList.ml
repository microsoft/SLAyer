(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


module SortedList = struct

  let rec is_sorted cmp = function
    | [] | [_] -> true
    | x :: (y :: _ as yzs) -> cmp x y <= 0 && is_sorted cmp yzs

  let check_sorted cmp xs = assert( is_sorted cmp xs ); xs

  let sort cmp x = List.fast_sort cmp x

  let rec add cmp x = function
    | [] -> [x]
    | y :: ys as yys ->
        let ord = cmp x y in
        if      ord < 0 then x :: yys
        else if ord = 0 then yys
        else (* ord > 0 *)   y :: add cmp x ys

  let rec merge cmp cons bbs0 bbs1 =
    match bbs0, bbs1 with
    | []       , bbs1      -> bbs1
    | bbs0     , []        -> bbs0
    | b0 :: bs0, b1 :: bs1 ->
        let ord = cmp b0 b1 in
        if      ord <= 0 then cons b0 (merge cmp cons bs0 bbs1)
        else (* ord > 0 *)    cons b1 (merge cmp cons bbs0 bs1)

  let rec union cmp cons bbs0 bbs1 =
    match bbs0, bbs1 with
    | []       , bbs1      -> bbs1
    | bbs0     , []        -> bbs0
    | b0 :: bs0, b1 :: bs1 ->
        let ord = cmp b0 b1 in
        if      ord < 0 then cons b0 (union cmp cons bs0 bbs1)
        else if ord = 0 then          union cmp cons bs0 bbs1
        else (* ord > 0 *)   cons b1 (union cmp cons bbs0 bs1)

  let rec inter cmp xxs yys =
    match xxs, yys with
    | []   , _     -> []
    | _    , []    -> []
    | x::xs, y::ys ->
        match cmp x y with
        | n when n < 0 -> inter cmp xs yys
        | 0            -> x :: (inter cmp xs ys)
        | _            -> inter cmp xxs ys

  let rec intersect cmp xxs yys =
    match xxs, yys with
    | []   , _     -> false
    | _    , []    -> false
    | x::xs, y::ys ->
        match cmp x y with
        | n when n < 0 -> intersect cmp xs yys
        | 0            -> true
        | _            -> intersect cmp xxs ys

  let rec diff cmp xxs yys =
    match xxs, yys with
    | []   , _     -> []
    | _    , []    -> xxs
    | x::xs, y::ys ->
        match cmp x y with
        | n when n < 0 -> x :: diff cmp xs yys
        | 0            -> diff cmp xs ys
        | _            -> diff cmp xxs ys

  let rec diff_inter_diff cmp xxs yys =
    match xxs, yys with
    | []   , []    -> ([], [], [])
    | []   , _     -> ([], [], yys)
    | _    , []    -> (xxs, [], [])
    | x::xs, y::ys ->
        match cmp x y with
        | n when n < 0 ->
            let xs_yys, is, yys_xs = diff_inter_diff cmp xs yys in
            (x::xs_yys, is, yys_xs)
        | 0            ->
            let xs_ys, is, ys_xs = diff_inter_diff cmp xs ys in
            (xs_ys, x::is, ys_xs)
        | _            ->
            let xxs_ys, is, ys_xxs = diff_inter_diff cmp xxs ys in
            (xxs_ys, is, y::ys_xxs)

  let rec mem cmp x = function
    | [] -> false
    | y :: xs ->
        match cmp x y with
        | n when n < 0 -> false
        | 0            -> true
        | _            -> mem cmp x xs

  let rec subsupset cmp xxs yys =
    match xxs, yys with
    | []   , []    -> (true, true)
    | _::_ , []    -> (false, true)
    | []   , _::_  -> (true, false)
    | x::xs, y::ys ->
        match cmp x y with
        | n when n < 0 -> (false, snd (subsupset cmp xs yys))
        | 0            -> subsupset cmp xs ys
        | _            -> (fst (subsupset cmp xxs ys), false)

end
