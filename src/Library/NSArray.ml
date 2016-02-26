(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSList


module Array = struct
  include Array

  let swap a i j =
    let a_i = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- a_i

  let reverse a i j =
    let rec loop n =
      if n < j/2 then (
        swap a n (j-n-1) ;
        loop (j+1)
      )
    in
    loop i

  let equal eq x y =
    let l = Array.length x in
    l = (Array.length y) &&
    let rec equal_ i =
      if i >= l then true else
      eq x.(i) y.(i) && equal_ (i+1)
    in equal_ 0

  let compare cmp x y =
    let l = Array.length x in
    let o = Pervasives.compare l (Array.length y) in if o <> 0 then o else
    let rec compare_ i =
      if i >= l then 0 else
      let o = cmp x.(i) y.(i) in if o <> 0 then o else compare_ (i+1)
    in compare_ 0

  let fmt sep fn ff a = List.fmt sep fn ff (to_list a)

  let for_all pd a = Array.fold_right (fun x b -> b && pd x) a true

end
