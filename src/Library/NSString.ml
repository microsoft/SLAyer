(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


module String = struct
  include String

  let filteri fn s =
    let len = length s in
    let s' = create len in
    let start = ref (-1) in
    let count = ref 0 in
    for i = 0 to len - 1 do
      if fn i s.[i] then (
        s'.[!count] <- s.[i] ;
        incr count ;
        if !start < 0 then start := i ;
      )
    done ;
    if !start < 0 then start := 0 ;
    sub s' !start !count

end
