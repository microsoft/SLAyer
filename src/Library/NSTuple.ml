(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSList


module Tuple = struct

  let map2 f g (x,y) = f x, g y
  let map3 fn fo fp (x,y,z) = fn x, fo y, fp z

  let fmt fn ff = Format.fprintf ff "@[(%a)@]" (List.fmt ",@," fn)

end
