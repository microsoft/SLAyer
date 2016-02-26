(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSHashtbl


module PolyHMap = struct
  include Hashtbl
  let add = replace
end
