(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSPolyHashMap


(* Note: remove polymorphic interface *)
module HashSet = struct
  type 'a t = ('a, unit) PolyHMap.t

  let create = PolyHMap.create
  let add s x = PolyHMap.add s x ()
  let remove s x = PolyHMap.remove s x

  let cardinal = PolyHMap.length
  let mem s x = PolyHMap.mem s x

  let iter f = PolyHMap.iter (fun x () -> f x)
  let exists p = PolyHMap.exists (fun x () -> p x)
  let fold f = PolyHMap.fold (fun x () -> f x)
  let to_list s = PolyHMap.fold (fun x () l -> x :: l) s []
end
