(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Hash-consing construction based on weak hash tables *)

open Library

let verbose = ref 0
module L = (val Log.std verbose : Log.LOG)


type 'a hc = {
  desc : 'a;
  id   : int;
  hash : int;
}


module Make (H : HashedType) = struct

  include Weak.Make (struct
    type t = H.t hc
    let equal x y = H.equal x.desc y.desc
    let hash x = x.hash
  end)


  let id = ref 0

  let intern t desc =
    assert( !id < max_int );
    let data = {desc; id= !id; hash= H.hash desc} in
    (* Note: If desc is already interned, this record construction is
       unnecessary.  If we copied the implementation of Weak into this module,
       we could avoid constructing the record just to search for it. *)
    let data' = merge t data in
    if data == data' (* data just added *) then incr id ;
    assert( data == data' || data.id <> data'.id );
    data'

end
