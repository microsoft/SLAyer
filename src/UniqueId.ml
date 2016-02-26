(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Unique Identifiers *)

open Library

module L = (val Log.std Config.vFE : Log.LOG)


module type S = sig
  type data
  type uniq

  val compare : uniq -> uniq -> int
  val equal : uniq -> uniq -> bool
  val hash : uniq -> int
  (* val fmt : uniq formatter *)

  val id : uniq -> int
  val gensym : data -> uniq

  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit

  val unsafe_create : int -> data -> uniq
end

module Make (M: sig
  type data
  type uniq
  val get : uniq -> int
  val set : int -> data -> uniq
end) =
struct
  type data = M.data
  type uniq = M.uniq

  let compare x y = Pervasives.compare (M.get x) (M.get y)
  let equal x y = Pervasives.( = ) (M.get x) (M.get y)
  let hash x = Hashtbl.hash (M.get x)

  let id x = M.get x

  let initial = 0
  let count = ref initial
  let gensym x = incr count ; M.set !count x
  let unsafe_create id data = count := max !count id; M.set id data

  let marshal chan =
    L.printf 100 "UniqueId.marshal %i" !count ;
    Marshal.to_channel chan !count []

  let unmarshal chan =
    (fun()-> L.printf 100 "UniqueId.unmarshal %i" !count) <& let()=()in
    (* unmarshaling makes no attempt to preserve uniqueness, so assumes none are already constructed *)
    assert( !count = initial );
    count := (Marshal.from_channel chan)
end
