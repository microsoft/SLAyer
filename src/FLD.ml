(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library


module type FLD = sig
  type typ

  type t

  val off : t -> int * int option
  val id : t -> int
  val name : t -> string
  val typ : t -> typ

  val is_first : t -> bool

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val mk : int * int option -> string -> t
    (** [mk offset name] creates a fresh field for [offset] and name based on [name]. *)

  val fmt : t formatter
  val fmt_caml : t formatter

  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit

  val unsafe_create : int -> int * int option -> string -> typ -> t

  val find_by_name : typ -> string -> (t * typ) option
    (** [find_by_name ty name] returns the member of [ty] named [name]. *)

end
