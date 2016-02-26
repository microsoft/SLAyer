(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Variables *)

open Library

open Type


(*============================================================================
                                     Var
  ============================================================================*)

(** Logical Variables *)
module rec Var : sig
  type sort = PointerSort | IntegerSort | BooleanSort | OffsetSort

  type t

  val id : t -> int
  val name : t -> string
  val sort : t -> sort

  (** [gensym s a] returns a fresh variable whose name is based on [s],
      and is of type [a]. *)
  val gensym : string -> sort -> t
  val gensyms : t list -> t list

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  (** Formatting contexts.  Members of [fst fxt] are existential and members of
      [snd fxt] additionally occur at most once. *)
  type fxt = Vars.t * Vars.t

  val fmtp : fxt -> t formatter
  val fmt : t formatter
  val fmt_sort : sort formatter
  val fmt_caml : t formatter

  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit

  val unsafe_create : int -> string -> sort -> t

  val sort_of_type : Typ.t -> sort
end



(*============================================================================
                                 Collections
  ============================================================================*)

(** Sets of variables *)
and Vars : sig
  include Set.S with type elt = Var.t

  val gensyms : t -> t

  val fmtp_embrace :
    (unit,Format.formatter,unit)format -> (unit,Format.formatter,unit)format -> Var.fxt -> t formatter

  val fmt_embrace :
    (unit,Format.formatter,unit)format -> (unit,Format.formatter,unit)format ->
    t formatter

  val fmtp : Var.fxt -> t formatter
  val fmt : t formatter
  val fmt_caml : t formatter
end


module VarMap : (Map.S with type key = Var.t)
