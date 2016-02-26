(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Graph representation of heap structure of symbolic heaps *)

open Library

open Expression
open SYMBOLIC_HEAP
open SymbolicHeap


module Edge : sig
  include BIEDGE
  with type a = Exp.t option

  val meet : t -> t -> t

  val append : t -> t -> t option

  val to_ls : Patn.t -> t -> SH.t
end


include Set.S with type elt = Edge.t

val fmt : t formatter

val add_with_closure : Edge.t -> t -> t

val union_with_closure : t -> t -> t

val transitive_closure : t -> t


val add_with_closure_tmr : Timer.t
