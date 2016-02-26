(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Discovery of patterns for parametric inductive predicates *)

open SymbolicHeap


(*============================================================================
                                  Discovery
  ============================================================================*)

type result = Done | More of Patn.t * (unit -> result)


val discover : XSH.t -> result

val fold : (Patn.t -> 'a -> 'a) -> XSH.t -> 'a -> 'a
