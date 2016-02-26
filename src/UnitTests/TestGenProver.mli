(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Utility code to generate stand-alone unit tests *)

open Variable
open SymbolicHeap


val gen_query : int * string * (Vars.t * SH.t * Vars.t * SH.t) -> unit
