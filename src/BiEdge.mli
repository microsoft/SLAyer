(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Labeled bi-directional multi-edges *)

open SYMBOLIC_HEAP


module Poly : POLY_BIEDGE


module Make (A: TERM) : (BIEDGE with type a = A.t)
