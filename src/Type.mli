(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Types for determining object layout *)

open FLD
open TYP


module
rec Fld : (FLD with type typ := Typ.t)
and Typ : (TYP with type fld := Fld.t)
