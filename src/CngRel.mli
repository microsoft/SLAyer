(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Congruence-closed relations *)

open CONGRUENCE_RELATION
open Expression


include CONGRUENCE_RELATION with type exp := Exp.t and type exps := Exps.t
