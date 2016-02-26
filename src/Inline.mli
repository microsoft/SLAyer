(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Optimize program by inlining function calls *)

open Program


(** Inline Cals, starting from [p.main]. *)
val prog : Prog.t -> Prog.t
