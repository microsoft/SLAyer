(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Initialization *)

open Library


let thunks = ref []

let register thunk =
  thunks := thunk :: !thunks

let initialize program =
  List.iter (fun thunk -> thunk program) !thunks
