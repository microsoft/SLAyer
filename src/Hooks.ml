(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Hooks for very applicaton-specific behavior. *)

open Library


let var_name name =
  if !Config.vVar > 2 then
    name
  else
    match name with
    | "arg_tmp" -> "at"
    | "cast_tmp" -> "ct"
    | "incr_load_tmp" -> "ilt"
    | "load_tmp" -> "lt"
    | "lval_cast_tmp" -> "lct"
    | "lval_kill_tmp" -> "lct"
    | "store_cast_tmp" -> "sct"
    | _ when !Config.vVar > 1 ->
        name
    | "_WDF_DEVICE_EXTENSION_TYPE_INFO" -> "DETI"
    | _ ->
        String.filteri (fun i c -> i = 0 || c <> Char.lowercase c) name
