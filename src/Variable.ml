(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Variables *)

open Library
open Type

module L = (val Log.std Config.vVar : Log.LOG)


(*============================================================================
                                     Var
  ============================================================================*)

module Var = struct

  type sort = PointerSort | IntegerSort | BooleanSort | OffsetSort

  type t = { id: int; name: string; sort: sort }

  include UniqueId.Make(struct
    type data = string * sort
    type uniq = t
    let get {id} = id
    let set id (name, sort) = {id; name; sort}
  end)

  let name v = v.name
  let sort v = v.sort

  let gensym n s = gensym (n, s)
  let gensyms vl = List.map (fun v -> gensym v.name v.sort) vl
  let unsafe_create i n s = unsafe_create i (n, s)


  module Vars = Set.Make(struct
    type _t = t
    type t = _t
    let compare = compare
    let equal = equal
  end)


  (** Formatting contexts.  Members of [fst fxt] are existential and members of
      [snd fxt] additionally occur at most once. *)
  type fxt = Vars.t * Vars.t

  (** Verbosity levels:
     0. display abbreviated names, quantifier strength
     1. add integer identities except of uniquely-occurring existentials
     2. do not abbreviate program specific names
     3. do not abbreviate frontend-introduced names
     4. add integer identities of uniquely-occurring existentials
     5. display sorts
  *)
  let fmt_sort ff = function
    | PointerSort -> Format.pp_print_string ff "P"
    | IntegerSort -> Format.pp_print_string ff "I"
    | BooleanSort -> Format.pp_print_string ff "B"
    | OffsetSort  -> Format.pp_print_string ff "O"

  let fmtp fxt ff v =
    let is_existl v = Vars.mem v (fst fxt) in
    let is_unique v = Vars.mem v (snd fxt) in
    let fmt_name ff v =
      if !Config.vVar >= (if is_unique v then 5 else 0) then
        Format.pp_print_string ff (Hooks.var_name v.name)
    in
    let fmt_strength ff v =
      if is_existl v then
        Format.pp_print_string ff "?"
      else if !Config.vVar >= 1 then
        Format.pp_print_string ff "!"
    in
    let fmt_id ff v =
      if !Config.vVar >= (if is_unique v then 4 else 1) then
        Format.pp_print_int ff v.id
    in
    let fmt_sort ff v =
      if !Config.vVar >= 5 then
        fmt_sort ff v.sort
    in
    Format.fprintf ff "%a%a%a%a" fmt_name v fmt_strength v fmt_id v fmt_sort v

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml_sort ff x =
    Format.pp_print_string ff
      (match x with
      | PointerSort -> "Var.PointerSort"
      | IntegerSort -> "Var.IntegerSort"
      | BooleanSort -> "Var.BooleanSort"
      | OffsetSort -> "Var.OffsetSort")

  let fmt_caml ff {id; name; sort} =
    Format.fprintf ff "(Var.unsafe_create %i \"%s\" %a)"
      id name fmt_caml_sort sort

  let sort_of_type ty =
    match Typ.desc ty with
    | Typ.Bool       -> BooleanSort
    | Typ.Int _
    | Typ.Float _    -> IntegerSort
    | Typ.Pointer _
    | Typ.Function _
    | Typ.Top
    | Typ.Array _    -> PointerSort
    | Typ.Named _
    | Typ.Structure _
    | Typ.Union _
    | Typ.Enum _ ->
        Config.vTyp := max !Config.vTyp 2 ;
        failwithf "sort_of_type: unexpected type: %a" Typ.fmt ty

end



(*============================================================================
                                 Collections
  ============================================================================*)

module Vars = struct
  include Var.Vars

  let gensyms s = map (fun v -> Var.gensym (Var.name v) (Var.sort v)) s

  let fmtp_embrace prefix suffix fxt ff s =
    match to_list s with
    | [] -> ()
    | vl ->
        Format.fprintf ff "%( fmt %)%a%( fmt %)"
          prefix (List.fmt ",@ " (Var.fmtp fxt)) vl suffix

  let fmt_embrace prefix suffix ff = fmtp_embrace prefix suffix (empty,empty) ff

  let fmtp fxt ff = fmtp_embrace "" "" fxt ff

  let fmt ff = fmtp (empty,empty) ff

  let fmt_caml ff vs =
    Format.fprintf ff "(Vars.of_list [@,@[%a@]])"
      (List.fmt ";@ " Var.fmt_caml) (to_list vs)

end


module VarMap = Map.Make(Var)
