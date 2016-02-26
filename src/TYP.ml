(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library


module type TYP = sig

  type fld

  type t = t_desc HashCons.hc

  and t_desc = private
    | Top                                           (** universal type *)
    | Named of string                               (** (recursive) reference to named structure/union *)
    | Bool
    | Int of bool * int                             (** unsigned, size *)
    | Float of int                                  (** size *)
    | Pointer of t                                  (** target *)
    | Array of t * int64 option * int64             (** element type, length, size *)
    | Structure of string * (fld * t) list * int64  (** name, members in declaration order, size *)
    | Union of string * (fld * t) list * int64      (** name, members in declaration order, size *)
    | Enum of string * (string * int) list * int64  (** name, enumerators, size *)
    | Function of t * t list * bool                 (** result, arguments, vararg *)

  val desc : t -> t_desc
    (** [desc t] is the descriptor named by type [t] *)

  val name : t_desc -> t
    (** [name d] is the unique type name of descriptor [d] *)

  val id : t -> int

  val mkTop : t
  val mkNamed : string -> t
  val mkBool : t
  val mkInt : bool -> int -> t
  val mkFloat : int -> t
  val mkPointer : t -> t
  val mkArray : t -> int64 option -> int64 -> t
  val mkStructure : string -> ((fld * t) list) -> int64 -> t
  val mkUnion : string -> ((fld * t) list) -> int64 -> t
  val mkEnum : string -> ((string * int) list) -> int64 -> t
  val mkFunction : t -> (t list) -> bool -> t


  val compare : t -> t -> int
  val equal : t -> t -> bool
  val equal_desc : t_desc -> t_desc -> bool
  val hash : t -> int

  val fmt : t formatter
  val fmt_caml : t formatter


  val fst_flds : t -> fld list

  val all_paths : t -> ((int * int option) * fld list * t) list
    (** [all_paths ty] is all the access paths through the type tree [ty], from
        the root downwards to scalar leaves.  The head of each path is the most
        deeply nested field, and the list is in the same order as the fields are
        defined. *)

  val all_offsets : t -> ((int * int option) * fld list * t) list
    (** [all_offsets ty] is a list of all the access paths to distinct offsets through the type tree [ty],
        from the root downwards to scalar leaves.  The head of each path is the most deeply nested field, and
        the list is in the same order as the fields are defined. *)

  val paths_at_offset : t -> (int * int option) -> (fld list * t) list

  val sizeof : t -> int64
    (** [sizeof t] is the number of bytes occupied by an object of type [t].
        Follows http://msdn.microsoft.com/en-us/library/cc953fe1.aspx. *)

  val find_by_name : string -> t option
    (** [find_by_name name] returns the type named [name]. *)

  val of_fld : t -> fld -> t option
    (** [of_fld t f] returns the type of the [f] field of type [t].  *)

  val fold_defined : (t -> 'z -> 'z) -> 'z -> 'z
    (** [fold_defined] enumerates all currently defined types. *)

end
