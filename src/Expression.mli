(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Expressions *)

open Library

open Type
open Variable

val stats : unit -> int * int * int * int * int * int


(*============================================================================
                                     Exp
  ============================================================================*)

module Exp : sig

  type op1 = Allocd | Not | ZMin
  type op2 = ZLt | ZLe | ZGt | ZGe | ZDiv | ZRem | ZMod
  type op3 = Ite
  type opN = Distinct | And | Or | ZAdd | ZMul | UFun of string

  type t = t_desc HashCons.hc

  and t_desc = private
    | Var of Var.t
    (* Pointer expressions *)
    | App of t * t     (** Application of pointer expressions                      *)
    | Nil              (** Constant symbol for NULL pointer                        *)
    | Add of Fld.t     (** Function symbol for adding field to pointer             *)
    | Sub of Fld.t     (** Function symbol for subtracting field from pointer      *)
    | Idx              (** Function symbol for indexing into an array              *)
    (* Offset expressions *)
    | Bas of Typ.t     (** Constant symbol for empty offset (from pointer of type) *)
    (* Integer, Boolean and String expressions *)
    | Eq of t * t
    | Num of int64
    | Str of string
    | Op1 of op1 * t_desc
    | Op2 of op2 * t_desc * t_desc
    | Op3 of op3 * t_desc * t_desc * t_desc
    | OpN of opN * t_desc array

  val desc : t -> t_desc
    (** [desc e] is the descriptor named by expression [e] *)

  val name : t_desc -> t
    (** [name d] is the unique expression name of descriptor [d] *)


  (* Sort operations ======================================================== *)

  val sort_of : t -> Var.sort

  val is_pointer : t -> bool
  val is_integer : t -> bool
  val is_boolean : t -> bool
  val is_offset : t -> bool


  (* Constructors =========================================================== *)

  exception IllSorted of t_desc

  val mkVar : Var.t -> t

  (* Pointer expressions *)
  val mkApp : t -> t -> t
    (** [mkApp f a] constructs an expression equivalent to [App(f,a)] *)
  val getApp : t -> (t * t) option
    (** [getApp (App(f,a))] is [Some(f,a)], and [None] in other cases *)
  val nil   : t
  val mkAdd : t -> Fld.t -> t
  val mkSub : t -> Fld.t -> t
  val mkAdds : t -> Fld.t list -> t
    (** [mkAdds e [fN; ...; f2; f1]] is [e+f1+f2+...+fN] *)
  val mkSubs : t -> Fld.t list -> t
    (** [mkSubs e [fN; ...; f2; f1]] is [e-fN-...-f2-f1] *)
  val invert : t -> t option
    (** [invert] transforms [Add(o)] to [Sub(o)] and vice versa, and returns
        [None] in other cases *)
  val mkIdx : t -> t -> t
    (** [mkIdx a i] creates an expression denoting the address of element [i] of array [a] *)

  (* Offset expressions *)
  val mkBas : Typ.t -> t

  (* Integer expressions *)
  val zero   : t
  val one    : t
  val mkNum  : int64 -> t
  val mkStr  : string -> t
  val mkZMin : t -> t
  val mkZDiv : t -> t -> t
  val mkZRem : t -> t -> t
  val mkZMod : t -> t -> t
  val mkZAdd : t array -> t
  val mkZSub : t array -> t
  val mkZMul : t array -> t
  val mkUFun : string -> t array -> t

  (* Boolean expressions *)
  val tt : t
  val ff : t

  val mkNot : t -> t
  val mkAnd : t array -> t
  val mkOr  : t array -> t
  val mkImp : t -> t -> t
  val mkIff : t -> t -> t
  val mkXor : t -> t -> t

  val mkEq : t -> t -> t
  val mkDq : t -> t -> t
  val mkDistinct : t array -> t
  val mkAllocd : t -> t

  val mkZLt : t -> t -> t
  val mkZLe : t -> t -> t
  val mkZGt : t -> t -> t
  val mkZGe : t -> t -> t

  (* Generic expressions *)
  val mkIte : t -> t -> t -> t

  (* Conversions *)
  val convert : Var.sort -> t -> t option

  val mkOp1 : op1 -> t_desc -> t
  val mkOp2 : op2 -> t_desc -> t_desc -> t
  val mkOp3 : op3 -> t_desc -> t_desc -> t_desc -> t
  val mkOpN : opN -> t_desc array -> t


  (* Queries ================================================================ *)

(*   val size : t -> int *)

(*   val is_atomic : t -> bool *)

  val fv : t -> Vars.t

(*   val diff_inter_diff : t -> t -> t * t * t *)

(*   val partition : (t -> bool) -> t -> t * t *)

  val remove : (t_desc -> bool) -> t -> t


  (* Maps and Folds ========================================================= *)

  (** [kmap_fold before after e sa0 la0] simultaneously maps and folds overa
      the structure of [e], in continuation-passing style.

      Each subexpression [x] of [e] is passed to [before] prior to traversing
      its children.

      Calling the continuation [k] traverses [x]'s children in right-to-left
      order.  When calling [k] the structural accumulator [sa] and linear
      accumulator [la] can be updated.  After traversing [x]'s children, the
      expression [x'] returned by [before] is passed to [after].

      The structural accumulator [sa] is accumulated over the structure of the
      expression, that is, it has been updated by [before] for each
      subexpression of [e] that is a proper superexpression of [x].  The
      linear accumulator [la] is accumulated over all subexpressions, that is,
      it has been updated by [before] and passed to [after] for each
      subexpression earlier than [x] in the right-to-left traversal.
  *)
(*   val kmap_fold : *)
(*     (('sa->'la-> 'a) -> t -> 'sa->'la-> t * 'la) -> *)
(*     (t -> 'sa -> 'la-> 'a) -> *)
(*     t -> 'sa -> 'la-> t * 'la *)

  (** [kfold] is like [kmap_fold] but does not transform, only accumulates. *)
(*   val kfold : *)
(*     (('sa->'la-> 'a) -> t -> 'sa->'la-> 'la) -> *)
(*     (t -> 'sa->'la-> 'a) -> *)
(*     t -> 'sa->'la-> 'la *)

  (** [kmap] is like [kmap_fold] but does not accumulate, only transforms. *)
(*   val kmap : ((unit -> t) -> t -> t) -> t -> t *)

  (** [pmap_fold fn e z] performs a pre-order traversal of [e] which descends
       into each subexpression [d] of [e] only if [fn d z] returns [None]. *)
(*   val pmap_fold : (t -> 'a -> (t * 'a) option) -> t -> 'a -> t * 'a *)

  (** [pfold] is like [pmap_fold] but does not transform, only accumulates. *)
(*   val pfold : (t -> 'a -> 'a option) -> t -> 'a -> 'a *)

  (** [pmap] is like [pmap_fold] but does not accumulate, only transforms. *)
  val pmap : (t -> t option) -> t -> t

  (** [map_fold fn e z] performs a standard post-order traversal of [e]. *)
(*   val map_fold : (t -> 'a -> t * 'a) -> t -> 'a -> t * 'a *)

  (** [fold] is like [map_fold] but does not transform, only accumulates. *)
  val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_sp : (t -> 's -> 's) -> (t -> 's -> 'p -> 'p) -> t -> 's -> 'p -> 'p

  (** [map] is like [map_fold] but does not accumulate, only transforms. *)
  val map : (t -> t) -> t -> t

  (** the [*_unord] operations are similar, but use an extended definition of
      subexpression, where the subexpressions of an offset include every
      unordered subset of the component fields. *)
(*   val pmap_unord : (t -> t option) -> t -> t *)

(*   val fold_unord : (t -> 'a -> 'a) -> t -> 'a -> 'a *)


  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val fmt : t formatter

  val fmtp : Var.fxt -> t formatter

  val fmt_caml : t formatter
end


(*============================================================================
                                     Off
  ============================================================================*)

module Off : sig

  type t = private Exp.t

  type desc = Var of Var.t | Path of Typ.t * Fld.t list

  val mk : Exp.t -> t
    (** [mk] constructs an offset from an expression representing an offset.
        Raises [Assert_failure] if argument does not satisfy [Exp.is_offset]. *)

  val mkVar : Var.t -> t
    (** [mkVar] constructs a variable offset. *)

  val mkPath : Typ.t -> Fld.t list -> t
    (** [mkPath] constructs a literal offset from an access path
        [\[fN; ...; f2; f1\]] through type [ty] where each [fI] is a
        member of the type containing [fI-1]. *)

  val desc : t -> desc
    (** [desc] returns the represented variable or access path *)

  val fv : t -> Vars.t

  val is_base : t -> Typ.t option
    (** [is_base o] returns the type [o] is an identity offset for, or [None] if [o] is not an identity
        offset. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val fmt : t formatter
  val fmtp : Var.fxt -> t formatter
  val fmt_caml : t formatter

end


(*============================================================================
                                 Collections
  ============================================================================*)

module Exps : sig
  include Set.S with type elt = Exp.t

  val fv : t -> Vars.t

(*   val pfold : (Exp.t -> 'a -> 'a option) -> t -> 'a -> 'a *)

  val fmt_sep : (unit,Format.formatter,unit)format -> t formatter
  val fmt : t formatter
end

module Expss : (Set.S with type elt = Exps.t)

module ExpMap : (Map.S with type key = Exp.t)

module ExpMMap : (MultiMap.S with type k = Exp.t
                              and type v = Exp.t
                              and type vs = Exps.t)

module ExpHMap : (HashMap.S with type key = Exp.t)
