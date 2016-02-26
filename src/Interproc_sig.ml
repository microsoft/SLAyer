(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Interfaces for analysis modules *)

(**/**)
open Library

open Type
open Variable
open Expression
open Program
(**/**)


module type PROCEDURE_OPERATIONS = sig
  type i_d_cp           (** (injected) data associated with control points *)
  type d_bk             (** data associated with control point free paths *)

  type r                (** domain-specific whole-program data *)
  val create : Prog.t -> r

  (** For procedure p given by p(f)\{local l in B\}, we use the following
      naming conventions for variables denoting abstract predicates based on
      the roles they play in judgments of the form
         {C \{pre\} p(f) \{post\} |- \{entry\} B \{exit\} }
      or {C \{pre\} p(f) \{post\} |- \{call\} p(f\[A/f\]) \{retn\} } *)

  (** For a procedure call \{call\} p(f\[A\]), [adapted_pre_substate_call r
      cxt pre call p \[A/f\]] determines if a summary from [pre] applies to
      the call and if so executes an interprocedural step to [pre] and returns
      [post_to_retn] such that for any specification \{pre\} p(f) \{\/_i
      post_i\}, [post_to_retn caller post_i] executes an intraprocedural post
      to return site step such that \{pre\} p(f\[A/f\]) \{\/_i post_to_retn
      post_i\} holds. *)
  val adapted_pre_substate_call : r -> Vars.t -> i_d_cp -> d_bk -> Proc.t Call.t -> (d_bk -> i_d_cp) option

  (** For a procedure call \{call\} p(A), [call_to_entry r call p A] executes
      an interprocedural call site to entry point step, returning a [pre]
      such that a specification \{pre\} p(x) \{post\} would apply. *)
  val call_to_entry : r -> d_bk -> Proc.t Call.t -> i_d_cp * (d_bk -> i_d_cp)

  (** [exit_to_retn p exit] executes an interprocedural exit point to return
      site step, returning [retn] such that if \{entry\} B \{exit\} holds
      where [p] is given by p(x)\{local l in B\}, then
      \{entry\} local l in B \{retn\} holds. *)
  val exit_to_retn : Proc.t -> d_bk -> d_bk

  (** [resolve_indirect_call r call fptr ftyp] returns an over-approximation of the possible procedures that
      [*fptr] of type [ftyp] in states satisfying [call] might call. *)
  val resolve_indirect_call : r -> d_bk -> Exp.t -> Typ.t -> Proc.Id.t list
end


(** An abstract domain for intraprocedural analysis *)
module type INTRAPROC_DOMAIN = sig

  type t

  include
    (PROCEDURE_OPERATIONS
     with type i_d_cp := t
      and type d_bk := t)


  (** element representing all error states *)
  val error : t

  (** element representing all non-error states *)
  val tt : t

  (** test if element represents error states *)
  val is_error : t -> bool

  (** test if element represents no states *)
  val is_false : t -> bool

  (** execution of instructions *)
  val exec_inst : Vars.t -> Inst.t -> t -> t

  (** abstract order *)
  val below : t -> t -> bool

  (** abstract element constructors *)
  val join : t -> t -> t
  val generalize : t -> t * bool

  (** utility routines *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fmt : t formatter
  val fmt_pre : (t * Proc.t) formatter
end



(** An abstract domain for intraprocedural analysis, but using a form of
    abstract states suitable for interprocedural analysis *)
module type RELATION_DOMAIN = sig
  include (INTRAPROC_DOMAIN)
  val equal_entry : t -> t -> bool
  val fmt_entry : t formatter
  val fmt_reln : t formatter

  type pred
  val inject : pred -> t
  val project : t -> pred
end



(** Intraprocedural abstract states (a d_cp and a control_point) together with
    other domain-specific data *)
module type INJECT_D_CONTROL_POINT = sig
  type t
  type d_cp

  val project : t -> d_cp * ControlPoint.t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val fmt : t formatter
end



(** An abstract domain for interprocedural analysis *)
module type INTERPROC_DOMAIN = sig
  module RD : RELATION_DOMAIN

  module I_D_cp : (INJECT_D_CONTROL_POINT with type d_cp = RD.t)

  include
    (PROCEDURE_OPERATIONS
     with type i_d_cp = I_D_cp.t
      and type d_bk = I_D_cp.t * (RD.t * ControlPoint.t))


  (** [init r p] is the initial element for [p] *)
  val init : r -> RD.t * ControlPoint.t -> i_d_cp


  (** [now_covered r p] tests if [p] is covered by some other element *)
  val now_covered : r -> i_d_cp -> bool

  (** [prev_to_join r (prev,(prev_to,next))] executes an intraprocedural
      step to a join point *)
  val prev_to_join : r -> d_bk -> i_d_cp

  (** [prev_to_cut r (prev,(prev_to,next))] executes an intraprocedural
      step to a cut point *)
  type __covered = WasCoveredByOld | NowCoveredByNew
  val prev_to_cut : r -> d_bk -> __covered * i_d_cp


  (** [procedure_pres r proc] are the pres of all summaries of [proc] *)
  val procedure_pres : r -> Proc.t -> i_d_cp list

  (** [register_pre r pre] declares that [pre] is the pre of some summary *)
  val register_pre : r -> i_d_cp -> unit


  val states_for : r -> ControlPoint.t -> I_D_cp.t list

  val errors : r -> I_D_cp.t list

  val leaks : r -> I_D_cp.t list

  val dead : r -> Position.t list

  val hit_limit : r -> bool

end
