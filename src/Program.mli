(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Internal representation of programs *)

open Library

open Type
open Variable
open Expression
open SymbolicHeap


(** Source Code Positions *)
module Position : sig

  type t = { dir : string; file : string; line : int; col : int; }

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fmt : t formatter

end


(** Instructions *)
module Inst : sig

  type desc =
    | Load of Var.t * Exp.t             (** [v = mem[e]   ] *)
    | Store of Exp.t * Exp.t            (** [mem[e] = f   ] *)
    | Alloc of Var.t * Exp.t            (** [v = alloc(e) ] *)
    | Free of Exp.t                     (** [free(e)      ] *)
    | Kill of Vars.t                    (** [vs = nondet()] *)
    | Move of Var.t * Exp.t             (** [v = e        ] *)
    | Cast of Var.t * Typ.t * Exp.t     (** [v = (T)e     ] *)
    | Assume of Exp.t
    | Assert of Exp.t
    | Nop
    | Generic of spec

  and spec = { ghosts : Vars.t; pre : XSH.t; insts : desc list; post : XSH.t; }

  type t = private { desc : desc; pos : Position.t; }

  val fv : t -> Vars.t
  val mv : t -> Vars.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fmt_desc : desc formatter
  val fmt : t formatter

  val mk : desc -> Position.t -> t

end


(** Procedure Calls *)
module rec Call : sig

  type 'a t = {
    proc    : 'a;               (** callee                                                     *)
    actuals : Var.t list;       (** actual parameters                                          *)
    areturn : Var.t option;     (** actual return variable                                     *)
    typ     : Typ.t;            (** expected function type of callee                           *)
    targets : Proc.Id.t list;   (** over-approximation of procedures to which call may resolve *)
  }

  val mk : Proc.t -> Var.t list -> Var.t option -> Proc.t t
  val args : Proc.t t -> Substitution.t * Var.t list

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val fmt : ('a formatter) -> 'a t formatter

end


(** Commands *)
and Cmnd : sig

  type t =
    | Inst of Inst.t
    | Call of Proc.Id.t Call.t
    | ICall of Exp.t Call.t

  val append : t -> t -> t option

  val fv : t -> Vars.t
  val mv : t -> Vars.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fmt : t formatter

end


(** Control-Flow Points *)
and ControlPoint : sig

  module Id : sig
    type t
    include UniqueId.S with type uniq := t
    val fmt : t formatter
  end

  type sort =
    | Entry     (** entry point of a procedure, no predecessors *)
    | Exit      (** exit point of a procedure, no successors    *)
    | Return    (** return site of a procedure call             *)
    | Cut       (** destination of a backward jump              *)
    | Join      (** two of more predecessors                    *)
    | Fork      (** two of more successors                      *)

  val fmt_sort : sort formatter

  type label

  val mk_label : ?sort:sort -> Position.t -> Proc.Id.t -> Id.t * label
  val set_sort : label -> sort option -> label

  type t

  val id : t -> Id.t
  val sort : t -> sort option
  val pos : t -> Position.t
  val proc : t -> Proc.Id.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val fmt : t formatter

end


(** Control-Flow Graphs *)
and CFG : sig
  include (Graph.GRAPH
           with type Vertex.t = ControlPoint.t
            and type index = ControlPoint.Id.t
            and type v_label = ControlPoint.label
            and type e_label = Cmnd.t)

  val add_block_edge : graph -> ControlPoint.t -> Inst.t list -> ControlPoint.t -> unit

end


(** Procedures *)
and Proc : sig

  module Id : (sig
    type t
    include UniqueId.S with type data = string and type uniq := t
    val name : t -> string
    val fmt : t formatter
  end)
  module IdHMap : (HashMap.S with type key = Id.t)

  type t = {
    id       : Id.t;
    fty      : Typ.t;
    formals  : Var.t list;
    freturn  : Var.t option;
    locals   : Vars.t;
    modifs   : Vars.t;
    accessed : Vars.t;
    cfg      : CFG.graph;
    entry    : ControlPoint.t;
    exit     : ControlPoint.t;
  }

  val fv : t -> Vars.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val fmt : t formatter

end


(** Programs *)
module Prog : sig

  type t = {
    globals      : Vars.t;                      (** global variables                              *)
    main         : Proc.Id.t;                   (** entry point of program                        *)
    procs        : Proc.t Proc.IdHMap.t;        (** map from procedure names to procedures        *)
    global_setup : Proc.Id.t list;              (** procedures to allocate and deallocate globals *)
    inits        : Proc.Id.t list;              (** procedures for initialising globals           *)
    addr_taken   : Proc.Id.t list;              (** procedures whose address is taken             *)
    constants    : int64 list;                  (** constants occurring in program                *)
  }

  val iter_procs : (Proc.Id.t -> Proc.t -> unit) -> t -> unit

  val map_procs : (Proc.t -> Proc.t) -> t -> t

  val fold_procs : (Proc.t -> 'z -> 'z) -> t -> 'z -> 'z

  (** fold inter ff cf fk ck x folds over x and applies ff to every proc, cf to every recursively called proc,
      fk to every control point, and ck to every backward jump destination control point.  If [inter] is set,
      also folds over called procedures. *)
  val fold_proc :
    Proc.t Proc.IdHMap.t option ->
    (Proc.t -> 'a -> 'a) -> (Proc.t -> 'a -> 'a) ->
    (Proc.t -> ControlPoint.t -> 'a -> 'a) -> (Proc.t -> ControlPoint.t -> 'a -> 'a) ->
    Proc.t -> 'a -> 'a

  val fold_cp :
    Proc.t Proc.IdHMap.t option ->
    (Proc.t -> 'b -> 'b) -> (Proc.t -> 'b -> 'b) ->
    (Proc.t -> ControlPoint.t -> 'b -> 'b) -> (Proc.t -> ControlPoint.t -> 'b -> 'b) ->
    Proc.t -> ControlPoint.t -> 'b -> 'b

  val write_dot : string -> string -> t -> unit

  val marshal : out_channel -> t -> unit
  val unmarshal : in_channel -> t

end


val unmarshal_tmr : Timer.t
