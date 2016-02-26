(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Symbolic Heap formulas *)

open Library

open Variable
open Expression
open SYMBOLIC_HEAP


val normalize_tmr : Timer.t
val normalize_stem_tmr : Timer.t
val exists_elim_tmr : Timer.t
(* val map_fold_normalize_tmr : Timer.t *)
val pure_consequences_tmr : Timer.t
val labeled_pure_consequences_tmr : Timer.t



(** Points-To formulas *)
module Pt : sig
  type t = { loc: Exp.t; off: Off.t; cnt: Exp.t option }
  include TERM with type t := t
end


(** Formal Parameters of List-Segment Patterns *)
module Params : (BIEDGE with type a = Var.t)

(** Actual Arguments of List-Segment formulas *)
module Args : sig
  include BIEDGE with type a = Exp.t
  val cycle_eqs : t -> Exp.t list
  val remove : bool -> t -> t -> t * t
end

(** List-Segment Patterns *)
module rec Patn : sig
  type t = private { params: Params.t; body: XSH.t; name: string }
  include TERM with type t := t
  val mk : ?name:string -> Params.t -> XSH.t -> t
  val instantiate : t -> Args.t -> XSH.t
end

(** List-Segment formulas.  The ls predicate is defined by:

        ls(L,k,p,f,b,n)  iff
                 k=0 * f=n * b=p
        \/ ?i,j. k>0 * L(p,f,i,j) * ls(L,k-1,i,j,b,n)

    or equivalently:
                 k=0 * f=n * b=p
        \/ ?i,j. k>0 * ls(L,k-1,p,f,i,j) * L(i,j,b,n)

    This justifies

        ls(L,k,p,f,i,j) * ls(L,l,i,j,b,n) |- ls(L,k+l,p,f,b,n)
    and
        L(p,f,b,n) |- ls(L,1,p,f,b,n)
*)
and Ls : sig
  type t = { pat: Patn.t; len: Exp.t; arg: Args.t }
  include TERM with type t := t
  val empty_eqs : t -> Exp.t list
  val fst_alloc : t -> Exp.t
  val may_allocs : t -> Exp.t list
  val direction : t -> Exp.t -> bool
  val split_on_fresh_point : t -> Vars.t * Args.t * Args.t
end


(** Disjunctions, set of disjuncts each of which is a quantifier-free formula *)
and Dj : sig
  include TERM
  val fv : ?include_cng_rels:bool -> t -> Vars.t
  include Set.R with type elt := SH.t and type t := t
end


(** (quantifier-free) Symbolic Heap formulas *)
and SH :
  (QUANTIFIER_FREE_SYMBOLIC_HEAP
   with type xsh := XSH.t
    and type pt := Pt.t and type ls := Ls.t and type dj := Dj.t)


(** eXistentially quantified Symbolic Heap formulas *)
and XSH :
  (EXISTENTIAL_SYMBOLIC_HEAP
   with type sh := SH.t
    and type pt := Pt.t and type ls := Ls.t and type dj := Dj.t)
