(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Signatures for formulas used by congruence and transitive closure *)

open Library


module type EXP = sig

  include OrderedType

  val mkApp : t -> t -> t
    (** [mkApp f a] constructs [App(f,a)] *)

  val getApp : t -> (t * t) option
    (** [getApp (App(f,a))] is [Some(f,a)], and [None] in other cases *)

  val invert : t -> t option
    (** [invert] transforms [Add(o)] to [Sub(o)] and vice versa, and returns
        [None] in other cases *)

  val fmt : t formatter

end


module type FORMULA = sig

  module Exp: (EXP with type t = Expression.Exp.t)
  module Exps: Set.S with type elt = Exp.t and type t = Expression.Exps.t
  module ExpMap: Map.S with type key = Exp.t and type 'a t = 'a Expression.ExpMap.t
(*   module Exp: EXP *)
(*   module Exps: Set.S with type elt = Exp.t *)
(*   module ExpMap: Map.S with type key = Exp.t *)

  type t

  val lbl : t -> int
    (** [lbl f] is the label of the root of formula [f] *)

  val is_leaf : t -> bool

  val fold_rels : (Exp.t list -> 'z -> 'z) -> (Exp.t -> Exp.t -> 'z -> 'z) -> t -> 'z -> 'z
    (** [fold_rels] folds over related pairs of expressions, first
    function takes a strongly connected component, the second
    individual edges. *)

  val fold_nrels : (Exp.t -> Exp.t -> 'z -> 'z) -> t -> 'z -> 'z
    (** [fold_nrels] folds over provably unrelated pairs of expressions. *)

  val fold_sp : (t -> 's -> 's) -> (t -> 's -> 'p -> 'p) -> t -> 's -> 'p -> 'p

  val fold_dnf : ?dnf:bool -> (t -> 'c*'d -> 'c*'d) -> ('c*'d -> 'd) -> t -> 'c->'d -> 'd
    (** The disjunctive traversal of a formula, [fold_dnf m r f c d], is a generalization of incremental
        conversion to disjunctive-normal form that accumulates an abstract conjunction [m] over each cube, and
        accumulates an abstract disjunction [r] over the clauses.  For example:
        [fold_dnf m r (f1 * ((f2 * f3) \/ f4)) c d =
          let c,d = m f1 (c,d) in r (m f4 (c, r (m (f2 * f3) (c,d))))]
    *)

  val labeled_pure_consequences : t -> Exp.t * Exp.t IntMap.t

  val fmt : t formatter

end
