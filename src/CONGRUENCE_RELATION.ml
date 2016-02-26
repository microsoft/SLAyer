(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library


module type CONGRUENCE_RELATION = sig

  type exp
    (** Exps. *)

  type exps
    (** Sets of exps. *)

  type t
    (** A congruence relation representation [r: t] denotes a relation [[r]]
        over a finite 'carrier' set of exps that is a congruence with respect
        to the subexp relation. *)

  val normalize : t -> exp -> exp
    (** [normalize r (normalize r e)] = [normalize r e] and [normalize r e] =
        [normalize r f] iff ([e],[f]) is in [[r]]. *)

  val mem : t -> exp -> exp -> bool
    (** [mem r e f] test if [r] proves [e]==[f]. *)

  val mem_dqs : t -> exp -> exp -> bool
    (** [mem_dqs r e f] test if [r] proves [e]!=[f]. *)

  val inconsistent : t -> bool
    (** [inconsistent r] holds only if [r] is inconsistent. *)

  val class_of : t -> exp -> exps
    (** [f] in [class_of r e] iff ([e],[f]) in [[r]]. *)

  val mem_carrier : exp -> t -> bool
    (** [mem_carrier e r] holds iff [e] is in the carrier of [r]. *)

  val carrier : t -> exps
    (** [e] in [carrier r] iff [e] is in the carrier of [r]. *)

  val representatives : t -> exps
    (** [e'] in [representatives r] iff [normalize r e'] = [e']. *)

  val fold : (exp -> exp -> 'z -> 'z) -> t -> 'z -> 'z
    (** [fold fn r] enumerates the equations in [r]. The representative is
        passed to [fn] first. *)

  val foldn : (exp -> exp -> 'z -> 'z) -> t -> 'z -> 'z
    (** [foldn fn r] enumerates the disequations in [r]. *)

  val fold_classes : (exp -> exps -> 'z -> 'z) -> t -> 'z -> 'z
    (** [fold_classes fn r] enumerates the equivalence classes of [r]. *)

  val empty : t
    (** [[empty]] is the empty relation. *)

  val merge : (exp -> exp -> bool) -> t -> exp -> exp -> t
    (* Note: specify *)

  val split : t -> exp -> exp -> t

  val union : (exp -> exp -> bool) -> t -> t -> t
    (** [[union leq p q]] = [[r]] is the strongest congruence relation
        containing [[p]] and [[q]].  [union leq p q] maintains the
        representatives of [p] and chooses between them using [leq]: for [leq]
        a preorder, and for [normalize q e] = [f] then if [leq f e] then
        [normalize r e] = [normalize p f] else [normalize r f] = [normalize p
        e]. *)

  val inter : (exp -> exp -> bool) -> t -> t -> t
    (** [[inter q r]] is the intersection of [[q]] and [[r]].  Smaller wrt
        [leq] representatives are chosen where possible. *)

  val subst : (exp -> exp -> bool) -> t -> Substitution.t -> t
    (* Note: specify *)

  val restrict : t -> exps -> t
    (** For [es] a superset of the representatives of [r], [normalize
        (restrict r es) e] = [normalize r e] if [e] in [es]. *)

  val remove_trivial : t -> exps -> t
    (** [normalize (remove_trivial r es) e] = [normalize r e] if [e] in [es] and [class_of r e] is only [e]
        itseslf.  [carrier (remove_trivial r es)] is the subset of [carrier r] excluding such expressions. *)

  val implied_by : (exp -> exp -> bool) -> t -> Pure.t -> exp array -> exps -> t

  val is_empty : t -> bool

  val fmt : t formatter

end
