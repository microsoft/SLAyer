(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Signatures describing Symbolic Heap formulas *)

open Library

open Variable
open Expression


(*TERM========================================================================*)

(** Common interface of formulas *)
module type TERM = sig
  type t

  val fv : t -> Vars.t
    (** set of free variables *)

  val map_exps : (Exp.t -> Exp.t) -> t -> t
    (** map over subexpressions *)

  val fold_exps : (Exp.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** fold over all expressions and their subexpressions *)

  val compare : t -> t -> int
    (** comparison *)

  val equal : t -> t -> bool
    (** equality *)

  val fmt : t formatter
    (** human-readable presentation *)

  val fmtp : Var.fxt -> t formatter
    (** human-readable presentation using a variable formatting context to prettify quantified variables *)

  val fmt_caml : t formatter
    (** [fmt_caml x] emits caml code that constructs [x]. *)

end


(*BIEDGE======================================================================*)

type 'a edg = { prev: 'a list; frnt: 'a list; back: 'a list; next: 'a list }
  (** {v
                frnt           back
               .----.         .----.    next
               |  --+-->      |  --+-->
       prev    |----|   ...   |----|
            <--+-   |      <--+--  |
               '----'         '----'        v} *)

(** Interface for labeled bi-directional multi-edges *)
module type POLY_BIEDGE = sig

  val map : ('a -> 'b) -> 'a edg -> 'b edg

  val map2 : ('a->'b->'c) -> 'a edg -> 'b edg -> 'c edg

(*   val fold : ('a -> 'z->'z) -> 'a edg -> 'z->'z *)

  val fold2 : ('a->'b -> 'z->'z) -> 'a edg -> 'b edg -> 'z->'z

  val fold_links : ('a*'a -> 'z->'z) -> 'a edg -> 'z->'z

  val fold_links2 : ('a*'a -> 'b*'b -> 'z->'z) -> 'a edg -> 'b edg -> 'z->'z

  val may_allocs : 'a edg -> 'a list

(*   val reverse : 'a edg -> 'a edg *)
    (** [reverse] [p,f,b,n] = [n,b,f,p] *)

  val between : 'a edg -> 'a edg -> 'a edg
    (** [between] [s, t;t', u, v;v'] [w, x;x', y, z;z']
        =                  [u, v;v',  w, x;x']          *)

  val append : 'a edg -> 'a edg -> 'a edg
    (** [append] [p, f;f', i, j;j'] [k, l;l', b, n;n']
               = [p, f;f',                    b, n;n'] *)

  val split : 'a list -> 'a list -> 'a edg -> 'a edg * 'a edg
    (** [split w x u_v_y_z] returns [(u_v_w_x, w_x_y_z)] such that
        [u_v_y_z = append (append u_v_w_x w_x_w_x) w_x_y_z] where
        [w_x_w_x] is the empty segment determined by [x] and [w].  Eg:
        [split]        w  x
                [u, v,        y, z]
             = ([u, v, w, x],
                      [w, x , y, z]) *)

  val remove_prefix : 'a edg -> 'a edg -> 'a edg * 'a edg
    (** [remove_prefix wy xz = (wx, yz)] such that if [wx] is empty, then
        [append wy yz = xz].  That is,
        [remove_prefix [s, t       ,  u, v]
                             [w, x        , y, z]
                    = ([s, t, w, x], [u, v, y, z])] *)

  val remove_suffix : 'a edg -> 'a edg -> 'a edg * 'a edg
    (** [remove_suffix xz wy = (yz, wx)] such that if [yz] is empty, then
        [append wx xz = wy].  That is,
        [remove_suffix       [s, t       , u, v]
                       [w, x,        y, z]
                    = (             [y, z, u, v],
                       [w, x, s, t]              )] *)

end

module type BIEDGE = sig

  include POLY_BIEDGE

  type a
  type t = a edg

  include (TERM with type t := t)

(*   val adjacent : t -> t -> bool *)
    (** [adjacent x y] holds if [x] and [y] are adjacent.  Eg:
        [adjacent] [p, f;f', i , j ;k]
                            [i', j';k', b, n;n']
        holds if i = i' & j = j' & k = k' *)

end


(*FORMULAS====================================================================*)

(** Common interface of sets of subformulas of Symbolic Heaps *)
module type FORMULAS = sig
  include Set.Q
  val star : elt list -> t -> t
  val remove : elt -> t -> t
  val empty : t -> t
end


(*COMMON_SH===================================================================*)

(** Common interface of quantified and quantifier-free Symbolic Heap formulas *)
module type COMMON_SH = sig
  include TERM

  type xsh
  type vs_t

  (** {7 Constructors } *)

  val emp : t
  val tt : t
  val ff : t

  (** *-conjunction *)
  val star : t list -> t -> t

  (** disjunction *)
  val disj : t list -> t -> t

  (** apply a substitution to a formula *)
  val subst : Substitution.t -> t -> t

  (** [rename_vs vs q] generates a substitution [s] from [vs] to fresh
      variables and calls [rename s q]. *)
  val rename_vs : Vars.t -> t -> t * Vars.t * Substitution.t * Substitution.t

  (** [exists_intro vs q] is equivalent to [? vs. q] but where some of the
      existential quantifiers may have been eliminated. *)
  val exists_intro : Vars.t -> t -> xsh

  (** {7 Normalization } *)

  val normalize : ?dnf:bool -> ?init:t -> vs_t -> vs_t

  val normalize_stem : ?init:t -> vs_t -> vs_t * Exps.t

  (** {7 Queries } *)

  (** [inconsistent q] holds if [q] is syntactically inconsistent.  See also
      [Prover.inconsistent].  *)
  val inconsistent : t -> bool

  (** [is_empty q] holds only if [q] entails [emp].
      Incomplete in the presence of inconsistent disjuncts. *)
  val is_empty : t -> bool

  (** [is_pure q] holds only if [q] is pure.  Tests if [q] is of form [p * tt]
      for some pure formula [p], and is further incomplete in the presence of
      inconsistent disjuncts. *)
  val is_pure : t -> bool

  (** [sizeof q] is a measure of the syntactic size of [q] *)
  val sizeof : t -> int

  val lbl : t -> int
  val set_lbl : int -> t -> t

  val fmt_xs : vs_t formatter
  val fmtp_xs : Var.fxt -> vs_t formatter
  val fmt_did : t * t -> (Format.formatter->unit) * (Format.formatter->unit) * (Format.formatter->unit)
  val fmt_did_xs : vs_t * vs_t -> (Format.formatter->unit) * (Format.formatter->unit) * (Format.formatter->unit)

end


(*SH==========================================================================*)

(** (quantifier-free) Symbolic Heap formulas *)
module type QUANTIFIER_FREE_SYMBOLIC_HEAP = sig
  type t
  type pt type ls type dj type xsh

  include COMMON_SH
  with type t := t and type xsh := xsh and type vs_t = Vars.t * t

  type f = Pt of pt | Ls of ls | Dj of dj

  (** {6 SH-specific operations } *)

  (** {7 Subformulas } *)

  (** Pure subformula *)
  module Pf : sig
    val star : Exp.t list -> t -> t

    val term : t -> Exp.t
      (** Return pure part of SH's stem as a boolean formula. *)

    val normalize : t -> Exp.t -> Exp.t
      (** [normalize q e] maps [e] to the representative of its congruence
          class induced by [q]. *)

    val mem_carrier : Exp.t -> t -> bool

    val class_of : t -> Exp.t -> Exps.t
      (** [class_of q e] is the equivalence class of [e] induced by [q]. *)

    val carrier : t -> Exps.t

    val classes : t -> Expss.t

    val fold_classes : (Exp.t -> Exps.t -> 'z -> 'z) -> t -> 'z -> 'z

    val empty : t -> t

    val trim : Vars.t -> Exps.t -> Substitution.t -> t -> t
      (** [trim xs ts s q] is [q'] where [q'] is [q] but with the congruence relations trimmed to not mention
          the domain of [s], using the range instead, and excluding trivial equations on [ts].  New
          representatives are chosen attempting to avoid [xs]. *)

    val union : (Exp.t -> Exp.t -> bool) -> t -> t -> t

    val merge : (Exp.t -> Exp.t -> bool) -> t -> Exp.t -> Exp.t -> t

    val extend : Vars.t -> t -> Exp.t -> t
      (** [extend xs q e] is logically equivalent to [q] but [e] is included in
          the carrier.  New representatives are chosen attempting to avoid [xs]. *)

    val mem : Exp.t -> t -> bool

  end

  (** Set of Points-To subformulas *)
  module PtS : sig
    include FORMULAS with type elt := pt and type t := t
    val find : Exp.t -> t -> pt
    val may_allocs : t -> Exps.t
  end

  (** Set of List-Segment subformulas *)
  module LsS : sig
    include FORMULAS with type elt := ls and type t := t
    val find : Exp.t -> t -> ls
    val may_allocs : t -> Exps.t
  end

  (** Set of Disjunction subformulas *)
  module DjS : sig
    include FORMULAS with type elt := dj and type t := t
    val add : dj -> t -> t              (* To be removed *)
    val filter : (dj -> bool) -> t -> t
    val extract_all : t -> t * t
    val fold_semiring : ('z->'z->'z)-> ('z->'z->'z)-> (t->'z)-> t->'z->'z
  end

  (** Junk subformula *)
  module Jnk : sig
    val star : t -> t
    val remove : t -> t

    (** [is_empty q] fails only if [q] is intuitionistic.  Tests if [q] is not
        of form [p * tt] for some formula [p], and is further incomplete in the
        presence of inconsistent disjuncts. *)
    val is_empty : t -> bool
  end

  (** [exists_elim (xs,q)] is [(ys,r)] where [? ys. r] is logically equivalent
      to [? xs. q], optimally syntactically simplified by eliminating some of
      the existentially quantified variables [xs]. *)
  val exists_elim : ?dnf:bool -> Vars.t * t -> Vars.t * t


  (** {7 Destructors } *)

  (** [pure_consequences q] is [(ps,c,d)] where
      [?xs. q ==> (?ps,xs. c ^ d) /\ (!ps.?xs. c ==> d)]. *)
  val pure_consequences : t -> Vars.t * Exp.t * Exp.t
  val labeled_pure_consequences : t -> Exp.t * Exp.t IntMap.t

  (** [pure_sf q] is the strongest pure syntactic subformula of [q]. *)
  val pure_sf : t -> Exp.t

  (** [spatial_sf q] is the weakest syntactic subformula of [q] such that
      [pure_sf q] ^ [spatial_sf q] implies [q]. *)
  val spatial_sf : t -> t

  val partition : (f -> bool) -> t -> t * t

  (** [diff_inter_diff p q] returns [(p_q,i,q_p)] where [p = p_q * i] and [q =
      q_p * i], such that [p_q] includes pure consequences of [p] that are not
      consequences of [p_q], but [q_p] does not. *)
  val diff_inter_diff : ?pas:bool -> t -> t -> t * t * t


  (** {7 Iterators } *)

(*   val iter_post : (t -> t -> unit) -> t -> unit *)

(*   val bfold : (t-> 's-> 'z-> 's) -> (t-> 's-> 'z-> 'z) -> t-> 's-> 'z-> 'z *)
  val fold : (t -> 'z -> 'z) -> t -> 'z -> 'z

  val fold_sp : (t -> 's -> 's) -> (t -> 's -> 'p -> 'p) -> t -> 's -> 'p -> 'p

(*   val deprecated_foldi : *)
(*     ((int*int)list -> t -> 'z->'z) -> ((int*int)list -> t -> 'z->'z) -> *)
(*     t -> 'z -> 'z *)

  val fold_dnf : ?dnf:bool -> (t -> 'c*'d -> 'c*'d) -> ('c*'d -> 'd) -> t -> 'c->'d -> 'd

  val map : (t -> t) -> t -> t

  val map_fold : (t -> 'z -> t * 'z) -> t -> 'z -> t * 'z

  val map_fold_distrib : (t -> 'z -> t * 'z) -> t -> 'z -> t * 'z

(*   val fold_filter : (f -> 'z -> 'z option) -> t -> 'z -> t * 'z *)

  val dcc : t -> (CngRel.t * CngRel.t) IntMap.t


  (** {7 Queries } *)

  (** set of free variables *)
  val fv : ?include_cng_rels:bool -> t -> Vars.t

  (** [find e q] returns the subformula of the stem of [q] that may allocate
      [e], raises Not_found if [q] contains no such subformula. *)
  val find : Exp.t -> t -> f
  val tryfind : Exp.t -> t -> f option

  (** [must_allocs q] is an under-approximation of the allocated locations of [q] *)
  val must_allocs : t -> Exps.t

  (** [may_alloc_stem q] is an over-approximation of the allocated locations
      of the stem of [q] *)
  val may_allocs_stem : t -> Exps.t

  (** [may_allocs q] is an over-approximation of the allocated locations of
      [q] *)
  val may_allocs : t -> Exps.t


  (** {7 Formatting } *)

  val mk_fxt : Vars.t * t -> Var.fxt

  val fmtsp : t -> Var.fxt -> t formatter
    (** human-readable presentation accepting a super-formula to reduce repeated equalities,
        and using a variable formatting context to prettify quantified variables *)

end


(*XSH=========================================================================*)

(** eXistentially quantified Symbolic Heap formulas *)
module type EXISTENTIAL_SYMBOLIC_HEAP = sig
  type t
  type pt type ls type dj type sh

  include COMMON_SH with type t := t and type xsh := t and type vs_t := t


  (** {6 XSH-specific operations } *)

  (** [exists_bind vs q] alpha-converts the existentially quantified variables
      of [q] to be disjoint from [vs] and then destructs the quantifier.  The
      returned bound variables are guaranteed to appear free in the body. *)
  val exists_bind : Vars.t -> t -> Vars.t * sh


  (** {7 Subformulas } *)

  (** Pure Formulas *)
  module Pf : sig
    val star : Exp.t list -> t -> t
  end

  (** Sets of Subformulas *)
  module PtS : sig
    val star : pt list -> t -> t
  end
(*
  module LsS : sig
    val star : ls list -> t -> t
  end

  module DjS : sig
    val star : dj list -> t -> t
  end
*)
  module Jnk : sig
    val star : t -> t
    val remove : t -> t
  end


  (** {7 Queries } *)

  (** [equivalent p q] returns [Some(s)] if applying [s] to [q] witnesses that
      [p] and [q] are equivalent. *)
  val equivalent : t -> t -> Substitution.t option

end
