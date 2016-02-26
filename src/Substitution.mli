(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Expression to expression substitutions *)

open Library

open Variable
open Expression



(*============================================================================
                                 Substitution
  ============================================================================*)

type t

(** {7 Constructors } *)

val empty : t

val singleton : Exp.t -> Exp.t -> t

(** [add] \[E/X\] [y f] = \[[f]/[y], E\[[f]/[y]\]/X\], so [subst (add]
    \[E/X\] [y f) D] = [subst] \[f/y\] [(subst] \[E/X\] [D)]. *)
val add : Exp.t -> Exp.t -> t -> t

(** Like [add] except that identity mappings are retained. *)
val add_id : Exp.t -> Exp.t -> t -> t

(** [compose] \[E/X\] \[F/Y\] = \[F/Y, E\[F/Y\]/X\], so [subst (compose]
    \[E/X\] \[F/Y\][) D] = [subst] \[F/Y\] [(subst] \[E/X\] [D)] = [subst]
    \[F/Y, E\[F/Y\]/X\] [D]. *)
val compose : t -> t -> t

val meet : t -> t -> t

val diff : t -> t -> t

(** [remove x] \[E/X\] removes [x] from the domain of \[E/X\]. *)
val remove : Exp.t -> t -> t

(** [remove_vs vs] \[E/X\] removes [vs] from the domain of \[E/X\]. *)
val remove_vs : Vars.t -> t -> t

(** [restrict vs] \[E/X\] restricts the domain of \[E/X\] to [vs]. *)
val restrict : Vars.t -> t -> t

(** [restrict_rng vs] \[E/X\] restricts the domain of \[E/X\] such that the range is contained in [vs]. *)
val restrict_rng : Vars.t -> t -> t

val of_assoc : (Exp.t * Exp.t) list -> t

(** {7 Queries } *)

val to_assoc : t -> (Exp.t * Exp.t) list
val to_exp : t -> Exp.t

val is_empty : t -> bool

(** [subst s e] returns [e]\[[f_0]/[d_0],…,[f_n]/[d_n]\] (where [s] is the map
    from each [f_i] to [d_i]). *)
val subst : t -> Exp.t -> Exp.t

val find : Exp.t -> t -> Exp.t

val tryfind : Exp.t -> t -> Exp.t option

val dom : t -> Exps.t
val in_dom : Exp.t -> t -> bool

val rng : t -> Exps.t
val fv : t -> Vars.t

(** {7 Operations on the underlying representation } *)

val fold : (Exp.t -> Exp.t -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (Exp.t -> Exp.t -> unit) -> t -> unit

val fmt : t formatter
val equal : t -> t -> bool
val compare : t -> t -> int
