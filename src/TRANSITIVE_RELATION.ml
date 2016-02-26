(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


module type TRANSITIVE_RELATION = sig

  type exp
    (** Exps. *)

  type exps
    (** Sets of exps. *)

  type t
    (** A transitive relation representation [r: t] denotes a relation [[r]]
        over a finite 'carrier' set of exps that is transitive. *)

  val empty : t
    (** [[empty]] is the empty relation. *)

  val add : exp * exp -> t -> t
    (** [[add (e,f) t]] is the smallest transitive relation containing [[t]]
        and ([e],[f]). *)

  val add_scc : exps -> t -> t

  val inter : t -> t -> t
    (** [[inter q r]] is the intersection of [[q]] and [[r]]. *)

(*
  val mem : t -> exp * exp -> bool
    (** [mem r (e,f)] holds iff ([e],[f]) in [[r]]. *)
*)
  val predecessors : t -> exp -> exps
    (** [f] in [predecessors r e] iff ([f],[e]) in [[r]].
        Raises [Not_found] iff [e] not in the carrier of [r]. *)

  val is_reachable : (exp -> bool) -> t -> exp -> bool

  val fmt : Format.formatter -> t -> unit


end
