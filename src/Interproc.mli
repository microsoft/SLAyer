(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Interprocedural abstract fixed-point computation *)

open Program
open Interproc_sig


module Pair (Inv: INTRAPROC_DOMAIN) : (RELATION_DOMAIN with type pred = Inv.t)


(** Construct an interprocedural analysis given a domain for representing sets
    of states and an interprocedural domain, parametric in the intraprocedural
    domain and in the lifting from intraprocedural abstract states to
    interprocedural abstract states *)
module Make (InterprocDomain: INTERPROC_DOMAIN) : sig

  module Summaries : sig type t end
  module Heights : sig type t end

  type t = {
    program: Prog.t;
    invariants: InterprocDomain.r;
    summaries: Summaries.t;
    heights: Heights.t;
    mutable safe: bool;
    mutable hit_limit: bool;
  }

  val exec_prog : Prog.t -> InterprocDomain.RD.pred -> t
end
