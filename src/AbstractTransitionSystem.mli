(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Program
open Interproc_sig


module Domain (RD: RELATION_DOMAIN) : sig

  module Tr : sig
    type t =
      | Intra of ControlPoint.t * Inst.t list * ControlPoint.t * bool
      | Call of Proc.t Call.t
      | Return
      | Summary

    val append : t -> t -> t option
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val fmt : t formatter
  end

  module ATS : sig
    include
      (Graph.GRAPH
       with type index = ControlPoint.t
        and type e_label = Tr.t)

    val concat_blocks : graph -> vertex -> unit
  end

  include
    (INTERPROC_DOMAIN
     with type RD.t = ATS.v_label
      and type RD.pred = RD.pred
      and type I_D_cp.t = ATS.vertex)

  val ats : r -> ATS.graph
  val write_ats : string -> Prog.t -> ATS.graph -> ATS.index -> unit

end
