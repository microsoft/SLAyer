(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Mutable edge- and vertex-labelled multi-graphs *)

open Library


module type GRAPH = Graph_sig.GRAPH

module Make
  (Index: sig
     type t
     val compare: t -> t -> int
     val equal: t -> t -> bool
     val hash: t -> int
     val fmt : t formatter
   end)
  (VertexLabel: sig
     type t
     val compare: t -> t -> int
     val equal: t -> t -> bool
     val fmt : t formatter
   end)
  (EdgeLabel: sig
     type t
     val compare: t -> t -> int
     val equal : t -> t -> bool
     val fmt : t formatter
   end)
  :
  (GRAPH
   with type index = Index.t
    and type v_label = VertexLabel.t
    and type e_label = EdgeLabel.t
  )
