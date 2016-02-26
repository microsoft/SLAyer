(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

(** Signature for Graph *)


module type GRAPH = sig

  type index
  type v_label
  type e_label
  type graph

  module Vertex : sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val fmt : t formatter
  end
  type vertex = Vertex.t

  (** sets of vertices *)
  module VertexISet : (ImperativeSet.S with type v = vertex)
  module VertexSet : (Set.S with type elt = vertex)

  (** multi-maps from vertices (to edge labels) *)
  module VertexIMap : (ImperativeMap.S with type key = vertex)
  module VertexMap : (Map.S with type key = vertex)

  val create : unit -> graph
  val clear : graph -> unit

  val index_of : vertex -> index
  val label_of : vertex -> v_label

  val in_degree : vertex -> int
  val out_degree : vertex -> int

  val iter_preds : (vertex -> e_label -> unit) -> vertex -> unit
  val iter_succs : (vertex -> e_label -> unit) -> vertex -> unit

  val fold_preds : (vertex -> e_label -> 'a -> 'a) -> vertex -> 'a -> 'a
  val fold_succs : (vertex -> e_label -> 'a -> 'a) -> vertex -> 'a -> 'a

  val predecessors : vertex -> (vertex * e_label) list
  val successors : vertex -> (vertex * e_label) list

  val vertices_for : graph -> index -> vertex list

  val roots : graph -> vertex list

  (** [mem_vertex g v] tests if [g] contains vertex [v] *)
  val mem_vertex : graph -> vertex -> bool

  (** [add_vertex g (k,l)] adds and returns a vertex for [(k,l)] to [g], unless
      there already is one, in which case the existing vertex is returned *)
  val add_vertex : graph -> index * v_label -> vertex

  (** [remove_vertex g v] removes [v] (which must exist) if it is
      disconnected, and any other non-rooted vertices that thereby get
      disconnected *)
  val remove_vertex : graph -> vertex -> unit

  (** [replace_vertex g new_lab old_trg new_trg] swing all incoming edges to
      [old_trg] to [new_trg] *)
  val replace_vertex : graph -> (e_label -> e_label) -> vertex -> vertex -> unit

  (** [relabel_vertex g v l] changes the label of [v] to [l], removing [v] from [g] and returning the new
      vertex, PROVIDED the old and new labels compare equal according to the [VertexLabel.equal] function
      passed to [Graph.Make]. *)
  val relabel_vertex : graph -> vertex -> v_label -> vertex

  (** [root_vertex g v] marks [v] as "rooted" so that it will not be removed
      if it becomes disconnected *)
  val root_vertex : graph -> vertex -> unit

  (** [unroot_vertex g v] unmarks [v] as "rooted" so that it will be removed
      if it becomes disconnected *)
  val unroot_vertex : graph -> vertex -> unit

  (** [remove_unreachable g] removes all vertices not reachable from a root. *)
  val remove_unreachable : graph -> unit

  (** [iter_vertices_index fn g k] applies [fn] to every vertex of [g] with
      index [k] *)
  val iter_vertices_index : (vertex -> unit) -> graph -> index -> unit

  (** [iter_vertices fn g] applies [fn] to every vertex of [g] *)
  val iter_vertices : (vertex -> unit) -> graph -> unit

  (** [fold_vertices_index fn g k] folds [fn] over every vertex of [g] with index [k] *)
  val fold_vertices_index : (vertex -> 'a -> 'a) -> graph -> index -> 'a -> 'a

  (** [fold_vertices fn g acc] folds [fn] over every vertex of [g] *)
  val fold_vertices : (vertex -> 'a -> 'a) -> graph -> 'a -> 'a

  (** [mem_edge g src label trg] tests if [g] contains an edge from [src] to
      [trg] labeled [label] *)
  val mem_edge : graph -> vertex -> e_label -> vertex -> bool

  (** [add_edge g src label trg] adds an edge from [src] to [trg] with label
      [label] *)
  val add_edge : graph -> vertex -> e_label -> vertex -> unit

  (** [remove_edge g src label trg] removes the edge (which must exist) from
      [src] to [trg] labeled [label] *)
  val remove_edge : graph -> vertex -> e_label -> vertex -> unit

  (** [collapse_edge_pre g src label trg] swings all outgoing edges of [trg] to [src] *)
  val collapse_edge_pre : graph -> vertex -> e_label -> vertex -> unit

  (** [collapse_edge_post g src label trg] swings all incoming edges to [src] to [trg] *)
  val collapse_edge_post : graph -> vertex -> e_label -> vertex -> unit

  (** [iter_edges vertex_fn edge_fn g root] applies [vertex_fn] to every
      vertex reachable from [root] in [g], and [edge_fn] to every edge of [g]
      between those vertices *)
  val iter_edges : (vertex->unit) -> (vertex*e_label*vertex->unit) -> graph -> index -> unit

  (** [fold_edges vertex_fn edge_fn g root z] accumulates [vertex_fn] over every vertex reachable from [root]
      in [g], and [edge_fn] over every edge of [g] between those vertices. *)
  val fold_edges : (vertex -> 'z -> 'z) -> (vertex * e_label * vertex -> 'z -> 'z) -> graph -> index -> 'z -> 'z

  (** [identify_vertices g] returns a map that associate a unique integer to
      each vertex of [g] *)
  val identify_vertices : graph -> int VertexIMap.t

  (** [cutpoints v] returns a set of cutpoints of the graph reachable from [v]. *)
  val cutpoints : vertex -> VertexSet.t

  (** [bfs g s] does a breadth-first search of [g] from [s]. Returns the
      distance and predecessor results. *)
  val bfs : graph -> vertex -> (int VertexIMap.t * ((vertex*e_label) option VertexIMap.t))

  (** [dfs_iter next forwards pre post vs] traverses depth-first from
      each elemtent of [vs], presenting vertices to [pre] in preorder,
      and to [post] in postorder. It calls [next] each time it moves
      to the next entry in [vs]. If [forwards]=true then it walks
      forwards, otherwise it walks backwards in the graph. By default
      [forwards] is true, and [next] does nothing.*)
  val dfs_iter : ?next:(unit->unit) -> ?forwards:bool -> (vertex -> unit) -> (vertex -> unit) -> vertex list -> unit

  (** [dfs v] returns a list of vertices reachable from [v] in preorder, a list of vertices in postorder, and
      functions mapping vertices to their position in the preorder and in the postorder. *)
  val dfs : vertex -> vertex list * vertex list * (vertex -> int) * (vertex -> int)

  (** [scc g] returns a map, that maps each vertex to the list of
      vertices in its strongly connected component. *)
  val scc : graph -> (vertex list) VertexMap.t

  (** [dominance_frontier g s] gives the dfset and dominator tree of [g]
      [dom n m] is the dominator relationship
      [parent_of n] is the immediate dominator of n
      [children_of n] is the set of vertices that are dominated by n *)
  val dominance_frontier :
    graph -> vertex -> (VertexISet.t VertexIMap.t) *    (* dfset       *)
                       (vertex -> vertex -> bool) *     (* dom         *)
                       (vertex -> vertex option)   *    (* parent_of   *)
                       (vertex -> VertexISet.t)         (* children_of *)

  (** [natural_loops g dom] returns a list of loops appearing in [g],
      each loop is given as a set of vertices in the loop's body *)
  val natural_loops : graph -> (vertex -> vertex -> bool) -> (VertexISet.t * vertex * vertex) list

  (** Floyd-Warshall returns distance and predecessor matrices.*)
  val fw : graph ->
           IntHMap.key VertexIMap.t * VertexIMap.key IntHMap.t *
           (int option) array array * (int option) array array

  val copy : graph -> vertex -> vertex VertexMap.t * graph

  val slice : vertex -> vertex -> graph -> vertex VertexMap.t * graph

  (** [write_dot g t root] outputs the subgraph of [g] reachable from [root] in
      dot format with tree edges given in [t] *)
  val cfg_write_dot : graph -> (Vertex.t -> VertexISet.t) -> index -> Buffer.t -> unit

  (** [write_dot g root] outputs the subgraph of [g] reachable from [root] in
      dot format *)
  val write_dot : graph -> index -> Buffer.t -> unit

  (** [write_dot_partitioned fn g root] outputs the subgraph of [g] reachable
      from [root] in dot format, where vertices are partitioned into
      subgraphs according to [fn] *)
  val write_dot_partitioned :
    (index -> (string * (Format.formatter->'a)) option) ->
    graph -> index list -> Buffer.t -> unit

end
