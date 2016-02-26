(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Mutable edge- and vertex-labelled multi-graphs *)

open Library

module L = (val Log.std Config.vGraph : Log.LOG)


(*============================================================================
                                    Graph
  ============================================================================*)

include Graph_sig


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
  =
struct

  type index = Index.t
  type v_label = VertexLabel.t
  type e_label = EdgeLabel.t

  module EdgeLabelSet = Set.Make(EdgeLabel)


  module rec Vertex : sig
    (* vertices of graphs are represented by pairs of an index and a
       label, and carries the sets of incoming and outgoing edges *)
    (* Note: do both incoming and outgoing edges need to be labeled? *)
    type label_n_neighbors = {
      label: v_label;
      incoming: VertexMMap.t;
      outgoing: VertexMMap.t;
    }
    type t = index * label_n_neighbors
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val fmt : t formatter
  end = struct
    type label_n_neighbors = {
      label: v_label;
      incoming: VertexMMap.t;
      outgoing: VertexMMap.t;
    }
    type t = index * label_n_neighbors

    (* comparison of vertices ignores incoming/outgoing edges *)
    let compare (x,{label=j}) (y,{label=k}) =
      let c = Index.compare x y in if c <> 0 then c else VertexLabel.compare j k

    let equal (x,{label=j}) (y,{label=k}) =
      (Index.equal x y) && (VertexLabel.equal j k)

    let hash (x,_) =
      Index.hash x

    let fmt ff (i,{label}) =
      Format.fprintf ff "@[%a:@ %a@]" Index.fmt i VertexLabel.fmt label
  end


  (* sets of edges to/from neighbors are represented as multi-maps from
     vertices (to edge labels) *)
  and VertexMMap : (ImperativeMultiMap.S
                    with type k = Vertex.t
                     and type v = EdgeLabel.t
                     and type vs = EdgeLabelSet.t)
    = ImperativeMultiMap.Make (Vertex) (EdgeLabelSet)


  module VertexISet = ImperativeSet.Make(Vertex)
  module VertexSet = Set.Make(Vertex)
  module VertexIMap = ImperativeMap.Make(Vertex)
  module VertexMap = Map.Make(Vertex)


  type label_n_neighbors = Vertex.label_n_neighbors = {
    label: v_label;
    incoming: VertexMMap.t;
    outgoing: VertexMMap.t;
  }
  type vertex = index * label_n_neighbors


  module IndexLabelSet =
    ImperativeSet.Make
      (struct
         type t = index * v_label
         let equal = equal_tup2 Index.equal VertexLabel.equal
         let compare = compare_tup2 Index.compare VertexLabel.compare
       end)
  type roots = IndexLabelSet.t


  (* graphs are represented by sets of vertices, which are implemented
     using the isomorphic domain of multi-maps from indices to sets of
     label-and-neighbor tuples, which are implemented using maps from
     labels to pairs of edge sets *)
  module SubVertex = ImperativeMap.Make(VertexLabel)
  module Vertices = HashMap.Make(Index)
  type rays = VertexMMap.t
  type graph = {
    verts: ((rays * rays) SubVertex.t) Vertices.t;
    roots: roots
  }


  let create () = {verts= Vertices.create 31; roots= IndexLabelSet.create ()}
  let clear g = Vertices.clear g.verts ; IndexLabelSet.clear g.roots

  let index_of v = fst v
  let label_of v = (snd v).label

  let in_degree v = VertexMMap.length (snd v).incoming
  let out_degree v = VertexMMap.length (snd v).outgoing

  let iter_preds fn v = VertexMMap.iter fn (snd v).incoming
  let iter_succs fn v = VertexMMap.iter fn (snd v).outgoing

  let fold_preds fn v = VertexMMap.fold fn (snd v).incoming
  let fold_succs fn v = VertexMMap.fold fn (snd v).outgoing

  let predecessors v = fold_preds (fun v e r -> (v,e)::r) v []
  let successors v = fold_succs (fun v e r -> (v,e)::r) v []


  let vertices_for g k =
    match Vertices.tryfind g.verts k with
    | None ->
        []
    | Some(vs) ->
        SubVertex.fold (fun label (incoming, outgoing) a ->
          (k, {label; incoming; outgoing}) :: a
        ) vs []


  let roots g =
    IndexLabelSet.fold (fun (k,l) roots ->
      List.fold (fun ((_,{label}) as v) roots ->
        if VertexLabel.equal l label then
            v :: roots
        else
            roots
      ) (vertices_for g k) roots
    ) g.roots []


  let mem_vertex g (k,{label=l} as v) =
    L.printf 2 "mem_vertex: %a" Vertex.fmt v
    ;
    match Vertices.tryfind g.verts k with
    | None -> false
    | Some(vs) -> SubVertex.mem vs l


  let mem_edge g src label trg =
    L.printf 2 "mem_edge: %a -> %a %a" Vertex.fmt src Vertex.fmt trg EdgeLabel.fmt label ;
    assert(mem_vertex g src) ;
    assert(mem_vertex g trg)
    ;
    let eq_label = EdgeLabel.equal label
    in
       VertexMMap.existsi trg eq_label (snd src).outgoing
    && VertexMMap.existsi src eq_label (snd trg).incoming


  let add_edge g src label trg =
    L.printf 1 "add_edge:   %a -> %a %a" Vertex.fmt src Vertex.fmt trg EdgeLabel.fmt label
    ;
    if mem_vertex g src && mem_vertex g trg && not (mem_edge g src label trg)
    then (
      VertexMMap.add (snd src).outgoing trg label ;
      VertexMMap.add (snd trg).incoming src label
    )


  let remove_edge g src label trg =
    L.printf 1 "remove_edge:   %a -> %a %a" Vertex.fmt src Vertex.fmt trg EdgeLabel.fmt label ;
    assert( mem_edge g src label trg ); (fun () -> assert( not (mem_edge g src label trg) )) <&
    let eq_label lbl = not (EdgeLabel.equal label lbl)
    in
    VertexMMap.filteri trg eq_label (snd src).outgoing ;
    VertexMMap.filteri src eq_label (snd trg).incoming


  let add_vertex g (k,l) =
    let do_add vs =
      let v =
        {label= l; incoming= VertexMMap.create(); outgoing= VertexMMap.create()}
      in
      L.printf 1 "add_vertex: %a" Vertex.fmt (k,v) ;
      SubVertex.add vs v.label (v.incoming, v.outgoing) ;
      (k,v)
    in
    match Vertices.tryfind g.verts k with
    | None ->
        let vs = SubVertex.create () in
        Vertices.add g.verts k vs ;
        do_add vs
    | Some(vs) ->
        match SubVertex.tryfind vs l with
        | None ->
            do_add vs
        | Some((i,o)) ->
            L.printf 1 "add_vertex: already exists: %a: %a" Index.fmt k VertexLabel.fmt l ;
            (k, {label= l; incoming= i; outgoing= o})


  let rec remove_vertex g ((k, {label= l; incoming= i; outgoing= o}) as v) =
    L.incf 1 "( remove_vertex: %a" Vertex.fmt v ;
    (fun _ -> L.decf 1 ") remove_vertex")
    <&
    if mem_vertex g v
      && VertexMMap.is_empty i
      && not (IndexLabelSet.mem g.roots (k,l))
    then (
      L.printf 1 "vertex is unreachable and not a root"
      ;
      VertexMMap.iter (fun u l -> remove_edge g v l u ; remove_vertex g u) o
      ;
      match Vertices.tryfind g.verts k with
      | None -> ()
      | Some(vs) ->
          SubVertex.remove vs l ;
          if SubVertex.is_empty vs then Vertices.remove g.verts k
    )


  let replace_vertex g new_label old_trg new_trg =
    L.incf 1 "( replace_vertex: %a with %a" Vertex.fmt old_trg Vertex.fmt new_trg ;
    (fun _ -> L.decf 1 ") replace_vertex")
    <&
    let swing_edge g src label old_trg new_trg =
      if mem_edge g src label old_trg then (
        remove_edge g src label old_trg ;
        add_edge g src (new_label label) new_trg
      )
    in
    assert( not (Vertex.equal old_trg new_trg) );
    VertexMMap.iter (fun src lab ->
      swing_edge g src lab old_trg new_trg
    ) (snd old_trg).incoming ;
    assert( VertexMMap.is_empty (snd old_trg).incoming );
    VertexMMap.iter (fun succ lab ->
      add_edge g new_trg (new_label lab) (if Vertex.equal succ old_trg then new_trg else succ)
    ) (snd old_trg).outgoing ;
    assert( VertexMMap.is_empty (snd old_trg).incoming );
    remove_vertex g old_trg


  let relabel_vertex g ((k,{label}) as old_vtx) new_label =
    assert( VertexLabel.equal label new_label
            || invalid_arg "relabel_vertex must preserve equality of labels" );
    assert( mem_vertex g old_vtx
            || invalid_arg "relabel_vertex must relabel existing vertex" );
    let vs = Vertices.find g.verts k in
    let i,o = SubVertex.find vs label in
    let new_vtx = (k, {label= new_label; incoming= i; outgoing= o}) in
    VertexMMap.iter (fun v l ->
      remove_edge g v l old_vtx ;
      add_edge g v l new_vtx
    ) i ;
    SubVertex.add vs new_label (i,o) ;
    new_vtx


  let collapse_edge_pre g src label trg =
    L.printf 1 "collapse_edge_pre:   %a -> %a %a" Vertex.fmt src Vertex.fmt trg EdgeLabel.fmt label ;
    assert(mem_edge g src label trg)
    ;
    remove_edge g src label trg
    ;
    VertexMMap.iter (fun u l ->
      remove_edge g u l trg ;
      add_edge g (if Vertex.equal u trg then src else u) l src
    ) (snd trg).incoming
    ;
    VertexMMap.iter (fun u l ->
      remove_edge g trg l u ;
      add_edge g src l (if Vertex.equal u trg then src else u)
    ) (snd trg).outgoing
    ;
    remove_vertex g trg


  let collapse_edge_post g src label trg =
    L.printf 1 "collapse_edge_post:   %a -> %a %a" Vertex.fmt src Vertex.fmt trg EdgeLabel.fmt label ;
    assert(mem_edge g src label trg)
    ;
    VertexMMap.iter (fun u l ->
      remove_edge g u l src ;
      add_edge g (if Vertex.equal u src then trg else u) l trg
    ) (snd src).incoming
    ;
    remove_edge g src label trg ;
    remove_vertex g src


  let root_vertex g (k,a) =
    IndexLabelSet.add g.roots (k,a.label)

  let unroot_vertex g (k,a) =
    IndexLabelSet.remove g.roots (k,a.label)


  let fold_vertices_index fn g k z =
    match Vertices.tryfind g.verts k with
    | None ->
        z
    | Some(vs) ->
        SubVertex.fold (fun label (incoming,outgoing) z ->
          let v = (k, {label; incoming; outgoing}) in
          if mem_vertex g v
          then fn v z
          else z
        ) vs z

  let iter_vertices_index fn g k =
    fold_vertices_index (fun v () -> fn v) g k ()


  let fold_vertices fn g z =
    Vertices.fold
      (fun k vs z ->
        SubVertex.fold
        (fun l (i,o) a ->
          let v = (k, {label=l; incoming=i; outgoing=o}) in
          fn v a
        ) vs z
      ) g.verts z

  let iter_vertices fn g =
    fold_vertices (fun v () -> fn v) g ()


  let fold_edges vertex_fn edge_fn g k z =
    let edge_opt_fn prev_opt curr z =
      match prev_opt with
      | Some(prev,label) -> edge_fn (prev,label,curr) z
      | None -> z
    in
    let memo = VertexISet.create ()
    in
    let rec walk prev_opt curr z =
      if VertexISet.mem memo curr then
        edge_opt_fn prev_opt curr z
      else
        let z = vertex_fn curr z in
        let z = edge_opt_fn prev_opt curr z in
        VertexISet.add memo curr ;
        VertexMMap.fold (fun next label z ->
          walk (Some(curr,label)) next z
        ) (snd curr).outgoing z
    in
    fold_vertices_index (walk None) g k z

  let iter_edges vertex_fn edge_fn g k =
    fold_edges (fun v () -> vertex_fn v) (fun e () -> edge_fn e) g k ()


  let identify_vertices g =
    let vertex_ids : int VertexIMap.t = VertexIMap.create () in
    let count = ref 0 in
    let identify_vertex v =
      if not (VertexIMap.mem vertex_ids v) then (
        VertexIMap.add vertex_ids v !count ;
        incr count
      )
    in
    iter_vertices identify_vertex g ;
    vertex_ids


  let cutpoints root =
    let rec df_walk src ancestors visited cutpoints =
      VertexMMap.fold (fun trg _ (visited, cutpoints) ->
        if not (VertexSet.mem trg visited) then
          df_walk trg (VertexSet.add trg ancestors) (VertexSet.add trg visited) cutpoints
        else if not (VertexSet.mem trg ancestors) then
          (visited, cutpoints)
        else
          (visited, VertexSet.add trg cutpoints)
      ) (snd src).outgoing (visited, cutpoints)
    in
    let roots = VertexSet.singleton root
    in
    snd (df_walk root roots roots VertexSet.empty)


  (* Breadth-first search from CLR Algorithms textbook. *)

  type visit_state = White | Grey | Black

  let bfs g start =
    (* State *)
    let colour : visit_state VertexIMap.t = VertexIMap.create () in
    let distance : int VertexIMap.t = VertexIMap.create () in
    let predecessor : (vertex * e_label) option VertexIMap.t =
      VertexIMap.create () in
    let grey_q : vertex Queue.t = Queue.create () in

    (* Initialize state. *)
    iter_vertices
      (fun v ->
         VertexIMap.add colour v White ;
         VertexIMap.add distance v max_int ;
         VertexIMap.add predecessor v None
      )
      g ;
    VertexIMap.add colour start Grey ;
    VertexIMap.add distance start 0 ;
    VertexIMap.add predecessor start None ;
    Queue.push start grey_q;
    (* Main loop. *)
    while (not (Queue.is_empty grey_q)) do
      L.incf 2 "( bfs visit" ; (fun _ -> L.decf 2 ") bfs visit") <&
      let u = Queue.peek grey_q in
      L.printf 2 " u --> v, where@\nu is %a" Vertex.fmt u ;
      List.iter (fun (v,tr) ->
        L.printf 2 " v is %a" Vertex.fmt v ;
        if (VertexIMap.find colour v = White) then (
          L.printf 2 " v is White" ;
          VertexIMap.add colour v Grey ;
          VertexIMap.add distance v ((VertexIMap.find distance u) + 1) ;
          VertexIMap.add predecessor v (Some (u,tr)) ;
          Queue.push v grey_q
        ) else
          L.printf 2 " v is not White" ;
      ) (successors u) ;
      let _ = Queue.pop grey_q in
      VertexIMap.add colour u Black
    done ;
    (distance,predecessor)


  (* Depth-first search *)

  let dfs_iter ?next:(next=fun () -> () ) ?forwards:(forwards=true) pre post starts =
    let visited = VertexISet.create ()
    in
    let rec dfs_visit u =
      L.incf 1 "( dfs visit: u is %a" Vertex.fmt u ; (fun _ -> L.decf 1 ") dfs visit") <&
      if not (VertexISet.mem visited u) then (
        VertexISet.add visited u ;
        pre u ;
        VertexMMap.iter (fun v _ ->
          dfs_visit v
        ) (if forwards then (snd u).outgoing else (snd u).incoming) ;
        post u
      )
    in
    List.iter (fun start -> dfs_visit start ; next()) starts


  (* Implementation from Introduction to Algorithms, Cormen et al
     Section 23.5 Page 488
  *)
  let scc graph =
    (fun scc_map -> assert(true$>
      if Config.check_scc then (
        let reaches x y =
          let res = ref false in
          dfs_iter (fun v -> if Vertex.equal v y then res := true) (fun _ -> ()) [x] ;
          !res in
        iter_vertices (fun x ->
          iter_vertices (fun y ->
            if not (Vertex.equal x y) then
              let scc_x = VertexMap.find x scc_map in
              let scc_y = VertexMap.find y scc_map in
              if (scc_x == scc_y) <> (reaches x y && reaches y x) then
                failwith "Graph.scc incorrect"
          ) graph
        ) graph
      )
    ))<&
    let vtxs = fold_vertices List.cons graph [] in
    let rev_postorder = ref [] in
    let skip = fun _ -> () in
    let add_to rl = fun v -> rl := v :: !rl in
    (* Get the finished times for each node *)
    dfs_iter skip
      (add_to rev_postorder)
      vtxs ;
    (* Walk backwards in reverse finished time *)
    let current_scc = ref [] in
    let scc_map = ref VertexMap.empty in
    dfs_iter ~forwards:false
      ~next:(fun () ->
        (* Add each vertex in the scc to the map, with the whole SCC *)
        List.iter
          (fun v ->
            scc_map := VertexMap.add v !current_scc !scc_map
          )
          (!current_scc) ;
        (* Setup next scc *)
        current_scc := []
      )
      skip
      (add_to current_scc)
      !rev_postorder ;
    !scc_map

  let dfs start =
    let rev_preorder : (vertex list) ref = ref [] in
    let rev_postorder : (vertex list) ref = ref [] in
    let preorder_map : int VertexIMap.t = VertexIMap.create () in
    let postorder_map : int VertexIMap.t = VertexIMap.create () in
    let preorder_count = ref 0 in
    let postorder_count = ref 0 in
    dfs_iter
      (fun u ->
        rev_preorder := u :: !rev_preorder ;
        VertexIMap.add preorder_map u !preorder_count ;
        incr preorder_count
      )
      (fun u ->
        rev_postorder := u :: !rev_postorder ;
        VertexIMap.add postorder_map u !postorder_count ;
        incr postorder_count
      )
      [start] ;
    let preorder = List.rev (!rev_preorder) in
    let postorder = List.rev (!rev_postorder) in
    let preorder_num = VertexIMap.find preorder_map in
    let postorder_num = VertexIMap.find postorder_map in
    (preorder, postorder, preorder_num, postorder_num)


  let dfs_revpost start =
    let rev_postorder = ref [] in
    let postorder_map = VertexIMap.create () in
    let postorder_count = ref 0 in
    dfs_iter (fun _ -> ()) (fun u ->
      rev_postorder := u :: !rev_postorder ;
      VertexIMap.add postorder_map u !postorder_count ;
      incr postorder_count
    ) [start] ;
    (!rev_postorder, VertexIMap.find postorder_map)


  (* Dominance, based on "A Simple, Fast Dominance Algorithm" (Cooper et al) *)

  let immediate_dominators start =
    let rev_postorder, postorder_num = dfs_revpost start in
    let nnodes = (postorder_num (List.hd rev_postorder)) + 1 in
    let undefined = -1 in
    let doms = Array.create nnodes undefined in
    doms.(postorder_num start) <- postorder_num start
    ;
    let intersect i j =
      let rec outer i j =
        if i <> j then
          let rec inner i j =
            if i < j then
              inner doms.(i) j
            else
              i
          in
          let i = inner i j in
          let j = inner j i in
          outer i j
        else
          i
      in
      outer i j
    in
    let not_root = List.tl rev_postorder
    in
    let rec build_dom_tree progress =
      if progress then build_dom_tree (
        List.fold (fun n progress ->
          let preds = fold_preds (fun p _ preds -> postorder_num p :: preds) n [] in
          let processed_pred, other_preds = List.take (fun p -> doms.(p) <> undefined) preds in
          let new_idom =
            List.fold (fun p new_idom ->
              if doms.(p) <> undefined then
                intersect p new_idom
              else
                new_idom
            ) other_preds processed_pred in
          let b = postorder_num n in
          if doms.(b) <> new_idom then (
            doms.(b) <- new_idom ;
            true
          ) else
            progress
        ) not_root false
      )
    in
    build_dom_tree true
    ;
    (doms, rev_postorder, postorder_num)


  (* v dominates u if v is the least-common ancestor of v and u *)
  let dominates doms postorder_num v u =
    let i = postorder_num v
    in
    let rec loop j =
      if j < i then
        loop doms.(j)
      else
        j = i
    in
    loop (postorder_num u)


  let dominance_frontier cfg start =
    let doms, rev_postorder, postorder_num = immediate_dominators start in
    let postorder = List.rev rev_postorder in
    (* the ith vertex in the postorder traversal *)
    let postorder_vtx i =
      List.nth postorder i
    in
    let dfset : VertexISet.t VertexIMap.t = VertexIMap.create () in
    iter_vertices
      (fun n ->
         VertexIMap.add dfset n (VertexISet.create ())
      )
      cfg;
    iter_vertices
      (fun n ->
         let preds = predecessors n in
         if List.length preds >= 2 then (
           let b = postorder_num n in
           List.iter
             (fun (p,_) ->
               let runner = ref (postorder_num p) in
               while (!runner <> doms.(b)) do
                 let runner_dfset = VertexIMap.find dfset (postorder_vtx !runner) in
                 VertexISet.add runner_dfset n;
                 runner := doms.(!runner)
               done
             )
             preds
         )
      )
      cfg;

    (* returns the immediate dominator of n *)
    let parent_of n =
      try Some(postorder_vtx doms.(postorder_num n))
      with Not_found -> None
    in

    (* returns the set of vertices immediately dominated by n *)
    let children_of =
      let map : VertexISet.t VertexIMap.t = VertexIMap.create () in
      iter_vertices (fun n -> VertexIMap.add map n (VertexISet.create ())) cfg ;
      iter_vertices (fun n ->
        Option.iter (fun p ->
          Option.iter (fun p ->
            VertexISet.add p n
          ) (VertexIMap.tryfind map p)
        ) (parent_of n)
      ) cfg ;
      VertexISet.remove (VertexIMap.find map start) start;
      VertexIMap.find map
    in

    (* return dominator frontier set,
              dominator relation, and
              dominator tree functions *)
    (dfset, dominates doms postorder_num, parent_of, children_of)

  (* Return the set of natural loops in [cfg] with [dominates] relation *)

  let natural_loops cfg dominates =
    (* n -> h(eader) is a backedge if h dominates n *)
    let backedges = fold_vertices
      (fun n acc ->
        let hs = List.filter
          (fun (h,_) -> dominates h n)
          (successors n)
        in
        acc @ (List.map (fun (h,_) -> (h,n)) hs)
      )
      cfg []
    in
    (* for each h,n pair, walk up the graph to gather the body *)
    List.map
      (fun (h,n) ->
        let body = VertexISet.create () in
        let s = Stack.create () in
        VertexISet.add body h;
        Stack.push n s;
        while (not (Stack.is_empty s)) do
          let d = Stack.pop s in
          if (not (VertexISet.mem body d)) then (
            VertexISet.add body d;
            List.iter
              (fun (p,_) -> Stack.push p s)
              (predecessors d)
          )
        done;
        (body,h,n)
      )
      backedges

  (* Floyd-Warshall, and construct shortest-path.
     From CLR Algorithms textbook. *)
  type distance = Infinity | D of int
  type pre      = Nil | P of int

  let is_infinity d = match d with Infinity -> true | _ -> false

  (* d + d' *)
  let add d d' =
    match d, d' with
    | Infinity,_ | _,Infinity -> Infinity
    | D(i), D(i') -> D(i+i')

  (* d <= d' *)
  let leq d d' =
    match d, d' with
    | Infinity, _ -> false
    | _, Infinity -> true
    | D(i), D(i') -> i <= i'

  (* SI: should return only in terms of
       d : VM.key -> VM.key -> int
       pre: VM.key -> VM.key -> int  *)
  let fw g =

    let vtx_to_i = identify_vertices g in

    let n = (VertexIMap.fold (fun _vtx i size -> max i size) vtx_to_i 0) + 1 in

    (* reverse vtx_to_i *)
    let i_to_vtx = IntHMap.create n in
    VertexIMap.iter (fun vtx i ->
      IntHMap.add i_to_vtx i vtx
    ) vtx_to_i
    ;

    (* Is there an edge between the vertex indexed by i
       and the one indexed by j?*)
    let edge i j =
        List.exists (fun i' ->
            Vertex.equal i' (IntHMap.find i_to_vtx j)
        ) (List.map fst (successors (IntHMap.find i_to_vtx i)))
    in

    (* Matrix d (pre) uses None for infinity (NIL). *)
    let d = Array.make_matrix n n Infinity  in
    let pre = Array.make_matrix n n Nil in

    (* initialize d *)
    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
        if (i=j) then d.(i).(j) <- D(0)
        else if (i <> j) then
          if (edge i j) then d.(i).(j) <- D(1)
          else d.(i).(j) <- Infinity
      done
    done
    ;
    (* initialize pre *)
    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
        if (i=j || (is_infinity d.(i).(j))) then
          pre.(i).(j) <- Nil
        else
          pre.(i).(j) <- P(i)
      done
    done
    ;

    (* main loop *)
    for k = 0 to (n-1) do
      for i = 0 to (n-1) do
        for j = 0 to (n-1) do
          if (leq (d.(i).(j))  (add d.(i).(k)  d.(k).(j))) then (
            (* SI: un-necessary *)
            d.(i).(j) <- d.(i).(j) ;
            pre.(i).(j) <- pre.(i).(j) ;
          ) else (
            d.(i).(j) <- add d.(i).(k)  d.(k).(j) ;
            pre.(i).(j) <- pre.(k).(j) ;
          )
        done
      done
    done
    ;

    (* Convert [d] and [pre] matrices into (sparse) Vtx->Vtx->data maps.  *)
    let d' = Array.make_matrix n n None  in
    let pre' = Array.make_matrix n n None in

    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
        d'.(i).(j) <- (match d.(i).(j) with
        | D(d) ->  Some d
        | Infinity -> None );
        pre'.(i).(j) <- (match pre.(i).(j) with
          | P(p) -> Some p
          | Nil -> None )
      done
    done
    ;
    (* return both [d] and [pre] matrices *)

    (vtx_to_i,i_to_vtx, d',pre')


  let remove_unreachable g =
    (fun () -> assert(true$>
      let reachable = VertexISet.create () in
      IndexLabelSet.iter (fun (k,label) ->
        let incoming, outgoing = SubVertex.find (Vertices.find g.verts k) label in
        let r = (k, {label; incoming; outgoing}) in
        dfs_iter (fun v ->
          VertexISet.add reachable v
        ) (fun _ -> ()) [r]
      ) g.roots ;
      iter_vertices (fun v ->
        assert( VertexISet.mem reachable v
              || L.warnf "remove_unreachable failed to remove: %a" Vertex.fmt v )
      ) g
    )) <&
    iter_vertices (remove_vertex g) g


  let copy g0 src =
    let g = create ()
    in
    let m =
      fold_edges
        (fun v m ->
           VertexMap.add v (add_vertex g (index_of v, label_of v)) m
        )
        (fun (u,l,v) m ->
           match VertexMap.tryfind u m, VertexMap.tryfind v m with
           | Some(u'), Some(v') ->
               add_edge g u' l v'; m
           | _ ->
               m
        )
        g0
        (index_of src)
        VertexMap.empty
    in
    (m, g)


  let slice src trg g0 =
    let vtx_to_id,_, distance,_ = fw g0
    in
    let trg_id = VertexIMap.find vtx_to_id trg
    in
    let g = create ()
    in
    let m =
      fold_edges
        (fun v m ->
           assert( distance.(VertexIMap.find vtx_to_id src).(VertexIMap.find vtx_to_id v) <> None );
           if distance.(VertexIMap.find vtx_to_id v).(trg_id) <> None then
             VertexMap.add v (add_vertex g (index_of v, label_of v)) m
           else
             m
        )
        (fun (u,l,v) m ->
           match VertexMap.tryfind u m, VertexMap.tryfind v m with
           | Some(u'), Some(v') ->
               add_edge g u' l v'; m
           | _ ->
               m
        )
        g0
        (index_of src)
        VertexMap.empty
    in
    (m, g)


  (* Conversion to dot format. *)

  let calculate_margin len =
    let rec loop height width =
      if width /. height > Config.margin_frac then
        loop (height +. 1.) (width /. 2.)
      else
        int_of_float width
    in
    if len <= 40 then 40 else
    loop 1. (float_of_int len)

  let reprint msg =
    let src_buf = Buffer.create 128 in
    let ff = Format.formatter_of_buffer src_buf in
    Format.pp_set_margin ff max_int ;
    msg ff ;
    Format.pp_print_flush ff () ;

    let len = Buffer.length src_buf in
    let src_buf = Buffer.create len in
    let ff = Format.formatter_of_buffer src_buf in
    Format.pp_set_margin ff (calculate_margin len) ;
    msg ff ;
    Format.pp_print_flush ff () ;

    let dst_buf = Buffer.create len in
    for ind = 0 to Buffer.length src_buf - 1 do
      match Buffer.nth src_buf ind with
      | '\n' -> Buffer.add_string dst_buf "\\l"
      | '\\' -> Buffer.add_string dst_buf "\\\\"
      | '\"' -> Buffer.add_string dst_buf "\\\""
      | char -> Buffer.add_char dst_buf char
    done ;
    Buffer.contents dst_buf

  let writers g out =
    let id =
      let m = identify_vertices g in
      VertexIMap.find m
    in
    let write_vertex v =
      let msg v = reprint (fun ff -> Vertex.fmt ff v) in
      Printf.bprintf out "%i [shape=box,label=\"v %i: %s\\l\"]\n" (id v) (id v) (msg v)
    in
    let write_edge (src,lab,trg) =
      let msg lab = reprint (fun ff -> EdgeLabel.fmt ff lab) in
      Printf.bprintf out "%i -> %i [label=\"%s\\l\"]\n" (id src) (id trg) (msg lab)
    in
    (write_vertex, write_edge, id)

  let write_dot_header out =
    Printf.bprintf out "digraph g {\n" ;
    if Config.font <> "" then (
      Printf.bprintf out "graph [fontname=\"%s\"]\n" Config.font ;
      Printf.bprintf out "node [fontname=\"%s\"]\n" Config.font ;
      Printf.bprintf out "edge [fontname=\"%s\"]\n" Config.font ;
    )

  let write_dot_footer out =
    Printf.bprintf out "}\n"

  (* cfg with dominator tree *)
  let cfg_write_dot cfg children_of k out =
    let write_vertex, write_edge, id = writers cfg out in
    write_dot_header out ;
    iter_edges write_vertex write_edge cfg k ;
    (* dominator tree edges *)
    iter_vertices (fun n ->
      VertexISet.iter (fun m ->
        Printf.bprintf out "%i -> %i [dir=none, weight=3, penwidth=3, color=\"#660000\"]\n" (id n) (id m))
        (children_of n);
    ) cfg;
    write_dot_footer out

  let write_dot g k out =
    let write_vertex, write_edge, _ = writers g out
    in
    write_dot_header out ;
    iter_edges write_vertex write_edge g k ;
    write_dot_footer out


  let write_dot_partitioned fn g roots out =
    let write_vertex, write_edge, _ = writers g out
    in
    let vs = ref [] in
    List.iter (iter_edges (fun v -> vs := v :: !vs) (fun _ -> ()) g) roots ;
    let partitions =
      List.classify (fun x y ->
        Option.equal (fun a b -> fst a = fst b)
          (fn (index_of x)) (fn (index_of y))
      ) !vs in
    write_dot_header out ;
    List.iter (fun vs ->
      match fn (index_of (List.hd vs)) with
      | None -> ()
      | Some(name,msg) ->
          Printf.bprintf out "subgraph cluster%s {\nlabel=\"%s\"\n" name (reprint msg) ;
          List.iter write_vertex vs ;
          Printf.bprintf out "}\n"
    ) partitions ;
    List.iter (fun root ->
      iter_edges
        (fun v ->
           match fn (index_of v) with
           | Some _ -> ()
           | None -> write_vertex v
        )
        write_edge
        g root
    ) roots ;
    write_dot_footer out

end
