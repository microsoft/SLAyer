(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library


(* Tracing ================================================================== *)

module L = struct
  let log = Log.std Graph.verbose
  let printf l x = Log.printf log l               x
  let incf   l x = Log.incf   log l ("@[<hov 2>"^^x^^"@]")
  let decf   l x = Log.decf   log l ("@[<hov 2>"^^x^^"@]")
  let warnf    x = Log.warnf  log   ("@[<hov 2>"^^x^^"@]")
  let errorf   x = Log.errorf log   ("@[<hov 2>"^^x^^"@]")
end


(* Test #1: (add_edge; remove_edge) should be id. *)
module Nodes = struct
  type t = int
  let compare = Pervasives.compare
  let equal x y = (x=y)
  let hash  = Hashtbl.hash
  let fmt ff = Format.fprintf ff "%d"
end

module Edges = struct
  type t = string
  let compare = Pervasives.compare
  let equal x y = (x = y)
  let fmt ff = Format.fprintf ff "%s"
end
module G = Graph.Make (Nodes) (Nodes) (Edges)

let test1 _ =
  begin
    let g = G.create () in
    G.clear g;
    let src, tgt = 1,100 in
    let vtx_src = G.add_vertex g src src in
    let vtx_tgt = G.add_vertex g tgt tgt in
    let lbl = "1_2_skipafew_99_100" in
    G.add_edge g vtx_src lbl vtx_tgt ;
    let result_add =
      if (G.mem_edge g vtx_src lbl vtx_tgt)
      then (L.printf 1 "Graph_test#1: edge added, OK" ; true)
      else (L.printf 1 "Graph_test#1: edge not added, FAIL!" ; false)
    in
    G.remove_edge g vtx_src lbl vtx_tgt ;
    let result_remove =
      if (G.mem_edge g vtx_src lbl vtx_tgt)
      then (L.printf 1 "Graph_test: edge found, FAIL!" ; false)
      else (L.printf 1 "Graph_test: edge not found, OK" ; true)
    in
    result_add && result_remove
  end



let test _ =
  L.printf 0 "testing Graph" ;
  assert (test1 ())
