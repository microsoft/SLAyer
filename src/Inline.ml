(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(**  Inline function calls. *)

open Library

(**/**)
open Variable
open Expression
module E = Exp
module S = Substitution
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint
(**/**)

module L = (val Log.std Config.vInline : Log.LOG)


(** Discover which procs are inline-able. *)


module CallGraph = Graph.Make (Proc.Id) (Proc.Id) (K.Id)

let calculate_call_map program =
  Prog.fold_procs (fun caller call_map ->
    CFG.fold_edges (fun _ cm -> cm) (fun (u,c,_v) call_map ->
      match c with
      | C.Call{Call.targets}
      | C.ICall{Call.targets} -> List.fold (fun x a -> (caller.Proc.id, x, K.id u) :: a) targets call_map
      | _ -> call_map
    ) caller.Proc.cfg (K.id caller.Proc.entry) call_map
  ) program []

let graph_of_call_map call_map =
  let g = CallGraph.create () in
  List.iter (fun (caller,callee,node_id) ->
    let u = CallGraph.add_vertex g (caller,caller) in
    let v = CallGraph.add_vertex g (callee,callee) in
    CallGraph.add_edge g u (node_id) v
  ) call_map ;
  g

let calculate_rec_procs program =
  let m = calculate_call_map program in
  let g = graph_of_call_map m in
  let main = program.Prog.main in
  let main_vtx = CallGraph.add_vertex g (main,main) in
  CallGraph.root_vertex g main_vtx ;
  CallGraph.remove_unreachable g ;
  let cs = CallGraph.cutpoints main_vtx in
  L.printf 2 "cutpoints %a" (List.fmt "," CallGraph.Vertex.fmt) (CallGraph.VertexSet.to_list cs) ;
  let recs =
    CallGraph.VertexSet.fold (fun c cs ->
      (CallGraph.index_of c) :: cs
    ) cs [] in
  let uses = Proc.IdHMap.create 20 in
  let main_idx = CallGraph.index_of main_vtx in
  CallGraph.iter_edges
    (fun c -> Proc.IdHMap.add uses (CallGraph.index_of c) (List.length (CallGraph.predecessors c)))
    (fun _edge -> ())
    g main_idx ;
  Proc.IdHMap.add uses main_idx 1 ;
  (recs, uses)

let recursive recs p =
  List.mem p.Proc.id recs

(* Returns true, if procedure is a leaf, (makes no calls) *)
let contains_loop_and_leaf_proc proc =
  let leaf_proc = ref true in
  let contains_loop = ref false in
  CFG.iter_edges
    (fun v ->
      match K.sort v with
      | Some(K.Cut) -> contains_loop := true
      | _ -> ())
    (fun (_v,c,_v') ->
      match c with
      | C.Call(_)
      | C.ICall _ -> leaf_proc := false
      | _ -> ()
    )
    proc.Proc.cfg (K.id proc.Proc.entry) ;
  (!contains_loop, !leaf_proc)


let inlineable (recs,uses) p in_loop =
  let contains_loops,leaf_proc = contains_loop_and_leaf_proc p in
  let macro = leaf_proc && (not contains_loops) in
  let no_rec = not (recursive recs p) in
  let thin_wrapper =  no_rec && (not contains_loops) in
  let single_call_not_in_loop = no_rec && (not in_loop) && (Proc.IdHMap.find uses (p.Proc.id) = 1) in
  L.printf 2 "proc %a: macro:%b, thin_wrapper:%b, single_call_not_in_loop: %b no_rec:%b"
    Proc.Id.fmt p.Proc.id macro thin_wrapper single_call_not_in_loop no_rec ;
  match Config.optimize_inline with
  | 0 -> false
  | 1 -> macro
  | 2 -> macro || thin_wrapper
  | 3 -> macro || thin_wrapper || single_call_not_in_loop
  | 4 -> no_rec
  | _ -> failwith "Unknown inline level"


(** inline_proc and inline_body *)

(* Clone callee graph. *)
let clone_for_inlining caller callee =

  (* Map of callee vertex to corresponding vertex in inlined caller code. *)
  let module CPHMap = HashMap.Make(K) in
  let proc_to_inline = CPHMap.create 32 in

  let {Proc.id= caller_id; cfg= caller_cfg} = caller in
  let {Proc.cfg= callee_cfg; entry= callee_entry; exit= callee_exit} = callee in

  (* Clone callee vertices and edges into caller. *)
  CFG.iter_vertices (fun v ->
    let v' = CFG.add_vertex caller_cfg (K.mk_label ?sort:(K.sort v) (K.pos v) caller_id) in
    CPHMap.add proc_to_inline v v'
  ) callee_cfg ;
  CFG.iter_edges (fun _ -> ()) (fun (u,c,v) ->
    let u' = CPHMap.find proc_to_inline u in
    let v' = CPHMap.find proc_to_inline v in
    CFG.add_edge caller_cfg u' c v'
  ) callee_cfg (K.id callee_entry) ;

  (* Find cloned entry and exit. *)
  let inlined_entry = CPHMap.find proc_to_inline callee_entry in
  let inlined_exit  = CPHMap.find proc_to_inline callee_exit in

  (* Return cloned callee_cfg as caller sub-graph. *)
  (inlined_entry, inlined_exit)


(*
  Translate this edge in [proc]:

   u --Call(p,args)--> v

  into this edge in [proc] (with p's locals and formals now added to [proc]'s):

   u --Inst[frmls=actls]--> clone_for_inlining(p) --Inst[Kill(frmls,locals)] --> v

  The clone_for_inlining function clones the body of p.
*)

let work_count = ref 0

let proc procs recs_uses ({Proc.id; locals; cfg; entry} as proc) =
  L.incf 1 "( analyzing %a" Proc.Id.fmt id ; (fun _p -> L.decf 1 ")") <&

  let in_loop =
    if Config.optimize_inline = 3 then (
      L.incf 3 "( calculating SCC" ; (fun _ -> L.decf 3 ")") <&
      let vtx_to_scc = CFG.scc cfg in
      fun v ->
        (List.length (CFG.VertexMap.find v vtx_to_scc)) > 1
    )
    else
      fun _ -> false in

  (* Replacing the graph while folding over it is bad. So we first collect eligible call-sites. *)
  let inline_candidates =
    CFG.fold_edges (fun _ acc -> acc)
      (fun (u,c,v) acc ->
        match c with
        | C.Call{Call.proc; actuals; areturn} ->
            let callee =
              try Proc.IdHMap.find procs proc
              with Not_found -> failwithf "Undefined procedure: %a" Proc.Id.fmt proc in
            if inlineable recs_uses callee (in_loop u) then (
              L.printf 1 "inlining %a" Proc.Id.fmt proc ;
              (u, c, v, Call.mk callee actuals areturn) :: acc
            )
            else (
              L.printf 2 "Not inlining %a" Proc.Id.fmt proc ;
              acc
            )
        (* Don't care about non-Call commands *)
        | _ ->
            acc
      ) cfg (K.id entry) [] in

  (* Inline an inline-candidate. Add it's (formals+locals) and ret-sites to [acc]. *)
  let inline (call_site, c, retn_site, call) caller_locals =
    let {Call.proc= callee; areturn} = call in
    let {Proc.formals; freturn; locals} = callee in
    incr work_count ;

    (* Move actuals to formals. *)
    let prologue =
      let frmls_to_actls,_ = Call.args {call with Call.areturn= None} in
      S.fold (fun f a blk ->
        match E.desc f with
        | E.Var(f) -> I.mk (I.Move(f, a)) (K.pos call_site) :: blk
        | _ -> assert false
      ) frmls_to_actls [] in

    (* Clone body of callee. *)
    let inlined_entry, inlined_exit =
      clone_for_inlining proc callee in

    (* Move formal return to actual return. *)
    let epilogue =
      match freturn, areturn with
      | Some(frtrn), Some(artrn) -> [I.mk (I.Move(artrn, E.mkVar frtrn)) (K.pos inlined_exit)]
      | _ -> [] in

    (* Kill callee formals, formal return, and locals *)
    let extra_locals =
      Vars.union (Vars.of_list formals) (Option.fold Vars.add freturn locals) in
    let epilogue =
      epilogue @ [I.mk I.(Kill(extra_locals)) (K.pos inlined_exit)] in

    (* Connect  call_site -prologue-> entry  and  exit -epilogue-> retn_site *)
    CFG.add_block_edge cfg call_site prologue inlined_entry ;
    CFG.add_block_edge cfg inlined_exit epilogue retn_site ;
    CFG.remove_edge cfg call_site c retn_site ;

    (Vars.union extra_locals caller_locals)
  in

  (* Inline calls. *)
  let locals = List.fold inline inline_candidates locals in

  {proc with Proc.locals}


(* Entry point *)
let prog program =
  L.incf 1 "( inline" ; (fun _p -> L.decf 1 ")") <&
  if Config.optimize_inline <= 0 then program
  else

  let rec loop p =
    work_count := 0 ;
    let recs, uses = calculate_rec_procs p in
    (* Notes: Recalculate addr_taken, based on call graph *)

    (* Remove all the procedures not used from call graph *)
    let procs = p.Prog.procs in
    Proc.IdHMap.filter (fun pid _ ->
      let b =  (* Don't remove procedures that are used *)
        try Proc.IdHMap.find uses pid > 0
        with Not_found -> false in
      if not b then (
        L.printf 2 "remove unused proc %a" Proc.Id.fmt pid ;
        incr work_count
      );
      b
    ) procs ;
    if !work_count > 0 then (
      L.printf 1 "removed %d unused procs" !work_count ;
      work_count := 0
    );

    let p' = Prog.map_procs (proc procs (recs, uses)) p in
    if !work_count > 0 then (
      L.printf 1 "inlined %d procs" !work_count ;
      loop p'
    )
    else
      p'
  in
  let program = loop program
  in
  let ({Prog.procs; global_setup; inits; addr_taken} as program) = program
  in
  let global_setup = List.filter (Proc.IdHMap.mem procs) global_setup
  and inits = List.filter (Proc.IdHMap.mem procs) inits
  and addr_taken = List.filter (Proc.IdHMap.mem procs) addr_taken
  in
  {program with Prog.global_setup; inits; addr_taken}
