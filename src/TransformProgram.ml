(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library
open Type

open Variable
open Expression
module E = Exp
module S = Substitution
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint

module L = (val Log.std Config.vPgm : Log.LOG)


let normalize_tmr = Timer.create "TransformProgram.normalize"


(*==========================================================================================================
                             Transform programs so that formals are not modified
  ==========================================================================================================*)

let eliminate_modified_formals prog =
  Prog.map_procs (fun ({Proc.id; formals; locals; cfg; entry} as proc) ->
    let ms =
      CFG.fold_edges (fun _ ms -> ms) (fun (_,c,_) ms ->
        match c with
        | C.Inst({I.desc= (I.Load(v,_) | I.Alloc(v,_) | I.Move(v,_) | I.Cast(v,_,_))}) ->
            Vars.add v ms
        | C.Inst({I.desc= I.Kill(vs)}) ->
            Vars.union vs ms
        | C.Inst({I.desc= (I.Store _ | I.Free _ | I.Assume _ | I.Assert _  | I.Nop | I.Generic _)})
        | C.Call _
        | C.ICall _ ->
            ms
      ) cfg (K.id entry) Vars.empty
    in
    let formals, moves, locals =
      List.fold_right (fun f (fs, moves, locals) ->
        if Vars.mem f ms then
          let f' = Var.gensym (Var.name f) (Var.sort f) in
          let mv = I.mk (I.Move(f, E.mkVar f')) (K.pos entry) in
          (f' :: fs, mv :: moves, Vars.add f locals)
        else
          (f :: fs, moves, locals)
      ) formals ([], [], locals)
    in
    let prefix_blk blk entry =
      let rec prefix_blk_ blk entry =
        match blk with
        | inst :: (_::_ as blk) ->
            let vtx = CFG.add_vertex cfg (K.mk_label inst.I.pos id) in
            CFG.add_edge cfg vtx (C.Inst(inst)) entry ;
            prefix_blk_ blk vtx
        | [inst] ->
            let vtx = CFG.add_vertex cfg (K.mk_label ~sort:K.Entry inst.I.pos id) in
            CFG.add_edge cfg vtx (C.Inst(inst)) entry ;
            vtx
        | [] ->
            entry
      in
      let entry' = prefix_blk_ blk entry in
      if blk <> [] && K.sort entry <> None then
        ignore( CFG.relabel_vertex cfg entry (K.set_sort (CFG.label_of entry) None) );
      entry'
    in
    let entry = prefix_blk moves entry
    in
    {proc with Proc.formals; locals; entry}
  ) prog


(*==========================================================================================================
                   Normalize programs wrt nop being the identity of sequential composition
  ==========================================================================================================*)

let concat_blocks g v0 =
  L.incf 10 "( concat_blocks" ; L.decf 10 ") concat_blocks" $>
  let visited = CFG.VertexISet.create ()
  in
  let collapse_pre_ok v w =
    match K.sort v                  , K.sort w with
    | _                             , None
    | Some(K.Entry | K.Cut | K.Join), Some(K.Join | K.Fork)
    | Some(K.Return)                , Some(K.Fork)                    -> true

    | Some(K.Return)                , Some(K.Join)
    | _                             , Some(K.Exit | K.Return | K.Cut)
    | None                          , Some(K.Join | K.Fork)           -> false

    | Some(K.Fork)                  , _                               -> failwith "only called when v has 1 succ"

    | Some(K.Exit)                  , _
    | _                             , Some(K.Entry)                   -> failwith "concat_blocks: malformed CFG"
  in
  let collapse_post_ok v w =
    match K.sort v                    , K.sort w with
    | None                            , _
    | Some(K.Join | K.Fork)           , Some(K.Exit | K.Cut | K.Join)
    | Some(K.Fork)                    , Some(K.Return | K.Fork)       -> true

    | Some(K.Join)                    , Some(K.Return | K.Fork)
    | Some(K.Entry | K.Return | K.Cut), _
    | Some(K.Join | K.Fork)           , None                          -> false

    | Some(K.Exit)                    , _
    | _                               , Some(K.Entry)                 -> failwith "concat_blocks: malformed CFG"
  in
  let concat_ok w =
    match K.sort w with
    | None                            -> true
    | Some(K.Return | K.Cut | K.Join) -> false
    | Some(K.Fork)                    -> failwith "only called when w has 1 succ"
    | Some(K.Entry | K.Exit)          -> failwith "concat_blocks: malformed CFG"
  in
  let rec start v =
    L.printf 10 "start: %a" CFG.Vertex.fmt v ;
    if not (CFG.VertexISet.mem visited v) then
      match CFG.successors v with
      | [(w, (C.Inst({I.desc= I.Nop}) as c))] when collapse_pre_ok v w ->
          CFG.collapse_edge_pre g v c w ;
          start v
      | succs ->
          CFG.VertexISet.add visited v ;
          List.iter (fun (w,c) -> continue v c w) succs
  and continue v c w =
    L.printf 10 "continue: @[%a@]@ @[%a@]@ @[%a@]" CFG.Vertex.fmt v C.fmt c CFG.Vertex.fmt w ;
    match CFG.successors w with
    | [] when collapse_post_ok v w ->
        (match c with
        | C.Inst({I.desc= I.Nop}) ->
            CFG.collapse_edge_post g v c w
        | _ -> ()
        )
    | [(x, (C.Inst({I.desc= I.Nop}) as d))] when collapse_post_ok w x ->
        CFG.collapse_edge_post g w d x ;
        continue v c x
    | [(x, d)] when concat_ok w ->
        (match C.append c d with
        | Some(cd) ->
            L.printf 10 "@[appending@ @[%a@]@ to @[%a@]@]" C.fmt c C.fmt d ;
            CFG.add_edge g v cd x ;
            CFG.remove_edge g v c w ;
            CFG.remove_edge g w d x ;
            CFG.remove_vertex g w ;
            continue v cd x
        | None ->
            start w
        )
    | _ ->
        start w
  in
  start v0


(*==========================================================================================================
            Normalize programs so that control point sorts accurately reflect flow graph structure
  ==========================================================================================================*)

let fmt_nbrs nbrs ff v =
  (List.fmt ";@ " CFG.Vertex.fmt) ff (List.map fst (nbrs v))

let set_sorts ({Proc.cfg; entry; exit} as proc) =
  let set_sort v sort =
    if K.sort v = sort then
        v
    else
        CFG.relabel_vertex cfg v (K.set_sort (CFG.label_of v) sort)
  in
  let cs = CFG.cutpoints entry
  in
  assert(
       (not (CFG.VertexSet.mem entry cs)
        || L.warnf "entry point a cutpoint")
    && (CFG.in_degree entry = 0
        || L.warnf "entry point %a@ has predecessors:@ %a" CFG.Vertex.fmt entry (fmt_nbrs CFG.predecessors) entry)
    && (not (CFG.VertexSet.mem exit cs)
        || L.warnf "exit point a cutpoint")
    && (CFG.out_degree exit = 0
        || L.warnf "exit point %a@ has successors:@ %a" CFG.Vertex.fmt exit (fmt_nbrs CFG.successors) exit)
  );
  let entry = set_sort entry (Some(K.Entry)) in
  assert( CFG.in_degree entry = 0 || L.warnf "entry point has predecessors: %a" CFG.Vertex.fmt entry )
  ;
  let exit = set_sort exit (Some(K.Exit)) in
  assert( CFG.out_degree exit = 0 || L.warnf "exit point has successors: %a" CFG.Vertex.fmt exit )
  ;
  let cs = CFG.cutpoints entry
  in
  CFG.iter_vertices (fun v ->
    if not (CFG.Vertex.equal exit v) then
    let in_degree = CFG.in_degree v in
    let out_degree = CFG.out_degree v in
    if CFG.VertexSet.mem v cs then ignore @@ set_sort v (Some(K.Cut))
    else if in_degree > 1 then ignore @@ set_sort v (Some(K.Join))
    else if out_degree > 1 then ignore @@ set_sort v (Some(K.Fork))
    else if in_degree = 0 then assert( CFG.Vertex.equal entry v || L.warnf "entry point not unique" )
    else ignore @@ set_sort v None
  ) cfg
  ;
  CFG.iter_edges (fun _ -> ()) (fun (_,c,v) ->
    match c with
    | C.Inst _ ->
        assert( (K.sort v <> Some(K.Return) || L.warnf "return point not preceded by call") )
    | C.Call _
    | C.ICall _ ->
        assert( (not (CFG.VertexSet.mem v cs) || L.warnf "return point a cutpoint")
             && (CFG.in_degree v = 1 || L.warnf "return point has multiple predecessors: %a" CFG.Vertex.fmt v)
             && (CFG.out_degree v = 1 || L.warnf "return point has multiple successors: %a" CFG.Vertex.fmt v) );
        ignore( set_sort v (Some(K.Return)) )
  ) cfg (CFG.index_of entry)
  ;
  {proc with Proc.entry; exit}


(*=========================================================================================================
                                     Compute callee's for each call-site
  =========================================================================================================*)

let compute_call_targets prog =
  (* Really should calculate using a may-alias analysis, for now use types *)
  let open Cmnd in
  let open Proc in
  let open Prog in
  Prog.iter_procs (fun _ {Proc.entry; cfg} ->
    CFG.iter_edges
      (fun _ -> ())
      (fun (c,e,n) ->
         match e with
         | Inst _ ->
             ()
         | Call{Call.proc; targets} ->
             assert( match targets with [target] -> Proc.Id.equal target proc | _ -> false );
         | ICall({Call.typ; targets= targets0} as call) ->
             let targets =
               if not Config.optimize_icall_targets then
                   prog.addr_taken
               else
                   List.filter (fun pid ->
                     let p = Proc.IdHMap.find prog.procs pid in
                     (*Format.printf "%a =?= %a@\n" Typ.fmt p.fty Typ.fmt ic.typ;*)
                     Typ.equal (Typ.mkPointer p.fty) typ
                   ) prog.addr_taken in
             if not (List.equal Proc.Id.equal targets targets0) then (
                 CFG.remove_edge cfg c e n ;
                 CFG.add_edge cfg c (ICall{call with Call.targets}) n ;
             )
      ) cfg (K.id entry)
  ) prog ;
  prog


(*==========================================================================================================
                                   Compute modified and accessed variables
  ==========================================================================================================*)

module PMM = MultiMap.Make (Proc.Id) (Vars)

(* MJP: Quite inefficient, could calc each procedure, and then use
   transitive call graph to get the rest *)
let compute_variables prog f =
  let {Prog.globals} = prog in
  (* initial base to calculate the direct values *)
  let m =
    Prog.fold_procs (fun {Proc.id; cfg; entry} ->
      CFG.fold_edges (fun _ m -> m) (fun (_,tr,_) m ->
        PMM.union id (f tr) m
      ) cfg (K.id entry)
    ) prog PMM.empty
  in
  (*  Keep pushing around the call graph, should be cleverer here *)
  let rec loop m =
    let m' =
      Prog.fold_procs (fun {Proc.id; cfg; entry} m ->
        CFG.fold_edges (fun _ m -> m) (fun (_,c,_) m ->
          match c with
          | C.Inst _ ->
              m
          | C.Call{Call.targets}
          | C.ICall{Call.targets} ->
              assert( targets <> []
                      || failwith "compute_call_targets must precede compute_variables" );
              List.fold (fun target m ->
                PMM.union id (Vars.inter globals (PMM.find target m)) m
              ) targets m
        ) cfg (K.id entry) m
      ) prog m in
    if PMM.length m = PMM.length m' then m else loop m'
  in
  loop m


let compute_modified_variables ({Prog.procs; globals} as prog) =
  let m = compute_variables prog C.mv in
  let procs =
    Proc.IdHMap.mapi (fun pid proc ->
      {proc with Proc.modifs= Vars.inter globals (PMM.find pid m)}
    ) procs
  in
  {prog with Prog.procs}

let compute_accessed_variables ({Prog.procs; globals} as prog) =
  let m = compute_variables prog C.fv in
  let procs =
    Proc.IdHMap.mapi (fun pid proc ->
      {proc with Proc.accessed= Vars.inter globals (PMM.find pid m)}
    ) procs
  in
  {prog with Prog.procs}

let compute_local_variables ({Prog.procs; globals} as prog) =
  let m = compute_variables prog C.fv in
  let procs =
    Proc.IdHMap.mapi (fun pid ({Proc.formals; freturn} as proc) ->
      let nonlocals = List.fold Vars.add formals (Option.fold Vars.add freturn globals) in
      {proc with Proc.locals= Vars.diff (PMM.find pid m) nonlocals}
    ) procs
  in
  {prog with Prog.procs}



(*==========================================================================================================
                                              Normalize programs
  ==========================================================================================================*)

let normalize_cfg prog =
  Prog.map_procs (fun ({Proc.entry; exit; cfg} as proc) ->
    CFG.root_vertex cfg entry ;
    CFG.root_vertex cfg exit ;
    CFG.remove_unreachable cfg ;
    let ({Proc.entry; cfg} as proc) = set_sorts proc in
    concat_blocks cfg entry ;
    proc
  ) prog

let normalize prog =
    L.incf 1 "( TransformProgram.normalize" ;
    Timer.start normalize_tmr ;
  (fun _ ->
    Timer.stop normalize_tmr ;
    L.decf 1 ") TransformProgram.normalize"
  )<& let()=()in
  prog |>
  compute_call_targets |>
  (* compute_accessed_variables depends on call targets having been computed *)
  compute_accessed_variables |>
  (* remove_unused_globals depends on accessed variables having been computed, and global_setup not yet inlined *)
  RemoveUnusedGlobals.remove_unused_globals |>
  normalize_cfg |>
  (* inline depends on control point sorts having been set *)
  Inline.prog |>
  (* fewer variables are formals after inlining *)
  eliminate_modified_formals |>
  Livevars.liveness_prog |>
  normalize_cfg |>
  compute_local_variables |>
  compute_modified_variables |>
  compute_accessed_variables |>
  id
