(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Generation of counter-example trace for sdvdefect viewer *)

open Library

open Variable
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint
module SHD = Analysis.SymbolicHeapsDomain
module ID = Analysis.InterprocDomain
module ATS = ID.ATS
module Tr = ID.Tr

module L = (val Log.std Config.vCEx : Log.LOG)


exception Unsupported


(*================================================================================================================
                                          Abstract Counter-Example Traces
  ================================================================================================================*)

let path_between shortest ats start error =
  let cmp0 x y =
    if shortest then x <= y else x >= y
  in
  let dist =
    let vtx_to_idx,_, dist,_ = ATS.fw ats in
    let to_idx vtx = ATS.VertexIMap.tryfind vtx_to_idx vtx |> Option.get in
    let error_idx = to_idx error in
    fun vtx ->
      dist.(to_idx vtx).(error_idx)
  in
  let rec walk cmp m vtx =
    if ATS.Vertex.equal error vtx then
        []
    else match
      ATS.fold_succs (fun succ edge dist_edge_succ ->
        let succ_dist = dist succ in
        match dist_edge_succ with
        | Some(dist,_,_) when cmp dist succ_dist ->
            dist_edge_succ
        | _ ->
            Some(succ_dist, edge, succ)
      ) vtx None
    with
    | Some(_, edge, succ) ->
        if ATS.VertexSet.mem succ m then
            (vtx, edge) :: walk ( <= ) m succ
        else
            (vtx, edge) :: walk cmp0 (ATS.VertexSet.add succ m) succ
    | None ->
        []
  in
  walk cmp0 (ATS.VertexSet.singleton start) start


let fmt_path ff (path, error) =
  Format.fprintf ff "%a@\n%a"
    (List.fmt "@\n" (fun ff (v,e) -> Format.fprintf ff "@[<v 4>%a@\n%a@\n@]" ATS.Vertex.fmt v Tr.fmt e)) path
    ATS.Vertex.fmt error


(*================================================================================================================
                                 Generation of Counter-Example Traces for sdvdefect
  ================================================================================================================*)

module SdvDefect = struct

  (* Some characters are interpreted specially by statestr2state in TraceTreeParser.cs:
     '^' is translated to '\n' and '_' is translated to ' '. *)
  let refmt fmt x =
    let src_buf = Buffer.create 1024 in
    let ff = Format.formatter_of_buffer src_buf in
    Format.pp_set_margin ff Config.margin ;
    fmt ff x ;
    Format.pp_print_flush ff () ;
    let dst_buf = Buffer.create 1024 in
    for i = 0 to Buffer.length src_buf - 1 do
      match Buffer.nth src_buf i with
      (* apply inverse of sdvdefect's translation when generating defect.tt files *)
      | '\n' -> Buffer.add_string dst_buf "^"
      | ' '  -> Buffer.add_string dst_buf "_"
      (* translate specially interpreted characters to avoid sdvdefect's translation *)
      | '^'  -> Buffer.add_string dst_buf "/\\"
      | '_'  -> Buffer.add_string dst_buf "."
      | char -> Buffer.add_char dst_buf char
    done ;
    Buffer.contents dst_buf


  (* sdvdefect generally does not accept empty/whitespace strings *)
  let fmt_str ff x =
    Format.pp_print_string ff (if x = "" then "?" else x)


  (* Regular expressions below are those from TraceTreeParser.cs used to parse defect.tt "InstructionType"s. *)

  (* "^(?<step>[0-9]+)[ \\t\"]+(?<file>[^\"]+)[ \\t\"]+(?<line>[0-9]+)[ \\t]+(?<isslice>[^ ]+)[ \\t]+(?<state>[^ ]+)[ \\t]+Call[ \t]+\"(?<caller>[^\"]+)\"+[ \\t]+\"(?<callee>[^\"]+)\"[ \\t]*" *)
  let write_call fmt_state ff (step, file, line, isslice, state, caller, callee) =
    Format.fprintf ff "%d %a %d %a %a Call \"%a\" \"%a\"@\n"
      step fmt_str file line fmt_str isslice fmt_str (refmt fmt_state state) fmt_str caller fmt_str callee

  (* "^(?<step>[0-9]+)[ \\t\"]+(?<file>[^\"]+)[ \\t\"]+(?<line>[0-9]+)[ \\t]+(?<isslice>[^ ]+)[ \\t]+(?<state>[^ ]+)[ \\t]+Return[ \\t]*" *)
  let write_return fmt_state ff (step, file, line, isslice, state) =
    Format.fprintf ff "%d %a %d %a %a Return@\n"
      step fmt_str file line fmt_str isslice fmt_str (refmt fmt_state state)

  (* "^(?<step>[0-9]+)[ \\t\"]+(?<file>[^\"]+)[ \\t\"]+(?<line>[0-9]+)[ \\t]+(?<isslice>[^ ]+)[ \\t]+(?<state>[^ ]+)[ \\t]+Atomic[ \\t]+(?<desc>[^ ]+)[ \\t]*" *)
  let write_atomic fmt_state ff (step, file, line, isslice, state, desc) =
    Format.fprintf ff "%d %a %d %a %a Atomic %a@\n"
      step fmt_str file line fmt_str isslice fmt_str (refmt fmt_state state) fmt_str desc

  (* "^Driver([ \\t\"]+)(?<driver>[^\"]+)[ \\t\"]*" *)
  let write_driver ff driver =
    Format.fprintf ff "Driver %a@\n"
      fmt_str driver

  (* "^Rule[ \\t]+(?<rule>[^ ]+)[ \\t]*" *)
  let write_rule ff rule =
    Format.fprintf ff "Rule %a@\n"
      fmt_str rule

  (* "^Error[ \\t]+(?<error>.* )" *)
  (* sdvdefect complains if there is more than one Error per defect file *)
  let _write_pattern ff error =
    Format.fprintf ff "Error %a@\n"
      fmt_str error

end


let write_defect_tt_path fmt ff path =
  let step =
    let step = ref 0 in
    fun () ->
      incr step ;
      !step
  in
  let write_blk vtx blk =
    let {I.pos= {Position.dir; file; line}} = List.hd blk in
    let fmt_blk ff blk = Format.fprintf ff "@[<hv>%a;@]" (List.fmt ";@ " I.fmt) blk in
    let fmt_state ff (vtx, blk) = Format.fprintf ff "@\n%a@\n@\n%a" fmt vtx fmt_blk blk in
    SdvDefect.write_atomic fmt_state ff (step(), dir^"\\"^file, line, "false", (vtx, blk), "")
  in
  let write_call vtx ({Call.proc= {Proc.id}} as call) =
    let _,k = ID.I_D_cp.project vtx in
    let {Position.dir; file; line} = K.pos k in
    let fmt_call ff call = Call.fmt (fun ff {Proc.id} -> Proc.Id.fmt ff id) ff call in
    let fmt_state ff (vtx, call) = Format.fprintf ff "@\n%a@\n%a" (Option.fmt "" fmt) vtx fmt_call call in
    let caller = Proc.Id.name (K.proc k) in
    let callee = Proc.Id.name id in
    SdvDefect.write_call fmt_state ff (step(), dir^"\\"^file, line, "false", (Some vtx, call), caller, callee)
  in
  let write_retn vtx =
    let _,k = ID.I_D_cp.project vtx in
    let {Position.dir; file; line} = K.pos k in
    let fmt_state ff vtx = Format.fprintf ff "@\n%a@\nreturn" (Option.fmt "" fmt) vtx in
    SdvDefect.write_return fmt_state ff (step(), dir^"\\"^file, line, "false", Some vtx)
  in
  let rec write_path = function
    | (vtx, edg) :: path ->
        (match edg with
        | Tr.Intra(_, [], _,_) -> ()
        | Tr.Intra(_, blk, _,_) -> write_blk vtx blk
        | Tr.Call(call) -> write_call vtx call
        | Tr.Return -> write_retn vtx
        | Tr.Summary -> ()
        );
        write_path path
    | [] ->
        ()
  in
  write_path path


let write_defect_tt paths buf =
  let c_syntax = !Config.c_syntax in
  Config.c_syntax := not c_syntax ;
  let ff = Format.formatter_of_buffer buf in
  SdvDefect.write_driver ff Config.testname ;
  SdvDefect.write_rule ff "Memory Safety" ;
  (* To include multiple traces in one file,
     add a bogus toplevel Call/Return and a bogus Call/Return around each trace. *)
  SdvDefect.write_call (fun _ () -> ()) ff (0, "", 0, "true", (), "", "Counter-examples") ;
  List.iter (fun (name, path, fmt) ->
    SdvDefect.write_call (fun _ () -> ()) ff (0, "", 0, "true", (), "", name) ;
    write_defect_tt_path fmt ff path ;
    SdvDefect.write_return (fun _ () -> ()) ff (0, "", 0, "true", ())
  ) paths ;
  SdvDefect.write_return (fun _ () -> ()) ff (0, "", 0, "true", ()) ;
  Config.c_syntax := c_syntax


(*================================================================================================================
                                    Abstract Counter-Example Transition Systems
  ================================================================================================================*)

let error_slice ats start_vtx error_vtx =
  let ats_to_slice, error_slice = ATS.slice start_vtx error_vtx ats in
  let start_vtx = Option.get (ATS.VertexMap.tryfind start_vtx ats_to_slice) in
  let error_vtx = Option.get (ATS.VertexMap.tryfind error_vtx ats_to_slice) in
  (error_slice, start_vtx, error_vtx)


let add_intermediate_states {Prog.globals; procs} ats start_vtx =
  let proc = Proc.IdHMap.tryfind procs (K.proc (snd (ID.I_D_cp.project start_vtx))) |> Option.get in
  let {Proc.id; formals; freturn; locals} = proc in
  let cfg = CFG.create () in
  let cxt = Vars.union globals (Vars.adds formals (Option.fold Vars.add freturn locals)) in
  ATS.iter_edges (fun _ -> ()) (fun (u,tr,w) ->
    match tr with
    | Tr.Intra(_, (_::_::_ as c), _, leak) ->
        let blks = List.divide (fun i j -> (not Config.tt_single_step) && I.(Position.equal i.pos j.pos)) c in
        let rec loop u p blks =
          match blks with
          | [] ->
              ()
          | [blk] ->
              let tr = Tr.Intra(ATS.index_of u, blk, ATS.index_of w, leak) in
              ATS.add_edge ats u tr w
          | blk :: blks ->
              let h = ATS.index_of u in
              let {I.pos} = List.hd blk in
              let k = CFG.add_vertex cfg (K.mk_label pos id) in
              let tr = Tr.Intra(h, blk, k, false) in
              let q = List.fold_left (fun q i -> ID.RD.exec_inst cxt i q) p blk in
              let v = ATS.add_vertex ats (k, q) in
              ATS.add_edge ats u tr v ;
              loop v q blks
        in
        ATS.remove_edge ats u tr w ;
        let p,_ = ID.I_D_cp.project u in
        loop u p blks
    | _ ->
        ()
  ) ats (ATS.index_of start_vtx)


let add_error_loops program ats start_vtx error_vtx =
  L.incf 10 "( add_error_loops" ; (fun _ -> L.decf 10 ") add_error_loops") <&
  let {Prog.procs} = program in
  let vtx_map, ats = ATS.copy ats start_vtx in
  let start_vtx = Option.get (ATS.VertexMap.tryfind start_vtx vtx_map) in
  let error_vtx = Option.get (ATS.VertexMap.tryfind error_vtx vtx_map) in
  ATS.iter_preds (fun pred_vtx _ ->
    L.printf 10 "error predecessor: %a" ATS.Vertex.fmt pred_vtx ;
    let pred_k = ATS.index_of pred_vtx in
    let pred_kid = K.id pred_k in
    let proc = K.proc pred_k in
    let {Proc.cfg} = Option.get (Proc.IdHMap.tryfind procs proc) in
    let cfg_scc_of = CFG.scc cfg in
    let pred_cfg_vtx = List.hd (CFG.vertices_for cfg pred_kid) in
    let pred_cfg_scc = CFG.VertexSet.of_list (Option.get (CFG.VertexMap.tryfind pred_k cfg_scc_of)) in
    let pred_cfg_scc_wo_pred = CFG.VertexSet.remove pred_cfg_vtx pred_cfg_scc in
    let cfg_to_ats = CFG.VertexMap.singleton pred_cfg_vtx pred_vtx in
    CFG.fold_edges
      (fun cfg_vtx cfg_to_ats ->
        if not (CFG.VertexSet.mem cfg_vtx pred_cfg_scc_wo_pred) then cfg_to_ats else
        let ats_vtx = ATS.add_vertex ats (cfg_vtx, ID.RD.inject SHD.tt) in
        CFG.VertexMap.add cfg_vtx ats_vtx cfg_to_ats
      )
      (fun (u,c,v) cfg_to_ats ->
        match CFG.VertexMap.tryfind u cfg_to_ats, c, CFG.VertexMap.tryfind v cfg_to_ats with
        | Some(u'), C.Inst(i), Some(v') ->
            let c' = Tr.Intra(u, [i], v, false) in
            ATS.add_edge ats u' c' v' ;
            cfg_to_ats
        | Some _, c, Some _ ->
            L.printf 1 "CounterExample: unexpected command: %a" C.fmt c ;
            raise Unsupported
        | _ ->
            cfg_to_ats
      ) cfg pred_kid cfg_to_ats
    |> ignore
  ) error_vtx ;
  ATS.concat_blocks ats start_vtx ;
  (ats, start_vtx, error_vtx)


(*================================================================================================================
                                         Counter-Example Generation Driver
  ================================================================================================================*)

let fmt_ats_state ff v =
  ID.RD.fmt ff (fst (ID.I_D_cp.project v))


let disprove results =
  L.incf 10 "( disprove" ; (fun _ -> L.decf 10 ") disprove") <&
  try
    let {Analysis.program; invariants} = results in
    let ats = ID.ats invariants in
    let start_vtx = List.hd (ATS.roots ats) in
    let traces =
      []
      |>
      List.fold_right (fun error_vtx traces ->
        L.incf 10 "( error: %a" ATS.Vertex.fmt error_vtx ; (fun _ -> L.decf 10 ")") <&
        let error_id = string_of_int (ATS.VertexIMap.find (ATS.identify_vertices ats) error_vtx) in

        let error_slice, slice_start_vtx, slice_error_vtx = error_slice ats start_vtx error_vtx in
        if Config.write_ats then
          ID.write_ats (Config.testname^".o.err"^error_id) program error_slice (ATS.index_of slice_start_vtx) ;

        add_intermediate_states program error_slice slice_start_vtx ;
        if Config.write_ats then
          ID.write_ats (Config.testname^".d.err"^error_id) program error_slice (ATS.index_of slice_start_vtx) ;

        let error_loops, loops_start_vtx, _ = add_error_loops program ats start_vtx error_vtx in
        if Config.write_ats then
          ID.write_ats (Config.testname^".l.err"^error_id) program error_loops (ATS.index_of loops_start_vtx) ;

        let shortest_path = path_between true error_slice slice_start_vtx slice_error_vtx in
        L.printf 1 "@\nshortest error path:@\n%a" fmt_path (shortest_path, error_vtx) ;
        let longest_path = path_between false error_slice slice_start_vtx slice_error_vtx in
        L.printf 1 "@\nlongest error path:@\n%a" fmt_path (longest_path, error_vtx) ;

        ("Shortest abstract path to Access Violation "^error_id, shortest_path, fmt_ats_state) ::
        ("Longest abstract path to Access Violation "^error_id, longest_path, fmt_ats_state) ::
        traces
      ) (Analysis.errors results)
      |>
      List.fold_right (fun leak_vtx traces ->
        L.incf 10 "( leak: %a" ATS.Vertex.fmt leak_vtx ; (fun _ -> L.decf 10 ")") <&
        let leak_id = string_of_int (ATS.VertexIMap.find (ATS.identify_vertices ats) leak_vtx) in

        let leak_slice, slice_start_vtx, slice_leak_vtx = error_slice ats start_vtx leak_vtx in
        if Config.write_ats then
          ID.write_ats (Config.testname^".o.leak"^leak_id) program leak_slice (ATS.index_of slice_start_vtx) ;

        add_intermediate_states program leak_slice slice_start_vtx ;
        if Config.write_ats then
          ID.write_ats (Config.testname^".d.leak"^leak_id) program leak_slice (ATS.index_of slice_start_vtx) ;

        let shortest_path = path_between true leak_slice slice_start_vtx slice_leak_vtx in
        L.printf 1 "@\nshortest leak path:@\n%a" fmt_path (shortest_path, leak_vtx) ;
        let longest_path = path_between false leak_slice slice_start_vtx slice_leak_vtx in
        L.printf 1 "@\nlongest leak path:@\n%a" fmt_path (longest_path, leak_vtx) ;

        ("Shortest abstract path to Leak "^leak_id, shortest_path, fmt_ats_state) ::
        ("Longest abstract path to Leak "^leak_id, longest_path, fmt_ats_state) ::
        traces
      ) (Analysis.leaks results)
    in
    if Config.write_tt then Library.with_out "defect.tt" (write_defect_tt traces) ;
    false
  with Unsupported ->
    L.printf 0 "CEx: Unsupported transition" ;
    false
