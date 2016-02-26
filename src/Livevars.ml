(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Variable
open Expression
open SymbolicHeap
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint


(* remove the variables assigned to by c from rs, and extend rs with the variables read by c *)
let rec inst_trans i rs =
  let open Inst in
  match i with
  | Nop ->
      rs
  | Kill(vs) ->
      Vars.diff rs vs
  | Move(x,e) when Vars.mem x rs ->
      Vars.union (Exp.fv e) (Vars.remove x rs)
  | Move _ ->
      rs
  | Cast(x,_,e)
  | Load(x,e)
  | Alloc(x,e) ->
      Vars.union (Exp.fv e) (Vars.remove x rs)
  | Store(e,f) ->
      Vars.union (Vars.union (Exp.fv e) (Exp.fv f)) rs
  | Free(e)
  | Assert(e)
  | Assume(e) ->
      Vars.union (Exp.fv e) rs
  | Generic({ghosts; pre; insts; post}) ->
      let pre_vars = Vars.diff (XSH.fv pre) ghosts in
      let post_vars = Vars.diff (XSH.fv post) ghosts in
      Vars.union pre_vars
        (List.fold inst_trans insts
           (Vars.union post_vars rs))

(* remove the variables assigned to by ct from rs, and extend rs with the variables read by ct *)
let cmnd_trans globals procs c rs =
  let open Cmnd in
  let add_accessed pid =
    Vars.union (try (Proc.IdHMap.find procs pid).Proc.accessed with Not_found -> globals)
  in
  let cmnd_call cmnd_proc {Call.proc; actuals; areturn; targets} rs =
    rs |>
    cmnd_proc proc |>
    List.fold Vars.add actuals |>
    Option.fold (fun ret rs -> Vars.remove ret rs) areturn |>
    List.fold add_accessed targets
  in
  match c with
  | Inst{I.desc} -> inst_trans desc rs
  | Call(call) -> cmnd_call (fun _ rs -> rs) call rs
  | ICall(call) -> cmnd_call (fun fp rs -> Vars.union (Exp.fv fp) rs) call rs


let lift_trans f = fun e x y ->
  let y_new =
    match x with
    | None -> f e Vars.empty
    | Some x -> f e x in
  match y with
  | None ->
      Some y_new
  | Some y ->
      if Vars.subset y_new y then
        None
      else
        Some (Vars.union y_new y)

(* Need to make this walk bits that can't reach the end *)
let graph_pred_traverse_fixedpoint entry start start_val trans upd graph =
  let module V = CFG.VertexIMap in
  let module Q = Queue in
  let live = V.create () in
  let get_live n = V.tryfind live n in
  let workset = Q.create () in
  CFG.iter_vertices (fun x -> Q.push x workset) graph ;
  Q.push start workset ;
  V.add live start start_val ;
  while not (Q.is_empty workset) do
    let n = Q.pop workset in
    let v_post = get_live n in
    CFG.iter_preds (fun n' e ->
      let v_pre = get_live n' in
      match trans e v_post v_pre with
      | Some(v_new) ->
          V.add live n' v_new ;
          Q.push n' workset
      | None ->
          ()
    ) n ;
  done ;
  let get_live n = try V.find live n with Not_found -> Vars.empty in
  CFG.dfs_iter
    (fun prev ->
      CFG.iter_succs (fun curr label ->
        upd prev (get_live prev) label curr (get_live curr)
      ) prev
    )
    (fun _ -> ())
    [entry]

let insert_kill cfg prev cmnd curr died live =
    match cmnd with
    | C.Inst {I.desc= I.Move(x,_); pos} when Vars.mem x died ->
        (* Replace moves to variables that kill their lhs with nops *)
        CFG.remove_edge cfg prev cmnd curr ;
        let inst' = C.Inst (I.mk I.Nop pos) in
        CFG.add_edge cfg prev inst' curr

    | C.Inst {I.desc= I.Move(x,_); pos} when not (Vars.mem x live) ->
        (* Replace moves to dead variables with kills *)
        CFG.remove_edge cfg prev cmnd curr ;
        let inst' = C.Inst (I.mk (I.Kill (Vars.add x died)) pos) in
        CFG.add_edge cfg prev inst' curr

    | C.Inst {I.desc= I.Kill(kill); pos} ->
        (* If edge is a kill *)
        if Vars.subset died kill then
          (* Do nothing as the kill will already happen *)
          ()
        else (
          (* Update the kill to include the dead vars *)
          CFG.remove_edge cfg prev cmnd curr ;
          let inst' = C.Inst (I.mk (I.Kill (Vars.union died kill)) pos) in
          CFG.add_edge cfg prev inst' curr
        )
    | _ ->
        (* Perform a little look ahead *)
        let a = Vars.inters (List.map (fun (_,ct) -> C.mv ct) (CFG.successors curr)) in
        let died = Vars.diff died a in
        (* Otherwise split the edge into the original, and a kill *)
        if Vars.is_empty died then
          ()
        else
          (* splitting prev -> curr into prev -> fresh -> curr
             sort of fresh will be Return if sort of curr is, otherwise None
             in Return case, must relabel curr as fresh will be the return site *)
          let sort, curr =
            if K.sort curr = Some(K.Return) then
              let curr = CFG.relabel_vertex cfg curr (K.set_sort (CFG.label_of curr) None) in
              (Some(K.Return), curr)
            else
              (None, curr) in

          let fresh = CFG.add_vertex cfg (K.mk_label ?sort (K.pos curr) (K.proc curr)) in

          let kill = C.Inst (I.mk (I.Kill died) (K.pos prev)) in

          CFG.remove_edge cfg prev cmnd curr ;
          CFG.add_edge cfg prev cmnd fresh ;
          CFG.add_edge cfg fresh kill curr ;
          ()


let upd_edge cfg prev live_prev inst curr live_curr =
  let died = Vars.diff (Vars.union (C.mv inst) live_prev) live_curr in
  insert_kill cfg prev inst curr died live_curr

let liveness_proc globals procs _ {Proc.formals; freturn; cfg; entry; exit} =
  graph_pred_traverse_fixedpoint entry exit
    (Option.fold Vars.add freturn (List.fold Vars.add formals globals))
    (lift_trans (cmnd_trans globals procs))
    (upd_edge cfg)
    cfg


let liveness_prog ({Prog.globals; procs} as prog) =
  if Config.optimize_liveness then
    Prog.iter_procs (fun p -> liveness_proc globals procs p) prog ;
  prog
