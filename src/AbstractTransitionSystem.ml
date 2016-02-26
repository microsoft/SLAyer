(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Abstract transition system abstract domains *)

(**/**)
open Library

open Variable
module S = Substitution
open Program
module I = Inst
module K = ControlPoint
module C = Cmnd
open Interproc_sig

module Positions = Set.Make(Position)

module L = (val Log.std Config.vATS : Log.LOG)
module LSE = (val Log.std Config.vSE : Log.LOG)
(**/**)


(*============================================================================
                                   Heights
  ============================================================================*)

(* PS#207: heights should be a map from entry * pt to int *)
module Heights = HashMap.Make(ControlPoint)


module Domain
  (* states / vertex labels of the abstract transition system *)
  (RD: RELATION_DOMAIN) =
struct

  (*============================================================================
              Transition Relation from Relation Construction
    ============================================================================*)

  module RD = struct
    type r = RD.r

    type pred = RD.pred

    type t = { rel: RD.t; blk: I.t list; }

    let create = RD.create

    let inject x = {rel= RD.inject x; blk= []}

    let project x = RD.project x.rel

    let adapted_pre_substate_call r cxt pre call pcall =
      match RD.adapted_pre_substate_call r cxt pre.rel call.rel pcall with
      | Some(post_rel_to_retn_rel) -> Some(fun post -> {post with rel= post_rel_to_retn_rel post.rel})
      | None -> None

    let call_to_entry r call pcall =
      let entry, post_rel_to_retn_rel = RD.call_to_entry r call.rel pcall in
      ({rel= entry; blk= []}, (fun post -> {post with rel= post_rel_to_retn_rel post.rel}))

    let exit_to_retn callee exit = {exit with rel= RD.exit_to_retn callee exit.rel}

    let resolve_indirect_call r call fptr ftyp =
      RD.resolve_indirect_call r call.rel fptr ftyp

    let error = {rel= RD.error; blk= []}

    let tt = {rel= RD.tt; blk= []}

    let is_error x = RD.is_error x.rel

    let is_false x = RD.is_false x.rel

    let exec_inst cxt i x =
      let rel = RD.exec_inst cxt i x.rel in
      let blk = match i with {I.desc= I.Nop} -> x.blk | _ -> i :: x.blk in
      {rel; blk}

    let below x y = RD.below x.rel y.rel

    let join x y = {rel= RD.join x.rel y.rel; blk= []}

    let generalize x =
      let rel, jnk = RD.generalize x.rel in
      ({x with rel}, jnk)

    let compare x y =
      let o = RD.compare x.rel y.rel in if o <> 0 then o else
              List.compare I.compare x.blk y.blk

    let equal x y = RD.equal x.rel y.rel && List.equal I.equal x.blk y.blk

    let equal_entry x y = RD.equal_entry x.rel y.rel

    let fmt ff {rel} = RD.fmt ff rel

    let fmt_entry ff x = RD.fmt_entry ff x.rel

    let fmt_pre ff (x,p) = RD.fmt_pre ff (x.rel,p)

    let fmt_reln ff {rel; blk} =
      Format.fprintf ff "@[<hv>%a@ @[<hv>%a@]@ %a@]"
        RD.fmt_entry rel (List.fmt ";@ " I.fmt) (List.rev blk) RD.fmt rel
  end


(*============================================================================
                       Transition / Edge Labels
  ============================================================================*)

  (* transitions / edge labels of the abstract transition system *)
  module Tr = struct

    type t =
      | Intra of K.t * I.t list * K.t * bool      (** start and end control points, block between, leak *)
      | Call of Proc.t Call.t
      | Return
      | Summary


    let append x y =
      match x, y with
      | Intra(k0,b0,k1,false), Intra(k1',b1,k2,false) when K.equal k1 k1' ->
          Some(Intra(k0, List.append b0 b1, k2, false))
      | _ ->
          None

    let equal x y =
      match x, y with
      | Intra(k0,b0,k0',l0), Intra(k1,b1,k1',l1) ->
          K.equal k0 k1 && K.equal k0' k1' && List.equal I.equal b0 b1 && (l0 = l1)
      | Call(c0), Call(c1) ->
          Call.equal Proc.equal c0 c1
      | _ ->
          Pervasives.( = ) x y

    let compare x y =
      match x, y with
      | Intra(k0,b0,k0',l0), Intra(k1,b1,k1',l1) ->
          let o = K.compare k0  k1  in if o <> 0 then o else
          let o = K.compare k0' k1' in if o <> 0 then o else
          let o = List.compare I.compare b0 b1 in if o <> 0 then o else
                  Pervasives.compare l0 l1
      | Call(c0), Call(c1) ->
          Call.compare Proc.compare c0 c1
      | _ ->
          Pervasives.compare x y

    let fmt ff tr =
      let fmt_blk ff blk =
        Format.fprintf ff "@[<hv>%a;@]" (List.fmt ";@ " I.fmt) blk
      in
      let fmt_blk_pos ff blk =
        let fmt_pos_inst ff {I.desc; pos} =
          Format.fprintf ff "@[<hov 2>%a:@ %a@]" Position.fmt pos I.fmt_desc desc in
        Format.fprintf ff "@[<v>%a;@]" (List.fmt ";@\n" fmt_pos_inst) blk
      in
      let fmt_assumes ff blk =
        fmt_blk ff (List.filter (fun {I.desc} -> match desc with I.Assume _ -> true | _ -> false) blk)
      in
      let aux leak b =
        match !Config.vTr with
        | 0 -> Format.fprintf ff "@[<hv>%( fmt %)@]" leak
        | 1 -> Format.fprintf ff "@[<hv>%( fmt %)%a@]" leak fmt_assumes b
        | 2 -> Format.fprintf ff "@[<hv>%( fmt %)%a@]" leak fmt_blk b
        | _ -> Format.fprintf ff "@[<hv>%( fmt %)%a@]" leak fmt_blk_pos b
      in
      match tr with
      | Intra(_,b,_,false) -> aux "" b
      | Intra(_,b,_,true) -> aux "LEAK@ " b
      | Call({Call.proc= {Proc.id; freturn}; areturn} as call) ->
          let fmt_ret ff =
            match freturn, areturn with
            | Some(freturn), Some(areturn) -> Format.fprintf ff "[%a/%a] =@ " Var.fmt areturn Var.fmt freturn
            | Some(freturn), None          -> Format.fprintf ff "[_/%a] =@ " Var.fmt freturn
            | None         , Some(areturn) -> Format.fprintf ff "[%a/_] =@ " Var.fmt areturn
            | _ -> ()
          in
          let frmls_to_actls,_ = Call.args {call with Call.areturn= None} in
          Format.fprintf ff "@[<hov 2>%t@,@[%a(@[%a@])@]@]" fmt_ret Proc.Id.fmt id S.fmt frmls_to_actls
      | Return -> Format.fprintf ff "return"
      | Summary -> Format.fprintf ff "summary"

  end


(*============================================================================
                              States / Vertices
  ============================================================================*)

  module Idx = struct
    include K

    let fmt ff k =
      Format.fprintf ff "%a: %a: %a: "
        Id.fmt (K.id k) Position.fmt (K.pos k) (Option.fmt "" K.fmt_sort) (K.sort k)
  end


  (** abstract transition system *)
  module ATS = struct
    include Graph.Make (Idx) (RD) (Tr)


    let concat_blocks g v0 =
      L.incf 10 "( concat_blocks" ; L.decf 10 ") concat_blocks" $>
      let visited = VertexISet.create ()
      in
      let collapse_pre_ok v w =
        match Idx.sort (index_of v)     , Idx.sort (index_of w) with
        | _                             , None
        | Some(K.Entry | K.Cut | K.Join), Some(K.Join | K.Fork)
        | Some(K.Return)                , Some(K.Fork)                    -> not (RD.is_error (label_of w))

        | Some(K.Return)                , Some(K.Join)
        | _                             , Some(K.Exit | K.Return | K.Cut)
        | None                          , Some(K.Join | K.Fork)
        | Some(K.Exit)                  , _
        | _                             , Some(K.Entry)                   -> false

        | Some(K.Fork)                  , _                               -> failwith "only called when one succ"
      in
      let collapse_post_ok v w =
        match Idx.sort (index_of v)       , Idx.sort (index_of w) with
        | None                            , _
        | Some(K.Join | K.Fork)           , Some(K.Exit | K.Cut | K.Join)
        | Some(K.Fork)                    , Some(K.Return | K.Fork)       -> not (RD.is_error (label_of w))

        | Some(K.Join)                    , Some(K.Return | K.Fork)
        | Some(K.Entry | K.Return | K.Cut), _
        | Some(K.Join | K.Fork)           , None
        | Some(K.Exit)                    , _
        | _                               , Some(K.Entry)                 -> false
      in
      let concat_ok v w =
           Idx.sort (index_of v) = None
        && not (RD.is_error (label_of w))
      in
      let rec start v =
        L.printf 10 "start: %a" Vertex.fmt v ;
        if not (VertexISet.mem visited v) then
          match successors v with
          | [(w, (Tr.Intra(_,[],_,false) as e))] when collapse_pre_ok v w ->
              collapse_edge_pre g v e w ;
              start v
          | succs ->
              VertexISet.add visited v ;
              List.iter (fun (w,e) -> continue v e w) succs
      and continue v e w =
        L.printf 10 "continue: @[%a@]@ @[%a@]@ @[%a@]" Vertex.fmt v Tr.fmt e Vertex.fmt w ;
        match successors w with
        | [] when collapse_post_ok v w ->
            (match e with
            | Tr.Intra(_,[],_,false) ->
                collapse_edge_post g v e w
            | _ -> ()
            )
        | [(x, (Tr.Intra(_,[],_,false) as f))] when collapse_post_ok w x ->
            collapse_edge_post g w f x ;
            continue v e x
        | [(x, f)] when concat_ok w x ->
            (match Tr.append e f with
            | Some(ef) ->
                L.printf 10 "@[appending@ @[%a@]@ to @[%a@]@]" Tr.fmt e Tr.fmt f ;
                add_edge g v ef x ;
                remove_edge g v e w ;
                remove_vertex g w ;
                continue v ef x
            | None ->
                start w
            )
        | _ ->
            start w
    in
    start v0

  end


  module Vtx = struct
    include ATS.Vertex

    type d_cp = RD.t

    let cp v = ATS.index_of v
    let d v = ATS.label_of v
    let project v = (d v, cp v)

    let fmt ff v = Format.fprintf ff "%a@ %a" Idx.fmt (cp v) RD.fmt (d v)
  end


(*============================================================================
                            Interprocedural Domain
  ============================================================================*)

  module I_D_cp = Vtx

  type i_d_cp = Vtx.t
  type d_bk = Vtx.t * (Vtx.d_cp * K.t)


  type r = {
    prog: Prog.t;
    ats: ATS.graph;
    heights: int Heights.t;
    mutable hit_limit: bool;
    d: RD.r;
  }

  let ats {ats} = ats

  let states_for {ats} k =
    ATS.vertices_for ats k

  let errors {ats} =
    ATS.fold_vertices (fun v errs ->
      if RD.is_error (ATS.label_of v) then
          v :: errs
      else
          errs
    ) ats []

  let leaks {prog; ats} =
    let {Prog.main; procs} = prog in
    let {Proc.entry} = Proc.IdHMap.find procs main in
    ATS.fold_edges (fun _ z -> z) (fun (v,e,_) leaks ->
      match e with
      | Tr.Intra(_,_,_,true) -> v :: leaks
      | _ -> leaks
    ) ats entry []

  let hit_limit r = r.hit_limit

  let dead {prog; ats} =
    let add {Proc.cfg; entry} unreachable =
      CFG.fold_edges (fun _ u -> u) (fun (_,c,_) unreached ->
        match c with
        | C.Inst({I.pos}) ->
            Positions.add (pos) unreached
        | C.Call _ | C.ICall _ ->
            unreached
      ) cfg (CFG.index_of entry) unreachable
    in
    let rem_edg (_,tr,_) unreached =
      match tr with
      | Tr.Intra(k, blk, k', _) ->
          unreached |>
          Positions.remove (K.pos k) |>
          List.fold (fun {I.pos} unreached ->
            Positions.remove (pos) unreached
          ) blk |>
          Positions.remove (K.pos k')
      | Tr.Call _ | Tr.Return | Tr.Summary ->
          unreached
    in
    let rem_dominated {Proc.cfg; entry} unreachable =
      let _,_,_, dominated_by = CFG.dominance_frontier cfg entry in
      CFG.fold_vertices (fun h unreachable ->
        if not (Positions.mem (K.pos h) unreachable) then unreachable
        else
        CFG.VertexISet.fold (fun k unreachable ->
          if Position.equal (K.pos h) (K.pos k) then unreachable
          else
          Positions.remove (K.pos k) unreachable
        ) (dominated_by h) unreachable
      ) cfg unreachable
    in
    Positions.empty |>
    Prog.fold_procs add prog |>
    ATS.fold_edges (fun _ u -> u) rem_edg ats (ATS.index_of (List.hd (ATS.roots ats))) |>
    Prog.fold_procs rem_dominated prog |>
    Positions.to_list

  let write_ats name prog ats entry =
    let open Prog in let open Proc in
    let fn k =
      Option.map (fun p -> (Id.name p.id, fun ff -> Proc.fmt ff p)) (IdHMap.tryfind prog.procs (K.proc k))
    in
    Library.with_out (name^".ats.dot") (ATS.write_dot ats entry) ;
    Library.with_out (name^".part.ats.dot") (ATS.write_dot_partitioned fn ats [entry])

  let create prog =
    let {Prog.main; procs} = prog in
    let {Proc.entry} = Proc.IdHMap.find procs main in
    let r = {
      prog;
      ats= ATS.create ();
      heights= Heights.create 31;
      hit_limit= false;
      d= RD.create prog;
    } in
    Pervasives.at_exit (fun()->
      if Config.write_ats then
        write_ats Config.testname prog r.ats entry
    );
    r


(*============================================================================
                         Transition System Operations
  ============================================================================*)

  let add_vertex_for_state r cp d =
    (fun v ->
       L.printf 2 "@\n@[<hov 2>adding state: %a@]" Vtx.fmt v ;
       assert( RD.equal_entry (Vtx.d v) d ))
    <&
    ATS.add_vertex r.ats (cp, {d with RD.blk= []})


  let add_transition_to_vertex r prev prev_to_call msg call =
    L.printf 2 "@\n@[<hv 2>adding transition:@\n@[%a@]@ @[<hov 2>from@ %a@]@ @[<hov 2>to@ %s@,%a@]@]"
      Tr.fmt prev_to_call Vtx.fmt prev msg Vtx.fmt call ;
    assert(
      match prev_to_call with
      | Tr.Call _ | Tr.Return -> true
      | _ -> RD.is_error (Vtx.d call) || RD.equal_entry (Vtx.d prev) (Vtx.d call)
    );
    ATS.add_edge r.ats prev prev_to_call call


(*============================================================================
                         Interproc Domain Operations
  ============================================================================*)

  let init r (init_d, init_cp) =
    let init = ATS.add_vertex r.ats (init_cp, init_d) in
    ATS.root_vertex r.ats init ;
    L.printf 2 "@[<hov 2>adding initial state %a@]" Vtx.fmt init ;
    init


  let now_covered r vtx = not (ATS.mem_vertex r.ats vtx)


  let remove_covered_by r new_vtx =
    let remove_if_covered old_vtx =
      let new_d = Vtx.d new_vtx in
      let old_d = Vtx.d old_vtx in
      if not (RD.equal old_d new_d) then
        if RD.below old_d new_d then (
          L.printf 2 "@\n@[<hv 2>removing %a@ now covered by@ %a@]" Vtx.fmt old_vtx RD.fmt new_d ;
          ATS.replace_vertex r.ats (fun x -> x) old_vtx new_vtx ;
          assert( RD.equal_entry (Vtx.d new_vtx) (Vtx.d old_vtx) )
        )
    in
    List.iter remove_if_covered
      (ATS.vertices_for r.ats (ATS.index_of new_vtx))

  let join msg r (prev, (tr, next_d, next_cp)) =
    if Config.join_powerset then
      let trg = add_vertex_for_state r next_cp next_d in
      if   (Config.join_reduce > 0 && (Some K.Cut) = (K.sort next_cp))
        || (Config.join_reduce > 1 && (Some K.Join) = (K.sort next_cp))
        || (Config.join_reduce > 2)
      then
        remove_covered_by r trg ;
      add_transition_to_vertex r prev tr msg trg ;
      trg
    else
      let trylookup_d r d =
        List.tryfind (fun v -> RD.equal_entry d (Vtx.d v))
          (ATS.vertices_for r.ats next_cp)
      in
      let trg =
        match trylookup_d r next_d with
        | None ->
            add_vertex_for_state r next_cp next_d
        | Some(old_vtx) ->
            let old_or_new = RD.join next_d (Vtx.d old_vtx) in
            let trg = add_vertex_for_state r next_cp old_or_new in
            ATS.replace_vertex r.ats (fun tr -> tr) old_vtx trg ;
            assert( RD.equal_entry (Vtx.d trg) (Vtx.d old_vtx) );
            trg
      in
      add_transition_to_vertex r prev tr msg trg ;
      trg


  let prev_to_join r (prev, (next_d, next_cp)) =
    join "join " r (prev, (Tr.Intra(Vtx.cp prev, List.rev next_d.RD.blk, next_cp, false), next_d, next_cp))


  type __covered = WasCoveredByOld | NowCoveredByNew


  let widen msg r (prev, (tr, next_d, next_cp)) =
    (* Note: this should be the below operation *)
    let covered_by r d =
      LSE.incf 5 "( covered_by: %a" RD.fmt d ;
      LSE.decf 5 ") covered_by: %a@\n" (Option.fmt "None" Vtx.fmt)
      <&
      let rec loop = function
        | [] -> None
        | v :: vs -> if RD.below d (Vtx.d v) then Some(v) else loop vs
      in
      loop (List.rev (ATS.vertices_for r.ats next_cp))
    in
    let generalize r d =
      let new_height =
        match Heights.tryfind r.heights next_cp with
        | Some(h) -> h + 1
        | None -> 1
      in
      Heights.add r.heights next_cp new_height
      ;
      if Config.limit <= 0 || new_height <= Config.limit then
        RD.generalize d
      else (
        r.hit_limit <- true ;
        LSE.printf 1 "ascending chain too long for %a@\n" K.fmt next_cp ;
        (RD.join d RD.tt, true)
      )
    in
    match covered_by r next_d with
    | Some(trg) ->
        add_transition_to_vertex r prev tr "covering " trg ;
        (WasCoveredByOld, trg)
    | None ->
        let abs_d, junk = generalize r next_d in
        let trg =
          if not junk then
            join msg r (prev, (tr, abs_d, next_cp))
          else
            let leak = join msg r (prev, (tr, next_d, next_cp)) in
            LSE.printf 1 "LEAK detected@\n@\n" ;
            let leak_to_abs = Tr.Intra(Vtx.cp leak, [], next_cp, true) in
            join msg r (leak, (leak_to_abs, abs_d, next_cp))
        in
        (NowCoveredByNew, trg)


  let prev_to_cut r (prev, (next_d, next_cp)) =
    widen "new " r (prev, (Tr.Intra(Vtx.cp prev, List.rev next_d.RD.blk, next_cp, false), next_d, next_cp))


  (* Note: shouldn't this call to join be made by the fixed-point comp? *)
  let post_to_retn r call post_d_to_retn_d (post, (post_d, retn_cp)) =
    let retn_d = post_d_to_retn_d post_d
    in
    (* add exit to return transition *)
    let retn =
      if Config.generalize_call_retn
      then snd (widen "return " r (post, (Tr.Return, retn_d, retn_cp)))
      else join "return " r (post, (Tr.Return, retn_d, retn_cp))
    in
    add_transition_to_vertex r call Tr.Summary "summary " retn
    ;
    retn


  let call_to_pre r call pcall pre =
    add_transition_to_vertex r call (Tr.Call(pcall)) "pre " pre


  let add_prev_to_call_to_pre r (prev, (call_d, call_cp)) pcall pre =
    (* add transition to call site *)
    let call =
      if K.equal (Vtx.cp prev) call_cp
      then prev
      else
        if Config.generalize_call_retn
        then snd (widen "call " r (prev, (Tr.Intra(Vtx.cp prev, List.rev call_d.RD.blk, call_cp, false), call_d, call_cp)))
        else join "call " r (prev, (Tr.Intra(Vtx.cp prev, List.rev call_d.RD.blk, call_cp, false), call_d, call_cp))
    in
    (* add call to pre transition *)
    call_to_pre r call pcall pre
    ;
    call


  (* Note: the signature of this is wrong somehow, call_d can be projected out
     of call, and hist and call_cp are not needed *)
  let adapted_pre_substate_call r cxt pre ((_, (call_d, _)) as prev_to_call) pcall =
    match RD.adapted_pre_substate_call r.d cxt (Vtx.d pre) call_d pcall with
    | None -> None
    | Some(post_d_to_retn_d) ->
        (* add transition to call site *)
        let call = add_prev_to_call_to_pre r prev_to_call pcall pre
        in
        Some(post_to_retn r call post_d_to_retn_d)


  let call_to_entry r ((_, (call_d, _)) as prev_to_call) ({Call.proc= callee} as pcall) =
    let entry, post_d_to_retn_d = RD.call_to_entry r.d call_d pcall
    in
    let entry = add_vertex_for_state r callee.Proc.entry entry
    in
    (* PS#208: here we should instead check if entry with the ghosts
       existentially quantified is covered *)
    remove_covered_by r entry
    ;
    (* add transition to call site *)
    let call = add_prev_to_call_to_pre r prev_to_call pcall entry
    in
    (entry, post_to_retn r call post_d_to_retn_d)


  let exit_to_retn callee (prev, (exit_d, retn_cp)) =
    (prev, (RD.exit_to_retn callee exit_d, retn_cp))


  let resolve_indirect_call r (_, (call_d, _)) fptr ftyp =
    RD.resolve_indirect_call r.d call_d fptr ftyp


  let procedure_pres r fn =
    (* PS#134: this doesn't necessarily yield reverse order of addition *)
    ATS.vertices_for r.ats fn.Proc.entry


  (* ensure that pre doesn't get GCed if it gets disconnected *)
  let register_pre r pre = ATS.root_vertex r.ats pre

end
