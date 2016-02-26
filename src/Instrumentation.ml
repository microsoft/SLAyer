(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Generation of instrumented arithmetic programs from analysis results *)

(**/**)
open Library

module HC = HashCons
open Type
open Variable
open Expression
module E = Exp
module S = Substitution
open SymbolicHeap
open Program
module I = Inst
module K = ControlPoint
(**/**)

module L = (val Log.std Config.vInstr : Log.LOG)


let fmt_xs = Vars.fmt_embrace "@[<hov 2>? " " .@]@ "



(*============================================================================
         Convert SLAyer Expressions, Formulas, Commands to T2 syntax
  ============================================================================*)

let emit_pos buf {Position.dir; file; line} =
  Printf.bprintf buf "AT(%i,\"%s\")\t" line (Filename.concat dir file)

(* Note: Should not use Var.fmt or else it should check for verbosity *)
(* The problem is we print bad variable names (for T2) if verbose is set *)
let emit_var buf v = Printf.bprintf buf "%s" (Format.asprintf "%a" Var.fmt v)

(* Note: " *)
let emit_fld buf f = Printf.bprintf buf "%s" (Format.asprintf "%a" Fld.fmt f)

let isternary e =
  match E.desc e with
  | E.Op3(E.Ite,_,E.Num(1L),E.Num(0L)) -> true
  | _ -> false

let rec emit_exp buf e =
  emit_exp_ buf (E.desc e)

and emit_exp_ buf e =

  match e with

  (* (0 != (g ? 1 : 0)) is just g *)
  | E.Op1(E.Not,E.Eq({HC.desc=E.Num(0L)},e2)) when isternary(e2) ->
    (match E.desc e2 with
      | E.Op3(E.Ite,g,_,_) ->
        L.printf 3 "[0] Saving ternary expression";
        emit_exp_ buf g
      | _ -> failwith "emit_exp: ternary fail") (* should be impossible to reach *)
  (* ((g ? 1 : 0) = v) can be rewritten into g && v == 1 || !g && v == 0 *)
  | E.Eq(e1,e2) when isternary(e1) || isternary(e2) ->
      (match E.desc e1, E.desc e2 with
       | E.Op3(E.Ite,g1,_,_), E.Op3(E.Ite,g2,_,_) ->
         L.printf 3 "Saving ternary expression %a" E.fmt e1;
         emit_exp buf (E.mkOr [|E.mkAnd[|E.name g1; E.name g2|];
                                E.mkAnd[|E.mkNot (E.name g1); E.mkNot (E.name g2)|]|])
       | E.Op3(E.Ite,g,_,_), E.Var(_) ->
         L.printf 3 "Saving ternary expression %a" E.fmt e2;
         emit_exp buf (E.mkOr [|E.mkAnd[|E.name g;           E.mkEq e2 E.one |];
                                E.mkAnd[|E.mkNot (E.name g); E.mkEq e2 E.zero|]|])
       | E.Var(_), E.Op3(E.Ite,g,_,_) ->
         L.printf 3 "Saving ternary expression (%a = %a)" E.fmt e1 E.fmt e2;
         emit_exp buf (E.mkOr [|E.mkAnd[|E.name g;           E.mkEq e1 E.one |];
                                E.mkAnd[|E.mkNot (E.name g); E.mkEq e1 E.zero|]|])
       | E.Op3(E.Ite,_,_,_), e ->
         L.printf 3 "[0] Instrumentation: Dropped %a" E.fmt e1;
         Printf.bprintf buf "(nondet() == %a)" emit_exp_ e
       | e, E.Op3(E.Ite,_,_,_) ->
         L.printf 3 "[0] Instrumentation: Dropped %a" E.fmt e2;
         Printf.bprintf buf "(%a == nondet())" emit_exp_ e
       | _, _ ->
         failwith "emit_exp: ternary fail") (* should be impossible to reach *)

  | E.Var(v) -> emit_var buf v
  | E.Nil -> Printf.bprintf buf "0"
  | E.App(f,a) ->
      (match E.desc f with
      | E.Add(f) -> Printf.bprintf buf "(%a + %a)"  emit_exp a emit_fld f
      | E.Sub(f) -> Printf.bprintf buf "(%a - %a)"  emit_exp a emit_fld f
      | _ -> failwith "emit_exp: unexpected function"
      )
  | E.Add(_) | E.Sub(_) | E.Idx -> failwith "emit_exp: unexpected offset"
  | E.Bas(_) -> Printf.bprintf buf "0"
  | E.Eq(e,f) -> Printf.bprintf buf "(%a == %a)" emit_exp e emit_exp f
  | E.Num(i) -> Printf.bprintf buf "%Li" i
  | E.Str(s) -> Printf.bprintf buf "\"%s\"" s
  | E.Op1(E.Not,b) -> Printf.bprintf buf "(! %a)" emit_exp_ b
  | E.Op1(E.ZMin,v) -> Printf.bprintf buf "(- %a)" emit_exp_ v
  | E.Op2(E.ZLt,e,f) -> Printf.bprintf buf "(%a < %a)"  emit_exp_ e emit_exp_ f
  | E.Op2(E.ZLe,e,f) -> Printf.bprintf buf "(%a <= %a)" emit_exp_ e emit_exp_ f
  | E.Op2(E.ZGt,e,f) -> Printf.bprintf buf "(%a > %a)"  emit_exp_ e emit_exp_ f
  | E.Op2(E.ZGe,e,f) -> Printf.bprintf buf "(%a >= %a)" emit_exp_ e emit_exp_ f
  | E.Op2(E.ZDiv,e,f) -> Printf.bprintf buf "(%a / %a)"  emit_exp_ e emit_exp_ f
  | E.Op2(E.ZRem,e,f) -> Printf.bprintf buf "(%a %% %a)" emit_exp_ e emit_exp_ f
  | E.Op3(E.Ite,g,t,e) when E.is_boolean (E.name t) ->
      emit_exp buf (E.mkOr [|E.mkAnd[|E.name g; E.name t|];
                             E.mkAnd[|E.mkNot (E.name g); E.name e|]|])
  | E.OpN(E.Distinct,es) ->
     (match Array.to_list es with
      | [_] -> emit_exp buf E.tt
      | _ ->
      emit_exp buf
        (E.mkAnd
           (Array.of_list
              (List.fold_pairs (fun e f dqs ->
                E.mkDq (E.name e) (E.name f) :: dqs
               ) (Array.to_list es) []))))
  | E.OpN(E.And,bs) -> emit_expl " && " "(0<1)" buf
      (List.filter
        (fun b ->
         match b with
         | E.Var(v) when Var.sort v == Var.BooleanSort ->
           let _ = L.printf 3 "Dropping singleton bool var %a" Var.fmt v  in
           false
         | E.Op1(E.Allocd,_) as e ->
           let _ = L.printf 3 "Dropping singleton allocd() %a" E.fmt (E.name e) in
           false
         | _ -> true) (Array.to_list bs))
  | E.OpN(E.Or,bs) -> emit_expl " || " "(0<0)" buf (Array.to_list bs)
  | E.OpN(E.ZAdd,es)  -> emit_expl " + "  "0" buf (Array.to_list es)
  | E.OpN(E.ZMul,es)  -> emit_expl " * "  "1" buf (Array.to_list es)

  | E.Op1(E.Allocd,_)
  | E.Op2(E.ZMod,_,_)
  | E.Op3(E.Ite,_,_,_)
  | E.OpN(E.UFun _,_) ->
      L.printf 3 "Instrumentation: Dropped %a" E.fmt (E.name e) ;
      Printf.bprintf buf "nondet()"


and emit_expl sep unit buf = function
  | [] ->
      Printf.bprintf buf "%s" unit
  | [e] ->
      emit_exp_ buf e
  | e::el ->
      Printf.bprintf buf "(%a%a)"
        emit_exp_ e
        (fun buf el ->
          List.iter (fun e ->
            Printf.bprintf buf "%s%a" sep emit_exp_ e
          ) el
        ) el


let emit_kill buf pos xs =
  if Vars.is_empty xs then () else
  Printf.bprintf buf "%a%a\n"
    emit_pos pos
    (fun buf ->
      Vars.iter (fun v -> Printf.bprintf buf "%a := nondet(); " emit_var v)
    ) xs


let emit_move buf pos v e =
  match E.desc e with
  | E.Var(u) when Var.equal v u -> ()
  | _ -> Printf.bprintf buf "%a%a := %a;\n" emit_pos pos emit_var v emit_exp e


let emit_assume buf pos bexp =
  if E.equal E.tt bexp then () else
  Printf.bprintf buf "%aassume(%a);\n" emit_pos pos emit_exp bexp



(*============================================================================
                 Core instrumentation approximation and logic
  ============================================================================*)

module KH = HashMap.Make(K.Id)

let enclosing_proc = KH.create 128


module ExpMSet = MultiSet.Make(Exp)

(** [approximate us xsh] is a quantified boolean expression [(xs,b)] no
    stronger than [xsh] that can be usefully translated to T2. *)
let approximate us xsh =
  assert(true$>
    L.incf 2 "( approximate:@ %a" XSH.fmt xsh);
  (fun (xs,b) -> assert(true$>
    L.decf 2 ") approximate:@ %a%a" fmt_xs xs E.fmt b))
  <&
(*
  let rec aux locs q z =
    (* take boolean part of q *)
    let bex = SH.Pf.term q
    in
    let q = SH.Pf.empty q
    in
    (* convert lists to disjunctions based on 0,1,many length, convert to DNF,
       for each branch assert the allocated locs on that branch are
       distinct *)
    let locs = SH.PtS.fold (fun {Pt.loc} z -> ExpMSet.add loc z) q locs in
    let q = SH.PtS.empty q
    in
    match SH.LsS.trychoose q with
    | Some({Ls.len; arg={Args.fore; back}} as ls) ->
        let q = SH.LsS.remove ls q in
        let q_ls_emp = SH.Pf.star (Ls.empty_eqs ls) q in
        let q_ls_nemp = SH.Pf.star [E.mkGt len E.zero] q
        in
        let ls_empty = aux locs q_ls_emp []
        in
        let link_allocd link z =
          if link = [] then z else
          let locs =
            List.fold (fun (a,_,_) z -> ExpMSet.add a z) link locs in
          aux locs q_ls_nemp z
        in
           bex
        :: E.mkGe len E.zero            (* logically redundant *)
        :: E.mkOr[ E.mkAnd ls_empty ;
                   E.mkAnd (link_allocd fore (link_allocd back [])) ]
        :: z
    | None ->
    match SH.DjS.trychoose q with
    | Some(dj) ->
        let q = SH.DjS.remove dj q in
        let dj_conseqs =
          Dj.fold (fun dt bs -> E.mkAnd (aux locs (SH.star [dt] q) []) :: bs)
            dj []
        in
        bex :: E.mkOr dj_conseqs :: z
    | None ->
        let mk_distinct_dqs es z =
          ExpMSet.fold_pairs (fun e f dqs ->
            E.mkDq e f :: dqs
          ) es z
        in
        bex :: mk_distinct_dqs (ExpMSet.add E.nil locs) z
  in
*)
  let xs, sh = XSH.exists_bind us xsh in
  let mk_distinct_dqs es z =
    ExpMSet.fold_pairs (fun e f dqs ->
      E.mkDq e f :: dqs
    ) es z
  in
  (* only surface-level points-to *)
  let locs = SH.PtS.fold (fun {Pt.loc} z -> ExpMSet.add loc z) sh ExpMSet.empty in
  let _ = ExpMSet.iter (fun l -> L.printf 3 "locs: %a" E.fmt l) locs in
  let z = mk_distinct_dqs (ExpMSet.add E.nil locs) [] in
  let _ = L.printf 3 "z: %a" E.fmt (E.mkAnd (Array.of_list z)) in
(*   let b = E.mkAnd (aux ExpMSet.empty sh []) in *)
  let _ps, c, d = SH.pure_consequences sh in
  let b = E.mkAnd [|c; d; (E.mkAnd (Array.of_list z))|] in
  (xs, b)


let emit_moves buf pos xs_to_es =
  S.iter (fun x e ->
    match E.desc x with
    | E.Var(x) -> emit_move buf pos x e
    | _ -> L.printf 3 "Instrumentation: Dropped %a := %a" E.fmt x E.fmt e
  ) xs_to_es


let emit_transition buf pos p _blk_start blk blk_end r =
  L.incf 1 "( emit_transition:@ @[<hv>%a@ @[%a@]@ %a@]"
    XSH.fmt p (List.fmt ";@ " I.fmt) blk XSH.fmt r
  ;
  let exists_bind_subst vs xsh =
    let xs, sh = XSH.exists_bind Vars.empty xsh in
    let ws, xs_m_vs = Vars.inter_diff xs vs in
    let sh', ws', ws_to_ws', ws'_to_ws = SH.rename_vs ws sh in
    (Vars.union ws' xs_m_vs, ws', sh', ws_to_ws', ws'_to_ws)
  in
  let ms =
    List.fold (fun i ms ->
      match i.I.desc with
      | I.Load(v,_) | I.Alloc(v,_) | I.Move(v,_) | I.Cast(v,_,_) -> Vars.add v ms
      | I.Kill(vs) -> Vars.union vs ms
      | I.Store _ | I.Free _ | I.Assume _ | I.Assert _ -> ms
      | _ -> failwithf "Instrumentation: unimplemented command: %a" I.fmt i
    ) blk Vars.empty
  in
  let ms', ms'_to_ms =
    Vars.fold (fun v (fs,i) ->
      let sort = Var.sort v in
      let fresh = Var.gensym (Var.name v) sort in
      let i' = S.add (E.mkVar fresh) (E.mkVar v) i in
      (Vars.add fresh fs, i')
    ) ms (Vars.empty, S.empty)
  in
  let tr_for r =
    let xs, vs', r', vs_to_vs', _ = exists_bind_subst (XSH.fv p) r in
    let ys, ws', p', _, ws'_to_ws = exists_bind_subst (SH.fv r') p
    in
    let p' = SH.Pf.star (S.fold (fun m' m acc -> E.mkEq m' m :: acc) ms'_to_ms []) p'
    in
    let cxt = Vars.unions [xs; ys; ms']
    in
    let q =
      match
        List.fold_left (fun q i ->
          SymbolicExecution.exec_inst cxt i q
        ) (Some (SH.exists_intro Vars.empty p')) blk
      with
      | Some(q) ->
          if (K.sort blk_end) = Some K.Exit then
            (* exiting scope of locals, so quantify them *)
            let proc = KH.find enclosing_proc (K.id blk_end) in
            XSH.exists_intro proc.Proc.locals q
          else
            q
      | None -> failwithf "Instrumentation: symbolic execution failed"
    in
    let us, q = XSH.exists_bind cxt q
    in
    match Prover.entails q xs r' with
    | Some(tr) -> Some(xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, XSH.exists_intro us tr)
    | None -> None
  in
  let xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, tr =
    match tr_for r with
    | Some(xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, tr) -> (xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, tr)
    | None ->
        (* r need not have well-guarded quantifiers, so abstract to collect garbage and retry *)
        let r',_ = Abstraction.abstract r in
        match tr_for r' with
        | Some(xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, tr) -> (xs, vs', vs_to_vs', ws', ws'_to_ws, cxt, tr)
        | None ->
            L.printf 1 "Instrumentation: failed to construct transition relation" ;
            (Vars.empty, Vars.empty, S.empty, Vars.empty, S.empty, Vars.empty, XSH.emp)
  in
  let zs, tr_bx = approximate cxt (XSH.normalize tr)
  in
  let rs = E.fv tr_bx in
  let ws'_to_ws = S.restrict rs ws'_to_ws
  and ms'_to_ms = S.restrict rs ms'_to_ms
  and vs_to_vs' = S.restrict_rng rs vs_to_vs'
  and ms = Vars.inter ms rs
  and xs = Vars.inter xs rs
  and ms' = Vars.inter ms' rs
  and vs' = Vars.inter vs' rs
  and ws' = Vars.inter ws' rs
  in
  (* rename from vocabulary of precondition to vocabulary of transition relation *)
  emit_moves buf pos ws'_to_ws ;
  (* copy modified variables *)
  emit_moves buf pos ms'_to_ms ;
  (* kill modified variables, and existentials of transition relation formula *)
  emit_kill buf pos (Vars.unions [ms; xs; zs]) ;
  (* constrain values of modified and existential variables *)
  emit_assume buf pos tr_bx ;
  (* rename from vocabulary of transition relation to vocabulary of postcondition *)
  emit_moves buf pos vs_to_vs' ;
  (* kill temporary variables *)
  emit_kill buf pos (Vars.unions [vs'; ws'; ms'])
  ;
  L.decf 1 ") emit_transition:@\n@[<v>\
              ws'_to_ws: %a@ ms'_to_ms: %a@ xs: @[%a@]@ zs: @[%a@]@ tr: %a@ vs_to_vs': %a@]"
    S.fmt ws'_to_ws S.fmt ms'_to_ms Vars.fmt xs Vars.fmt zs E.fmt tr_bx S.fmt vs_to_vs'



(*============================================================================
       Convert SLAyer Abstract Transition Systems to T2 program syntax
  ============================================================================*)


module ID = Analysis.InterprocDomain
module Tr = ID.Tr
module ATS = ID.ATS

let emit_edge buf m error_state id0 p0 (v1, tr) =
  Printf.bprintf buf "FROM: %d;\n" id0 ;
  let r1, cp1 = ID.I_D_cp.project v1 in
  let xshT1 = ID.RD.project r1 in
  let id1 = ATS.VertexIMap.find m v1 in
  match xshT1 with
  | None ->
      Printf.bprintf buf "TO: %s;\n\n" error_state
  | Some(xsh1) ->
      (match tr with
      | Tr.Intra(blk_start, blk, blk_end, _) ->
          emit_transition buf (K.pos cp1) p0 blk_start blk blk_end xsh1
      | Tr.Call _
      | Tr.Return
      | Tr.Summary ->
          failwith "Unsupported: Instrumentation of non-inlined procedures"
      );
      Printf.bprintf buf "TO: %d;\n\n" id1


(** Print the arithmetic commands originating from the vertex [v0]. *)
let emit_vertex buf m error_state v0 id0 =
  let xshT0 = ID.RD.project (fst (ID.I_D_cp.project v0)) in
  L.printf 4 "[Instrumentation] emit_vertex %a" ATS.Vertex.fmt v0;
  match xshT0 with
  | None ->
      Printf.bprintf buf "FROM: %s;\nTO: %s;\n\n" error_state error_state
  | Some p0 ->
      List.iter
        (fun v_tr -> emit_edge buf m error_state id0 p0 v_tr)
        (ATS.successors v0)


(** Generate arithmetic program in T2's input format. *)
let write_arith_program {Prog.main; procs} ({Analysis.invariants} as results) buf =
  let ats = ID.ats invariants in
  let m = ATS.identify_vertices ats in
  (* Find start vertex *)
  let {Proc.entry} = Proc.IdHMap.find procs main in
  let start =
    match ATS.vertices_for ats entry with
    | [v] -> ATS.VertexIMap.find m v
    | _ -> failwith "abstract transition system must have a unique start"
  in
  (* Is this ATS a proof or a counterexample? *)
  let error_state =
    if Analysis.safe results then "ERROR"
    else string_of_int (ATS.VertexIMap.length m)
  in
  (* Output the program *)
  if not (Analysis.safe results) then (
    Printf.bprintf buf "// Counterexample error state: %s\n" error_state
  );
  Printf.bprintf buf "START: %d;\n\n" start;
  ATS.VertexIMap.iter (fun v id -> emit_vertex buf m error_state v id) m


let instrument program results =
  let {Prog.main; procs} = program in
  let main = Proc.IdHMap.find procs main in
  if not Config.instrument then ()
  else
  let instr_fname = if Analysis.safe results then (Config.testname^".t2") else (Config.testname^".cex.t2") in
  Prog.fold_proc (Some procs)
    (fun _ () -> ())
    (fun _ () -> ())
    (fun p k () -> KH.add enclosing_proc (K.id k) p)
    (fun _ _ () -> ())
    main () ;
  try
    Library.with_out instr_fname (write_arith_program program results)
  with exc ->
    prerr_endline ("\nInstrumentation Error: "^(Printexc.to_string exc))
