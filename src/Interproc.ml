(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Interprocedural abstract fixed-point computation *)

(**/**)
open Library

open Variable
module S = Substitution
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint

include Interproc_sig

module L = (val Log.raw Config.vSE : Log.LOG)


(* PS#193: change to a priority queue where the priorities are computed so as
   to attempt to mimic the denotational semantics *)
module DepthFirstWorklist = struct
  type t = Nil | Cons of int * (t -> t) * t

  let count = ref 0

  let rec ids = function
    | Nil -> []
    | Cons(id,_,items) -> id :: ids items

  let empty = Nil

  let add item items =
    (fun items -> L.printf 5 "W.add: @[%a@]" (List.fmt " " Format.pp_print_int) (ids items)) <& let()=()in
    incr count ;
    Cons(!count, item, items)

  let rem items =
    L.printf 5 "W.rem: @[%a@]" (List.fmt " " Format.pp_print_int) (ids items) ;
    match items with
    | Nil -> None
    | Cons(_,item,items) -> Some(item, items)
end
module W = DepthFirstWorklist



module Summaries (I_D_cp: INJECT_D_CONTROL_POINT) : sig
  type t
  val empty : unit -> t
  val await : t -> I_D_cp.t -> (I_D_cp.t -> W.t -> W.t) -> unit
  val add : t -> Proc.t -> I_D_cp.t -> I_D_cp.t -> W.t -> W.t
  val posts : t -> Proc.t -> I_D_cp.t -> I_D_cp.t list
end = struct

  module I_D_cpTbl =
    ImperativeMultiMap.Make
      (I_D_cp)
      (List.Set(struct type t = I_D_cp.t -> W.t -> W.t end))

  module I_D_cp_Set = Set.Make(I_D_cp)

  module ProcI_D_cpTbl =
    ImperativeMultiMap.Make
      (struct
        type t = Proc.t * I_D_cp.t
        let equal = equal_tup2 Proc.equal I_D_cp.equal
        let compare = compare_tup2 Proc.compare I_D_cp.compare
      end)
      (I_D_cp_Set)

  type t = {
    waiting: I_D_cpTbl.t;
    triples: ProcI_D_cpTbl.t
  }

  let empty () =
    {waiting= I_D_cpTbl.create (); triples= ProcI_D_cpTbl.create ()}

  let await sums pre thunk = I_D_cpTbl.add sums.waiting pre thunk

  let add sums fn pre post wl =
    ProcI_D_cpTbl.add sums.triples (fn,pre) post ;
    let waiting_on_pre = I_D_cpTbl.find sums.waiting pre
    in
    List.fold (fun waiting wl -> waiting post wl) waiting_on_pre wl

  let posts sums fn entry =
    I_D_cp_Set.to_list (ProcI_D_cpTbl.find sums.triples (fn,entry))
end



module Heights (I_D_cp: INJECT_D_CONTROL_POINT) = struct
  module M = ImperativeMap.Make(ControlPoint)

  type t = int M.t

  let create () = M.create ()

  let exceeded t k =
    let n = try (M.find t k) + 1 with Not_found -> 1 in
    M.add t k n ;
    (0 < Config.limit && Config.limit < n)
end



module Pair (Inv: INTRAPROC_DOMAIN) = struct

  type pred = Inv.t

  type r = Inv.r
  let create = Inv.create

  type t = { entry: Inv.t; curr: Inv.t; }

  let inject x = {entry= x; curr= x}
  let project x = x.curr

  let exec_inst cxt i x = {x with curr= Inv.exec_inst cxt i x.curr}

  let error = inject Inv.error
  let tt = inject Inv.tt
  let is_error x = Inv.is_error x.curr
  let is_false x = Inv.is_false x.curr

  let join x y = {x with curr= Inv.join x.curr y.curr}

  let generalize x =
    let x', junk = Inv.generalize x.curr in
    ({x with curr= x'}, junk)

  (* equality on abstract elements at procedure entry points *)
  let equal_entry x y = Inv.equal x.entry y.entry

  let below x y = equal_entry x y && Inv.below x.curr y.curr

  let adapted_pre_substate_call r cxt pre call pcall =
    match Inv.adapted_pre_substate_call r cxt pre.curr call.curr pcall with
    | None -> None
    | Some(post_ra_to_retn_ra) ->
        let post_to_retn post_er =
          (* pop call stack by reinstating call's entry *)
          {call with curr= post_ra_to_retn_ra post_er.curr}
        in
        Some(post_to_retn)

  let call_to_entry r call pcall =
    let entry, post_ra_to_retn_ra = Inv.call_to_entry r call.curr pcall
    in
    let post_to_retn post_er =
      (* pop call stack by reinstating call's entry *)
      {call with curr= post_ra_to_retn_ra post_er.curr}
    in
    (inject entry, post_to_retn)

  let exit_to_retn callee exit =
    {exit with curr= Inv.exit_to_retn callee exit.curr}

  let resolve_indirect_call r call fptr ftyp =
    Inv.resolve_indirect_call r call.curr fptr ftyp

  let compare x y =
    compare_tup2 Inv.compare Inv.compare (x.entry,x.curr) (y.entry,y.curr)

  let equal x y = Inv.equal x.curr y.curr && Inv.equal x.entry y.entry

  let fmt ff x = Inv.fmt ff x.curr
  let fmt_entry ff x = Inv.fmt ff x.entry
  let fmt_pre ff (x,p) = Inv.fmt_pre ff (x.curr,p)
  let fmt_reln ff x =
    Format.fprintf ff "(@[%a,@ %a@])" Inv.fmt x.entry Inv.fmt x.curr

end (* Pair *)



module Make (InterprocDomain: INTERPROC_DOMAIN) = struct

  module ID = InterprocDomain
  module PInv = ID.RD                   (* a pair of invariants *)
  module InjPInv = ID.I_D_cp            (* a pair of invariants injected into the InterprocDomain carrier *)


  module Summaries = Summaries(InjPInv)

  let fmt_summary lvl msg pre ({Proc.formals; freturn} as proc) post =
    L.printf lvl "@[<hov 2>%s summary:@ @[<hv>%a@ %a@ %a@]@\n" msg
      PInv.fmt_pre (fst (InjPInv.project pre), proc)
      (Call.fmt (fun ff {Proc.id} -> Proc.Id.fmt ff id)) (Call.mk proc formals freturn)
      PInv.fmt (fst (InjPInv.project post))


  module Heights = Heights(InjPInv)


  type t = {
    program: Prog.t;
    invariants: ID.r;
    summaries: Summaries.t;
    heights: Heights.t;
    mutable safe: bool;
    mutable hit_limit: bool;
  }


  let exec_prog ({Prog.globals; procs; main} as program) init =
    let r = ID.create program in
    let sums = Summaries.empty () in
    let heights = Heights.create () in
    let res = {program; invariants= r; summaries= sums; heights; safe= true; hit_limit= false}
    in
    let rec exec_cont
        (proc: Proc.t)              (* proc currently executing               *)
        ((prev: InjPInv.t),         (* invariant for previous "control point" *)
         ((curr: PInv.t),           (* invariant for current cont             *)
          (pc: K.t))                (* current cont / program counter         *)
         as prev_to_curr: ID.d_bk)  (* path edge from prev to curr points     *)
        wl                          (* worklist                               *)
      =
      let cxt = Vars.union globals proc.Proc.locals in

      let fail ((prev, (_error, _k)) as prev_to_error) wl =
        res.safe <- false ;
        let next = ID.prev_to_join r prev_to_error in
        if Config.propagate then
          W.add (exec_cont proc (prev, (InjPInv.project next))) wl
        else if Config.continue then
          wl
        else
          W.empty
      in

      let rec exec_succ ((prev, (curr, pc)) as prev_to_curr) cmnd k wl =
        match cmnd with
        | C.Inst(inst) ->
            L.incf 3 "  %a@\n@[<hov 2>exec: %a@\n%a@]" PInv.fmt curr Position.fmt (K.pos pc) I.fmt inst ;
            (fun _ -> L.decf 3 "@ ")
            <&
            let prev_to_k = PInv.exec_inst cxt inst curr in
            if PInv.is_error prev_to_k then
              let curr = if K.sort pc <> None then prev else ID.prev_to_join r prev_to_curr in
              let curr_to_error = PInv.exec_inst cxt inst (fst (InjPInv.project curr)) in
              fail (curr, (curr_to_error, k)) wl
            else
              W.add (exec_cont proc (prev, (prev_to_k, k))) wl

        | C.Call({Call.proc= callee_name} as pcall) ->
            L.printf 2 "  %a@\n@[<hov 2>exec: @[%a@]@\n%a@\n@]" PInv.fmt curr K.fmt pc C.fmt cmnd
            ;
            let callee =
              try Proc.IdHMap.find procs callee_name
              with Not_found -> failwithf "Undefined procedure: %a" Proc.Id.fmt callee_name
            in
            let exec_post_to_retn pre post_to_retn post wl =
              let retn = post_to_retn (post, (fst (ID.I_D_cp.project post), k))
              in
              fmt_summary 2 "applying" pre callee post ;
              (* execute from the return site *)
              W.add (exec_cont proc (retn, ID.I_D_cp.project retn)) wl
            in
            let apply_existing_summary pre post_to_retn wl =
              (* pre covers call *)
              let exec_post_to_retn = exec_post_to_retn pre post_to_retn
              in
              (* execute this call's return site from any posts added to this specification later *)
              Summaries.await sums pre exec_post_to_retn
              ;
              (* execute this call's return site from the existing summary's post states *)
              List.fold exec_post_to_retn (Summaries.posts sums callee pre) wl
            in
            let create_new_summary entry post_to_retn wl =
              (* execute this call's return site from any posts added to this specification later *)
              Summaries.await sums entry (exec_post_to_retn entry post_to_retn)
              ;
              (* execute the callee's body *)
              W.add (exec_cont callee (entry, ID.I_D_cp.project entry)) wl
            in
            let pres = ID.procedure_pres r callee
            in
            let rec search_for_covering_pre = function
              | [] ->
                  (* no covering pre, compute a new specification *)
                  L.printf 7 "@[<hov 2>no summary for:@ %a@]" PInv.fmt curr
                  ;
                  let entry, post_to_retn = ID.call_to_entry r prev_to_curr {pcall with Call.proc= callee}
                  in
                  if List.exists (fun pre -> InjPInv.equal entry pre) pres then
                    apply_existing_summary entry post_to_retn wl
                  else
                    create_new_summary entry post_to_retn wl
              | pre :: pres ->
                  (* found a pre *)
                  L.printf 7 "@[<hov 2>trying summary with pre:@ %a@]" PInv.fmt (fst (InjPInv.project pre))
                  ;
                  (* check if pre covers call *)
                  match ID.adapted_pre_substate_call r cxt pre prev_to_curr {pcall with Call.proc= callee} with
                  | None ->
                      (* no, keep looking *)
                      search_for_covering_pre pres
                  | Some(post_to_retn) ->
                      (* pre covers call *)
                      apply_existing_summary pre post_to_retn wl
            in
            (* find all preconditions for callee *)
            search_for_covering_pre pres

        | C.ICall({Call.proc; typ; targets} as pcall) ->
            L.printf 2 "  %a@\n@[<hov 2>exec: @[%a@]@\n%a@\n@]" PInv.fmt curr K.fmt pc C.fmt cmnd
            ;
            let callees = ID.resolve_indirect_call r prev_to_curr proc typ in
            if callees = [] then
              let curr = ID.prev_to_join r prev_to_curr in
              let curr_to_error = PInv.join (fst (InjPInv.project curr)) PInv.error in
              fail (curr, (curr_to_error, k)) wl
            else
              List.fold (fun callee wl ->
                if not (List.exists (fun target -> Proc.Id.equal callee target) targets) then
                  failwith "Unsound static approximation of indirect call targets" ;
                exec_succ prev_to_curr (C.Call({pcall with Call.proc= callee})) k wl
              ) callees wl
      in

      (* exec_cont *)
      if PInv.is_false curr then (
        (* don't execute if curr is inconsistent *)
        L.printf 3 "  %a@\n@[<hov 2>exec: @[%a@]@]@\nunreachable@\n" PInv.fmt curr K.fmt pc ;
        if Config.show_unreachable then ignore( ID.prev_to_join r prev_to_curr );
        wl
      ) else if ID.now_covered r prev then (
        (* don't execute if pre got covered since it was scheduled *)
        L.printf 3 "  %a@\n@[<hov 2>exec: @[%a@]@]@\n@[covered:@ %a@]@\n" PInv.fmt curr K.fmt pc InjPInv.fmt prev ;
        wl
      ) else if Heights.exceeded heights pc then (
        (* fail if limit on number of iterations is hit *)
        L.printf 3 "  %a@\n@[<hov 2>exec: @[%a@]@\nchain too long@]@\n" PInv.fmt curr K.fmt pc ;
        res.hit_limit <- true ;
        fail prev_to_curr wl
      ) else (
        let exec_succs ((_prev, (_curr, pc)) as prev_to_curr) wl =
          List.fold (fun (k, edge) wl ->
            exec_succ prev_to_curr edge k wl
          ) (CFG.successors pc) wl
        in
        match K.sort pc with
        | Some K.Exit ->
            let prev_to_retn = ID.exit_to_retn proc prev_to_curr
            in
            let covered, retn = ID.prev_to_cut r prev_to_retn
            in
            (match covered with
            | ID.WasCoveredByOld ->
                wl
            | ID.NowCoveredByNew ->
                let pre =
                  let equal_entry x y = ID.RD.equal_entry (fst (ID.I_D_cp.project x)) (fst (ID.I_D_cp.project y)) in
                  try
                    List.find (equal_entry retn) (ID.procedure_pres r proc)
                  with Not_found ->
                    L.printf 2 "@[retn:@ @[%a@]@ pres:@ @[%a@]@]"
                      PInv.fmt_reln (fst (InjPInv.project retn))
                      (List.fmt ",@ " (fun ff pre -> PInv.fmt ff (fst (InjPInv.project pre))))
                      (ID.procedure_pres r proc) ;
                    failwith "no pre for retn found"
                in
                fmt_summary 1 "adding" pre proc retn
                ;
                ID.register_pre r pre
                ;
                Summaries.add sums proc pre retn wl
            )
        | Some K.Cut ->
            let covered, cut = ID.prev_to_cut r prev_to_curr in
            (match covered with
            | ID.WasCoveredByOld ->
                wl
            | ID.NowCoveredByNew ->
                exec_succs (cut, ID.I_D_cp.project cut) wl
            )
        | Some K.Join ->
            let join = ID.prev_to_join r prev_to_curr in
            exec_succs (join, ID.I_D_cp.project join) wl

        | Some K.Fork ->
            let fork = ID.prev_to_join r prev_to_curr in
            exec_succs (fork, ID.I_D_cp.project fork) wl

        | Some K.Entry | Some K.Return | None ->
            exec_succs prev_to_curr wl
      )
    in
    (* exec_prog *)
    let rec compute_invariants_fixed_point wl =
      match W.rem wl with
      | Some(thunk, wl) ->
          compute_invariants_fixed_point (thunk wl)
      | None ->
          res
    in
    let {Proc.entry} as main = Proc.IdHMap.find procs main
    in
    let init = ID.init r (PInv.inject init, entry)
    in
    compute_invariants_fixed_point (W.add (exec_cont main (init, ID.I_D_cp.project init)) W.empty)

end
