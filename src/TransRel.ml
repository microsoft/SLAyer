(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Expression

let add_edge_tmr = Timer.create "TransRel.add_edge"
let add_scc_tmr = Timer.create "TransRel.add_scc"
let preds_tmr = Timer.create "TransRel.predecessors"

module ExpTransRel = struct
  type exp = Exp.t
  type exps = Exps.t

  type uf = {
    parent: exp ExpMap.t;
    terms: ExpMMap.t;
    rank: int ExpMap.t;
  }
  type tt = {
    pred: ExpMMap.t;            (** maps each exp to it's predecessors *)
    succ: ExpMMap.t;            (** maps each exp to it's succecessors *)
    uf: uf;                     (** maps each exp to the representative of its strongly-connected component *)
  }
  type t = tt list              (** the intersection of multiple relations *)

  let empty_uf = {
    parent = ExpMap.empty;
    terms = ExpMMap.empty;
    rank = ExpMap.empty;
  }
  let empty = [{
    pred = ExpMMap.empty;
    succ = ExpMMap.empty;
    uf = empty_uf;
  }]

  (* Successors and predessors are the same. *)
  let same_succ_and_pred ms =
    List.equal (fun (h,u) (k,v) -> Exp.equal h k && Exp.equal u v)
      (ExpMMap.to_list ms.succ)
      (ExpMMap.to_list (ExpMMap.fold (fun x y -> ExpMMap.add y x) ms.pred ExpMMap.empty))

  (* Print representation of Transitive relation. *)
  let fmt1 ff ms =
    assert( same_succ_and_pred ms );
    let fmt_succ ff succ =
      let fmt_exp_succs ff (exp,succs) =
        Format.fprintf ff "@[<hov 2>(%a ->@ {@[%a@]})@]" Exp.fmt exp Exps.fmt succs
      in
      Format.fprintf ff "@[%a@]" (List.fmt "@ " fmt_exp_succs)
        (ExpMMap.fold_keys (fun exp succs exp_succs -> (exp,succs) :: exp_succs) succ [])
    in
    let fmt_pred ff pred =
      let fmt_exp_preds ff (exp,preds) =
        Format.fprintf ff "@[<hov 2>(%a <-@ {@[%a@]})@]" Exp.fmt exp Exps.fmt preds
      in
      Format.fprintf ff "@[%a@]" (List.fmt "@ " fmt_exp_preds)
        (ExpMMap.fold_keys (fun exp preds exp_preds -> (exp,preds) :: exp_preds) pred [])
    in
    let fmt_uf ff uf =
      let fmt_rep_cls ff (rep,cls) =
        Format.fprintf ff "@[<hov 2>(%a <->@ %a)@]" Exp.fmt rep (Exps.fmt_sep " <->@ ") cls
      in
      Format.fprintf ff "@[%a@]" (List.fmt "@ " fmt_rep_cls)
        (ExpMMap.fold_keys (fun rep cls rep_cls -> (rep,cls) :: rep_cls) uf.terms [])
    in
    Format.fprintf ff "@[<hv>@[<hov 2>Succ:@ %a@]@ @[<hov 2>Pred:@ %a@]@ @[<hov 2>SCCs:@ %a@]@]"
      fmt_succ ms.succ fmt_pred ms.pred fmt_uf ms.uf

  let fmt ff mss =
    Format.fprintf ff "%a" (List.fmt "@; /\\ " fmt1) mss

  let rec find uf e  =
    (* Representatives don't have a parent *)
    match ExpMap.tryfind e uf.parent with
    | Some e' -> find uf e'
    | None -> e

  let rank uf e =
    match ExpMap.tryfind e uf.rank with
    | Some n -> n
    | None -> 0 (* We use 0 as the initial rank *)

  let terms uf e =
    Exps.add e (ExpMMap.find e uf.terms)

  let union_set es uf =
    let first = Exps.choose es in
    let es' = Exps.remove first es in
    (* Find the max rank of the equivalence class to choose the representative.
       If there are more than one with the max, remember so we can add one to the rank. *)
    let max_rank, rep, multiple =
      Exps.fold (fun e (rnk,elt,mlt) ->
        let rnk' = rank uf e in
        if rnk' > rnk then
          (rnk', e, false)
        else if rnk' = rnk then
          (rnk, elt, true)
        else
          (rnk, elt, mlt)
      ) es' (rank uf first, first, false)
    in
    let es = Exps.remove rep es in
    (* Increase rank if combining with something of same rank *)
    let rank =
      if multiple then ExpMap.add rep (max_rank+1) (uf.rank)
      else uf.rank in
    (* Update the parents to point to the representative, and add all their children to the set *)
    let (parent,terms) =
      Exps.fold (fun e (pa,re)->
        (ExpMap.add e rep pa,
         ExpMMap.remove e (ExpMMap.union rep (Exps.add e (ExpMMap.find e re)) re))
      ) es (uf.parent,uf.terms) in
    let uf = {parent = parent; terms = terms; rank= rank} in
    (uf, rep)


  let only_representatives ms =
    (* Assumes same_succ_pred has been called, so don't need to check the range of succ or the preds. *)
    ExpMMap.fold_keys (fun k _ b -> b  && (Exp.equal k (find ms.uf k))) ms.succ true

  let invariant ms =
    assert( same_succ_and_pred ms );
    assert( only_representatives ms )

  (* Combines all of the vertices in eqs into one vertex*)
  let merge eqs m =
    let preds = m.pred in
    let succs = m.succ in
    let uf,r = union_set eqs m.uf in

    (* Remove all the nodes from the preds and succs, and add the succs and preds to the equivalence class's*)
    let rest = Exps.remove r eqs in
    let all_ nodes =
      Exps.fold (fun e -> Exps.union (Exps.diff (ExpMMap.find e nodes) rest))
        eqs (Exps.empty) in
    let all_succs = Exps.add r (all_ succs) in
    let all_preds = Exps.add r (all_ preds) in
    let succs =
      Exps.fold (fun e m -> (*ExpMMap.diff e (Exps.singleton e)*)
        (ExpMMap.union e all_succs
           (ExpMMap.diff e rest m))
      ) all_preds succs in
    let preds =
      Exps.fold (fun e m -> (*ExpMMap.diff e (Exps.singleton e)*)
        (ExpMMap.union e all_preds
           (ExpMMap.diff e rest m))
      ) all_succs preds in
    let succ = Exps.fold ExpMMap.remove rest succs in
    let pred = Exps.fold ExpMMap.remove rest preds in
    { pred; succ; uf }


  let floydWarshall preds succs =
    ExpMMap.fold_keys (fun mid _ (preds,succs) ->
      (* Note, the second param is almost su_mid, but has not been updated *)
      let pr_mid = ExpMMap.find mid preds in
      let su_mid = ExpMMap.find mid succs in
      (* Just add cross product of edges *)
      let preds = Exps.fold (fun k -> ExpMMap.union k pr_mid) su_mid preds in
      let succs = Exps.fold (fun k -> ExpMMap.union k su_mid) pr_mid succs in
      ( preds, succs)
    ) succs (preds,succs)


  (** [[add (e,f) t]] is the smallest transitive relation containing [[t]] and ([e],[f]). *)
  let add_inner (s,e) m =
    assert(true$>
      invariant m );
    (fun m' -> assert(
      invariant m' ;
      let s = find m.uf s in
      let e = find m.uf e in
      let pred'', succ'' = floydWarshall (ExpMMap.add e s m.pred) (ExpMMap.add s e m.succ) in
      let pred' = m'.pred in
      let b =
        ExpMMap.fold_keys (fun k xs b ->
          let ys = ExpMMap.find k pred'' in
          let xs_all = Exps.unions ((terms m'.uf k)::(List.map (terms m'.uf) (Exps.to_list xs))) in
          let ys_all = Exps.unions ((terms m.uf k)::(List.map (terms m.uf) (Exps.to_list ys))) in
          if Exps.equal xs_all ys_all then b else let()=()in
            Format.printf "k:@\n  @{%a@]@\nxs_all : @\n  @[%a@]@\nys_all:@\n  @[%a@]@\n"
              Exp.fmt k Exps.fmt xs_all Exps.fmt ys_all ;
            Format.printf "Pre:@\n  @[%a@]@\nPost@\n  @[%a@]@\nFW@\n  @[%a@]@\n"
              fmt [m] fmt [m'] fmt [{pred=pred''; succ=succ''; uf = m.uf}] ;
            assert false
        ) pred' true in
      b
      || (Format.printf "Start @\n%a@\nNew existing@\n%a@\nFW@\n%a@\n"
            fmt [m] fmt [m'] fmt [{pred=pred'';succ=succ'';uf=m.uf}] ;
          false)
    )) <&
    let s = find m.uf s in
    let e = find m.uf e in
    let preds = m.pred in
    let succs = m.succ in
    (* If s and e are currently equal, or the edge already exists do noting *)
    if Exp.equal s e || Exps.mem s (ExpMMap.find e preds) then
      m
    else
      (* Otherwise add the edges and the transitive new edges *)
      let pr_s = Exps.add s (ExpMMap.find s preds) in
      let su_e = Exps.add e (ExpMMap.find e succs) in
      if Exps.mem e pr_s then
        (* We are creating a cycle so do the UF work *)
        (* Any node that is both a succ of e and pred of s, will be made equal by this update *)
        let eqs = Exps.inter su_e pr_s in
        (* Add eqs to UF *)
        let eqs = Exps.add s (Exps.add e eqs) in
        merge eqs m
      else
        (* Just add cross product of edges *)
        let pred = Exps.fold (fun k -> ExpMMap.union k pr_s) su_e preds in
        let succ = Exps.fold (fun k -> ExpMMap.union k su_e) pr_s succs in
        { m with pred; succ }


  let predecessors_inner s m =
    let s = find m.uf s in
    Exps.fold (fun p ps ->
      Exps.union ps (terms m.uf p)
    ) (ExpMMap.find s m.pred) (terms m.uf s)

  let predecessors ms e =
    Timer.start preds_tmr ; (fun _ -> Timer.stop preds_tmr) <&
    Exps.inters (List.map (predecessors_inner e) ms)

  let inter xs ys =
    (* Just check if there are the same structures multiple times *)
    List.fold (fun x rs -> if List.memq x ys then rs else x::rs) xs ys

  let is_reachable p rg x =
    List.for_all (fun m ->
      Exps.exists p (predecessors_inner x m)
    ) rg

  let add (s,e) ms =
    Timer.start add_edge_tmr ; (fun _ -> Timer.stop add_edge_tmr) <&
    List.map (add_inner (s,e)) ms

  let add_scc_list_no_opt xs rg =
    let rec add_scc_list_ xs rg =
      match xs with
      | x::y::zs -> add_scc_list_ (y::zs) (add_inner (x,y) (add_inner (y,x) rg))
      | _ -> rg in
    add_scc_list_ xs rg


  let add_scc_list_inner xs rg =
    invariant rg ; (fun rg' -> invariant rg') <&
    (* MJP: Not sure which is faster, I think option 2, but left
       alternatives for further profiling, when I have chance *)
    let option = 2 in
    match option with
    | 0 ->
        add_scc_list_no_opt xs rg
    | 1 ->
        merge (List.fold (fun e -> Exps.add (find rg.uf e)) xs Exps.empty) rg
    | 2 ->
        let no_edges, has_edges =
          List.fold (fun x (no_edges,has_edges) ->
            let x = find rg.uf x in
            if ExpMMap.mem x rg.pred || ExpMMap.mem x rg.succ then
              (no_edges, Exps.add x has_edges)
            else
              (Exps.add x no_edges, has_edges)
          ) xs (Exps.empty,Exps.empty) in
        let edges, uf =
          match Exps.cardinal no_edges with
          | 0 ->
              (has_edges, rg.uf)
          | 1 ->
              (Exps.add (Exps.choose no_edges) has_edges, rg.uf)
          | _ ->
              let uf, r = union_set no_edges rg.uf in
              (Exps.add r has_edges, uf)
        in
        let rg = {rg with uf} in
        (match Exps.cardinal edges with
        | 0 | 1 -> rg
        | _ -> merge edges rg
        )
    | _ ->
        let no_edges,has_edges =
          List.fold (fun x (no_edges,has_edges) ->
            let x = find rg.uf x in
            if ExpMMap.mem x rg.pred || ExpMMap.mem x rg.succ then
              (no_edges, Exps.add x has_edges)
            else
              (Exps.add x no_edges, has_edges)
          ) xs (Exps.empty,Exps.empty) in
        let has_edges = Exps.to_list has_edges in
        let edges, uf =
          match Exps.cardinal no_edges with
          | 0 ->
              (has_edges, rg.uf)
          | 1 ->
              ((Exps.choose no_edges)::has_edges, rg.uf)
          | _ ->
              let uf, r = union_set no_edges rg.uf in
              (r::has_edges, uf) in
        let rg = {rg with uf = uf} in
        add_scc_list_no_opt edges rg


  let add_scc_list xs rg =
    Timer.start add_scc_tmr ; (fun _ -> Timer.stop add_scc_tmr) <&
    List.map (add_scc_list_inner xs) rg

  let add_scc xs rg =
    if Exps.cardinal xs > 1 then
      add_scc_list (Exps.to_list xs) rg
    else
      rg


  let fmt ff mss =
    let car r =
      Exps.empty
      |>
      ExpMMap.fold_keys (fun e' es car ->
        Exps.add e' (Exps.union es car)
      ) r.uf.terms
      |>
      ExpMMap.fold_keys (fun s ps car ->
        Exps.add s (Exps.union ps car)
      ) r.pred
    in
    let preds s r =
      predecessors_inner s r
    in
    let fold fn r z =
      Exps.fold (fun s z ->
        Exps.fold (fun p z ->
          fn p s z
        ) (preds s r) z
      ) (car r) z
    in
    let mem p s r =
      Exps.mem (find r.uf p) (preds s r)
    in
    let inter q r =
      fold (fun p s i ->
        if mem p s r then
          add_inner (p,s) i
        else
          i
      ) q (List.hd empty)
    in
    let inters = function
      | [] -> List.hd empty
      | [r] -> r
      | r::rs -> List.fold inter rs r
    in
    Format.fprintf ff "%a" fmt1 (inters mss)

end
