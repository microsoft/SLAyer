(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Abstraction of symbolic heaps *)

(* Notes:
   - The definition of abs_junk effectively distributes every [_ * true] to the
     root of the formula.  An alternative is to keep the [_ * true] subformulas
     in the *-\/ tree where the garbage they abstract came from.  The second is
     logically stronger, but perhaps disturbing to the prover.  Overall it is
     not clear which is preferable, there should be an option to use either
     technique.

   - It seems that, since abstracted formulas only get weaker, it should be
     possible to normalize the formula to be abstracted just once at the
     beginning and then simply reuse the resulting congruence relation.
*)

open Library

open Variable
open Expression
module E = Exp
module S = Substitution
open SymbolicHeap
open Program

(* lvl 1 = entry-point entry/exit and summary rewrite
       2 = individual abstraction rewrites
       3 = each abstraction rule entry/exit and summary rewrite
    >    = subsidiary computations *)
module L = (val Log.std Config.vAbs : Log.LOG)



(* Timing =================================================================== *)

let abstract_tmr = Timer.create "Abstraction.abstract"
let abs_junk_tmr = Timer.create "Abstraction.abs_junk"
let abs_ls_tmr = Timer.create "Abstraction.abs_ls"
let abs_arith_tmr = Timer.create "Abstraction.abs_arith"
let abs_pure_tmr = Timer.create "Abstraction.abs_pure"
let normalize_tmr = Timer.create "Abstraction.SH.normalize"


module XSH = struct include XSH
  let normalize xsh =
    Timer.start normalize_tmr ;
    let res = normalize xsh in
    Timer.stop normalize_tmr ;
    res
end


(* Formatting =============================================================== *)

let fmt_abs lvl msg (xs,sh) (xs',sh') =
  if lvl <= !Config.vAbs then
    let fmt_o, _fmt_i, fmt_n = SH.fmt_did_xs ((xs,sh), (xs',sh')) in
    L.printf lvl "@[<hov 2>%sreplace:@ %t@]@ @[<hov 2>with:@ %t@]" msg fmt_o fmt_n



(*============================================================================
                Abstract unreachable points-tos and list-segs.
  ============================================================================*)

let abs_junk (xs,sh) =
    Timer.start abs_junk_tmr ;
    L.incf 3 "( abs_junk:@ %a" SH.fmt_xs (xs,sh) ;
  (fun xsho ->
    Timer.stop abs_junk_tmr ;
    Option.option () (fun (xs',sh') -> fmt_abs 2 "abs_junk: " (xs,sh) (xs', SH.Jnk.star sh')) xsho ;
    L.decf 3 ") abs_junk:@ %a" (Option.fmt "unchanged" SH.fmt_xs) xsho )
  <&
  let keep = (fun loc -> Vars.disjoint xs (E.fv loc))
  in
  let is_reachable = Reachability.is_reachable keep sh
  in
  let sh, pgs =
    SH.map_fold (fun sh pgs ->
      let sh, pgs =
        SH.PtS.fold (fun ({Pt.loc} as pt) (sh, pgs) ->
          if is_reachable sh loc then (sh, pgs)
          else (SH.PtS.remove pt sh, true)
        ) sh (sh, pgs) in
      let sh, pgs =
        SH.LsS.fold (fun ls (sh, pgs) ->
          let loc = Ls.fst_alloc ls in
          if is_reachable sh loc then (sh, pgs)
          else (SH.LsS.remove ls sh, true)
        ) sh (sh, pgs) in
      (sh, pgs)
    ) sh false
  in
  if pgs then
    Some(xs,sh)
  else
    None



(*============================================================================
                               Abstract lists.
  ============================================================================*)

let abs_ls (xs,sh) =
    Timer.start abs_ls_tmr ;
    L.incf 3 "( abs_ls:@ %a" SH.fmt_xs (xs,sh) ;
  (fun xsho ->
    Timer.stop abs_ls_tmr ;
    L.decf 3 ") abs_ls:@ %a" (Option.fmt "unchanged" SH.fmt_xs) xsho )
  <&
  HeapAbstraction.abstract (xs,sh)



(*============================================================================
                       Abstract arithmetic expressions.
  ============================================================================*)

(* Keep integer program constants, as control flow often depends on them.
   (For instance, PS#146 documents the case of kdbclass.c using the constant
   NT_STATUS returned by IoCreateDevice.)
*)
let pgm_consts : unit Int64HMap.t = Int64HMap.create 31

let _ = Initialize.register (fun {Prog.constants} ->
  List.iter (fun i -> Int64HMap.add pgm_consts i ()) constants
)

let pgm_const e =
  assert(true$> L.incf 8 "( pgm_const:@ %a" E.fmt e ); (fun b -> assert(true$> L.decf 8 ") pgm_const:@ %b" b )) <&
  match E.desc e with
  | E.Num(n) when (Int64HMap.mem pgm_consts n) -> true
  | _ -> false


let abs_arith (xs,sh) =
    Timer.start abs_arith_tmr ;
    L.incf 3 "( abs_arith:@ %a" SH.fmt_xs (xs,sh) ;
  (fun xsho ->
    Timer.stop abs_arith_tmr ;
    Option.option () (fmt_abs 2 "abs_arith: " (xs,sh)) xsho ;
    L.decf 3 ") abs_arith:@ %a" (Option.fmt "unchanged" SH.fmt_xs) xsho )
  <&
  (* Abstract constant list lengths *)
  let sh, (pgs, fresh_vs) =
    SH.map_fold (fun sh (pgs,vs) ->
      let vs, lss, sh =
        SH.LsS.fold (fun ({Ls.len} as ls) (vs,lss,sh) ->
          match E.desc len with
          | E.Num _ ->
              let k = Var.gensym "a" Var.IntegerSort in
              ( Vars.add k vs
              , {ls with Ls.len= E.mkVar k} :: lss
              , SH.LsS.remove ls sh )
          | _ ->
              (vs,lss,sh)
        ) sh (vs, [], sh) in
      if lss = [] then (sh, (pgs, vs))
      else (SH.LsS.star lss sh, (true, vs))
    ) sh (false, Vars.empty)
  in
  (* Abstract all integer constants except 0, 1, and those appearing in program *)
  let to_abstract e (s, fresh_vs) =
    if Config.preserve_consts && pgm_const e then (s, fresh_vs)
    else if S.in_dom e s then (s, fresh_vs)
    else match E.desc e with
    | E.Var _ | E.Num(0L) | E.Num(1L) -> (s, fresh_vs)
    | E.Op3(E.Ite,_,E.Num(1L),E.Num(0L)) -> (s, fresh_vs)
    | _ when E.sort_of e = Var.IntegerSort ->
        let f = Var.gensym "a" Var.IntegerSort in
        (S.add e (E.mkVar f) s, Vars.add f fresh_vs)
    | _ -> (s, fresh_vs)
  in
  let exp_subst, fresh_vs = SH.fold_exps to_abstract sh (S.empty, fresh_vs)
  in
  if not pgs && S.is_empty exp_subst then
    None
  else
    (* Note: change this SH.subst to preserve normalization *)
    let sh' = SH.subst exp_subst sh in
    Some(Vars.union fresh_vs xs, sh')



(*============================================================================
                           Abstract Pure Formulas.
  ============================================================================*)

let abs_pure (xs,sh) =
    Timer.start abs_pure_tmr ;
    L.incf 3 "( abs_pure:@ %a" SH.fmt_xs (xs,sh) ;
  (fun xsho ->
    Timer.stop abs_pure_tmr ;
    Option.option () (fmt_abs 2 "abs_pure: " (xs,sh)) xsho ;
    L.decf 3 ") abs_pure:@ %a" (Option.fmt "unchanged" SH.fmt_xs) xsho )
  <&

  let pgs = ref false in

  let kills = Vars.diff xs (SH.fv (SH.spatial_sf sh)) in

  L.printf 6 "kills: @[<hov 1>{%a}@]" Vars.fmt kills ;

  if Vars.is_empty kills then None else

  let abs_pure_q q _ =
(*     L.incf 6 "( abs_pure_q:@ %a" SH.fmt q ; (fun (q',_) -> L.decf 6 ") abs_pure_q:@ %a" SH.fmt q' ) <& *)
    (* convert the classes to a list of equalities between not killable exps *)
    let classes = SH.Pf.classes q in
    let eqs =
      Expss.fold (fun cls eqs ->
        L.printf 6 "cls: %a" Exps.fmt cls ;
        if Exps.cardinal cls <= 1 then eqs
        else
        (* filter the killable exps out of the class *)
        let filtered_cls =
          Exps.fold (fun e filtered_cls ->
            let fv_e = Exp.fv e in
            if   Vars.disjoint fv_e kills
            then filtered_cls
            else
              let filtered_cls' = Exps.remove e filtered_cls in
              pgs := true ;
              L.printf 6 "abs_pure: %a" E.fmt e ;
              filtered_cls'
          ) cls cls in
        if Exps.cardinal filtered_cls <= 1 then eqs
        else
        (* convert the filtered class to a list of equalities *)
        let x = Exps.choose filtered_cls in
        let xs = Exps.remove x filtered_cls in
        let rec aux xs =
          if Exps.is_empty xs then
            eqs
          else
            let y = Exps.choose xs in
            let ys = Exps.remove y xs in
            E.mkEq x y :: aux ys in
        (* add killed exps to subst *)
        aux xs
      ) classes []
    in
(*     L.printf 5 "eqs_pgs: %b" !pgs ; *)
(*     L.printf 5 "eqs: %a" (List.fmt ";@ " E.fmt) eqs ; *)
    (* remove any boolean exp containing a kill *)
    let bex = SH.Pf.term q in
(*     L.incf 5 "( remove: %a" E.fmt bex ; *)
    let bex' =
      E.remove (fun d ->
        if Vars.disjoint (E.fv (E.name d)) kills then false
        else (
          pgs := true ;
          true
        )
      ) bex in
(*     L.decf 5 ") remove: %a" E.fmt bex' ; *)
    (* conjoin the abstracted boolean expression and filtered equalities *)
    (* Note: change this SH.Pf.empty to preserve normalization *)
    let q' = SH.Pf.star (bex' :: eqs) (SH.Pf.empty q) in
    (* remove pure disjunctions *)
    let q' =
      SH.DjS.filter (fun dj ->
        if Dj.for_all SH.is_empty dj then (
          pgs := true ;
          false
        ) else
          true
      ) q' in
    (q', !pgs)
  in
  let sh', pgs =
    SH.map_fold abs_pure_q sh false
  in
  if pgs then Some(xs,sh') else None



(*============================================================================
                                 Entry Point.
  ============================================================================*)

let gen_query id xsh =
  TestGen.gen ("abs_" ^ (string_of_int id)) (fun ff ->
    Format.fprintf ff
      "let tmr = Timer.create() in Timer.start tmr ;@\n\
       @\n@[<hov 3>(* abstract:@ %a@]@\n*)@\n\
       @[<hov 2>Abstraction.abstract@\n%a@] |> ignore;@\n\
       Timer.stop Timer.init ;@\n\
       Statistics.report (Timer.create()) tmr (Timer.create())"
      XSH.fmt xsh
      XSH.fmt_caml xsh
  )


let new_query =
  let count = ref 0
  in
  fun xsh ->
    incr count;
    if !count = Config.abs_query_to_gen then gen_query !count xsh;
    !count


let seq f g x = g (Option.from_option x (f x))

let fix f x =
  match f x with
  | None -> None
  | Some(x') ->
      let rec loop x =
        match f x with
        | None -> Some(x)
        | Some(x') -> loop x' in
      loop x'


let abstract xsh =
  Timer.start abstract_tmr ;
  let xsh = XSH.normalize xsh in

  let query = new_query xsh in
  L.incf 1 "( abstract %d:@ %a" query XSH.fmt xsh ;

  if XSH.inconsistent xsh then
    (XSH.ff, false)
  else

  let junk = ref false in
  let abs_junk x =
    let xo = abs_junk x in
    match xo with
    | Some _ -> junk := true; xo
    | None -> xo
  in

  let abs =
    (seq abs_ls
    (seq abs_junk
    (seq (fix abs_arith)
    (seq abs_pure
         id
    )))) in

  let xs, sh = XSH.exists_bind Vars.empty xsh in

  let xs',sh' = abs (xs, sh) in
  let sh' = if !junk then SH.Jnk.star sh' else sh' in

  let xsh' = SH.exists_intro xs' sh' in

  if not (SH.equal sh sh') then (
    fmt_abs 1 "" (xs,sh) (xs',sh') ;
    L.decf 1 ") abstract:@ %a@\n" XSH.fmt xsh'
  ) else
    L.decf 1 ") abstract:@ unchanged@\n" ;

  Timer.stop abstract_tmr ;
  assert( not Config.check_abs || (
    L.printf 6 "checking abstraction" ;
    Prover.entailsx xsh xsh' <> None
    || (gen_query query xsh ; failwithf "Abstraction not provably sound" )
  ));

  (xsh', !junk)
