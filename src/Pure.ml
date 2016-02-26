(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Pure formulas and theorem prover *)

open Library

module HC = HashCons
open Type
open Variable
open Expression
module E = Exp
module S = Substitution

module L = (val Log.std Config.vPure : Log.LOG)



(* Timing =================================================================== *)

let z3_assert_tmr = Timer.create "Z3.assert"
let z3_push_tmr = Timer.create "Z3.push"
let z3_pop_tmr = Timer.create "Z3.pop"
let z3_check_tmr = Timer.create "Z3.check"
let z3_check_assumptions_tmr = Timer.create "Z3.check_assumptions"
let z3_eval_tmr = Timer.create "Z3.eval"
let z3_get_implied_equalities_tmr = Timer.create "Z3.get_implied_equalities"
let get_implied_equalities_tmr = Timer.create "Pure.get_implied_equalities"
let find_provable_equality_tmr = Timer.create "Pure.find_provable_equality"
let conjoin_tmr = Timer.create "Pure.conjoin"
let inconsistent_tmr = Timer.create "Pure.inconsistent"
let implies_tmr = Timer.create "Pure.impliesx"


(* Formatting =============================================================== *)

let fmt_xs = Vars.fmt_embrace "@[<hov 2>? " " .@]@ "


(* Initialization =========================================================== *)

(* let gie_log = *)
(*   open_out *)
(*     (Config.testname *)
(*     ^".gie" *)
(*     ^(string_of_int Config.gie) *)
(*     ^(if Config.gie_incremental then "1" else "0") *)
(*     ^(if Config.gie_weak then "1" else "0") *)
(*     ^".dat" *)
(*     ) *)

let _ =
  (* Enable warnings and logging *)
  Z3.toggle_warning_messages (!Config.vZ3 > 0) ;
  if Config.z3_log then
    let logname = Config.testname ^ ".z3.log" in
    ignore( Z3.open_log logname )



(*============================================================================
                             Extended Z3 context
  ============================================================================*)

type xcontext = {
  mutable z: Z3.context;                        (* backing Z3.context state   *)
  mutable s: Z3.solver;
  mutable i_sort: Z3.sort;                      (* integer sort               *)
  mutable v_sort: Z3.sort;                      (* value sort                 *)
  mutable v_ctor: Z3.func_decl;                 (* value constructor          *)
  mutable v_dtors: Z3.func_decl array;          (* value destructors          *)
  mutable b_sort: Z3.sort;                      (* boolean sort               *)
  mutable t_sort: Z3.sort;                      (* type sort                  *)
  mutable allocd: Z3.func_decl;                 (* allocd predicate           *)
  mutable zero: Z3.ast;                         (* cached 0                   *)
  mutable one: Z3.ast;                          (* cached 1                   *)
  mutable mone: Z3.ast;                         (* cached -1                  *)
  (* for debugging / test generation: *)
  mutable clear_count: int;                     (* number of 'clear' calls    *)
  (* for the weak/uninterpreted encoding *)
  mutable p_sort: Z3.sort;                      (* pointer sort               *)
  mutable u_sort: Z3.sort;                      (* uninterpreted integer sort *)
}

type t = {
  a: Z3.ast list;                               (* address into...            *)
  c: xcontext;                                  (* context tree               *)
}

type partition = Z3.ast


let assumptions x parts =
  List.rev_append parts x.a

let assertions ({c={z= ctx; s= slv}} as x) parts =
  let v = Z3.solver_get_assertions ctx slv in
  let n = Z3.ast_vector_size ctx v in
  let rec loop i asserts =
    if i < n then
      loop (i+1) (Z3.ast_vector_get ctx v i :: asserts)
    else
      asserts
  in
  loop 0 (assumptions x parts)




(*============================================================================
                            Extended Z3 interface
  ============================================================================*)

module Z3 = struct
  include Z3

  (* Wrappers to establish preconditions ==================================== *)

  let mk_and x = function
    | [||]  -> mk_true x
    | [|b|] -> b
    | bs    -> mk_and x bs

  let mk_or x = function
    | [||]  -> mk_false x
    | [|b|] -> b
    | bs    -> mk_or x bs

  let mk_add ({c={z= ctx}} as x) = function
    | [||]  -> x.c.zero
    | [|e|] -> e
    | es    -> mk_add ctx es

  let mk_mul ({c={z= ctx}} as x) = function
    | [||]  -> x.c.one
    | [|e|] -> e
    | es    -> mk_mul ctx es

  let mk_sub ({c={z= ctx}} as x) = function
    | [||]  -> x.c.zero
    | [|e|] -> e
    | es    -> mk_sub ctx es

  let mk_distinct x = function
    | [||] | [|_|] -> mk_true x
(*     | es           -> mk_distinct x es *)
    (* Note: Z3 sometimes fails to eliminate quantified vars in distinct's? *)
    | es ->
        if Config.z3_distinct then
          mk_distinct x es
        else
          mk_and x (Array.of_list
            (List.fold_pairs (fun e f dqs ->
              mk_not x (mk_eq x e f) :: dqs
            ) (Array.to_list es) []))


  (* Formatting ============================================================= *)

  let fmt_lbool ff b =
    Format.fprintf ff "%s" (match b with L_TRUE -> "true" | L_FALSE -> "false" | L_UNDEF -> "undef")

  let fmt_ast x ff p = Format.fprintf ff "%s" (ast_to_string x p)

  let fmt_cnstrs ?(parts=[]) ff ({c={z= ctx}} as x) =
    List.fmt "@\n" (fmt_ast ctx) ff (assertions x parts)


  (* Test generation and Error reporting ==================================== *)

  let test_count = ref 0

  let gen_test_ ?(parts=[]) ({c={z= ctx}} as x) f trailer =
    let testname = Config.testname ^ "_" ^ (string_of_int !test_count) in
    incr test_count ;
    let cnstr = assertions x parts in
    let str = benchmark_to_smtlib_string ctx testname "" "unknown" "" (Array.of_list cnstr) f in
    let chan = open_out (testname ^ ".smt") in
    output_string chan str ;
    trailer chan ;
    close_out chan

  let _gen_test ?(parts=[]) x f =
    (* let f = simplify ctx f in *)
    gen_test_ ~parts x f (fun _ -> ())

  let _gen_test_smtc fn ?(parts=[]) ({c={z= ctx}} as x) es =
    if Config.z3_print_mode <> "smt2" then
      L.printf 0 "%s tests only supported for smt2 z3_print_mode" fn
    else
      (* add trivial equalities to ensure every variable in es is declared *)
      let f = mk_and ctx (Array.map (fun e -> mk_eq ctx e e) es) in
      gen_test_ ~parts x f (fun chan ->
        output_string chan "(" ;
        output_string chan fn ;
        Array.iter (fun e ->
          output_string chan " " ;
          output_string chan (ast_to_string ctx e)
        ) es ;
        output_string chan ")\n" ;
      )

  let _report_failure _expecting_sat {c={z= ctx; s= slv}} _ps =
    assert(true$>
      let failure = solver_get_reason_unknown ctx slv in
      match failure with
      | "" -> ()
(*       | QUANTIFIERS when expecting_sat -> () *)
      | _ ->
          (* gen_test x ((* simplify ctx *) (mk_and ctx _ps)) ; *)
          L.printf 1 "WARNING: Z3 search failed (%s)" failure
    )

  let _report_model {c={z= ctx; s= slv}} =
    assert(
      not Config.z3_model ||
      match solver_check ctx slv with
      | L_TRUE ->
          let m = solver_get_model ctx slv in
          L.warnf "model:@\n%s" (model_to_string ctx m)
      | _ ->
          true
    )


  (* Timing ================================================================= *)

  let solver_assert ctx slv f =
(*     L.printf 0 "ASSERT:@\n%a" (fmt_ast ctx) f ; *)
    Timer.start z3_assert_tmr ; (fun _ -> Timer.stop z3_assert_tmr) <&
    solver_assert ctx slv f

  let solver_push ctx slv =
(*     L.incf 0 "( PUSH: ctx#%i" x.c.id ; *)
    Timer.start z3_push_tmr ; (fun _ -> Timer.stop z3_push_tmr) <&
    solver_push ctx slv

  let solver_pop ctx slv n =
(*     L.decf 0 ") POP: ctx#%i %i" x.c.id n ; *)
    Timer.start z3_pop_tmr ; (fun _ -> Timer.stop z3_pop_tmr) <&
    solver_pop ctx slv n

  let solver_check ctx slv =
    Timer.start z3_check_tmr ; (fun _ -> Timer.stop z3_check_tmr) <&
    solver_check ctx slv

  let solver_check_assumptions ctx slv a =
      Timer.start z3_check_assumptions_tmr ;
    (fun _ ->
      Timer.stop_report z3_check_assumptions_tmr
        (if Config.check_assumptions_time >= 0.
         then Config.check_assumptions_time
         else z3_check_assumptions_tmr.Timer.max)
        (L.printf 0 "check_assumptions time: %12.6f  %12.6f sec"))
    <&
    solver_check_assumptions ctx slv a

  let model_eval ctx slv m a =
    Timer.start z3_eval_tmr ; (fun _ -> Timer.stop z3_eval_tmr) <&
    model_eval ctx slv m a

  let get_implied_equalities ctx slv a =
    Timer.start z3_get_implied_equalities_tmr ; (fun _ -> Timer.stop z3_get_implied_equalities_tmr) <&
    get_implied_equalities ctx slv a


  (* Implied Equalities ===================================================== *)

  let get_implied_equalities_naive ctx slv terms =
    let n = Array.length terms in
    let rep = Array.init n (fun i -> i) in
    match solver_check ctx slv with
    | L_TRUE ->
        for i = 0 to n-1 do
          for j = i+1 to n-1 do
            if is_eq_sort ctx (get_sort ctx terms.(i)) (get_sort ctx terms.(j)) then (
              solver_push ctx slv ;
              solver_assert ctx slv (mk_not ctx (mk_eq ctx terms.(i) terms.(j))) ;
              if solver_check ctx slv = L_FALSE then rep.(j) <- rep.(i) ;
              solver_pop ctx slv 1 ;
            )
          done
        done ;
        (L_TRUE, rep)
    | res ->
        (res, rep)


  exception Unknown

  (* asserts constraints, assumes client wraps call in push/pop *)
  let get_implied_equalities_refine ctx slv terms =
    assert( terms <> [||] )
    ;
    (* temporary table to forward values to the terms that have been chosen as their representatives *)
    let val_to_rep = PolyHMap.create 128
    in
    let new_rep ctx m terms current_partition i =
      match model_eval ctx m terms.(i) false with
      | Some(v) ->
          (try
            PolyHMap.find val_to_rep (current_partition, v)
          with Not_found ->
            PolyHMap.add val_to_rep (current_partition, v) terms.(i) ;
            terms.(i)
          )
      | None ->
          failwith "eval failed"
    in
    try
      (* check satisfiability *)
      match solver_check ctx slv with
      | L_TRUE ->
          let m = solver_get_model ctx slv
          in
          let n = Array.length terms
          in
          (* initialize representative array with equalities in model *)
          let rep = Array.init n (fun i -> new_rep ctx m terms 0 i)
          in
          PolyHMap.clear val_to_rep
          ;
          (* cache bool sort and true value *)
          let bool_sort = mk_bool_sort ctx
          and tt = mk_true ctx
          in
          (* initialize proposition heap *)
          let ph = Array.init (2*n-1) (fun _ -> mk_fresh_const ctx "phi" bool_sort)
          in
          (* assert that each internal node holds iff one of its children does *)
          let assert_internal i =
            let l = 2*i+1 in
            let r = l+1 in
            solver_assert ctx slv (mk_iff ctx ph.(i) (mk_or ctx [|ph.(l); ph.(r)|]))
          in
          for i = 0 to n-2 do assert_internal i done
          ;
          (* assert that each leaf node holds iff the corresponding term is not equal to its representative *)
          let assert_leaf i =
            let j = i - (n-1) in
            solver_assert ctx slv (mk_iff ctx ph.(i) (mk_not ctx (mk_eq ctx terms.(j) rep.(j))))
          in
          for i = n-1 to 2*n-2 do assert_leaf i done
          ;
          (* iteratively refine partition into equivalence classes determined by rep *)
          let rec loop () =
            (* assert that the partition is too coarse *)
            solver_assert ctx slv ph.(0) ;
            match solver_check ctx slv with
            | L_TRUE ->
                let m = solver_get_model ctx slv
                in
                (* at least one equation has been broken, refine partition based on new model *)
                let rec update i =
                  match model_eval ctx m ph.(i) false with
                  | Some(b) when is_eq_ast ctx tt b ->
                      (* some equality under i was broken *)
                      ph.(i) <- mk_fresh_const ctx "phi" bool_sort
                      ;
                      if i < n-1 then (
                        (* i is internal so consider its children *)
                        let l = 2*i+1 in
                        let r = l+1 in
                        update l ;
                        update r ;
                        assert_internal i
                      ) else (
                        (* i is a leaf so refine partition *)
                        let j = i - (n-1) in
                        rep.(j) <- new_rep ctx m terms (get_ast_id ctx rep.(j)) j ;
                        assert_leaf i
                      )
                  | Some(_) ->
                      (* no equality under i was broken *)
                      ()
                  | None ->
                      failwith "eval failed"
                in
                update 0
                ;
                PolyHMap.clear val_to_rep
                ;
                loop ()
            | L_FALSE ->
                (* no more equations can be broken, all equalities in rep are implied *)
                (L_TRUE, Array.map (fun e -> get_ast_id ctx e) rep)
            | L_UNDEF ->
                raise Unknown
          in
          loop ()
      | L_FALSE ->
          (* context is inconsistent, equate all terms of the same sort *)
          (L_FALSE, Array.map (fun e -> get_ast_id ctx (get_sort ctx e :>ast)) terms)
      | L_UNDEF ->
          raise Unknown
    with Unknown ->
      (* do not equate any terms *)
      (L_UNDEF, Array.mapi (fun i _ -> i) terms)

end



(*============================================================================
                       Translation from Exp.t to Z3.ast
  ============================================================================*)

(* This translation code assumes that the Z3 ast constructors return
   (logically) equivalent results for equal arguments, except for
   Z3.mk_fresh_*. *)

let is_val ({c={z= ctx}} as x) e = Z3.is_eq_sort ctx x.c.v_sort (Z3.get_sort ctx e)

let mk_v ({c={z= ctx}} as x) (b,y,i) = Z3.mk_app ctx x.c.v_ctor [|b;y;i|]

let is_val_app ({c={z= ctx}} as x) v =
  Z3.get_ast_kind ctx v = Z3.APP_AST
  && Z3.is_eq_func_decl ctx (Z3.get_app_decl ctx (Z3.to_app ctx v)) x.c.v_ctor

let get_v_loc ({c={z= ctx}} as x) v =
  if is_val_app x v then
    Z3.get_app_arg ctx (Z3.to_app ctx v) 0
  else
    Z3.mk_app ctx x.c.v_dtors.(0) [|v|]

let get_v_off ({c={z= ctx}} as x) v =
  if is_val_app x v then
    Z3.get_app_arg ctx (Z3.to_app ctx v) 1
  else
    Z3.mk_app ctx x.c.v_dtors.(1) [|v|]

let get_v_bit ({c={z= ctx}} as x) v =
  if is_val_app x v then
    Z3.get_app_arg ctx (Z3.to_app ctx v) 2
  else
    Z3.mk_app ctx x.c.v_dtors.(2) [|v|]

let get_val x e =
  if is_val x e then (get_v_loc x e, get_v_off x e, get_v_bit x e) else (e, x.c.zero, x.c.mone)

(* let get_int x e = *)
(*   if is_val x e then *)
(*     let e_l = get_v_loc x e and e_o = get_v_off x e in *)
(*     Z3.mk_add x [|e_l; e_o|] *)
(*   else *)
(*     e *)


let mk_var ({c={z= ctx}} as x) v =
  Z3.mk_const ctx (Z3.mk_int_symbol ctx (Var.id v))
    (match Var.sort v with
    | Var.PointerSort -> x.c.v_sort
    | Var.OffsetSort  -> x.c.v_sort
    | Var.IntegerSort -> x.c.i_sort
    | Var.BooleanSort -> x.c.b_sort)

let mk_vars ({c={z= ctx}} as x) vs =
  Array.of_list (Vars.fold (fun v vs' -> Z3.to_app ctx (mk_var x v) :: vs') vs [])

let mk_exists ({c={z= ctx}} as x) vs q =
  if Vars.is_empty vs then q else
  Z3.mk_exists_const ctx Config.quant_weight (mk_vars x vs) [||] q


let rec to_z3 x d =
  to_z3_ x (E.desc d)

and to_z3_ ({c={z= ctx}} as x) d = try
  match d with

  | E.Var(v) -> mk_var x v

  | E.App({HC.desc=E.App({HC.desc=E.Idx},f)},e) ->
      let e_l, e_o, e_b = get_val x (to_z3 x e) in
      let f' = to_z3 x f in
      mk_v x (e_l, Z3.mk_add x [|e_o; f'|], e_b)

  | E.App({HC.desc=E.Idx},e) ->
      to_z3 x (E.mkIdx E.nil e)

  | E.Idx ->
      to_z3 x (E.mkIdx E.nil E.zero)

  | E.App(f,e) ->
      let mk_o, f =
        match E.desc f with
        | E.Add(f) -> (Z3.mk_add, f)
        | E.Sub(f) -> (Z3.mk_sub, f)
        | _ -> failwithf "to_z3: unexpected operand: %a" E.fmt f
      and e_l, e_o, _ = get_val x (to_z3 x e) in
      let f_o, f_b = Fld.off f in
      let f_o' = Z3.mk_int ctx f_o x.c.i_sort in
      let f_b' = Option.option x.c.mone (fun b -> Z3.mk_int ctx b x.c.i_sort) f_b in
      mk_v x (e_l, mk_o x [|e_o; f_o'|], f_b')

  | E.Nil -> Z3.mk_int ctx 0 x.c.i_sort

  | E.Add(f) -> to_z3 x (E.mkAdd (E.mkBas (Fld.typ f)) f)

  | E.Sub(f) -> to_z3 x (E.mkSub (E.mkBas (Fld.typ f)) f)

  | E.Bas(_) -> Z3.mk_int ctx 0 x.c.i_sort

  | E.Eq(e,f) ->
      let e' = to_z3 x e in
      let f' = to_z3 x f in
      let e_l, e_o, e_b = get_val x e'
      and f_l, f_o, f_b = get_val x f' in
      Z3.mk_and ctx [|Z3.mk_eq ctx e_l f_l; Z3.mk_eq ctx e_o f_o; Z3.mk_eq ctx e_b f_b|]

  | E.Num(n) -> Z3.mk_int64 ctx n x.c.i_sort

  | E.Str(s) -> Z3.mk_const ctx (Z3.mk_string_symbol ctx ("\""^s^"\"")) x.c.i_sort

  | E.Op1(E.Allocd,e) ->
      let e' = to_z3_ x e in
      if is_val x e' then
        Z3.mk_and ctx [|Z3.mk_app ctx x.c.allocd [|e'|];
                        Z3.mk_gt ctx (get_v_loc x e') x.c.zero;
                        Z3.mk_ge ctx (get_v_off x e') x.c.zero;
                        Z3.mk_ge ctx (get_v_bit x e') x.c.mone|]
      else
        Z3.mk_and ctx [|Z3.mk_app ctx x.c.allocd [|mk_v x (e', x.c.zero, x.c.mone)|];
                        Z3.mk_gt ctx e' x.c.zero|]

  | E.Op1(E.Not,b) -> Z3.mk_not ctx (to_z3_ x b)

  | E.Op1(E.ZMin,e) ->
      let e' = to_z3_ x e in
      assert( not (is_val x e') );
      Z3.mk_unary_minus ctx e'

  | E.Op2(o,e,f) ->
      let mk_o =
        match o with
        | E.ZDiv -> Z3.mk_div
        | E.ZRem -> Z3.mk_rem
        | E.ZMod -> Z3.mk_mod
        | E.ZLt  -> Z3.mk_lt
        | E.ZLe  -> Z3.mk_le
        | E.ZGt  -> Z3.mk_gt
        | E.ZGe  -> Z3.mk_ge in
      let e' = to_z3_ x e in
      let f' = to_z3_ x f in
(*       assert( not (is_val x e') ); *)
(*       assert( not (is_val x f') ); *)
      (* Note: Once integers and pointers are correctly distinguished, use the
         preceding assertions and remove all following cases but the last. *)
      if is_val x e' then
        if is_val x f' then
          Z3.mk_and ctx [|mk_o ctx (get_v_loc x e') (get_v_loc x f');
                          Z3.mk_eq ctx (get_v_off x e') x.c.zero;
                          Z3.mk_eq ctx (get_v_off x f') x.c.zero;
                          Z3.mk_eq ctx (get_v_bit x e') x.c.mone;
                          Z3.mk_eq ctx (get_v_bit x f') x.c.mone|]
        else
          Z3.mk_and ctx [|mk_o ctx (get_v_loc x e') f';
                          Z3.mk_eq ctx (get_v_off x e') x.c.zero;
                          Z3.mk_eq ctx (get_v_bit x e') x.c.mone|]
      else
        if is_val x f' then
          Z3.mk_and ctx [|mk_o ctx e' (get_v_loc x f');
                          Z3.mk_eq ctx (get_v_off x f') x.c.zero;
                          Z3.mk_eq ctx (get_v_bit x f') x.c.mone|]
        else
          mk_o ctx e' f'

  | E.Op3(E.Ite,g,t,e) ->
      let g' = to_z3_ x g in
      let t' = to_z3_ x t in
      let e' = to_z3_ x e in
      (match is_val x t', is_val x e' with
      | true, false -> Z3.mk_ite ctx g' t' (mk_v x (e', x.c.zero, x.c.mone))
      | false, true -> Z3.mk_ite ctx g' (mk_v x (t', x.c.zero, x.c.mone)) e'
      | _           -> Z3.mk_ite ctx g' t' e'
      )
  | E.OpN(E.Distinct,el) ->
      let el' =
        Array.map (fun e ->
          let e' = to_z3_ x e in
          if is_val x e' then e' else mk_v x (e', x.c.zero, x.c.mone)
        ) el
      in Z3.mk_distinct ctx el'

  | E.OpN(E.And,cn) -> Z3.mk_and ctx (Array.map (to_z3_ x) cn)

  | E.OpN(E.Or,dn)  -> Z3.mk_or  ctx (Array.map (to_z3_ x) dn)

  | E.OpN((E.ZAdd | E.ZMul) as o, el) ->
      let mk_o = match o with E.ZAdd -> Z3.mk_add | E.ZMul -> Z3.mk_mul | _ -> assert false in
      let el' =
        Array.map (fun e ->
          let e' = to_z3_ x e in
          assert( not (is_val x e') || failwithf "to_z3 unexpected Value: %a" E.fmt (E.name e) );
          e'
        ) el
      in mk_o x el'

  | E.OpN(E.UFun(s),el) ->
      let el' =
        Array.map (fun e ->
          let e' = to_z3_ x e in
          assert( not (is_val x e') );
          e'
        ) el in
      let arg_sorts = Array.make (Array.length el') x.c.i_sort in
      Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx s) arg_sorts x.c.i_sort) el'

  with exc -> L.printf 0 "to_z3:@ %a" E.fmt (E.name d); raise exc


let to_z3 x e =
  let e' = to_z3 x e in
  match E.sort_of e with
  | (Var.PointerSort | Var.IntegerSort | Var.OffsetSort) when not (is_val x e') -> mk_v x (e', x.c.zero, x.c.mone)
  | _ -> e'



(*============================================================================
                      Contexts, aka Imperative Formulas
  ============================================================================*)

(* Constructors ============================================================= *)

let mk_partition {c={z= ctx}} =
  Z3.mk_fresh_const ctx "partition" (Z3.mk_bool_sort ctx)


let mk () =
  let ctx = Z3.mk_context []
  in
  let slv =
    if Config.pur_eager_qe then
      Z3.mk_simple_solver ctx
    else
      Z3.mk_solver_from_tactic ctx (Z3.tactic_and_then ctx (Z3.mk_tactic ctx "qe") (Z3.mk_tactic ctx "smt"))
  in
  let i_sort = Z3.mk_int_sort ctx
  and b_sort = Z3.mk_bool_sort ctx
  in
  let v_sort, v_ctor, v_dtors =
    Z3.mk_tuple_sort ctx (Z3.mk_string_symbol ctx "Val")
      [|Z3.mk_string_symbol ctx "get_loc"; Z3.mk_string_symbol ctx "get_off"; Z3.mk_string_symbol ctx "get_bit"|]
      [|i_sort; i_sort; i_sort|]
  in
  let t_sort = Z3.mk_uninterpreted_sort ctx (Z3.mk_string_symbol ctx "Typ")
  in
  let allocd = Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "allocd") [|v_sort|] b_sort
  and zero = Z3.mk_int ctx 0 i_sort
  and one = Z3.mk_int ctx 1 i_sort
  and mone = Z3.mk_int ctx (-1) i_sort
  in
  let p_sort = Z3.mk_uninterpreted_sort ctx (Z3.mk_string_symbol ctx "Ptr")
  and u_sort = Z3.mk_uninterpreted_sort ctx (Z3.mk_string_symbol ctx "Int")
  in
  let params = Z3.mk_params ctx in
  Z3.params_set_bool ctx params (Z3.mk_string_symbol ctx "model") true ;
  if Config.z3_timeout > 0 then
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "timeout") Config.z3_timeout ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "verbose") (max 0 (!Config.vZ3 - 1)) ;
  Z3.params_set_bool ctx params (Z3.mk_string_symbol ctx "unsat_core") true ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "memory_high_watermark") Config.z3_memout ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "memory_max_size") Config.z3_memout ;
  Z3.params_set_double ctx params (Z3.mk_string_symbol ctx "sat.random_freq") 0. ;
  Z3.params_set_bool ctx params (Z3.mk_string_symbol ctx "nlsat.randomize") false ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "nlsat.seed") 0 ;
  Z3.params_set_bool ctx params (Z3.mk_string_symbol ctx "smt.arith.nl") false ;
  Z3.params_set_bool ctx params (Z3.mk_string_symbol ctx "smt.ematching") Config.z3_ematching ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "smt.random_seed") 0 ;
  Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "smt.relevancy") Config.z3_relevancy ;
  Z3.solver_set_params ctx slv params ;
  (* Set print mode *)
  Z3.set_ast_print_mode ctx
    (match Config.z3_print_mode with
    | "full" -> Z3.PRINT_SMTLIB_FULL
    | "low"  -> Z3.PRINT_LOW_LEVEL
    | "smt"  -> Z3.PRINT_SMTLIB_COMPLIANT
    | "smt2" -> Z3.PRINT_SMTLIB2_COMPLIANT
    | s -> failwithf "unrecognized print mode: " s
    );
  { a= []
  ; c={
    z= ctx; s= slv;
    i_sort; v_sort; v_ctor; v_dtors; b_sort;
    t_sort; allocd; zero; one; mone;
    clear_count= 0;
    p_sort; u_sort;
  }}


let clear ({c={z= ctx; s= slv}} as x) =
  x.c.clear_count <- x.c.clear_count + 1 ;
  if x.c.clear_count <> Config.reset_freq then (
    Z3.solver_reset ctx slv
  ) else (
    let y = mk () in
    x.c.z <- y.c.z ;
    x.c.s <- y.c.s ;
    x.c.i_sort <- y.c.i_sort ;
    x.c.v_sort <- y.c.v_sort ;
    x.c.v_ctor <- y.c.v_ctor ;
    x.c.v_dtors <- y.c.v_dtors ;
    x.c.b_sort <- y.c.b_sort ;
    x.c.t_sort <- y.c.t_sort ;
    x.c.allocd <- y.c.allocd ;
    x.c.zero <- y.c.zero ;
    x.c.one <- y.c.one ;
    x.c.mone <- y.c.mone ;
    x.c.clear_count <- y.c.clear_count ;
    x.c.p_sort <- y.c.p_sort ;
    x.c.u_sort <- y.c.u_sort ;
  )



let extend ({c={z= ctx}} as x) =
  {x with a= (Z3.mk_fresh_const ctx "hyp" (Z3.mk_bool_sort ctx)) :: x.a}



let conjoin ?(parts=[]) ({c={z= ctx; s= slv}} as x) bex =
  assert(true$> L.printf 10 "Pure.conjoin: %a" E.fmt bex );
  Timer.start conjoin_tmr ; (fun _ -> Timer.stop conjoin_tmr)
  <&
  let cnstr = to_z3 x bex in
(*   L.printf 0 "Pure.conjoin: %a" (Z3.fmt_ast ctx) cnstr ; *)
  let assumptions = assumptions x parts in
  let imp_cnstr =
    if assumptions = [] then cnstr else
    Z3.mk_implies ctx (Z3.mk_and ctx (Array.of_list assumptions)) cnstr in
  Z3.solver_assert ctx slv imp_cnstr



(*============================================================================
                               Logical Queries
  ============================================================================*)


let report_eval ({c={z= ctx; s= slv}} as x) assumptions bex is_unsat = assert(true$>
  if not Config.z3_model then () else
  match is_unsat with
  | Some(false) ->
      (match Z3.solver_check_assumptions ctx slv assumptions with
      | Z3.L_TRUE ->
          let m = Z3.solver_get_model ctx slv in
          L.printf 30 "model:@\n%s" (Z3.model_to_string ctx m) ;
          (* keep only subexps that evaluate to false *)
          let bex_ff =
            let ff = Z3.mk_false ctx in
            E.map (fun b ->
              if not (E.is_boolean b) then b
              else match Z3.model_eval ctx m (to_z3 x b) false with
              | Some(b') when Z3.is_eq_ast ctx ff (Z3.simplify ctx b') ->
                  b
              | _ ->
                  E.tt
            ) bex
          in
          (* keep only subexps that evaluate to non-true *)
          let bex_tt =
            let tt = Z3.mk_true ctx in
            E.map (fun b ->
              if not (E.is_boolean b) then b
              else match Z3.model_eval ctx m (to_z3 x b) false with
              | Some(b') when Z3.is_eq_ast ctx tt (Z3.simplify ctx b') ->
                  E.tt
              | _ ->
                  b
            ) bex
          in
          let es =
            E.fold (fun e es ->
              match E.desc e with
              | E.Num _ -> es
              | E.Op1 _ -> Exps.add e es
              | E.Var _ when E.is_boolean e -> Exps.add e es
              | _ when not (E.is_boolean e) -> Exps.add e es
              | _ -> es
            ) bex Exps.empty in
          let es' =
            Exps.fold (fun e es' ->
              (* Note: once types are correct, remove this exception handling *)
              try match Z3.model_eval ctx m (to_z3 x e) false with
              | Some(e') -> (e, Z3.simplify ctx e') :: es'
              | _ -> es'
              with Z3.Error _ -> es'
            ) es [] in
          L.printf 3 "bex_ff: %a" E.fmt bex_ff ;
          L.printf 4 "bex_tt: %a@\n@[%a@]" E.fmt bex_tt
            (List.fmt "@\n" (fun ff (e,e') -> Format.fprintf ff "%a = @[%a@]" E.fmt e (Z3.fmt_ast ctx) e')) es'
      | _ -> ()
      )
  | Some(true) ->
      (match Z3.solver_check_assumptions ctx slv assumptions with
      | Z3.L_FALSE ->
          let core_vec = Z3.solver_get_unsat_core ctx slv in
          let core =
            Array.init (Z3.ast_vector_size ctx core_vec) (fun i -> Z3.ast_vector_get ctx core_vec i) in
          L.printf 3 "core: %a"
            (List.fmt " " (fun ff e -> Z3.fmt_ast ctx ff e))
            (Array.to_list core)
      | Z3.L_TRUE ->
          L.printf 0 "spontaneous consistency"
      | Z3.L_UNDEF ->
          L.printf 0 "report_eval got undef"
      )
  | None ->
      ()
)


let inconsistent ?(parts=[]) ({c={z= ctx; s= slv}} as x) =
  assert(true$>(
    L.incf 1 "( Pure.inconsistent: %i" !Z3.test_count ;
    L.printf 10 "assertions:@\n%a" (Z3.fmt_cnstrs ~parts) x ;
(*     Z3.gen_test ~parts x (Z3.mk_and ctx (Array.of_list *)
(*       (assumptions x parts))) *))) ;
    Timer.start inconsistent_tmr ;
  (fun a ->
    Timer.stop inconsistent_tmr ; assert(true$>(
    report_eval x (Array.of_list (assumptions x parts)) E.tt (Some(a)) ;
    L.decf 1 ") Pure.inconsistent: %B" a)))
  <&
  ((Z3.solver_check_assumptions ctx slv (Array.of_list (assumptions x parts))) = Z3.L_FALSE)


let impliesx ?(parts=[]) ({c={z= ctx; s= slv}} as x) (xs,bex) =
  if E.equal bex E.ff then
    Some(inconsistent ~parts x)
  else if E.equal bex E.tt then
    Some(true)
  else (
    assert(true$>(
      L.incf 1 "( Pure.implies@[<hv>x: %i @[%a%a@]@]" !Z3.test_count fmt_xs xs E.fmt bex ));
      Timer.start implies_tmr ;
    (fun a ->
      Timer.stop implies_tmr ; assert(true$>(
      L.decf 1 ") Pure.impliesx: %a" (Option.fmt "failed" Format.pp_print_bool) a)))
    <&
    let concl = Z3.mk_fresh_const ctx "concl" x.c.b_sort in
    let not_xs_bex' = Z3.mk_not ctx (mk_exists x xs (to_z3 x bex)) in
    let cnstr = Z3.mk_implies ctx concl not_xs_bex' in
    if Config.pur_eager_qe then
      let qe_tac = Z3.mk_tactic ctx "qe" in
      let qe_goal = Z3.mk_goal ctx false false false in
      Z3.goal_assert ctx qe_goal cnstr ;
      let ar = Z3.tactic_apply ctx qe_tac qe_goal in
      Z3.goal_reset ctx qe_goal ;
      let n = Z3.apply_result_get_num_subgoals ctx ar in
      let rec assert_subgoals i =
        if i < n then
          let subgoal = Z3.apply_result_get_subgoal ctx ar i in
          let formulas = Array.init (Z3.goal_size ctx subgoal) (Z3.goal_formula ctx subgoal) in
          Z3.solver_assert ctx slv (Z3.mk_or ctx formulas) ;
          assert_subgoals (i+1)
      in
      assert_subgoals 0
    else
      Z3.solver_assert ctx slv cnstr ;
    assert(true$>(
      L.printf 10 "assertions:@\n%a" (Z3.fmt_cnstrs ~parts) x ;
      L.printf 10 "not_xs_bex':@\n%a" (Z3.fmt_ast ctx)
        ((* Z3.simplify ctx *) not_xs_bex') ));
(*     Z3.gen_test ~parts x (Z3.mk_and ctx (Array.of_list (not_xs_bex' :: assumptions x parts))) ; *)
    let assumptions = Array.of_list (concl :: assumptions x parts) in
    match Z3.solver_check_assumptions ctx slv assumptions with
    | Z3.L_UNDEF -> None
    | Z3.L_FALSE -> Some(true)
    | Z3.L_TRUE  -> Some(false)
        &> report_eval x assumptions bex
  )

let implies ?parts x bex = impliesx ?parts x (Vars.empty, bex)



type find_provable_equality_t =
  | Inconsistent
  | Equality of Exp.t * Exp.t
  | Disjunctions of (Exp.t * Exp.t) list list


let find_provable_equality ?(parts=[]) ({c={z= ctx; s= slv}} as x) m_locs s_locs keep =
    Timer.start find_provable_equality_tmr ;
  (fun _ ->
    Timer.stop find_provable_equality_tmr )
  <&
  let module AstSet = Set.Make(struct
      type t = Z3.ast
      let compare e f = Pervasives.compare (Z3.get_ast_id ctx e) (Z3.get_ast_id ctx f)
      let equal e f = Z3.is_eq_ast ctx e f
    end) in

  let eqs =
    Exps.fold (fun m_loc eqs ->
      Exps.fold (fun s_loc eqs ->
        if keep m_loc s_loc then (m_loc, s_loc) :: eqs else eqs
      ) s_locs eqs
    ) m_locs [] in

  (* check if there are any equalities to try *)
  if eqs = [] then
    Disjunctions([])
  else

  (* check if all equalities are vacuously implied *)
  if Z3.solver_check_assumptions ctx slv (Array.of_list (assumptions x parts)) = Z3.L_FALSE then
    Inconsistent
  else let()=()in

  (* Note: remove this push/pop, add assumptions to others and adjust *)
  Z3.solver_push ctx slv ;

  List.iter (fun f -> Z3.solver_assert ctx slv f) (assumptions x parts) ;

  let dq_name_to_locs = PolyHMap.create 127 in (* Note: tune *)

  (* assert conditional disequalities *)
  let dq_names =
    List.fold (fun (m_loc, s_loc) dq_names ->
      let m_loc' = to_z3 x m_loc in
      let s_loc' = to_z3 x s_loc in
      (* generate a literal 'name' for [m_loc != s_loc] *)
      let str = if !Config.vPure <= 1 then "dq_name" else Format.asprintf "%a" E.fmt (E.mkDq m_loc s_loc) in
      let dq_name = Z3.mk_fresh_const ctx str x.c.b_sort in
      (* assert [!dq_name \/ m_loc != s_loc] so that assuming [dq_name]
         forces [m_loc != s_loc] *)
      Z3.solver_assert ctx slv
        (Z3.mk_implies ctx dq_name
           (Z3.mk_not ctx (Z3.mk_eq ctx m_loc' s_loc'))) ;
      PolyHMap.add dq_name_to_locs dq_name (m_loc, s_loc) ;
      AstSet.add dq_name dq_names
    ) eqs AstSet.empty in

  let rec search cores dq_names =
(*     L.printf 0 "Pure.search: %a" *)
(*       (List.fmt " " (fun ff e -> Z3.fmt_ast ctx ff e)) *)
(*       (AstSet.to_list dq_names) ; *)
    if AstSet.is_empty dq_names then
      Disjunctions(cores)
    else
      let assumptions = AstSet.to_array dq_names in
      if Z3.solver_check_assumptions ctx slv assumptions = Z3.L_FALSE then
        let core = Z3.solver_get_unsat_core ctx slv in
        let rec fold_range fn i j z =
          if i >= j then z else fold_range fn (i+1) j (fn (Z3.ast_vector_get ctx core i) z)
        in
        (match Z3.ast_vector_size ctx core with
        | 1 ->
            (* the negated disequality named by core.(0) is implied *)
            let m_loc, s_loc = PolyHMap.find dq_name_to_locs (Z3.ast_vector_get ctx core 0) in
(*             L.printf 0 "proved: %a = %a" E.fmt m_loc E.fmt s_loc ; *)
            Equality(m_loc, s_loc)
        | core_len when core_len > 1 ->
            (* the disjunction of negated disequalities in core is implied,
               perform binary search over core to try to find one that is
               implied, if not remove core from dq_names and search again *)
(*             L.printf 0 "core: %a" (List.fmt " " (fun ff e -> Z3.fmt_ast ctx ff e)) *)
(*               (fold_range List.cons 0 core_len []) ; *)
            let m = core_len / 2 in
            let subrange0 = fold_range AstSet.add 0 m AstSet.empty in
            (match search cores subrange0 with
            | Equality _ as res -> res
            | _ ->
                let subrange1 = fold_range AstSet.add m core_len AstSet.empty in
                match search cores subrange1 with
                | Equality _ as res -> res
                | _ ->
                    let core' =
                      fold_range (fun core_i core' ->
                        PolyHMap.find dq_name_to_locs core_i :: core'
                      ) 0 core_len [] in
                    let cores = core' :: cores in
                    search cores (fold_range AstSet.remove 0 core_len dq_names)
            )
        | core_len -> failwithf "Z3 bug: unsat core length %i < 1" core_len
        )
      else
(*         L.printf 0 "failed" ; *)
        Disjunctions(cores)
  in
  let res = search [] dq_names in
  Z3.solver_pop ctx slv 1 ;
  res




let rec to_z3_weak x d =
  if not Config.gie_weak then to_z3 x d else
  to_z3_weak_ x (E.desc d)

and to_z3_weak_ ({c={z= ctx}} as x) d = try
  match d with

  | E.Var(v) ->
      Z3.mk_const ctx (Z3.mk_int_symbol ctx (Var.id v))
        (match Var.sort v with
        | Var.PointerSort -> x.c.p_sort
        | Var.OffsetSort  -> x.c.p_sort
        | Var.IntegerSort -> x.c.u_sort
        | Var.BooleanSort -> x.c.b_sort)

  | E.App({HC.desc= E.Add(f)},e) ->
      let sym = Z3.mk_string_symbol ctx ("fld_const_"^(Fld.name f)) in
      let f' = Z3.mk_func_decl ctx sym [|x.c.p_sort|] x.c.p_sort in
      Z3.mk_app ctx f' [|to_z3_weak x e|]

  | E.App({HC.desc= E.Sub(f)},e) ->
      let sym = Z3.mk_string_symbol ctx ("fld_const_"^(Fld.name f)) in
      let f' = Z3.mk_func_decl ctx sym [|x.c.p_sort|] x.c.p_sort in
      let inv = Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "inv") [|x.c.p_sort|] x.c.p_sort in
      Z3.mk_app ctx inv [|Z3.mk_app ctx f' [|to_z3_weak x e|]|]

  | E.App({HC.desc= E.App({HC.desc= E.Idx}, i)}, a) ->
      let sym = Z3.mk_string_symbol ctx "idx" in
      let f' = Z3.mk_func_decl ctx sym [|x.c.p_sort; x.c.i_sort|] x.c.p_sort in
      Z3.mk_app ctx f' [|to_z3_weak x a; to_z3_weak x i|]

  | E.App _ | E.Idx -> failwithf "to_z3_weak_: malformed expression: %a" E.fmt (E.name d)

  | E.Nil ->
      Z3.mk_const ctx (Z3.mk_string_symbol ctx "nil") x.c.p_sort

  | E.Add(f) -> to_z3_weak x (E.mkAdd (E.mkBas (Fld.typ f)) f)
  | E.Sub(f) -> to_z3_weak x (E.mkSub (E.mkBas (Fld.typ f)) f)

  | E.Bas(ty) ->
      let eps = Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "eps") [|x.c.t_sort|] x.c.p_sort in
      let sym = Z3.mk_string_symbol ctx ("typ_const_"^(string_of_int (Typ.id ty))) in
      let ty' = Z3.mk_const ctx sym x.c.t_sort in
      Z3.mk_app ctx eps [|ty'|]

  | E.Eq(e,f) -> Z3.mk_eq ctx (to_z3_weak x e) (to_z3_weak x f)

  | E.OpN(E.Distinct,es) -> Z3.mk_distinct ctx (Array.map (fun e -> to_z3_weak_ x e) es)

  | E.Op1(E.Not,e)  -> Z3.mk_not ctx (to_z3_weak_ x e)
  | E.OpN(E.And,cn) -> Z3.mk_and ctx (Array.map (to_z3_weak_ x) cn)
  | E.OpN(E.Or,dn)  -> Z3.mk_or  ctx (Array.map (to_z3_weak_ x) dn)

  | E.Op3(E.Ite,g,t,E.Num(n)) when E.is_pointer (E.name t) && n < 0L ->
      let name = "ptr_const_"^(Int64.to_string (Int64.neg n)) in
      let n' = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) x.c.p_sort in
      Z3.mk_ite ctx (to_z3_weak_ x g) (to_z3_weak_ x t) n'

  | E.Op3(E.Ite,g,t,e) -> Z3.mk_ite ctx (to_z3_weak_ x g) (to_z3_weak_ x t) (to_z3_weak_ x e)

  | E.Op1(E.Allocd,e) ->
      Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "Allocd") [|x.c.p_sort|] x.c.b_sort)
        [|to_z3_weak_ x e|]

  | E.Num(n) ->
      Z3.mk_const ctx (Z3.mk_string_symbol ctx ("int_const_"^(Int64.to_string n))) x.c.u_sort

  | E.Str(s) ->
      Z3.mk_const ctx (Z3.mk_string_symbol ctx ("\""^s^"\"")) x.c.p_sort

  | E.Op1(E.ZMin,e) ->
      let o' = Z3.mk_string_symbol ctx "ZMin" in
      Z3.mk_app ctx (Z3.mk_func_decl ctx o' [|x.c.u_sort|] x.c.u_sort) [|to_z3_weak_ x e|]

  | E.Op2(o,e,f) ->
      let name =
        match o with
        | E.ZDiv -> "ZDiv" | E.ZRem -> "ZRem" | E.ZMod -> "ZMod"
        | E.ZLt -> "ZLt" | E.ZLe -> "ZLe" | E.ZGt -> "ZGt" | E.ZGe -> "ZGe" in
      let o' = Z3.mk_string_symbol ctx name in
      let sort =
        match o with
        | E.ZDiv | E.ZRem | E.ZMod -> x.c.u_sort
        | E.ZLt | E.ZLe | E.ZGt | E.ZGe -> x.c.b_sort in
      Z3.mk_app ctx (Z3.mk_func_decl ctx o' [|x.c.u_sort; x.c.u_sort|] sort)
        [|to_z3_weak_ x e; to_z3_weak_ x f|]

  | E.OpN(o,es) ->
      let name =
        match o with
        | E.ZAdd -> "ZAdd" | E.ZMul -> "ZMul" | E.UFun(s) -> s
        | E.Distinct | E.And | E.Or -> assert false in
      let o' = Z3.mk_string_symbol ctx name in
      Z3.mk_app ctx (Z3.mk_func_decl ctx o' (Array.map (fun _ -> x.c.u_sort) es) x.c.u_sort)
        (Array.map (fun e -> to_z3_weak_ x e) es)

  with exc -> L.printf 20 "to_z3_weak:@ %a" E.fmt (E.name d) ; raise exc


let conjoin_weak ?(parts=[]) ({c={z= ctx; s= slv}} as x) bex =
  assert(true$> L.printf 10 "Pure.conjoin_weak: %a" E.fmt bex );
  Timer.time conjoin_tmr @@fun()->
  let cnstr = to_z3_weak x bex in
(*   L.printf 0 "Pure.conjoin_weak: %a" (Z3.fmt_ast ctx) cnstr ; *)
  let assumptions = assumptions x parts in
  let imp_cnstr =
    if assumptions = [] then cnstr else
    Z3.mk_implies ctx (Z3.mk_and ctx (Array.of_list assumptions)) cnstr in
  Z3.solver_assert ctx slv imp_cnstr


let get_implied_equalities x assumptions terms =
  match terms with
  | [||]  -> Some([||])
  | [|_|] -> Some([|0|])
  | _ ->
      let ({c={z= ctx; s= slv}} as x) =
        if Config.gie_incremental then x
        else
          let {c={z= ctx0}} = x in
          let ({c={z= ctx; s= slv}} as y) = mk () in
          List.iter (fun f ->
            Z3.solver_assert ctx slv (Z3.translate ctx0 f ctx)
          ) (assertions x []) ;
          y
      in
      let assumptions = Array.map (to_z3_weak x) assumptions in
      let terms = Array.map (to_z3_weak x) terms in
      match terms with
      | [|e;f|] when Z3.is_eq_sort x.c.z (Z3.get_sort x.c.z e) (Z3.get_sort x.c.z f) ->
          Z3.solver_push ctx slv ;
          Array.iter (Z3.solver_assert ctx slv) assumptions ;
          Z3.solver_assert ctx slv (Z3.mk_not ctx (Z3.mk_eq ctx e f)) ;
          (match Z3.solver_check ctx slv with
          | Z3.L_UNDEF -> None
          | Z3.L_FALSE -> Some([|0;0|])
          | Z3.L_TRUE  -> Some([|0;1|])
          ) $>
          Z3.solver_pop ctx slv 1
      | [|_;_|] ->
          Some([|0;1|])
      | _ ->
          assert(true$>
            L.incf 10 "( get_implied_equalities:@ @[%a@]" (List.fmt "@ " (Z3.fmt_ast ctx)) (Array.to_list terms) );
(*           let {Timer.max= max0; count= count0} = z3_check_tmr in *)
(*           z3_check_tmr.Timer.max <- 0. ; *)
          Timer.start get_implied_equalities_tmr ;

          Z3.solver_push ctx slv ;
          Array.iter (fun a -> Z3.solver_assert ctx slv a) assumptions ;
          let sat, ids =
            match Config.gie with
            | 0 -> Z3.get_implied_equalities_naive ctx slv terms
            | 1 -> Z3.get_implied_equalities ctx slv terms
            | 2 -> Z3.get_implied_equalities_refine ctx slv terms
            | _ -> invalid_arg "unrecognized get_implied_equalities implementation"
          in
          Z3.solver_pop ctx slv 1 ;

          Timer.stop get_implied_equalities_tmr ;
(*           let {Timer.max= max1; count= count1} = z3_check_tmr in *)
(*           z3_check_tmr.Timer.max <- max max0 max1 ; *)
(*           Timer.log get_implied_equalities_tmr gie_log *)
(*             ( (match sat with Z3.L_TRUE -> "SAT" | Z3.L_FALSE -> "UNSAT" | Z3.L_UNDEF -> "UNKNOWN") *)
(*             ^ "\t" ^ (string_of_int (Array.length terms)) *)
(*             ^ "\t" ^ (string_of_int (count1 - count0)) *)
(*             ^ "\t" ^ (string_of_float (max1 *. 1000.)) ) ; *)

          assert(true$>
            if sat <> Z3.L_TRUE then
              L.decf 10 ") get_implied_equalities:@ %a" Z3.fmt_lbool sat
            else
              let n = Array.length terms in
              let ast_to_idx = PolyHMap.create n in
              let id_to_class = IntHMMap.create n in
              Array.iteri (fun i e ->
                PolyHMap.add ast_to_idx e i ;
                IntHMMap.add id_to_class ids.(i) e
              ) terms ;
              let _, rep =
                Array.fold_left (fun (i, rep) e ->
                  let cmp_ast x y = compare (PolyHMap.find ast_to_idx x) (PolyHMap.find ast_to_idx y) in
                  (i+1, (e, List.hd (List.sort cmp_ast (IntHMMap.find id_to_class ids.(i)))) :: rep)
                ) (0, []) terms in
              L.decf 10 ") get_implied_equalities:@ @[%a@]"
                (fun ff ->
                  let aux ff (e,f) = Format.fprintf ff "@[%a/%a@]" (Z3.fmt_ast ctx) f (Z3.fmt_ast ctx) e in
                  Format.fprintf ff "@[<hov 1>[%a]@]" (List.fmt ",@ " aux)
                ) rep ;
              if Config.check_gie then (
                Z3.solver_push ctx slv ;
                Array.iter (Z3.solver_assert ctx slv) assumptions ;
                for i = 0 to n-1 do
                  for j = i+1 to n-1 do
                    if Z3.is_eq_sort ctx (Z3.get_sort ctx terms.(i)) (Z3.get_sort ctx terms.(j)) then (
                      Z3.solver_push ctx slv ;
                      Z3.solver_assert ctx slv (Z3.mk_not ctx (Z3.mk_eq ctx terms.(i) terms.(j))) ;
                      let naive_proved_i_eq_j = (Z3.solver_check ctx slv = Z3.L_FALSE) in
                      let gie_proved_i_eq_j = (ids.(i) = ids.(j)) in
                      Z3.solver_pop ctx slv 1 ;
                      match naive_proved_i_eq_j, gie_proved_i_eq_j with
                      | true, false ->
                          failwithf "gie incomplete for %a = %a"
                            (Z3.fmt_ast ctx) terms.(i) (Z3.fmt_ast ctx) terms.(j)
                      | false, true ->
                          failwithf "gie unsound for %a = %a"
                            (Z3.fmt_ast ctx) terms.(i) (Z3.fmt_ast ctx) terms.(j)
                      | _ -> ()
                    )
                  done
                done ;
                Z3.solver_pop ctx slv 1
              )
          );

          if sat = Z3.L_UNDEF then
            None
          else
            Some(ids)
