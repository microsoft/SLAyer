(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Expression
module E = Exp
open SymbolicHeap
module D = Discovery

let log = Log.std Prover.verbose
let printf x = Log.printf log x

let fmt lab ff (us,p,xs,q) =
  Format.fprintf ff "%s@[: @[%a%a@]@\n\\ @[%a%a@]@]" lab
    (Vars.fmt_embrace "@[<hov 2>! " " .@]@ ") us SH.fmt p
    (Vars.fmt_embrace "@[<hov 2>? " " .@]@ ") xs SH.fmt q


(*============================================================================
                                 Test running
  ============================================================================*)

let test_reachable name expect xs_sh rootset e =
  let _,sh = XSH.exists_bind Vars.empty xs_sh in
  let keep l = Exps.mem l rootset in
  let sh, closure = Reachability.reachable keep sh in
  let is_reachable l =
    keep l || Exps.mem l (closure sh)
  in
  (try Printexc.print (fun()-> is_reachable e) ()
  with exc -> if !Config.stop then raise exc else not expect
  )|>
  (fun b -> printf 0 "%s: %s" name (if b = expect then "PASS" else "FAIL"))





(*============================================================================
                                    Tests
  ============================================================================*)

open Prover_test


let test () =
  printf 0 "testing Frame.reachable" ;

(*   let _d = [E.Pos,E.mkFld(Var.gensym "D" ())] in *)
(*   let _o = [E.Pos,E.mkFld(Var.gensym "O" ())] in *)
(*   let _flink = [E.Pos,E.mkFld(Var.gensym "Flink" ())] in *)

  let f' = Var.gensym "f" E.OffsetSort in let _f = E.mkVar f' in
  let g' = Var.gensym "g" E.ValueSort in let g = E.mkVar g' in
  let j' = Var.gensym "j" E.ValueSort in let j = E.mkVar j' in
  let k' = Var.gensym "k" E.ValueSort in let _k = E.mkVar k' in
  let l' = Var.gensym "l" E.ValueSort in let _l = E.mkVar l' in
  let r' = Var.gensym "r" E.RecordSort in let _r = E.mkVar r' in
  let r0' = Var.gensym "r0" E.RecordSort in let r0 = E.mkVar r0' in
  let r1' = Var.gensym "r1" E.RecordSort in let r1 = E.mkVar r1' in
  let v' = Var.gensym "v" E.ValueSort in let _v = E.mkVar v' in
  let w' = Var.gensym "w" E.ValueSort in let w = E.mkVar w' in
  let x = E.mkVar (Var.gensym "x" E.ValueSort) in
  let y = E.mkVar (Var.gensym "y" E.ValueSort) in
  let z' = Var.gensym "z" E.ValueSort in let z = E.mkVar z' in

  (*
    PS #375 01/10/2009 23:33, with h_12/y, h_49/g, i_11/j, t_25/x, t_27/z:
    q = { ? g, y . z==y * j=(y+1) * (0<=(-y+3)) * x->[] * z->[+F: g] * g->[+F: x] }
    Check:  g \in (reachable q {x,z})
  *)
  let y_eq_2 = eq y (E.mkNum 2) (* was y <= 3*) and
      j_eq_yplus1 = eq j (E.mkZAdd [y; E.mkNum 1]) and
      z_eq_y = eq z y in
  let q =
    (XSH.starx [z_eq_y; j_eq_yplus1; y_eq_2; pt x ; ptF z g ]
      (ptF g x)) in
  test_reachable "test 1" true q (Exps.of_list [x; z]) g ;

(*
  PS #375 05/11/2009 16:37, with r_15/y, n_11/x, de_13/g, nS-_17/j, r_34/w, r_35/z:

  q =
   { ? w, r_41, r_58, f_65, r_80 .
     y==x * z==g * 0==j * (y!=0) * (w!=0) * (z!=0) *
     w->[r_80; +IDL+F: z+IDD; +IDL+B: z+IDD] * z->[r_58; +IDD+B: w+IDL; +IDD+F: w+IDL] }

  Check: w \in (reachable q {z}).
*)
  let y_eq_x = eq y x and
      z_eq_g = eq z g and
      j_eq_0 = eq E.zero j and
      y_ne_0 = dq y E.zero and
      w_ne_0 = dq w E.zero and
      z_ne_0 = dq z E.zero in
  let w_obj =
    let idl_f = E.mkAdd [E.mkFld(Var.gensym "F" ()); E.mkFld(Var.gensym "IDL" ())] in
    let idl_b = E.mkAdd [E.mkFld(Var.gensym "B" ()); E.mkFld(Var.gensym "IDL" ())] in
    E.mkUpd
      (E.mkUpd r0 idl_f (E.mkOff z (E.mkFld(Var.gensym "IDD" ()))))
      idl_b
      (E.mkOff z (E.mkFld(Var.gensym "IDD" ())))
  in
  let z_obj =
    let idd_b = E.mkAdd [E.mkFld(Var.gensym "B" ()); E.mkFld(Var.gensym "IDD" ())] in
    let idd_f = E.mkAdd [E.mkFld(Var.gensym "F" ()); E.mkFld(Var.gensym "IDD" ())] in
    E.mkUpd
      (E.mkUpd r1 idd_b (E.mkOff w (E.mkFld(Var.gensym "IDL" ()))))
      idd_f
      (E.mkOff w (E.mkFld(Var.gensym "IDL" ())))
  in
  let q =
    XSH.star [ y_eq_x; z_eq_g; j_eq_0; y_ne_0; w_ne_0; z_ne_0 ]
      (SH.PtS.star [Pt.mk (w,w_obj); Pt.mk (z,z_obj)] SH.emp)
  in
  test_reachable "test 2" true q (Exps.of_list [z]) w ;


  ()
