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

(* Note: This is quite crude: only checks whether subtract fails or not, we
   could also check the:
   - number of distinct remainders found
   - remainders found are the expected ones
   - number of duplicate proofs of each remainder
   - size of the proof search space explored (Prover.try_count)
*)
let test_subtract name expect p q =
  let ys, p = XSH.exists_bind (XSH.fv q) p in
  let xs, q = XSH.exists_bind (Vars.union (SH.fv p) ys) q in (
  try Printexc.print (fun()-> Prover.subtract p xs q <> Prover.Failure) ()
  with exc -> if !Config.stop then raise exc else not expect
  )|>
  (fun b -> printf 0 "%s: %s" name (if b = expect then "PASS" else "FAIL"))


let test_entails  name expect xs p q =
  (Prover.entails p xs q <> None)
  |>
  (fun b -> printf 0 "%s: %s" name (if b = expect then "PASS" else "FAIL"))

let test_entailsx  name expect p q =
  (Prover.entailsx p q <> None)
  |>
  (fun b -> printf 0 "%s: %s" name (if b = expect then "PASS" else "FAIL"))



(*============================================================================
                                    Tests
  ============================================================================*)

let eq x y = XSH.Pf.star [E.mkEq x y] XSH.emp

let dq x y = XSH.Pf.star [E.mkDq x y] XSH.emp

let pt a =
  let r = Var.gensym "r" E.RecordSort in
  SH.exists_intro (Vars.singleton r) (SH.PtS.star [Pt.mk (a, E.mkVar r)] SH.emp)

let ptV a r = XSH.PtS.star [Pt.mk (a, E.mkVar(r))] XSH.emp

let ptF a c =
  let r = Var.gensym "r" E.RecordSort in
  SH.exists_intro (Vars.singleton r)
    (SH.PtS.star [Pt.mk (a, E.mkUpd (E.mkVar r) (E.mkFld(Var.gensym "Flink" ())) c)] SH.emp)

let sll l f n =
  XSH.exists_intro (Vars.singleton l)
    (XSH.LsS.star
      [{Ls.pat= !!D.sll;
           len= E.mkVar l;
           arg= {Ends.fore=[(f,(Type.Top,E.eps),n)]; back=[]}}]
      XSH.emp)


let ls patn len fore back =
  (patn, len, {Ends.fore= fore; back=back}, Exps.empty)



let test () =
  printf 0 "testing Prover" ;

  let d = E.mkFld(Var.gensym "D" ()) in
  let o = E.mkFld(Var.gensym "O" ()) in
  let flink = E.mkFld(Var.gensym "Flink" ()) in
  let f' = Var.gensym "f" E.OffsetSort in let f = E.mkVar(f') in
  let g' = Var.gensym "g" E.OffsetSort in let _g = E.mkVar(g') in
  let j' = Var.gensym "j" E.ValueSort in let _j = E.mkVar j' in
  let k' = Var.gensym "k" E.ValueSort in let k = E.mkVar k' in
  let l' = Var.gensym "l" E.ValueSort in let l = E.mkVar l' in
  let r' = Var.gensym "r" E.RecordSort in let r = E.mkVar r' in
  let r0' = Var.gensym "r0" E.RecordSort in let r0 = E.mkVar r0' in
  let r1' = Var.gensym "r1" E.RecordSort in let r1 = E.mkVar r1' in
  let v' = Var.gensym "v" E.ValueSort in let v = E.mkVar v' in
  let w' = Var.gensym "w" E.ValueSort in let w = E.mkVar w' in
  let x = E.mkVar (Var.gensym "x" E.ValueSort) in
  let y = E.mkVar (Var.gensym "y" E.ValueSort) in
  let z' = Var.gensym "z" E.ValueSort in let z = E.mkVar z' in

  (* { x->[F: y] * y->[F: z] * z->[] } \ ? l . { ls(sll,l,,x.,,x) } *)
  let p = XSH.starx [ptF x y; ptF y z; pt z] XSH.emp in
  let q = sll l' x x in
  test_subtract "test  1" true p q ;

  (* { x->[F: y] * y->[F: z] * z->[] } \ ? l . { ls(sll,l,,x.,,y) } *)
  let p = XSH.starx [ptF x y; ptF y z; pt z] XSH.emp in
  let q = sll l' x y in
  test_subtract "test  2" true p q ;

  (* { ls(sll,k,,x.,,y) * y->[F: z] } \ ? l . { ls(sll,l,,x.,,z) } *)
  let p = XSH.starx [sll k' x y] (ptF y z) in
  let q = sll l' x z in
  test_subtract "test  3" true p q ;

  (* { ls(sll,j,,x.,,y) * ls(sll,k,,y.,,z) } \ ? l . { ls(sll,l,,x.,,z) } *)
  let p = XSH.starx [sll j' x y] (sll k' y z) in
  let q = sll l' x z in
  test_subtract "test  4" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[] } \ { x->[] } *)
  let p = XSH.starx [sll l' x y] (pt y) in
  let q = pt x in
  test_subtract "test  5" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[] } \ ? r . { x->r } *)
  let p = XSH.starx [sll l' x y] (pt y) in
  let q = ptV x r' in
  let q = XSH.exists_intro (Vars.singleton r') q in
  test_subtract "test  6" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[] } \ ? w . { x->[F: w] } *)
  let p = XSH.starx [sll l' x y] (pt y) in
  let q = ptF x w in
  let q = XSH.exists_intro (Vars.singleton w') q in
  test_subtract "test  7" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[F: v] } \ ? w . { x->[F: w] } *)
  let p = XSH.starx [sll l' x y] (ptF y v) in
  let q = ptF x w in
  let q = XSH.exists_intro (Vars.singleton w') q in
  test_subtract "test  8" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[F: v] } \ ? r,f. { x+D-f->r } *)
  let p = XSH.starx [sll l' x y] (ptF y v) in
  let xdf = E.mkOff (E.mkOff x d) (E.mkMin f) in
  let q = ptV xdf r' in
  let q = XSH.exists_intro (Vars.of_list [r';f']) q in
  test_subtract "test  9" true p q ;

  (* { ls(sll,l,,x.,,y) * y->[D: w; F: v] } \ ? r,f. { x+D-f->r } *)
  let p = XSH.starx [sll l' x y; ptF y v] (pt z) in
  let q = XSH.starx [ptF x w] (pt z) in
  let q = XSH.exists_intro (Vars.singleton w') q in
  test_subtract "test 10" true p q ;

  (* { (y!=0) * y->[r_92; +F: x] * ls(sll,k,,x.,,0) }
     \ ? l . { ls(sll,l,,y.,,x) } *)
  let p = XSH.starx [dq x E.nil; ptF y x] (sll k' x E.nil) in
  let q = sll l' y x in
  test_subtract "test 11" true p q ;

  (* { x->r0 * { y==x * 0==k } \/ { l=(k-1) * y->r1 * ls(sll,l,,r0.+F.,,y) } }
     \ ? r . { x->r } *)
  let p0 = XSH.starx [eq y x] (eq E.zero k)
  and p1 = XSH.starx [pt y;
                      sll l' (E.mkSel r0 flink) y]
                     (eq l (E.mkZSub [k; E.mkNum 1])) in
  let p = XSH.starx [XSH.disj p0 p1] (ptV x r0')
  in
  let q = pt x
  in
  test_subtract "test 12" true p q ;

  (* { { y==x * y->r0 } \/ { x->r0 } } \ ? r . { x->r } *)
  let p0 = XSH.starx [eq x y] (pt y)
  and p1 = pt x in
  let p = XSH.disj p0 p1 in
  let q = pt x in
  test_subtract "test 13" true p q ;

  (* { r0.+f-D+F==y *
       { z==x * D==f * [r4; +F: z]==r0 * z->r2 } \/
       { z==x * D==f * z->r3 } }
     \ ? r1, g . { x+F-g->r1 } *)
  let r0fDF = E.mkSel r0 (E.mkAdd [E.mkFld (Var.gensym "F" ()); E.mkMin (E.mkFld (Var.gensym "D" ())); E.mkVar f']) in
  let p =
    XSH.Pf.star [E.mkEq r0fDF y]
    (XSH.disj
      (XSH.Pf.star
        [E.mkEq z x;
         E.mkEq (E.mkFld((Var.gensym "D" ()))) (E.mkVar(f'));
         E.mkEq (E.mkUpd r (E.mkFld((Var.gensym "F" ()))) z) r0]
        (pt z))
      (XSH.Pf.star
        [E.mkEq z x;
         E.mkEq (E.mkFld((Var.gensym "D" ()))) (E.mkVar(f'))]
        (pt z)))
  in
  let q =
    XSH.exists_intro (Vars.singleton g')
      (pt (E.mkOff x (E.mkAdd [E.mkMin (E.mkVar g'); E.mkFld (Var.gensym "F" ())]))) in

  test_subtract "test 14" true p q ;

  (* { x==r1.+D * r1==[r0; +F: y] } \ { x==r0.+D==r1.+D * r1==[r0; +F: y] } *)
  let p =
    XSH.Pf.star
      [E.mkEq x (E.mkSel r1 (E.mkFld((Var.gensym "D" ()))));
       E.mkEq r1 (E.mkUpd r0 (E.mkFld((Var.gensym "F" ()))) y);
       E.mkDq (E.mkFld (Var.gensym "D" ())) (E.mkFld (Var.gensym "F" ()))]
      XSH.emp in
  let q =
    XSH.Pf.star
      [E.mkEq x (E.mkSel r0 (E.mkFld((Var.gensym "D" ()))))]
      p in
  test_subtract "test 15" true p q ;

  (* { x->[r0; O+F: y] } \ ? z, r1 . { x->[r1; O+F: z+O] } *)
  let p =
    XSH.Pf.star [E.mkDq (E.mkFld (Var.gensym "F" ())) (E.mkFld (Var.gensym "O" ()))] <|
    XSH.PtS.star [Pt.mk (x, E.mkUpd r0 (E.mkAdd [E.mkFld (Var.gensym "O" ()); E.mkFld (Var.gensym "F" ())]) y)] XSH.emp
  and q =
    XSH.exists_intro (Vars.of_list [z'; r1'])
      (XSH.PtS.star
          [Pt.mk (x, E.mkUpd r0 (E.mkAdd [E.mkFld (Var.gensym "O" ()); E.mkFld (Var.gensym "F" ())])
                 (E.mkOff z (E.mkFld((Var.gensym "O" ())))))]
          XSH.emp )
  in
  test_subtract "test 16" true p q ;

  (* { x==y-O * x->r0 } \ ? f, r1 . { y+B-f->r1 } *)
  let p =
    XSH.Pf.star [E.mkDq (E.mkFld (Var.gensym "F" ())) (E.mkFld (Var.gensym "O" ()))] <|
    XSH.starx [eq x (E.mkOff y (E.mkMin o))] (pt x)
  and q =
    XSH.exists_intro (Vars.of_list [f'])
      (pt (E.mkOff y (E.mkAdd [E.mkFld (Var.gensym "B" ()); E.mkMin (E.mkVar f')])) )
  in
  test_subtract "test 17" true p q ;

  (*           { [+IDD+F]==f * v[+F-f]->[r; [+f]: w] }
     \ ? v, w. { [+IDD+F]==f * v[+F-f]->[r; [+f]: w] } *)
  let idd_f = (E.mkAdd [E.mkFld (Var.gensym "F" ()); E.mkFld (Var.gensym "IDD" ())]) in
  let vFf = E.mkOff v (E.mkAdd [E.mkMin (E.mkVar f'); E.mkFld (Var.gensym "F" ())]) in
  let q =
    XSH.exists_intro (Vars.of_list [v';w'])
      (XSH.Pf.star [E.mkEq idd_f f]
          (XSH.PtS.star
            [Pt.mk (vFf, E.mkUpd r (E.mkVar(f')) w)]
            XSH.emp) )
  in
  let p = XSH.Pf.star [E.mkDq (E.mkFld (Var.gensym "F" ())) (E.mkFld (Var.gensym "IDD" ()))] q
  in
  test_subtract "test 18" true p p ;

  (* \? w. w->[] * v->[] |- v->[] * tt *)
  let p = XSH.exists_intro (Vars.singleton w') (pt w) in
  let q = pt v  in
  let p_q = XSH.starx [p] q in
  let q_tt = XSH.starx [q] XSH.tt in
  test_subtract  "test 19" true p_q q_tt ;

  (* Like test #18, but the equality is inside the disjuncts.

     { ? v, w.  v[+F-f]->[r; [+f]: w] *
                ([+IDD+F]==f * x->_) \/ ([+IDD+F]==f * y->_) }

    (The pts are there to force the equalities to stay inside the disjuncts.) *)
  let idd_f = (E.mkAdd [E.mkFld (Var.gensym "F" ()); E.mkFld (Var.gensym "IDD" ())]) in
  let vFf = E.mkOff v (E.mkAdd [E.mkMin (E.mkVar f'); E.mkFld (Var.gensym "F" ())]) in
  let idd_f_eq_f = XSH.Pf.star [E.mkEq idd_f f] XSH.emp in
  let p =
    XSH.exists_intro (Vars.of_list [v';w'])
      (XSH.PtS.star
          [ Pt.mk (vFf, E.mkUpd r (E.mkVar(f')) w) ]
          (XSH.disj (XSH.starx [idd_f_eq_f] (pt x))
                    (XSH.starx [(pt y)] idd_f_eq_f)) )
  in
  test_subtract "test 20" true p p ;

  (* { (+F = +f) ^ x+F-f->[] } \ { x->[] } *)
  let p =
    XSH.Pf.star
      [E.mkEq (E.mkFld (Var.gensym "F" ())) f;
       E.mkEq (E.mkAdd [E.mkMin (E.mkVar f'); E.mkFld (Var.gensym "F" ())])
              (E.mkAdd [E.mkMin (E.mkVar g'); E.mkFld (Var.gensym "G" ())])]
      (pt (E.mkOff (E.mkOff x (E.mkFld (Var.gensym "F" ()))) (E.mkMin f)))
  in
  let q = pt x
  in
  test_subtract "test 21" true p q ;

  ()
