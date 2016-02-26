(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Congruence-closed relations *)

(* Notes:
   - It is possible for use lists to contain redundant equations.  This might
   slow down propagation, but detecting and removing them might be more
   expensive.

   - It is pointless to store use lists for offset exps, they are all
   distinct constants and so their use lists will never be looked up.

   - Would representing classes as lists instead of sets be more efficient?

   - Would separating cls_use into two maps be more efficient?  The cost would
   be two more finds in propagate1, the benefit would be eliminating the
   Cls-Use pairs and reduced domain size of the use map.
*)

open Library

(* open FORMULA *)
(* open CONGRUENCE_RELATION *)
module S = Substitution

module L = (val Log.std Config.vCng : Log.LOG)


open Variable
open Expression
(* Either open Expression or use functor to reduce dependencies, also see EOF *)
(* module Make *)
(*   (Exp: EXP) *)
(*   (Exps: Set.S with type elt = Exp.t) *)
(*   (ExpMap: Map.S with type key = Exp.t) *)
(*   : *)
(*   (CONGRUENCE_RELATION with type exp := Exp.t and type exps := Exps.t) = *)
(* struct *)

(* module Exps = struct *)
(*   include Exps *)
(*   let fmt ff es = (List.fmt ",@ " Exp.fmt) ff (to_list es) *)
(* end *)


(* Class lists ============================================================== *)

module type CLS = sig
  type t
  val mem : Exp.t -> t -> bool
  val fold : (Exp.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Exp.t -> unit) -> t -> unit
  val to_set : t -> Exps.t
  val add : Exp.t -> t -> t
  val remove : Exp.t -> t -> t
  val is_empty : t -> bool
  val singleton : Exp.t -> t
  val union : t -> t -> t
  val inter : Exps.t -> t -> t
  val fmt : t formatter
end

module Cls : CLS = struct
  include Exps
  let to_set x = x
  let fmt ff x = Format.fprintf ff "@[<hov 1>{@[%a@]}@]" fmt x
end


(* Disequality lists ======================================================== *)

module type DQS = sig
  type t
  val mem : Exp.t -> t -> bool
  val fold : (Exp.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Exp.t -> unit) -> t -> unit
  val add : Exp.t -> t -> t
  val empty : t
  val is_empty : t -> bool
  val union : t -> t -> t
  val inter : Exps.t -> t -> t
  val fmt : t formatter
end

module Dqs : DQS = struct
  include Exps
  let fmt ff x = Format.fprintf ff "@[<hov 1>{@[%a@]}@]" fmt x
end


(* Use lists / Super-expression equations =================================== *)

module type USE = sig
  type t
  val empty : t
  val is_empty : t -> bool
  val add : Exp.t * Exp.t * Exp.t -> t -> t
  val iter : (Exp.t * Exp.t * Exp.t -> unit) -> t -> unit
  val fold : (Exp.t * Exp.t * Exp.t -> 'a -> 'a) -> t -> 'a -> 'a
  val exists : (Exp.t * Exp.t * Exp.t -> bool) -> t -> bool
  val union : t -> t -> t
  val fmt : t formatter
end

(* Represents f(a)==c as App(App(f,a),c), and uses sets of such exps. *)
module Use0 : USE = struct
  type t = Exps.t

  let inj (f,a,c) = Exp.mkApp (Exp.mkApp f a) c
  let prj fac =
    let fa, c = Option.from_some (Exp.getApp fac) in
    let f, a  = Option.from_some (Exp.getApp fa) in
    (f,a,c)

  let fold fn s z = Exps.fold (fun fac z -> fn (prj fac) z) s z
  let iter fn s = Exps.iter (fun fac -> fn (prj fac)) s
  let exists pd s = Exps.exists (fun fac -> pd (prj fac)) s
  let add fac s = Exps.add (inj fac) s
  let union s t = Exps.union s t
  let empty = Exps.empty
  let is_empty = Exps.is_empty

  let fmt ff s =
    let fmt_eq ff fac =
      match Exp.getApp fac with
      | None ->
          Format.fprintf ff "@[%a@]" Exp.fmt fac
      | Some(fa,c) -> match Exp.getApp fa with
        | None ->
            Format.fprintf ff "@[%a = %a@]" Exp.fmt fa Exp.fmt c
        | Some(f,a) ->
            Format.fprintf ff "@[%a(%a) = %a@]" Exp.fmt f Exp.fmt a Exp.fmt c
    in
    Format.fprintf ff "@[<hov 1>{@[%a@]}@]" (List.fmt ",@ " fmt_eq) (Exps.to_list s)
end

(* Represents f(a)==c as (f,a,c), and uses lists of such triples.  *)
module Use1 : USE = struct
  include List.Set(struct type t = Exp.t * Exp.t * Exp.t end)
  let union = List.rev_append
  let fmt ff l =
    let fmt_eq ff (f,a,c) =
      Format.fprintf ff "@[%a(%a) = %a@]" Exp.fmt f Exp.fmt a Exp.fmt c in
    Format.fprintf ff "@[<hov 1>[@[%a@]]@]" (List.fmt ",@ " fmt_eq) l
end

(* Represents f(a)==c as (f,a,c), and uses sets of such triples.  *)
module Use2 : USE = struct
  include Set.Make(struct
    type t = Exp.t * Exp.t * Exp.t

    let equal (u,v,w) (x,y,z) =
      Exp.equal u x && Exp.equal v y && Exp.equal w z

    let compare (u,v,w) (x,y,z) =
      let c = Exp.compare u x in if c <> 0 then c else
      let c = Exp.compare v y in if c <> 0 then c else
              Exp.compare w z
  end)
  let fmt _ = failwith "Use2.fmt unimplemented"
end

(* Note: remove other implementations *)
module Use = Use1


(* Congruence relations ===================================================== *)

type t = {
  sat: bool;
  rep: Exp.t ExpMap.t;
    (** maps each expression in carrier to its representative *)
  cls_use: (Cls.t * Use.t * Dqs.t) ExpMap.t;
    (** maps each representative to the expressions in its class, equations
        involving a super-expression, and disequal expressions *)
  pnd: (Exp.t * Exp.t) list;
    (** pairs of expressions in carrier to be equated *)
}
(** See [invariant] below for additional representation properties. *)


(* Formatting =============================================================== *)

let fmt ff {sat; rep; cls_use; pnd} =
  let fmt_assocl fmt_k fmt_v ff assocl =
    let fmt_assoc ff (k,v) =
      Format.fprintf ff "@[[%a => %a]@]" fmt_k k fmt_v v in
    Format.fprintf ff "@[<hv>%a@]" (List.fmt ";@ " fmt_assoc) assocl
  in
  let fmt_pnd ff pnd =
    let fmt_eq ff (e,f) = Format.fprintf ff "@[%a = %a@]" Exp.fmt e Exp.fmt f in
    if pnd <> [] then
      Format.fprintf ff "@ pnd= @[%a@];" (List.fmt ";@ " fmt_eq) pnd
  in
  let cls, use, dqs =
    ExpMap.fold (fun e (e_cls, e_use, e_dqs) (cls, use, dqs) ->
      let cls = (e, e_cls) :: cls in
      let use = if Use.is_empty e_use then use else (e, e_use) :: use in
      let dqs = if Dqs.is_empty e_dqs then dqs else (e, e_dqs) :: dqs in
      (cls, use, dqs)
    ) cls_use ([],[],[])
  in
  Format.fprintf ff
    "@[{@[<hv>sat= %b;@ rep= %a;@ cls= %a;@ dqs= %a;@ use= %a;%a@]@;}@]"
    sat
    (fmt_assocl Exp.fmt Exp.fmt) (ExpMap.to_list rep)
    (fmt_assocl Exp.fmt Cls.fmt) cls
    (fmt_assocl Exp.fmt Dqs.fmt) dqs
    (fmt_assocl Exp.fmt Use.fmt) use
    fmt_pnd pnd


(* Well-formedness ========================================================== *)

let is_rep e r =
  try ExpMap.find e r.rep == e
  with Not_found ->
    failwithf "%a should be a rep but is not in car of@ %a" Exp.fmt e fmt r

let in_car e r =
  ExpMap.mem e r.rep

let find_chk e m =
  try ExpMap.find e m with Not_found -> failwithf "%a not in car" Exp.fmt e

(* A property maintained by propagating individual equations.  This is not
   strong enough to establish invariant below since it does not consider
   pending equations, which must be added and their consequences propagated
   before the relation is guaranteed to be closed. *)
let pre_invariant r =
  not Config.check_cng || let()=()in
  ExpMap.iter (fun e e' ->
    (* carrier is closed under sub-expressions *)
    (match Exp.getApp e with None -> () | Some(f,a) ->
      assert( in_car f r );
      assert( in_car a r );
    );
    (* rep is idempotent, ie, every representative is its own representative *)
    assert( is_rep e' r
     || failwithf "%a not rep in %a" Exp.fmt e' fmt r );
    (* every representative is in domain of cls_use *)
    assert( not (Exp.equal e e') || ExpMap.mem e' r.cls_use
     || failwithf "%a not in cls_use %a" Exp.fmt e' fmt r );
    (* every expression is in class of its representative *)
    assert( Cls.mem e (fst3 (find_chk e' r.cls_use))
     || failwithf "@[%a not in cls of %a in@ %a@]" Exp.fmt e Exp.fmt e' fmt r );
    (* use lists encode the super-expression relation for representatives, ie,
       for each application g(b)==e in the carrier *)
    if r.pnd = [] then
    (match Exp.getApp e with None -> () | Some(g,b) ->
      let aux g b =
        (* if an immediate sub-expression g is a representative *)
        try
          let _,use,_ = ExpMap.find g r.cls_use in
          let g' = g in
          let b' = find_chk b r.rep in
          (* then there is a corresponding equation f(a)==c in g's use list *)
          assert(
            Use.exists (fun (f,a,c) ->
              (* where equations correspond if they are equal under rep *)
              let f' = find_chk f r.rep in
              let a' = find_chk a r.rep in
              let c' = find_chk c r.rep in
                 Exp.equal c' e'
              && ( (Exp.equal f' g' && Exp.equal a' b')
                || (Exp.equal f' b' && Exp.equal a' g') )
            ) use
            || failwithf "no use equation for %a(%a) = %a  %a(%a) = %a"
                 Exp.fmt b Exp.fmt g Exp.fmt e
                 Exp.fmt b' Exp.fmt g' Exp.fmt e'
          )
        with Not_found -> ()
      in
      aux g b ;
      aux b g ;
    );
    (* relation is closed under a±F==b ==> a==b∓F *)
    if r.pnd = [] then
    (match Exp.getApp e with None -> () | Some(f,a) ->
      match Exp.invert f with None -> () | Some(inv_f) ->
        let a' = find_chk a r.rep in
        let e_inv_f' = find_chk (Exp.mkApp inv_f e') r.rep in
        assert( Exp.equal a' e_inv_f'
                || failwithf "%a = %a but missing %a = %a (= %a(%a))"
                     Exp.fmt e Exp.fmt e' Exp.fmt a' Exp.fmt e_inv_f' Exp.fmt inv_f Exp.fmt e' );
    );
  ) r.rep
  ;
  ExpMap.iter (fun e' (cls,use,dqs) ->
    (* every expression in domain of cls_use is a representative *)
    assert( is_rep e' r );
    (* rep maps every expression in class of e' to e' *)
    Cls.iter (fun e -> assert( find_chk e r.rep == e' ) ) cls ;
    (* every super-expression equation for e': *)
    Use.iter (fun (f,a,c) ->
      (* is between expressions in carrier *)
      let f' = find_chk f r.rep in
      let a' = find_chk a r.rep in
      let c' = find_chk c r.rep in
      (* involves an application super-expression of e' *)
      assert( Exp.equal f' e' || Exp.equal a' e' );
      (* is proved by the relation *)
      if r.pnd = [] then
      assert( Exp.equal (find_chk (Exp.mkApp f' a') r.rep) c'
       || failwithf "%a(%a) = %a not proved" Exp.fmt f' Exp.fmt a' Exp.fmt c' );
    ) use ;
    (* every expression in dqs is in carrier *)
    Dqs.iter (fun e ->
      assert( in_car e r )
    ) dqs
  ) r.cls_use
  ;
  true


(* A property maintained by exported operations. *)
let invariant r =
  assert( r.pnd = [] );
  assert( pre_invariant r );
  true


(* Helpers ================================================================== *)

let add_to_rep e e' rep =
  ExpMap.add e e' rep

let add_to_cls e' e cls_use =
  ExpMap.modify (fun (cls,use,dqs) -> (Cls.add e cls, use, dqs)) e' cls_use

let add_to_use e' fac cls_use =
  ExpMap.modify (fun (cls,use,dqs) -> (cls, Use.add fac use, dqs)) e' cls_use

let add_to_uses ((f',a',_) as fac) cls_use =
  add_to_use f' fac (add_to_use a' fac cls_use)

let add_to_dqs e' e r =
  {r with cls_use= ExpMap.modify (fun (cls,use,dqs) -> (cls, use, Dqs.add e dqs)) e' r.cls_use}

let union_to_dqs e' es r =
  {r with cls_use= ExpMap.modify (fun (cls,use,dqs) -> (cls, use, Dqs.union es dqs)) e' r.cls_use}

let add_to_pnd d e r =
  if Exp.equal d e then r else
  {r with pnd= (d, e) :: r.pnd}

let mem_dqs d' e' r =
     Dqs.mem e' (thd3 (ExpMap.find d' r.cls_use))
  || Dqs.mem d' (thd3 (ExpMap.find e' r.cls_use))


(* Extending the carrier ==================================================== *)

exception Found_extend
exception Found_find_extend of Exp.t

let rec extend_ if_found r e =
  let rep = ExpMap.modify_add if_found e e r.rep in
  let cls_use = ExpMap.add e (Cls.singleton e, Use.empty, Dqs.empty) r.cls_use in
  let r = {r with rep; cls_use} in
  match Exp.getApp e with
  | Some(f,a) ->
      let f', r = find_extend f r in
      let a', r = find_extend a r in
      let f'_a', r = find_extend (Exp.mkApp f' a') r in
      let cls_use = add_to_uses (f',a',e) r.cls_use in
      let pnd = if Exp.equal f'_a' e then r.pnd else (f'_a', e) :: r.pnd in
      {r with cls_use; pnd}
  | None ->
      r

(** [find_extend e r] is [(e',r')] where [e] is in the carrier of [r'] and
    represented by [e'].  [[r']] is the union of (e,e') and [[r]]. *)
and find_extend e r =
  try
    (e, extend_ (fun e' -> raise (Found_find_extend e')) r e)
  with Found_find_extend e' ->
    (e', r)

let extend e r =
  try
    (fun r' -> assert(true$>
      L.printf 20 "extend: %a@,@[  %a@\n= %a@]" Exp.fmt e fmt r fmt r' ))
    <&
    extend_ (fun _ -> raise Found_extend) r e
  with Found_extend ->
    r


(* Injectivity and Inverse axioms =========================================== *)

let apply_add_sub_axiom r f'_a' c' c'_cls =
  assert( Exp.sort_of f'_a' = Exp.sort_of c' );
  match Exp.getApp f'_a' with
  | None ->
      r
  | Some(f',a') ->
      assert(true$>
        L.incf 10 "( apply_add_sub_axiom: %a(%a) = %a@ %a@ %a"
          Exp.fmt f' Exp.fmt a' Exp.fmt c' Cls.fmt c'_cls fmt r );
      (fun r' -> assert(true$>
        L.decf 10 ") apply_add_sub_axiom:@ %a" fmt r' ))
      <&
      match Exp.invert f' with
      | None ->
          r
      | Some(inv_f) ->
          (* added a'±o==c', so add c∓o==a' for each c==c' *)
          Cls.fold (fun c r -> add_to_pnd (Exp.mkApp inv_f c) a' r) c'_cls r


(* Propagating equalities =================================================== *)

(** [propagate1 e' e r] adds [e'==e] to [r] using [e'] as the representative. *)
let propagate1 e' e r =
  assert(
    L.incf 10 "( propagate1:@ %a@ %a@ %a" Exp.fmt e' Exp.fmt e fmt r ;
    pre_invariant {r with pnd= (e',e) :: r.pnd} );
  (fun r' -> assert(
    L.decf 10 ") propagate1:@ %a" fmt r' ;
    pre_invariant r' ))
  <&
  (* remove class, superexp equations, and disequations of e, it is no longer a rep *)
  let (e_cls, e_use, e_dqs), cls_use = ExpMap.extract e r.cls_use in
  let r = {r with cls_use}
  in
  let r =
    Cls.fold (fun d r ->
      (* make e' the rep of every exp in class of e *)
      let r = {r with rep= ExpMap.add d e' r.rep} in
      (* and close under the axiom *)
      let e'_cls = if Exp.equal e' e then e_cls else fst3 (ExpMap.find e' r.cls_use) in
      apply_add_sub_axiom r d e' e'_cls
    ) e_cls r
  in
  (* add disequalities of e to e' *)
  let r = union_to_dqs e' e_dqs r
  in
  (* closing under the axiom cannot promote e back to a rep *)
  assert( not (ExpMap.mem e r.cls_use) )
  ;
  (* traverse up exp dag from e, whose rep changed, looking for new equations *)
  let r, e'_use_delta =
    Use.fold (fun (f,a,c) (r, e'_use_delta) ->
      let f' = ExpMap.find f r.rep in
      let a' = ExpMap.find a r.rep in
      let f'_a' = Exp.mkApp f' a' in
      try
        let b = ExpMap.find f'_a' r.rep in
        (* f'(a')==b already, so add b==c *)
        (add_to_pnd b c r, e'_use_delta)
      with Not_found ->
        (* f'(a') not in relation yet, so add f'(a')==c' *)
        let c' = ExpMap.find c r.rep in
        let rep = add_to_rep f'_a' c' r.rep in
        let cls_use = add_to_cls c' f'_a' r.cls_use in
        let r = {r with rep; cls_use} in
        let c'_cls,_,_ = ExpMap.find c' r.cls_use in
        let r = apply_add_sub_axiom r f'_a' c' c'_cls in
        (* Don't need to add (f',a',c') to use of f' or a' since e' is one of
           f' or a' and (f',a',c') will be added to use of e' below, and
           (f,a,c) will already be in use of the other. *)
        let e'_use_delta = Use.add (f',a',c') e'_use_delta in
        (r, e'_use_delta)
    ) e_use (r, Use.empty)
  in
  (* add class and superexps of e to e' *)
  let cls_use =
    ExpMap.modify (fun (e'_cls, e'_use, e'_dqs) ->
      (Cls.union e_cls e'_cls, Use.union e'_use_delta e'_use, e'_dqs)
    ) e' r.cls_use
  in
  {r with cls_use}


module SortMMap =
  MultiMap.Make
    (struct type t = Var.sort let compare = Pervasives.compare let equal = Pervasives.( = ) end)
    (Exps)


(** [propagate leq r] adds pending equations until closure reached. *)
let rec propagate leq r =
  assert(
    L.incf 10 "( propagate:@ %a" fmt r ;
    pre_invariant r );
  (fun r' -> assert(
    L.decf 10 ") propagate:@ %a" fmt r' ;
    invariant r' ))
  <&
  match r.pnd with
  | [] ->
      r
  | (d,e) :: pnd ->
      let r = {r with pnd} in
      let d', r = find_extend d r in
      let e', r = find_extend e r in
      if Exp.equal d' e' then
        propagate leq r
      else if mem_dqs d' e' r then
        {r with sat= false; pnd= []}
      (* use e as the new rep if leq e d *)
      else if leq e' d' then
        propagate leq (propagate1 e' d' r)
      else
        propagate leq (propagate1 d' e' r)




(* Exported operations ====================================================== *)

let normalize r e =
  try ExpMap.find e r.rep with Not_found -> e


let mem r e f =
  Exp.equal (normalize r e) (normalize r f)


let mem_dqs r e f =
  try mem_dqs (normalize r e) (normalize r f) r
  with Not_found -> false


let inconsistent r =
  not r.sat


let class_of r e =
  try Cls.to_set (fst3 (ExpMap.find (normalize r e) r.cls_use))
  with Not_found -> Exps.singleton e


let mem_carrier e r =
  ExpMap.mem e r.rep


let carrier r =
  ExpMap.fold (fun e _ car ->
    Exps.add e car
  ) r.rep Exps.empty


let representatives r =
  ExpMap.fold (fun e' _ reps ->
    Exps.add e' reps
  ) r.cls_use Exps.empty


let fold fn r z =
  ExpMap.fold (fun e' (cls,_,_) z ->
    Cls.fold (fun e z ->
      fn e' e z
    ) (Cls.remove e' cls) z
  ) r.cls_use z


let foldn fn r z =
  ExpMap.fold (fun e' (_,_,dqs) z ->
    Dqs.fold (fun e z ->
      if Exp.compare e' e > 0 then z else
      fn e' e z
    ) dqs z
  ) r.cls_use z


let fold_classes fn r z =
  ExpMap.fold (fun e' (cls,_,_) z ->
    fn e' (Cls.to_set cls) z
  ) r.cls_use z


let empty =
  (fun r -> assert( invariant r ))
  <&
  { sat= true; rep= ExpMap.empty; cls_use= ExpMap.empty; pnd= []; }


(** [merge leq r d e] adds an equation between expressions [d] and [e], and
    propagates consequences.  Extends carrier as needed. *)
let merge leq r d e =
  assert(true$>
    L.incf 10 "( merge: %a %s %a@ %a"
      Exp.fmt d (if leq e d then ">=" else "<") Exp.fmt e fmt r );
  (fun r' -> assert(
    L.decf 10 ") merge:@ %a" fmt r' ;
    invariant r'
    &&
    (not r'.sat
     ||
     (normalize r' d == normalize r' e
      || failwithf "%a = %a not in %a" Exp.fmt d Exp.fmt e fmt r' ) &&
     not (in_car d r && in_car e r)
     || is_rep (normalize r' d) r ) ))
(* Review: This assertion does not hold, check for reliance on it.
     || let d0' = normalize r d and e0' = normalize r e
        and d' = normalize r' d and e' = normalize r' e in
        if leq e0' d0'
        then d' == e0' || failwithf "%a != %a" Exp.fmt d' Exp.fmt e0'
        else e' == d0' || failwithf "%a != %a" Exp.fmt e' Exp.fmt d0' ))
*)
  <&
  if not r.sat then r else
  propagate leq (add_to_pnd d e (extend d (extend e r)))


let split r d e =
  if not r.sat then r else
  let d', r = find_extend d r in
  let e', r = find_extend e r in
  if Exp.equal d' e' then
    {r with sat= false}
  else
    add_to_dqs d' e' (add_to_dqs e' d' r)


let union leq p q =
  assert(true$>
    L.incf 10 "( union:@ @[<hv>%a@ %a@]" fmt p fmt q );
  (fun r -> assert(
    L.decf 10 ") union:@ %a" fmt r ;
    ExpMap.for_all (fun e f ->
      match leq e f, leq f e with
      | true, false ->
          Exp.equal (normalize r e) (normalize p f)
          || failwithf "%a should have rep %a" Exp.fmt e Exp.fmt (normalize p f)
      | false, true ->
          Exp.equal (normalize r f) (normalize p e)
          || failwithf "%a should have rep %a" Exp.fmt f Exp.fmt (normalize p e)
      | _ -> true
    ) q.rep &&
    invariant r ))
  <&
  if not p.sat then p else if not q.sat then q else
  p |>
  ExpMap.fold (fun e e' r -> merge leq r e e') q.rep |>
  ExpMap.fold (fun e' (_,_,dqs) r -> Dqs.fold (fun e r -> split r e' e) dqs r) q.cls_use


let inter leq p q =
  assert(true$>
    L.incf 10 "( inter:@ @[<hv>%a@ %a@]" fmt p fmt q );
  (fun r -> assert(
    L.decf 10 ") inter:@ %a" fmt r ;
    invariant r ))
  <&
  let merge_mem p q r =
    ExpMap.fold (fun e e' r ->
      if try Exp.equal (ExpMap.find e q.rep) (ExpMap.find e' q.rep) with Not_found -> false
      then merge leq r e e'
      else r
    ) p.rep r
  in
  let split_dqs p q r =
    ExpMap.fold (fun e' (_,_,dqs) r ->
      Dqs.fold (fun e r ->
        if mem_dqs p e' e then
          split r e' e
        else
          r
      ) dqs r
    ) q.cls_use r
  in
  if not p.sat then q else if not q.sat then p else
  empty |>
  merge_mem q p |>
  merge_mem p q |>
  split_dqs q p |>
  split_dqs p q


(* Notes *)
(* - Is there a better implementation, that avoids enumerating the maps? *)
(* - The carrier need not be preserved by subst since the carrier need not be
   closed under application of expressions in the carrier.  Therefore a less
   naive implementation of updating the use lists is not obvious. *)
(* - Specify more precisely. *)
exception NewEquality of Exp.t * Exp.t
let rec subst leq r kill_to_keep =
  assert(true$>
    L.incf 10 "( subst: %a@ %a" S.fmt kill_to_keep fmt r );
  (fun r' -> assert(
    L.decf 10 ") subst:@ %a" fmt r' ;
    (not (try invariant r with _ -> false)) || invariant r' ))
  <&
  if S.is_empty kill_to_keep then r
  else try
    let rep =
      ExpMap.fold (fun d d' rep ->
        let e = S.subst kill_to_keep d in
        let e' = S.subst kill_to_keep d' in
        try
          let f = ExpMap.find e r.rep in
          let f' = ExpMap.find e' r.rep in
          if not (Exp.equal f f') then
            (* Substituting revealed a new equality. This is possible since
               Substitution sees more internal structure of Expressions than
               CngRel does, for instance arithmetic expressions are treated as
               atoms in CngRel, but are substituted through. *)
            raise (NewEquality (f,f'))
          else
            raise Not_found
        with Not_found ->
          if Exp.equal e d then
            if Exp.equal e' d'
            then rep
            else ExpMap.add e e' rep
          else ExpMap.add e e' (ExpMap.remove d rep)
      ) r.rep r.rep
    in
    let cls_use =
      ExpMap.fold (fun d' (cls,_,_) cls_use ->
        let e' = S.subst kill_to_keep d' in
        let cls =
          Cls.fold (fun e cls ->
            let e' = S.subst kill_to_keep e in
            if Exp.equal e e' then cls else Cls.add e' (Cls.remove e cls)
          ) cls cls in
        let use = Use.empty in
        let dqs = Dqs.empty in
        if Exp.equal e' d'
        then ExpMap.add e' (cls,use,dqs) cls_use
        else ExpMap.add e' (cls,use,dqs) (ExpMap.remove d' cls_use)
      ) r.cls_use r.cls_use
    in
    let r =
      ExpMap.fold (fun c _ r ->
        match Exp.getApp c with
        | None ->
            r
        | Some(f,a) ->
            let f', r = find_extend f r in
            let a', r = find_extend a r in
            {r with cls_use= add_to_uses (f',a',c) r.cls_use}
      ) rep {r with rep; cls_use}
    in
    r
  with
  | NewEquality (f,f') ->
      (* add new equality and restart *)
      subst leq (merge leq r f f') kill_to_keep


(* Notes *)
(* - Is there a better implementation, that avoids enumerating the maps? *)
(* - The result of restrict is not a well-formed relation.  The carrier is
   not even closed under sub-expressions.  Restrict should return a different
   type. *)
let restrict r es =
  assert(
    L.incf 10 "( restrict: @[{%a}@]@ %a" Exps.fmt es fmt r;
    let reps = representatives r in
    Exps.subset reps es
    || failwithf "reps not in es: {@[%a@]}" Exps.fmt (Exps.diff reps es) );
  (fun r -> assert(true$>
    L.decf 10 ") restrict:@ %a" fmt r ))
  <&
  (* restrict dom of rep to es *)
  let rep = ExpMap.filter (fun e _ -> Exps.mem e es) r.rep
  in
  let cls_use =
    ExpMap.fold (fun e' (cls,_,dqs) cls_use ->
      (* restrict dom of cls_use to es *)
      if not (Exps.mem e' es) then
        cls_use
      else
        (* restrict classes to es *)
        let cls = Cls.inter es cls in
        (* clear superexp equations *)
        let use = Use.empty in
        (* restrict disequalities to es *)
        let dqs = Dqs.inter es dqs in
        ExpMap.add e' (cls,use,dqs) cls_use
    ) r.cls_use ExpMap.empty
  in
  {r with rep; cls_use}


let remove_trivial r es =
  assert(true$>
    L.incf 10 "( remove_trivial: @[{%a}@]@ %a" Exps.fmt es fmt r );
  (fun r -> assert(true$>
    L.decf 10 ") remove_trivial:@ %a" fmt r ))
  <&
  Exps.fold (fun e r ->
    try
      let cls,_,_ = ExpMap.find e r.cls_use in
      if Cls.is_empty (Cls.remove e cls) then
        let rep = ExpMap.remove e r.rep in
        let cls_use = ExpMap.remove e r.cls_use in
        {r with rep; cls_use}
      else
        r
    with Not_found ->
      r
  ) es r


let implied_by leq init x assumptions carrier =
  let terms = Exps.to_array carrier in
  match Pure.get_implied_equalities x assumptions terms with
  | None ->
      empty
  | Some(ids) ->
(*       L.printf 0 "terms: @[%a@]" (Array.fmt "@ " Exp.fmt) terms ; *)
(*       L.printf 0 "ids: @[%a@]" (Array.fmt "@ " Format.pp_print_int) ids ; *)
      let n = Array.length terms in
      let id_to_rep = IntHMap.create n in
      snd (Array.fold_left (fun (i, cng) terms_i ->
        try
          let e' = IntHMap.find id_to_rep ids.(i) in
(*           L.printf 0 "merging %a and %a" Exp.fmt terms_i Exp.fmt e' ; *)
          (i+1, merge leq cng terms_i e')
        with Not_found ->
(*           L.printf 0 "choosing %a as rep for id %n" Exp.fmt terms_i ids.(i) ; *)
          IntHMap.add id_to_rep ids.(i) terms_i ;
          (i+1, merge leq cng terms_i terms_i)
      ) (0, init) terms)


let is_empty {rep; cls_use; pnd} =
  ExpMap.is_empty rep && ExpMap.is_empty cls_use && pnd = []


(* Debug wrappers for entry points *)
(* let merge = debug_wrap4 Config.vCng 10 merge *)
(* let union = debug_wrap3 Config.vCng 10 union *)
(* let inter = debug_wrap2 Config.vCng 10 inter *)
(* let subst = debug_wrap2 Config.vCng 10 subst *)
(* let restrict = debug_wrap2 Config.vCng 10 restrict *)
(* let implied_by = debug_wrap5 Config.vCng 10 implied_by *)


(* end *)
(* include Make (Expression.Exp) (Expression.Exps) (Expression.ExpMap) *)
