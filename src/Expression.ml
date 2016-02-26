(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Expressions *)

open Library

open Type
open Variable
module HC = HashCons

module L = (val Log.std Config.vExp : Log.LOG)
module LCng = (val Log.std Config.vCng : Log.LOG)


(*============================================================================
                                     Exp0
  ============================================================================*)

(** The [Exp0] module encapsulates construction of expression descriptors.  In
    particular, ensuring that any simplification performed by the constructors
    is applied by the expression manipulation operations in [Exp]. *)
module Exp0 : sig

  type op1 = Allocd | Not | ZMin
  type op2 = ZLt | ZLe | ZGt | ZGe | ZDiv | ZRem | ZMod
  type op3 = Ite
  type opN = Distinct | And | Or | ZAdd | ZMul | UFun of string

  type t = t_desc HC.hc

  and t_desc = private
    | Var of Var.t
    (* Pointer expressions *)
    | App of t * t     (** Application of pointer expressions                      *)
    | Nil              (** Constant symbol for NULL pointer                        *)
    | Add of Fld.t     (** Function symbol for adding field to pointer             *)
    | Sub of Fld.t     (** Function symbol for subtracting field from pointer      *)
    | Idx              (** Function symbol for indexing into an array              *)
    (* Offset expressions *)
    | Bas of Typ.t     (** Constant symbol for empty offset (from pointer of type) *)
    (* Integer, Boolean and String expressions *)
    | Eq of t * t
    | Num of int64
    | Str of string
    | Op1 of op1 * t_desc
    | Op2 of op2 * t_desc * t_desc
    | Op3 of op3 * t_desc * t_desc * t_desc
    | OpN of opN * t_desc array

  val desc : t -> t_desc
  val name : t_desc -> t

  val sort_of : t -> Var.sort
  val sort_of_desc : t_desc -> Var.sort

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val fmt : t formatter
  val fmtp : Var.fxt -> t formatter
  val fmt_caml : t formatter

  exception IllSorted of t_desc

  val mkVar_ : Var.t -> t_desc
  val mkApp_ : t -> t -> t_desc
  val mkNil_ : t_desc
  val mkAdd : t -> Fld.t -> t
  val mkSub : t -> Fld.t -> t
  val invert : t -> t option
  val mkIdx : t -> t -> t
  val mkBas_ : Typ.t -> t_desc
  val mkEq_ : t -> t -> t_desc
  val mkNum_ : int64 -> t_desc
  val mkStr_ : string -> t_desc
  val mkOp1_ : op1 -> t_desc -> t_desc
  val mkOp2_ : op2 -> t_desc -> t_desc -> t_desc
  val mkOp3_ : op3 -> t_desc -> t_desc -> t_desc -> t_desc
  val mkOpN_ : opN -> t_desc array -> t_desc

  module Desc : HashedType with type t = t_desc

  val report_ill_sorted : t_desc -> bool
  val stats : unit -> int * int * int * int * int * int

end = struct

(* Representation Types ===================================================== *)

(* Expressions, [t], are hash-consed expression descriptors, [t_desc].  That
   is, expression [e: t] is the unique name of descriptor [desc e].  There are
   three sorts of expressions: pointer, integer, and boolean.  Pointer
   expressions are curried (all pointer expressions except function
   application are constants) and flattened (all subexpressions are
   hash-consed, and thereby uniquely named) and so subexpressions of pointer
   expressions are of type [t].  Other sorts of expression are not curried or
   flattened, and so their subexpressions are of type [t_desc]. *)

type op1 = Allocd | Not | ZMin
type op2 = ZLt | ZLe | ZGt | ZGe | ZDiv | ZRem | ZMod
type op3 = Ite
type opN = Distinct | And | Or | ZAdd | ZMul | UFun of string

type t = t_desc HC.hc

and t_desc =
  | Var of Var.t
  (* Pointer expressions *)
  | App of t * t     (** Application of pointer expressions                      *)
  | Nil              (** Constant symbol for NULL pointer                        *)
  | Add of Fld.t     (** Function symbol for adding field to pointer             *)
  | Sub of Fld.t     (** Function symbol for subtracting field from pointer      *)
  | Idx              (** Function symbol for indexing into an array              *)
  (* Offset expressions *)
  | Bas of Typ.t     (** Constant symbol for empty offset (from pointer of type) *)
  (* Integer, Boolean and String expressions *)
  | Eq of t * t
  | Num of int64
  | Str of string
  | Op1 of op1 * t_desc
  | Op2 of op2 * t_desc * t_desc
  | Op3 of op3 * t_desc * t_desc * t_desc
  | OpN of opN * t_desc array


let desc e = e.HC.desc


(* Sort operations ========================================================== *)

let rec sort_of_desc e =
  match e with
  | Add(_) | Sub(_) | Idx | App({HC.desc= Idx},_) ->
      Var.OffsetSort
  | Var(x) ->
      Var.sort x
  | Nil ->
      Var.PointerSort
  | Bas(_) ->
      Var.OffsetSort
  | App(f,a) ->
      assert(
        match desc f with
        | Add _ | Sub _ -> true
        | App({HC.desc= Idx}, i) -> sort_of_desc (desc i) = Var.IntegerSort
        | _ -> false
      );
      assert(
        match desc a with
        | Var(v) -> Var.sort v = Var.PointerSort
                    || L.warnf "sort_of_desc expected pointer: %a" Var.fmt v
        | _ -> true
      );
      (fun s -> assert(
        s = Var.PointerSort || s = Var.OffsetSort
        || L.warnf "sort_of_desc expected pointer or offset"
      )) <&
      sort_of_desc (desc a)
  | Str(_) ->
      Var.PointerSort
  | Num(_)
  | Op1(ZMin,_)
  | Op2((ZDiv | ZRem | ZMod),_,_)
  | OpN((ZAdd | ZMul | UFun(_)),_) ->
      Var.IntegerSort
  | Op1((Allocd | Not),_)
  | Eq(_,_)
  | Op2((ZLt | ZLe | ZGt | ZGe),_,_)
  | OpN((Distinct | And | Or),_) ->
      Var.BooleanSort
  | Op3(Ite,_,e,_) ->
      (fun s -> assert( s <> Var.OffsetSort )) <&
      sort_of_desc e

let sort_of e = sort_of_desc (desc e)

let is_pointer e = sort_of_desc e = Var.PointerSort
let is_integer e = sort_of_desc e = Var.IntegerSort
let is_boolean e = sort_of_desc e = Var.BooleanSort
let is_offset  e = sort_of_desc e = Var.OffsetSort


(* Formatting =============================================================== *)

let rec fmtp_desc fxt ff e =
  let fmtp ff e = fmtp fxt ff e in
  let fmtp_desc ff e = fmtp_desc fxt ff e in
  try
    match e with
    | Var(v) ->
        Var.fmtp fxt ff v
    | Bas(t) ->
        Format.fprintf ff "@[%a@@@]" Typ.fmt (Typ.mkPointer t)
    | App({HC.desc= Add(f)},{HC.desc= Bas(_)}) ->
        Format.fprintf ff "@[%a@]" Fld.fmt f
    | App({HC.desc= Add(f)},a) when is_offset e ->
        Format.fprintf ff "@[%a.%a@]" fmtp a Fld.fmt f
    | Nil ->
        Format.fprintf ff "@[NULL@]"
    | App({HC.desc= Add(f)}, a) when !Config.c_syntax ->
        Format.fprintf ff "@[&(%a->%a)@]" fmtp a Fld.fmt f
    | App({HC.desc= Add(f)}, a) ->
        Format.fprintf ff "@[%a+%a@]" fmtp a Fld.fmt f
    | App({HC.desc= Sub(f)}, a) when !Config.c_syntax ->
        Format.fprintf ff "@[CONTAINING_RECORD(%a,@ %a,@ %a)@]" fmtp a Typ.fmt (Fld.typ f) Fld.fmt f
    | App({HC.desc= Sub(f)}, a) ->
        Format.fprintf ff "@[%a-%a@]" fmtp a Fld.fmt f
    | App({HC.desc= App({HC.desc=Idx},i)}, a) ->
        Format.fprintf ff "@[%a[%a]@]" fmtp a fmtp i
    | App(f,a) ->
        Format.fprintf ff "@[%a(%a)@]" fmtp f fmtp a
    | Add(f) ->
        Format.fprintf ff "@[+%a@]" Fld.fmt f
    | Sub(f) ->
        Format.fprintf ff "@[-%a@]" Fld.fmt f
    | Idx ->
        Format.fprintf ff "@[Idx@]"
    | Str(s) ->
        Format.fprintf ff "@[\"%s\"@]" s
    | Num(n) ->
        Format.fprintf ff "%Li" n
    | Op1(Allocd,e) ->
        Format.fprintf ff "@[allocd(%a)@]" fmtp_desc e
    | Op1(Not,Eq(e,f)) ->
        Format.fprintf ff "@[<hov 1>(%a !=@ %a)@]" fmtp e fmtp f
    | Op1(Not,a) ->
        Format.fprintf ff "@[!%a@]" fmtp_desc a
    | Op1(ZMin,e) ->
        Format.fprintf ff "@[-%a@]" fmtp_desc e
    | Eq(e,f) ->
        Format.fprintf ff "@[<hov 1>(%a ==@ %a)@]" fmtp e fmtp f
    | Op2(ZLt,e,f) ->
        Format.fprintf ff "@[<hov 1>(%a <@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZLe,e,f) ->
        Format.fprintf ff "@[<hov 1>(%a <=@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZGt,e,f) ->
        Format.fprintf ff "@[<hov 1>(%a >@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZGe,e,f) ->
        Format.fprintf ff "@[<hov 1>(%a >=@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZDiv,e,f) ->
        Format.fprintf ff "@[(%a /@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZRem,e,f) ->
        Format.fprintf ff "@[(%a %%@ %a)@]" fmtp_desc e fmtp_desc f
    | Op2(ZMod,e,f) ->
        Format.fprintf ff "@[(%a mod@ %a)@]" fmtp_desc e fmtp_desc f
    | Op3(Ite,g,t,e) ->
        Format.fprintf ff "@[<hov 1>(%a ? %a : %a)@]" fmtp_desc g fmtp_desc t fmtp_desc e
    | OpN(Distinct,es) ->
        Format.fprintf ff "@[<hov 1><%a>@]" (Array.fmt " !=@ " fmtp_desc) es
    | OpN(And,[||]) ->
        Format.fprintf ff "tt"
    | OpN(And,[|OpN(Or,[||])|]) ->
        Format.fprintf ff "ff"
    | OpN(And,cn) ->
        Format.fprintf ff "@[[@[%a@]]@]" (Array.fmt " ^@ " fmtp_desc) cn
    | OpN(Or,[||]) ->
        Format.fprintf ff "ff"
    | OpN(Or,dn) ->
        Format.fprintf ff "@[{@[%a@]}@]" (Array.fmt " v@ " fmtp_desc) dn
    | OpN(ZAdd,es) ->
        Format.fprintf ff "@[(%a)@]" (Array.fmt " +@ " fmtp_desc) es
    | OpN(ZMul,es) ->
        Format.fprintf ff "@[(%a)@]" (Array.fmt " *@ " fmtp_desc) es
    | OpN(UFun(s),es) ->
        Format.fprintf ff "@[\"%s\"(%a)@]" s (Array.fmt ",@ " fmtp_desc) es
  with Nothing_to_fmt -> ()

and fmtp fxt ff e =
  if !Config.vExp > 0
  then Format.fprintf ff "@[(%a)@@%i@]" (fmtp_desc fxt) (desc e) e.HC.id
  else fmtp_desc fxt ff (desc e)

let fmt_desc ff e = fmtp_desc (Vars.empty,Vars.empty) ff e

let fmt ff e = fmtp (Vars.empty,Vars.empty) ff e


let fmt_caml_op1 = function
  | Allocd -> "E.Allocd" | Not -> "E.Not" | ZMin -> "E.ZMin"

let fmt_caml_op2 = function
  | ZLt -> "E.ZLt" | ZLe -> "E.ZLe" | ZGt -> "E.ZGt" | ZGe -> "E.ZGe"
  | ZDiv -> "E.ZDiv" | ZRem -> "E.ZRem" | ZMod -> "E.ZMod"

let fmt_caml_op3 = function
  | Ite -> "E.Ite"

let fmt_caml_opN = function
  | Distinct -> "E.Distinct" | And -> "E.And" | Or -> "E.Or"
  | ZAdd -> "E.ZAdd" | ZMul -> "E.ZMul" | UFun(s) -> "(E.UFun \""^s^"\")"

let fmt_caml ff e =
  let rec fmt_caml_ ff = function
    | Var(v) ->
        Format.fprintf ff "@[<hov 2>(E.mkVar@ %a)@]" Var.fmt_caml v
    | App(f,a) ->
        (match desc f with
        | Add(f) ->
            Format.fprintf ff "@[<hov 2>(E.mkAdd@ %a@ %a)@]" fmt_caml_ (desc a) Fld.fmt_caml f
        | Sub(f) ->
            Format.fprintf ff "@[<hov 2>(E.mkSub@ %a@ %a)@]" fmt_caml_ (desc a) Fld.fmt_caml f
        | App({HC.desc=Idx},i) ->
            Format.fprintf ff "@[<hov 2>(E.mkIdx@ %a@ %a)@]" fmt_caml_ (desc a) fmt_caml_ (desc i)
        | f ->
            Format.fprintf ff "@[<hov 2>(E.mkApp@ %a@ %a)@]" fmt_caml_ f fmt_caml_ (desc a)
        )
    | Nil ->
        Format.fprintf ff "@[<hov 2>E.mkNil@]"
    | Bas(t) ->
        Format.fprintf ff "@[<hov 2>(E.mkBas(%a))@]" Typ.fmt_caml t
    | Eq(x,y) ->
        Format.fprintf ff "@[<hov 2>(E.mkEq@ %a@ %a)@]" fmt_caml_ (desc x) fmt_caml_ (desc y)
    | Num(n) ->
        Format.fprintf ff "@[<hov 2>(E.mkNum(%Li))@]" n
    | Str(s) ->
        Format.fprintf ff "@[<hov 2>(E.mkStr(\"%s\"))@]" s
    | Op1(o,x) ->
        Format.fprintf ff "@[<hov 2>(E.mkOp1@ %s@ %a)@]" (fmt_caml_op1 o) fmt_caml_ x
    | Op2(o,x,y) ->
        Format.fprintf ff "@[<hov 2>(E.mkOp2@ %s@ %a@ %a)@]" (fmt_caml_op2 o) fmt_caml_ x fmt_caml_ y
    | Op3(o,g,t,e) ->
        Format.fprintf ff "@[<hov 2>(E.mkOp3@ %s@ %a@ %a@ %a)@]" (fmt_caml_op3 o) fmt_caml_ g fmt_caml_ t fmt_caml_ e
    | OpN(f,xs) ->
        Format.fprintf ff "@[<hov 2>(E.mkOpN@ %s@ [|@[%a@]|])@]" (fmt_caml_opN f) (Array.fmt ";@ " fmt_caml_) xs
    | Add _ | Sub _ | Idx ->
        assert false     (* malformed *)
  in
  fmt_caml_ ff (desc e)


(* Comparison =============================================================== *)

let rec hash_desc x =
  match x with
  | Var(v) -> Var.hash v
  | App(f,a) -> Hashtbl.hash (hash f, hash a)
  | Add(f) -> Hashtbl.hash (Fld.id f)
  | Sub(f) -> Hashtbl.hash (- (Fld.id f))
  | Bas(t) -> Typ.hash t
  | Eq(a,b) -> Hashtbl.hash (hash a, hash b)
  | Nil | Idx | Num _ | Str _ | Op1 _ | Op2 _ | Op3 _ | OpN _ -> Hashtbl.hash x

and hash x = x.HC.hash
  &> (fun n -> assert( n = hash_desc x.HC.desc || failwithf "mis-hashed: %a" fmt x ))

let compare_sort x y =
  let open Var in
  if x == y then 0 else
  match x, y with
  | (PointerSort | IntegerSort | BooleanSort), OffsetSort -> -1
  | OffsetSort, (PointerSort | IntegerSort | BooleanSort) -> 1
  | _ -> Pervasives.compare x y

(* Note that this order is used in the selection of equivalence class representatives, and there is an
   implicit dependency that Var's compare greater than other offset expressions.  Otherwise, normalizing
   offset expressions wrt a congruence relation may not preserve well-formedness of offsets. *)
let rec compare_desc x y =
  if x == y then 0 else
  match x, y with
  | Var(v), Var(w) -> let z = compare_sort (Var.sort v) (Var.sort w) in if z<>0 then z else Var.compare v w
  | App(f,a), App(g,d) -> let z = compare a d in if z<>0 then z else compare f g
  | Nil, Nil -> 0
  | Add(f), Add(g) -> Fld.compare f g
  | Sub(f), Sub(g) -> Fld.compare f g
  | Idx, Idx -> 0
  | Bas(s), Bas(t) -> Typ.compare s t
  | Eq(a,b), Eq(d,e) -> let z = compare a d in if z<>0 then z else compare b e
  | Num(m), Num(n) -> Int64.compare m n
  | Str(x), Str(y) -> String.compare x y
  | Op1(o,a), Op1(p,d) ->
      let z = Pervasives.compare o p in if z<>0 then z else compare_desc a d
  | Op2(o,a,b), Op2(p,d,e) ->
      let z = Pervasives.compare o p in if z<>0 then z else
      let z =       compare_desc a d in if z<>0 then z else
                    compare_desc b e
  | Op3(o,a,b,c), Op3(p,d,e,f) ->
      let z = Pervasives.compare o p in if z<>0 then z else
      let z =       compare_desc a d in if z<>0 then z else
      let z =       compare_desc b e in if z<>0 then z else
                    compare_desc c f
  | OpN(o,a), OpN(p,d) ->
      let z = Pervasives.compare o p in if z<>0 then z else
      Array.compare compare_desc a d
  | Nil  , _ -> -1 | _, Nil   -> 1
  | Bas _, _ -> -1 | _, Bas _ -> 1
  | Num _, _ -> -1 | _, Num _ -> 1
  | Str _, _ -> -1 | _, Str _ -> 1
  | Add _, _ -> -1 | _, Add _ -> 1
  | Sub _, _ -> -1 | _, Sub _ -> 1
  | Idx  , _ -> -1 | _, Idx   -> 1
  | _, Var(v) when Var.sort v = Var.OffsetSort -> -1
  | Var(v), _ when Var.sort v = Var.OffsetSort -> 1
  | Var _, _ -> -1 | _, Var _ -> 1
  | App _, _ -> -1 | _, App _ -> 1
  | Eq  _, _ -> -1 | _, Eq  _ -> 1
  | Op1 _, _ -> -1 | _, Op1 _ -> 1
  | Op2 _, _ -> -1 | _, Op2 _ -> 1
  | Op3 _, _ -> -1 | _, Op3 _ -> 1

and compare x y =
  if x == y then 0 else
  compare_desc x.HC.desc y.HC.desc

(* let compare x y = Pervasives.compare x.HC.id y.HC.id *)

(* The equality relation passed to HashCons.Make is used on shallow copies of possibly-dead values, so
   physical disequality and disequality of ids cannot be relied upon. *)
let rec equal_desc x y =
  (fun eq -> assert(
     (not eq || hash_desc x = hash_desc y
      || failwithf "@[hash %a = %i <> %i = hash %a@]" fmt_desc x (hash_desc x) (hash_desc y) fmt_desc y) ))
  <&
  let equal x y =
    (x == y) || (x.HC.id = y.HC.id) || (equal_desc x.HC.desc y.HC.desc)
  in
  (x == y) ||
  match x, y with
  | Var(v), Var(w) -> Var.equal v w
  | App(f,a), App(g,d) -> equal f g && equal a d
  | Nil, Nil -> true
  | Add(f), Add(g) -> Fld.equal f g
  | Sub(f), Sub(g) -> Fld.equal f g
  | Idx, Idx -> true
  | Bas(s), Bas(t) -> Typ.equal s t
  | Eq(a,b), Eq(d,e) -> equal a d && equal b e
  | Num(m), Num(n) -> m = n
  | Str(x), Str(y) -> x = y
  | Op1(o,a), Op1(p,d) -> o = p && equal_desc a d
  | Op2(o,a,b), Op2(p,d,e) -> o = p && equal_desc a d && equal_desc b e
  | Op3(o,a,b,c), Op3(p,d,e,f) -> o = p && equal_desc a d && equal_desc b e && equal_desc c f
  | OpN(o,a), OpN(p,d) -> o = p && Array.equal equal_desc a d
  | ( Var _ | App _ | Nil | Add _ | Sub _ | Idx | Bas _ |
      Eq _ | Num _ | Str _ | Op1 _ | Op2 _ | Op3 _ | OpN _ ), _ -> false


(* Hash-Consing ============================================================= *)

module Desc = struct
  type t = t_desc
  let equal x y = equal_desc x y
  let hash x = hash_desc x
  let fmt = fmt_desc
end

module HCTbl = HC.Make(Desc)

let tbl = HCTbl.create Config.exp_hc_initial_size

let stats () = HCTbl.stats tbl

let name d = HCTbl.intern tbl d

let equal x y = (x == y)
  &> (fun ptr_eq -> assert(
       let id_eq = (x.HC.id = y.HC.id) in
       let desc_eq = equal_desc x.HC.desc y.HC.desc in
       let hash_eq = (hash x = hash y) in
       (not desc_eq || hash_eq
        || failwithf "@[hash %a = %i != %i = hash %a@]" fmt x (hash x) (hash y) fmt y) &&
       (match HCTbl.find_all tbl x with
        | [] -> L.warnf "@[not in tbl: %a@]" fmt x
        | [x'] -> (x' == x) || L.warnf "@[!= in tbl: %a != %a@]" fmt x fmt x'
        | _ -> L.warnf "@[multiply in tbl: %a@]" fmt x) &&
       (match HCTbl.find_all tbl y with
        | [] -> L.warnf "@[not in tbl: %a@]" fmt y
        | [y'] -> (y' == y) || L.warnf "@[!= in tbl: %a != %a@]" fmt y fmt y'
        | _ -> L.warnf "@[multiply in tbl: %a@]" fmt y) &&
       (ptr_eq = id_eq
        || L.warnf "@[%a %s %a@]" fmt x (if ptr_eq then "== but id <>" else "!= but id =") fmt y) &&
       (id_eq = desc_eq
        || L.warnf "@[%a %s %a@]" fmt x (if id_eq then "id = but desc <>" else "id <> but desc =") fmt y) &&
       (desc_eq = ptr_eq
        || L.warnf "@[%a %s %a@]" fmt x (if ptr_eq then "== but desc <>" else "!= but desc =") fmt y) ))


(* Desc Constructors ======================================================== *)

exception IllSorted of t_desc

let _ =
  Printexc.register_printer (function
    | IllSorted(e) -> Config.vVar := 5 ; Some(Format.asprintf "ill-sorted: %a" fmt_desc e)
    | _ -> None
  )

let report_ill_sorted e =
  if Config.check_sorts
  then raise (IllSorted(e))
  else L.warnf "ill-sorted: %a" fmt_desc e


let mkVar_ v = Var(v)

let mkApp_ f a =
  match desc f, desc a with
  (* d+g ==> d when g is the first field of its type *)
  | Add g, d when Fld.is_first g ->
      d
  (* d-g ==> d when g is the first field of its type *)
  | Sub g, d when Fld.is_first g ->
      d
  (* d-g+g ==> d *)
  | Add g, App({HC.desc= Sub h}, {HC.desc= d}) when Fld.equal g h ->
      d
  (* d+g-g ==> d *)
  | Sub g, App({HC.desc= Add h}, {HC.desc= d}) when Fld.equal g h ->
      d
  | _ ->
      App(f, a)

let mkNil_ = Nil

let mkAdd e f = name (mkApp_ (name (Add(f))) e)
let mkSub e f = name (mkApp_ (name (Sub(f))) e)
let mkIdx_ = name Idx
let mkIdx a i = name (mkApp_ (name (mkApp_ mkIdx_ i)) a)

let mkBas_ t = Bas(t)

let invert e =
  match desc e with
  | Add(o) -> Some(name(Sub(o)))
  | Sub(o) -> Some(name(Add(o)))
  | _ -> None

let mkNum_ n = Num(n)

let mkStr_ s = Str(s)

let tt = OpN(And,[||])
let ff = OpN(Or,[||])


let mkEq__ i j =
  match compare i j with
  | 0            -> tt
  | o when o > 0 -> Eq(j,i)
  | _            -> Eq(i,j)


let rec expandIte g t e exp =
  if not Config.exp_expand_ite then exp else
  mkOpN_ Or [|mkOpN_ And [|g; t|]; mkOpN_ And [|mkOp1_ Not g; e|]|]

and mkEqIte e gtf g t f =
  match g with
  | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> mkEq_ e (name t)
  | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkEq_ e (name f)
  | _ ->
  match mkEq_ e (name t) with
  | (* ff *) OpN(Or,[||]) | OpN(And,[|OpN(Or,[||])|]) -> mkOpN_ And [|mkOp1_ Not g; mkEq_ e (name f)|]
  | e_t ->
  match mkEq_ e (name f) with
  | (* ff *) OpN(Or,[||]) | OpN(And,[|OpN(Or,[||])|]) -> mkOpN_ And [|g; mkEq_ e (name t)|]
  | e_f ->
      expandIte g e_t e_f (mkEq__ e gtf)

and mkEq_ i j =
  assert( sort_of i = sort_of j
          || report_ill_sorted (Eq(i,j))
  );
  if not Config.exp_simplify then Eq(i,j) else
  match desc i, desc j with
  | Num(m), Num(n) -> if m = n then tt else ff
  | _, Op3(Ite,g,t,f) -> mkEqIte i j g t f
  | Op3(Ite,g,t,f), _ -> mkEqIte j i g t f
  | _ -> mkEq__ i j

(* Converts to negation-normal form.  This is not conditional on simplify
   since other operations (e.g. remove) do not track polarity. *)
and mkOp1_ o i =
  assert(
    (match o with
    | Allocd -> is_pointer i
    | Not -> is_boolean i
    | ZMin -> is_integer i
    ) || report_ill_sorted (Op1(o, i))
  );
  match o with
  | Not ->
      (match i with
      | Op1(Not,j)     -> j
      | Op2(ZLt,j,k)   -> mkOp2_ ZGe j k
      | Op2(ZLe,j,k)   -> mkOp2_ ZGt j k
      | Op2(ZGt,j,k)   -> mkOp2_ ZLe j k
      | Op2(ZGe,j,k)   -> mkOp2_ ZLt j k
      | Op3(Ite,g,t,f) -> mkOp3_ Ite g (mkOp1_ Not t) (mkOp1_ Not f)
      | OpN(And,is)    -> mkOpN_ Or  (Array.map (fun i -> mkOp1_ Not i) is)
      | OpN(Or,is)     -> mkOpN_ And (Array.map (fun i -> mkOp1_ Not i) is)
      | _              -> Op1(Not, i)
      )
  | _ ->
      Op1(o, i)

and mkOp2_ o i j =
  assert( is_integer i && is_integer j
          || report_ill_sorted (Op2(o, i, j))
  );
  if not Config.exp_simplify then Op2(o, i, j) else
  match o, i, j with
  | ZLt, Num(m), Num(n) -> if m <  n then tt else ff
  | ZLe, Num(m), Num(n) -> if m <= n then tt else ff
  | ZGt, Num(m), Num(n) -> if m >  n then tt else ff
  | ZGe, Num(m), Num(n) -> if m >= n then tt else ff
  | _, e, Op3(Ite,g,t,f) ->
      (match g with
      | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> mkOp2_ o e t
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o e f
      | _ ->
      match mkOp2_ o e t with
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o e f
      | e_t ->
      match mkOp2_ o e f with
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o e t
      | e_f ->
          expandIte g e_t e_f (Op2(o, e, j))
      )
  | _, Op3(Ite,g,t,f), e ->
      (match g with
      | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> mkOp2_ o t e
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o f e
      | _ ->
      match mkOp2_ o t e with
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o f e
      | e_t ->
      match mkOp2_ o f e with
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOp2_ o t e
      | e_f ->
          expandIte g e_t e_f (Op2(o, i, e))
      )
  | _ -> Op2(o, i, j)

and mkOp3_ o g t f =
  assert( is_boolean g && sort_of_desc t = sort_of_desc f
          (* Warning: SH.pure_consequences constructs ill-sorted exps of the following form: *)
          || (sort_of_desc t = Var.PointerSort && match f with Num(n) when n < 0L -> true | _ -> false)
          || report_ill_sorted (Op3(o, g, t, f))
  );
  if not Config.exp_simplify then Op3(o, g, t, f) else
  match o with
  | Ite ->
      match g with
      | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> t
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> f
      | _ ->
      match t with
      | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> mkOpN_ Or [|g; f|]
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOpN_ And [|mkOp1_ Not g; f|]
      | _ ->
      match f with
      | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> mkOpN_ Or [|g; t|]
      | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> mkOpN_ And [|g; t|]
      | _ ->
          expandIte g t f (Op3(Ite, g, t, f))

and mkOpN_ o es =
  assert(
    (match o with
    | Distinct -> Array.for_all (fun j -> sort_of_desc es.(0) = sort_of_desc j) es
    | And | Or -> Array.for_all is_boolean es
    | ZAdd | ZMul | UFun _ -> Array.for_all is_integer es
    ) || report_ill_sorted (OpN(o, es))
  );
  if not Config.exp_simplify then OpN(o, es) else
  match o with
  | And ->
      let rec filter e es =
        match es with
        | [f] when f == ff -> es
        | _ ->
        match e with
        | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> es
        | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> [ff]
        | Op3(Ite,g,t,f) ->
            let ite = expandIte g t f e in
            if ite == e then
                e :: es
            else
                List.fold filter [ite] es
        | _ -> e :: es
      in
      (match Array.fold_right filter es [] with
      | [e] -> e
      | es -> OpN(And, Array.of_list es)
      )
  | Or ->
      let rec filter e es =
        match es with
        | [t] when t == tt -> es
        | _ ->
        match e with
        | (* tt *) OpN(And,[||]) | OpN(Or,[|OpN(And,[||])|]) -> [tt]
        | (* ff *) OpN(Or,[||])  | OpN(And,[|OpN(Or,[||])|]) -> es
        | Op3(Ite,g,t,f) ->
            let ite = expandIte g t f e in
            if ite == e then
                e :: es
            else
                List.fold filter [ite] es
        | _ -> e :: es
      in
      (match Array.fold_right filter es [] with
      | [e] -> e
      | es -> OpN(Or, Array.of_list es)
      )
  | Distinct ->
      (match es with
      | [||] | [|_|] -> tt
      | _ -> OpN(o, es)
      )
  | _ ->
      OpN(o, es)

end


(*============================================================================
                                     Exp
  ============================================================================*)

module Exp = struct

include Exp0

(* Sort operations ========================================================== *)

let is_pointer e = sort_of e = Var.PointerSort
let is_integer e = sort_of e = Var.IntegerSort
let is_boolean e = sort_of e = Var.BooleanSort
let is_offset  e = sort_of e = Var.OffsetSort


(* Constructors ============================================================= *)

let mkVar v = name (mkVar_ v)

(* Pointer expressions *)

let mkApp e f = name (mkApp_ e f)
let getApp e = match desc e with App(f,a) -> Some(f,a) | _ -> None

let nil = name mkNil_

let mkAdds e fN_f1 = List.fold_right (fun fI e_f1_fI1 -> mkAdd e_f1_fI1 fI) fN_f1 e
let mkSubs e fN_f1 = List.fold_left (fun e_fN_fI1 fI -> mkSub e_fN_fI1 fI) e fN_f1

(* Offset expressions *)

let mkBas t = name (mkBas_ t)

(* Integer expressions *)

let mkNum n = name (mkNum_ n)
let zero = mkNum 0L
let one  = mkNum 1L

let mkStr n = name (mkStr_ n)

let mkOp1 o i = name (mkOp1_ o i)
let mkOp2 o i j = name (mkOp2_ o i j)
let mkOp3 o i j k = name (mkOp3_ o i j k)
let mkOpN o is = name (mkOpN_ o is)

let mkZMin i = mkOp1 ZMin (desc i)
let mkZAdd is = mkOpN ZAdd (Array.map desc is)
let mkZSub is = mkOpN ZAdd (Array.mapi (fun n i -> if n = 0 then desc i else mkOp1_ ZMin (desc i)) is)
let mkZMul is = mkOpN ZMul (Array.map desc is)
let mkZDiv i j = mkOp2 ZDiv (desc i) (desc j)
let mkZRem i j = mkOp2 ZRem (desc i) (desc j)
let mkZMod i j = mkOp2 ZMod (desc i) (desc j)

let mkUFun s es = mkOpN (UFun(s)) (Array.map desc es)

(* Boolean expressions *)

let mkAllocd e = mkOp1 Allocd (desc e)
let mkEq e f = name (mkEq_ e f)
let mkDq e f = mkOp1 Not (mkEq_ e f)
let mkDistinct es = mkOpN Distinct (Array.map desc es)

let mkZLt i j = mkOp2 ZLt (desc i) (desc j)
let mkZLe i j = mkOp2 ZLe (desc i) (desc j)
let mkZGt i j = mkOp2 ZGt (desc i) (desc j)
let mkZGe i j = mkOp2 ZGe (desc i) (desc j)

let mkNot b = mkOp1 Not (desc b)
let mkAnd bs = mkOpN And (Array.map desc bs)
let mkOr bs = mkOpN Or (Array.map desc bs)

let mkImp b c = mkOpN Or [|mkOp1_ Not (desc b); desc c|]
let mkIff b c = mkOpN And [|mkOpN_ Or [|mkOp1_ Not (desc b); desc c|];
                            mkOpN_ Or [|mkOp1_ Not (desc c); desc b|]|]
let mkXor b c = mkOpN Or [|mkOpN_ And [|mkOp1_ Not (desc b); desc c|];
                           mkOpN_ And [|mkOp1_ Not (desc c); desc b|]|]

let tt = mkAnd [||]
let ff = mkOr  [||]

(* Generic expressions *)

let mkIte b e f = mkOp3 Ite (desc b) (desc e) (desc f)


(* Conversions ============================================================== *)

let convert s e =
  (fun eo -> assert( match eo with Some(e') -> sort_of e' = s || report_ill_sorted (desc e') | None -> true ))
  <&
  match s, desc e with
  | Var.IntegerSort, Nil     -> Some(zero)
  | Var.BooleanSort, Nil     -> Some(ff)
  | Var.PointerSort, Num(0L) -> Some(nil)
  | Var.BooleanSort, Num(0L) -> Some(ff)
  | Var.BooleanSort, Num _   -> Some(tt)
  | _ ->
  match s, sort_of e with
  | Var.PointerSort, Var.PointerSort
  | Var.IntegerSort, Var.IntegerSort
  | Var.BooleanSort, Var.BooleanSort
  | Var.OffsetSort , Var.OffsetSort  -> Some(e)
  | Var.BooleanSort, Var.PointerSort -> Some(mkDq nil e)
  | Var.BooleanSort, Var.IntegerSort -> Some(mkDq zero e)
  | Var.IntegerSort, Var.BooleanSort -> Some(mkIte e one zero)
  | Var.BooleanSort, Var.OffsetSort
  | Var.IntegerSort, (Var.PointerSort | Var.OffsetSort)
  | Var.PointerSort, (Var.IntegerSort | Var.BooleanSort | Var.OffsetSort)
  | Var.OffsetSort , (Var.PointerSort | Var.IntegerSort | Var.BooleanSort) -> None


(* Traversals =============================================================== *)

let fold_sp dn up e sa pa =
  let rec fsp e sa pa =
    let sa = dn e sa in
    let pa =
      match desc e with
      | App(f,a)     -> fsp f sa (fsp a sa pa)
      | Op1(_,a)     -> fsp (name a) sa pa
      | Eq(a,b)      -> fsp a sa (fsp b sa pa)
      | Op2(_,a,b)   -> fsp (name a) sa (fsp (name b) sa pa)
      | Op3(_,a,b,c) -> fsp (name a) sa (fsp (name b) sa (fsp (name c) sa pa))
      | OpN(_,a)     -> Array.fold_right (fun a pa -> fsp (name a) sa pa) a pa
      | _            -> pa
    in up e sa pa
  in fsp e sa pa


let rec fold fn e z =
  let z =
    match desc e with
    | App(f,a)     -> fold fn f (fold fn a z)
    | Op1(_,a)     -> fold fn (name a) z
    | Eq(a,b)      -> fold fn a (fold fn b z)
    | Op2(_,a,b)   -> fold fn (name a) (fold fn (name b) z)
    | Op3(_,a,b,c) -> fold fn (name a) (fold fn (name b) (fold fn (name c) z))
    | OpN(_,a)     -> Array.fold_right (fun a z -> fold fn (name a) z) a z
    | _            -> z
  in fn e z


let map fn e =
(*   L.incf 0 "( Exp.map: %a" fmt e ; L.decf 0 ") Exp.map: %a" fmt <& *)
  let rec map_desc_ e = desc (map_ (name e))
  and map_ e =
    let e' =
      match desc e with
      | App(f,a)     -> mkApp (map_ f) (map_ a)
      | Op1(o,a)     -> mkOp1 o (map_desc_ a)
      | Eq(a,b)      -> mkEq (map_ a) (map_ b)
      | Op2(o,a,b)   -> mkOp2 o (map_desc_ a) (map_desc_ b)
      | Op3(o,a,b,c) -> mkOp3 o (map_desc_ a) (map_desc_ b) (map_desc_ c)
      | OpN(o,a)     -> mkOpN o (Array.map map_desc_ a)
      | _            -> e
    in fn e'
  in map_ e


let pmap fn e =
(*   L.incf 0 "( Exp.pmap: %a" fmt e ; L.decf 0 ") Exp.pmap: %a" fmt <& *)
  let rec pmap_desc_ e = desc (pmap_ (name e))
  and pmap_ e =
(*     L.incf 0 "( Exp.pmap_: %a" fmt e ; L.decf 0 ") Exp.pmap_: %a" fmt <& *)
    match fn e with
    | Some(e') -> e'
    | None ->
        match desc e with
        | App(f,a)     -> mkApp (pmap_ f) (pmap_ a)
        | Op1(o,a)     -> mkOp1 o (pmap_desc_ a)
        | Eq(a,b)      -> mkEq (pmap_ a) (pmap_ b)
        | Op2(o,a,b)   -> mkOp2 o (pmap_desc_ a) (pmap_desc_ b)
        | Op3(o,a,b,c) -> mkOp3 o (pmap_desc_ a) (pmap_desc_ b) (pmap_desc_ c)
        | OpN(o,a)     -> mkOpN o (Array.map pmap_desc_ a)
        | (Var _ | Nil | Bas _ | Add _ | Sub _ | Idx | Num _ | Str _) -> e
  in pmap_ e


let remove pred e =
  (fun e' -> assert(
    e == e' || not (equal e e')
    || failwithf "Exp.remove broke ==: %a %a" fmt e fmt e'))
  <&
  let rec remove_ e =
    (* [mul* mk] transforms constructors that treat [tt] as a zero *)
    let mul1 mk x =
      Option.map (fun x' ->
        if Desc.equal x x' then e else mk x'
      ) (remove_ x)
    in
    let mul2 mk x y =
      Option.map2 (fun x' y' ->
        if Desc.equal x x' && Desc.equal y y' then e else mk x' y'
      ) (remove_ x) (remove_ y)
    in
    let mul2t mk x y =
      mul2 (fun x y -> mk (name x) (name y)) (desc x) (desc y)
    in
    let mul3 mk x y z =
      Option.map3 (fun x' y' z' ->
        if Desc.equal x x' && Desc.equal y y' && Desc.equal z z' then e
        else mk x' y' z'
      ) (remove_ x) (remove_ y) (remove_ z)
    in
    let mulN mk xs =
      Option.mapN (fun xs' ->
        if Array.equal Desc.equal xs xs' then e else mk xs'
      ) (Array.map remove_ xs)
    in
    (* [add* mk] transforms constructors that treat [tt] as a unit *)
    let addN mk xs =
      let xs' =
        Array.of_list (Array.fold_right (fun x xs' ->
          match remove_ x with
          | None     -> xs'
          | Some(x') -> x' :: xs'
        ) xs []) in
      Some(if Array.equal Desc.equal xs xs' then e else mk xs')
    in
    match e with
    (* compound boolean expressions: descend *)
    | OpN(And,cn)     -> addN (mkOpN_ And) cn
    | OpN(Or,dn)      -> mulN (mkOpN_ Or) dn
    | Op1(Not,x)      -> mul1 (mkOp1_ Not) x
    | Op3(Ite,g,t,e) when sort_of_desc t = Var.BooleanSort
                      -> mul3 (mkOp3_ Ite) g t e

    (* non-boolean and atomic boolean expressions: remove if pred fails *)
    | _ when pred e   -> None

    (* non-boolean and atomic boolean expressions where pred holds: descend *)
    | Var _ | Nil | Bas _ | Add _ | Sub _ | Idx | Num _ | Str _
                      -> Some(e)
    | App(x,y)        -> mul2t mkApp_ x y
    | Op1(o,x)        -> mul1 (mkOp1_ o) x
    | Eq(x,y)         -> mul2t mkEq_ x y
    | Op2(o,x,y)      -> mul2 (mkOp2_ o) x y
    | Op3(Ite,g,t,e)  -> mul3 (mkOp3_ Ite) g t e
    | OpN(Distinct,x) -> addN (mkOpN_ Distinct) x
    | OpN(o,x)        -> mulN (mkOpN_ o) x
  in
  match remove_ (desc e) with
  | None     -> tt
  | Some(e') -> name e'


(* Queries ================================================================== *)

let fv x =
  fold (fun e vs ->
    match desc e with
    | Var(v) -> Vars.add v vs
    | _ -> vs
  ) x Vars.empty


end



(*============================================================================
                                     Off
  ============================================================================*)

module Off = struct

  type t = Exp.t
  (* Offsets are expressions of the form accepted by [Exp.is_offset]. *)

  type desc = Var of Var.t | Path of Typ.t * Fld.t list


  let mk e = assert( Exp.is_offset e ); e

  let mkVar v = Exp.mkVar v

  (* [mkPath ty [fN; ...; f2; f1]] is [(ty)0+f1+f2+...+fN] *)
  let mkPath ty fs = Exp.mkAdds (Exp.mkBas(ty)) fs


  let rec fold_path fn o z =
    match Exp.desc o with
    | Exp.Bas(ty) -> (ty, z)
    | Exp.Add(fld) -> (Fld.typ fld, fn fld z)
    | Exp.App(f,a) ->
        (match Exp.desc f with
        | Exp.Add(fld) ->
            let ty, z = fold_path fn a z in
            (ty, fn fld z)
        | _ -> invalid_argf "malformed offset: %a" Exp.fmt f)
    | _ -> invalid_argf "malformed offset: %a" Exp.fmt o

  let desc o =
    match Exp.desc o with
    | Exp.Var(v) ->
        Var(v)
    | _ ->
        let ty, fs = fold_path List.cons o [] in
        Path(ty, fs)


  let fv o =
    match desc o with
    | Var(v) -> Vars.singleton v
    | Path _ -> Vars.empty


  let is_base o =
    match Exp.desc o with
    | Exp.Var _ ->
        None
    | _ ->
        match
          fold_path (fun fld is_base ->
            is_base && Fld.is_first fld
          ) o true
        with
        | typ, true ->
            Some(typ)
        | _ ->
            None


  let equal = Exp.equal
  let compare = Exp.compare

  let fmtp fxt ff o =
    match desc o with
    | Var(v) ->
        Var.fmtp fxt ff v
    | Path(ty,fs) ->
        let fmt_ty ff ty =
          if !Config.vTyp > 0
          then Format.fprintf ff "(%a0)%s" Typ.fmt ty (if fs=[] then "" else ".")
        and fmt_fs ff fs =
          let fs = if fs <> [] then fs else match List.rev (Typ.fst_flds ty) with f :: _ -> [f] | [] -> [] in
          List.fmt "." Fld.fmt ff fs
        in
        Format.fprintf ff "%a%a" fmt_ty ty fmt_fs (List.rev fs)

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml ff o =
    match desc o with
    | Var(v) ->
        Format.fprintf ff "Off.mkVar %a" Var.fmt_caml v
    | Path(t,p) ->
        Format.fprintf ff "Off.mkPath %a [%a]"
          Typ.fmt_caml t (List.fmt ";" Fld.fmt_caml) p

end



(*============================================================================
                                 Collections
  ============================================================================*)

module Exps = struct
  include Set.Make(Exp)
(*   include Set.Make(struct *)
(*     include Exp *)
(*     let compare x y = Pervasives.compare x.HC.id y.HC.id *)
(*   end) *)

  let fv es = fold (fun e -> Vars.union (Exp.fv e)) es Vars.empty

  let fmt_sep sep ff es = List.fmt sep Exp.fmt ff (to_list es)
  let fmt = fmt_sep ",@ "
end

module Expss = Set.Make(Exps)
module ExpMap = Map.Make(Exp)
module ExpMMap = MultiMap.Make(Exp)(Exps)
module ExpHMap = HashMap.Make(Exp)

let stats = Exp.stats
