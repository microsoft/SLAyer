(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** ESP -> SLAyer translator **)

open Library

open Type
open Variable
open Expression
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint

(*============================================================================
  Tracing
  ============================================================================*)

(* lvl are: 2=func, 4=stmt, 8=expr. *)
module L = (val Log.std Config.vFE : Log.LOG)

(*============================================================================
  Wrap up Expr functions.
  ============================================================================*)
module Exp_ = Exp
module E : (module type of Exp_) = struct

  include Exp

  (* Conversions *)

  let conv_to_ptr_sort e =
    match Exp.convert Var.PointerSort e with
    | Some(e) -> e
    | None -> failwithf "conversion to PointerSort failed: %a" Exp.fmt e

  let conv_to_int_sort e =
    match Exp.convert Var.IntegerSort e with
    | Some(e) -> e
    | None -> failwithf "conversion to IntegerSort failed: %a" Exp.fmt e

  let conv_to_bool_sort e =
    match Exp.convert Var.BooleanSort e with
    | Some(e) -> e
    | None -> failwithf "conversion to BooleanSort failed: %a" Exp.fmt e


  (* Convert arguments to required sort *)

  let mkAdd t f = Exp.mkAdd (conv_to_ptr_sort t) f

  let mkSub t f = Exp.mkSub (conv_to_ptr_sort t) f

  let mkAdds t ff = Exp.mkAdds (conv_to_ptr_sort t) ff

  let mkSubs t ff = Exp.mkSubs (conv_to_ptr_sort t) ff

  let mkIdx e0 e1 = Exp.mkIdx (conv_to_ptr_sort e0) (conv_to_int_sort e1)

  let mkZMin e0 = Exp.mkZMin (conv_to_int_sort e0)

  let mkZDiv e0 e1 = Exp.mkZDiv (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZRem e0 e1 = Exp.mkZRem (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZMod e0 e1 = Exp.mkZMod (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZAdd ee = Exp.mkZAdd (Array.map conv_to_int_sort ee)

  let mkZSub ee = Exp.mkZSub (Array.map conv_to_int_sort ee)

  let mkZMul ee = Exp.mkZMul (Array.map conv_to_int_sort ee)

  let mkUFun op ee = Exp.mkUFun op (Array.map conv_to_int_sort ee)

  let mkNot e0 = Exp.mkNot (conv_to_bool_sort e0)

  let mkAnd ee = Exp.mkAnd (Array.map conv_to_bool_sort ee)

  let mkOr ee = Exp.mkOr (Array.map conv_to_bool_sort ee)

  let mkImp e0 e1 = Exp.mkImp (conv_to_bool_sort e0) (conv_to_bool_sort e1)

  let mkIff e0 e1 = Exp.mkIff (conv_to_bool_sort e0) (conv_to_bool_sort e1)

  let mkXor e0 e1 = Exp.mkXor (conv_to_bool_sort e0) (conv_to_bool_sort e1)

  let mkZLt e0 e1 = Exp.mkZLt (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZLe e0 e1 = Exp.mkZLe (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZGt e0 e1 = Exp.mkZGt (conv_to_int_sort e0) (conv_to_int_sort e1)

  let mkZGe e0 e1 = Exp.mkZGe (conv_to_int_sort e0) (conv_to_int_sort e1)


  (* Debug printing *)

  let mkAdd t f =
    try mkAdd t f with e -> (L.printf 0 "mkAdd %a %a failed" fmt t Fld.fmt f ; raise(e))

  let mkSub t f =
    try mkSub t f with e -> (L.printf 0 "mkSub %a %a failed" fmt t Fld.fmt f ; raise(e))

  let mkAdds t ff =
    try mkAdds t ff with e -> (L.printf 0 "mkAdds %a %a failed" fmt t (List.fmt "+" Fld.fmt) ff ; raise(e))

  let mkSubs t ff =
    try mkSubs t ff with e -> (L.printf 0 "mkSubs %a %a failed" fmt t (List.fmt "+" Fld.fmt) ff ; raise(e))

  let mkIdx e0 e1 =
    try mkIdx e0 e1 with e -> (L.printf 0 "mkIdx %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZMin e0 =
    try mkZMin e0 with e -> (L.printf 0 "mkZMin %a failed" fmt e0 ; raise(e))

  let mkZDiv e0 e1 =
    try mkZDiv e0 e1 with e -> (L.printf 0 "mkZDiv %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZRem e0 e1 =
    try mkZRem e0 e1 with e -> (L.printf 0 "mkZRem %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZMod e0 e1 =
    try mkZMod e0 e1 with e -> (L.printf 0 "mkZMod %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZAdd ee =
    try mkZAdd ee with e -> (L.printf 0 "mkZAdd %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkZSub ee =
    try mkZSub ee with e -> (L.printf 0 "mkZSub %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkZMul ee =
    try mkZMul ee with e -> (L.printf 0 "mkZMul %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkUFun op ee =
    try mkUFun op ee with e -> (L.printf 0 "mkZUFun %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkNot e0 =
    try mkNot e0 with e -> (L.printf 0 "mkNot %a failed" fmt e0 ; raise(e))

  let mkAnd ee =
    try mkAnd ee with e -> (L.printf 0 "mkAnd %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkOr ee =
    try mkOr ee with e -> (L.printf 0 "mkOr %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkImp e0 e1 =
    try mkImp e0 e1 with e -> (L.printf 0 "mkImp %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkIff e0 e1 =
    try mkIff e0 e1 with e -> (L.printf 0 "mkIff %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkXor e0 e1 =
    try mkXor e0 e1 with e -> (L.printf 0 "mkXor %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkEq e0 e1 =
    try mkEq e0 e1 with e -> (L.printf 0 "mkEq %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkDq e0 e1 =
    try mkDq e0 e1 with e -> (L.printf 0 "mkDq %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkDistinct ee =
    try mkDistinct ee with e -> (L.printf 0 "mkDistinct %a failed" (Array.fmt "," fmt) ee ; raise(e))

  let mkZLt e0 e1 =
    try mkZLt e0 e1 with e -> (L.printf 0 "mkZLt %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZLe e0 e1 =
    try mkZLe e0 e1 with e -> (L.printf 0 "mkZLe %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZGt e0 e1 =
    try mkZGt e0 e1 with e -> (L.printf 0 "mkZGt %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkZGe e0 e1 =
    try mkZGe e0 e1 with e -> (L.printf 0 "mkZGe %a %a failed" fmt e0 fmt e1 ; raise(e))

  let mkIte e0 e1 e2 =
    try mkIte e0 e1 e2 with e -> (L.printf 0 "mkIte %a %a %a failed" fmt e0 fmt e1 fmt e2 ; raise(e))

end

(*============================================================================
  ESP data structure functions.
  ============================================================================*)

module SESPExt = struct

let rec fold_expr fn_symb fn_expr expr z =
  let open SESP in
  let z = fn_expr expr z in
  match Expr.refine expr with
  | ExprLEAF(SYMBOL(s)) ->
      fn_symb s z
  | ExprUNARY(_, e) ->
      fold_expr fn_symb fn_expr e z
  | ExprASSIGN(_,(e,f))
  | ExprINCREMENT(_,(e,f))
  | ExprCOMPARE(_,(e,f))
  | ExprBINARY(_,(e,f)) ->
      fold_expr fn_symb fn_expr e (fold_expr fn_symb fn_expr f z)
  | ExprTERNARY(_,(e,f,g)) ->
      fold_expr fn_symb fn_expr e (fold_expr fn_symb fn_expr f (fold_expr fn_symb fn_expr g z))
  | ExprNARY(_,es) ->
      Array.fold_right (fold_expr fn_symb fn_expr) es z
  | _ ->
      z

let fold_node fn_symb fn_expr fn_node node z =
  let open SESP in
  let z = fn_node node z in
  match Node.refine node with
  | Decl(s,_,_)
  | StaticDeclBegin(s,_,_) ->
      fn_symb s z
  | CallReturn(Some(e),_)
  | Branch(e,_,_)
  | Expression(Some(e),_)
  | Return(e,_) ->
      fold_expr fn_symb fn_expr e z
  | Call( (Direct(symb) | DirectMethod(symb) | Virtual(symb) | Intrinsic(symb)), es,_,_,_,_) ->
      fn_symb symb (Array.fold_right (fold_expr fn_symb fn_expr) es z)
  | Call( (Indirect(e) | IndirectMethod(e)), es,_,_,_,_) ->
      fold_expr fn_symb fn_expr e (Array.fold_right (fold_expr fn_symb fn_expr) es z)
  | Switch(e, ens,_) ->
      Array.fold_right (fun (e,_) z -> fold_expr fn_symb fn_expr e z) ens (fold_expr fn_symb fn_expr e z)
  | _ ->
      z

let fold_cfg fn_symb fn_expr fn_node cfg z =
  let open SESP in
  let formals,_ = Cfg.formals cfg in
  let freturn = Cfg.formal_return cfg in
  z |>
  Array.fold_right fn_symb formals |>
  Option.fold fn_symb freturn |>
  Array.fold_right (fold_node fn_symb fn_expr fn_node) (Cfg.nodes cfg)


(* Types *)
let rec get_type ty =
  let open SESP in
  let open Type in
  match refine ty with
  | TypeAlias(_,ty) | TypeModifier(_,ty) -> get_type ty
  | ety -> ety

(* SI: why is this MM? In fe_slam too. *)
module CfgHMMap = HashMultiMap.Make(struct
  type t = SESP.symb
  let equal = Pervasives.( = )
  let hash = Hashtbl.hash
end)

module CfgNodeHMap = HashMap.Make(struct
  type t = SESP.symb * SESP.id
  let equal = Pervasives.( = )
  let hash = Hashtbl.hash
end)

module CfgSymbHMap = HashMap.Make(struct
  type t = SESP.symb  * SESP.symb
  let equal = Pervasives.( = )
  let hash = Hashtbl.hash
end)

module SymbHMap = HashMap.Make(struct
  type t = SESP.symb
  let equal = Pervasives.( = )
  let hash = Hashtbl.hash
end)


(* fmt *)

let fmt_assign_op ff = function
  | SESP.ASSIGN           -> Format.fprintf ff "="
  | SESP.ASSIGNPLUS       -> Format.fprintf ff "+="
  | SESP.ASSIGNMINUS      -> Format.fprintf ff "-="
  | SESP.ASSIGNMULT       -> Format.fprintf ff "*="
  | SESP.ASSIGNDIV        -> Format.fprintf ff "/="
  | SESP.ASSIGNREM        -> Format.fprintf ff "%%="
  | SESP.ASSIGNAND        -> Format.fprintf ff "&="
  | SESP.ASSIGNOR         -> Format.fprintf ff "|="
  | SESP.ASSIGNXOR        -> Format.fprintf ff "^="
  | SESP.ASSIGNLSHIFT     -> Format.fprintf ff "<<="
  | SESP.ASSIGNRSHIFT     -> Format.fprintf ff ">>="
  | SESP.ASSIGNRSHIFTU    -> Format.fprintf ff ">>="
  | SESP.DOTSTAR          -> Format.fprintf ff ".*"
  | SESP.ARROWSTAR        -> Format.fprintf ff "->*"

let assign_op_conv op =
  (match op with
  | SESP.ASSIGN           -> None
  | SESP.ASSIGNPLUS       -> Some (SESP.PLUS)
  | SESP.ASSIGNMINUS      -> Some (SESP.MINUS)
  | SESP.ASSIGNMULT       -> Some (SESP.MULT)
  | SESP.ASSIGNDIV        -> Some (SESP.DIV)
  | SESP.ASSIGNREM        -> Some (SESP.REM)
  | SESP.ASSIGNAND        -> Some (SESP.BITAND)
  | SESP.ASSIGNOR         -> Some (SESP.BITOR)
  | SESP.ASSIGNXOR        -> Some (SESP.XOR)
  | SESP.ASSIGNLSHIFT     -> Some (SESP.LSHIFT)
  | SESP.ASSIGNRSHIFT     -> Some (SESP.RSHIFT)
  | SESP.ASSIGNRSHIFTU    -> Some (SESP.RSHIFTU)
  | SESP.DOTSTAR
  | SESP.ARROWSTAR        -> failwith "I dont know what this does")

let fmt_increment_op ff = function
  | SESP.PREINCR  | SESP.POSTINCR  ->
      Format.fprintf ff "++"
  | SESP.PREDECR  | SESP.POSTDECR  ->
      Format.fprintf ff "--"

let fmt_binary_op ff = function
  | SESP.BITAND   -> Format.fprintf ff "&"
  | SESP.BITOR    -> Format.fprintf ff "|"
  | SESP.XOR      -> Format.fprintf ff "^"
  | SESP.DIV      -> Format.fprintf ff "\\"
  | SESP.LSHIFT   -> Format.fprintf ff "<<"
  | SESP.MINUS    -> Format.fprintf ff "-"
  | SESP.MULT     -> Format.fprintf ff "*"
  | SESP.PLUS     -> Format.fprintf ff "+"
  | SESP.REM      -> Format.fprintf ff "%%"
  | SESP.RSHIFT   -> Format.fprintf ff ">>"
  | SESP.INDEX    -> Format.fprintf ff "[]"
  | SESP.PLUS_OVF -> Format.fprintf ff "+o"
  | SESP.MINUS_OVF -> Format.fprintf ff "-o"
  | SESP.MULT_OVF -> Format.fprintf ff "*o"
  | SESP.RSHIFTU  -> Format.fprintf ff ">>u"
  | SESP.BINARY   -> Format.fprintf ff "binop"

let fmt_compare_op ff = function
  | SESP.EQUALS  -> Format.fprintf ff "=="
  | SESP.GE      -> Format.fprintf ff ">="
  | SESP.GT      -> Format.fprintf ff ">"
  | SESP.LE      -> Format.fprintf ff "<="
  | SESP.LEU     -> Format.fprintf ff "<=u"
  | SESP.LT      -> Format.fprintf ff "<"
  | SESP.LTU     -> Format.fprintf ff "<u"
  | SESP.NE      -> Format.fprintf ff "!="

let fmt_symb ff symb =
  let symb_str = SESP.Symb.undecoratedname symb in
  Format.fprintf ff "%s" symb_str

let rec fmt_expr ff pe =
  fmt_expr_ ff pe
(*   Format.fprintf ff "%a : %s" fmt_expr_ pe (SESP.Type.to_string (SESP.Expr.ctype pe)) *)

and fmt_expr_ ff pe =
  match SESP.Expr.refine pe with
  | SESP.ExprLEAF leaf -> (
      match leaf with
      | SESP.NOP -> Format.fprintf ff "NOP"
      | SESP.CONSTANT i -> Format.fprintf ff "(%Ld)" i
      | SESP.FLOAT    f -> Format.fprintf ff "(%f)" f
      | SESP.STRING(_, str, _)-> Format.fprintf ff "(\"%s\")" str
      | SESP.FIELD ->
          let symb = SESP.Expr.field pe in
          let symb_str =
            match symb with
            | Some s -> SESP.Symb.name s
            | None -> failwith "fmt_expr:FIELD"
          in
          Format.fprintf ff "(\"%s\")" symb_str
      | SESP.SYMBOL symb ->
          Format.fprintf ff "(%a)" fmt_symb symb
      | SESP.CURRENTEXCEPTION ->
          Format.fprintf ff "current_exn"
      | SESP.NEWOBJ ->
          Format.fprintf ff "new_obj"
      | SESP.TYPEID_TYPE ->
          Format.fprintf ff "typeid_ty")
  | SESP.ExprUNARY(SESP.DOT,e1) ->
      let fldo = SESP.Expr.field pe in
      (match fldo with
      | Some fld -> Format.fprintf ff "(%a . %a)" fmt_expr e1 fmt_symb fld
      | None -> failwith "ExprUnary(DOT,_) has no fld")
  | SESP.ExprUNARY(SESP.ARROW,e1) ->
      let fldo = SESP.Expr.field pe in
      (match fldo with
      | Some fld -> Format.fprintf ff "(%a -> %a)" fmt_expr e1 fmt_symb fld
      | None -> failwith "ExprUnary(ARROW,_) has no fld")
  | SESP.ExprUNARY(SESP.NEGATE, e1)  -> Format.fprintf ff "(- %a)" fmt_expr e1
  | SESP.ExprUNARY(SESP.NOT, e1)     -> Format.fprintf ff "(! %a)" fmt_expr e1
  | SESP.ExprUNARY(SESP.BITNOT, e1)  -> Format.fprintf ff "(~ %a)" fmt_expr e1
  | SESP.ExprUNARY(SESP.UPLUS, e1)   -> Format.fprintf ff "(+ %a)" fmt_expr e1
  | SESP.ExprUNARY(SESP.ADDRESS, e1) -> Format.fprintf ff "(& %a)" fmt_expr e1
  | SESP.ExprUNARY(SESP.DEREF, e1)   -> Format.fprintf ff "(* %a)" fmt_expr e1
  | SESP.ExprUNARY(_, e1)            -> Format.fprintf ff "(%s %a)" (SESP.Expr.kind_as_string pe) fmt_expr e1
  | SESP.ExprASSIGN(op,(e1,e2))  ->
      Format.fprintf ff "(%a %a %a)"
        fmt_expr e1 fmt_assign_op op fmt_expr e2
  | SESP.ExprINCREMENT(op,(e1,_e2)) -> (
      match op with
      | SESP.PREINCR | SESP.PREDECR ->
          Format.fprintf ff "(%a %a)"
            fmt_increment_op op fmt_expr e1
      | SESP.POSTINCR | SESP.POSTDECR ->
          Format.fprintf ff "(%a %a)"
            fmt_expr e1 fmt_increment_op op)
  | SESP.ExprCOMPARE(op,(e1,e2)) ->
      Format.fprintf ff "(%a %a %a)"
        fmt_expr e1 fmt_compare_op op fmt_expr e2
  | SESP.ExprBINARY(op,(e1,e2)) ->
      Format.fprintf ff "(%a %a %a)"
        fmt_expr e1 fmt_binary_op op fmt_expr e2
  | SESP.ExprTERNARY(_,(e1,e2,e3)) ->
      Format.fprintf ff "(%a ? %a : %a)"
        fmt_expr e1 fmt_expr e2 fmt_expr e3
  | SESP.ExprNARY(SESP.ARRAY_INIT, ee) ->
      Format.fprintf ff "([%a])"
        (List.fmt "," fmt_expr) (Array.to_list ee)
  | SESP.ExprNARY(SESP.STRUCT_INIT, ee)  ->
      Format.fprintf ff "({%a})"
        (List.fmt "," fmt_expr) (Array.to_list ee)

let rec fmt_type ff t =
  let open SESP in
  match Type.refine t with
  | TypeUnknown -> Format.fprintf ff "unknown"
  | TypeVoid -> Format.fprintf ff "void"
  | TypeBool -> Format.fprintf ff "bool"
  | TypeChar(unsigned,_size) ->
      Format.fprintf ff "%s char" (if unsigned then "unsigned" else "" )
  | TypeInteger(unsigned,_size) ->
      Format.fprintf ff "%s int" (if unsigned then "unsigned" else "" )
  | TypeFloatingPoint(_size) -> Format.fprintf ff "float"
  | TypeModifier((const,vol),ty') ->
      Format.fprintf ff "%s %s %a"
        (if const then "const" else "")
        (if vol then "volatile" else "")
        fmt_type ty'
  | TypePointer(ty') -> Format.fprintf ff "*%a" fmt_type ty'
  | TypeCXXReference(ty') -> Format.fprintf ff "%a &" fmt_type ty'
  | TypeCXXArray(ty',Some size) ->
      Format.fprintf ff "%a[%d]" fmt_type ty' size
  | TypeCXXArray(ty',None) ->
      Format.fprintf ff "%a[]" fmt_type ty'
  | TypeEnum(id,ty') ->
      Format.fprintf ff "enum %s %a"
        (Symb.undecoratedname id) fmt_type ty'
  | TypeAlias(id,ty') ->
      Format.fprintf ff "typedef %a %s"
        fmt_type ty' (Symb.undecoratedname id)
  | TypeClass(id,aggr) ->
      Format.fprintf ff "class %s { %a } "
        (Symb.undecoratedname id) fmt_ctype_aggr aggr
  | TypeStruct(id,aggr) ->
      Format.fprintf ff "struct %s { %a } "
        (Symb.undecoratedname id) fmt_ctype_aggr aggr
  | TypeUnion(id,aggr) ->
      Format.fprintf ff "union %s { %a } "
        (Symb.undecoratedname id) fmt_ctype_aggr aggr
  | TypeFunction(f) -> Format.fprintf ff "%a" fmt_ctype_func f
  | TypeMethod(m) ->  Format.fprintf ff "%a" fmt_ctype_func m

and fmt_ctype_aggr ff aggr =
    let flds = Array.to_list (SESP.Type.fields aggr) in
    Format.fprintf ff "%a" (List.fmt ";" fmt_symb) flds

and fmt_ctype_func ff f =
    let ret = SESP.Type.returns f in
    let args,_varg = SESP.Type.arguments f in
    Format.fprintf ff "%a -> %a"
      (List.fmt "->" fmt_type) (Array.to_list args)
      fmt_type ret

let rec fmt_node ff node =
  let n_id = SESP.Node.id node in
  match SESP.Node.refine node with
  | SESP.Entry(succ,_pre) ->  Format.fprintf ff "%d:Entry(%d)" n_id (SESP.Node.id succ)
  | SESP.Exit(_post) -> Format.fprintf ff "%d:Exit" n_id
  | SESP.ExceptionExit -> Format.fprintf ff "%d:ExceptionExit" n_id
  | SESP.Pattern(_) -> Format.fprintf ff "Pattern"
  | SESP.Event -> Format.fprintf ff "Event"
  | SESP.Call(schema, args, _call_retn, _call_unwind, _pre, _post) ->
      let args = Array.to_list args in
        (match schema with
        | (SESP.Direct symb) ->
            Format.fprintf ff "%d:@ DirectCall(%s, @[%a@],_) "
              n_id (SESP.Symb.undecoratedname symb) (List.fmt "," fmt_expr) args
        | (SESP.Indirect fp) ->
            Format.fprintf ff "%d:@ InDirectCall(%a, @[%a@],_) "
              n_id fmt_expr fp (List.fmt "," fmt_expr) args
        | SESP.DirectMethod(symb) ->
            Format.fprintf ff "%d:@ DirectMethod(%s, @[%a@],_) "
              n_id (SESP.Symb.undecoratedname symb) (List.fmt "," fmt_expr) args
        | SESP.IndirectMethod(fp) ->
            Format.fprintf ff "%d:@ InDirectMethod(%a, @[%a@],_) "
              n_id fmt_expr fp (List.fmt "," fmt_expr) args
        | SESP.Virtual(symb) ->
            Format.fprintf ff "%d:@ Virtual(%s, @[%a@],_) "
              n_id (SESP.Symb.undecoratedname symb) (List.fmt "," fmt_expr) args
        | SESP.Intrinsic(symb) ->
            Format.fprintf ff "%d:@ Intrinsic(%s, @[%a@],_) "
              n_id (SESP.Symb.undecoratedname symb) (List.fmt "," fmt_expr) args
        )
  | SESP.CallReturn(pe_o, None) ->
      Format.fprintf ff "%d:@ CallReturn(%a, None)"
        n_id (Option.fmt "None" fmt_expr) pe_o
  | SESP.CallReturn(pe_o, Some succ) ->
      Format.fprintf ff "%d:@ CallReturn(%a, %d)"
        n_id (Option.fmt "None" fmt_expr) pe_o
        (SESP.Node.id succ)
  | SESP.CallUnwind(_n) -> Format.fprintf ff "CallUnwind"
  | SESP.Decl(s,ty,succ) -> Format.fprintf ff "%d:Decl(%s,%a,%d)"
      n_id (SESP.Symb.undecoratedname s) fmt_type ty (SESP.Node.id succ)
  | SESP.StaticDeclBegin(_s,_ct,_n) -> Format.fprintf ff "StaticDeclBegin"
  | SESP.StaticDeclEnd(_n) -> Format.fprintf ff "StaticDeclBegin"
  | SESP.Endscope -> Format.fprintf ff "Endscope"
  | SESP.Throw(_n) -> Format.fprintf ff "Throw"
  | SESP.RaiseException -> Format.fprintf ff "RaiseException"
  | SESP.CatchBegin(_n) -> Format.fprintf ff "CatchBegin"
  | SESP.CatchEnd(_n) -> Format.fprintf ff "CatchEnd"
  | SESP.Branch(pe, n1, n2) ->
      Format.fprintf ff "%d:@ Branch(%a, %i, %i)"
        n_id fmt_expr pe (SESP.Node.id n1) (SESP.Node.id n2)
  | SESP.Switch(pe1, label_bodys, default) ->
      let label_bodys = Array.to_list label_bodys in
      let rec loop label_bodys =
        match label_bodys with
        | []  -> ()
        | (label,body) :: rest ->
            (Format.fprintf ff "case(%a): %i; "
               fmt_expr label (SESP.Node.id body)) ;
            loop rest
      in
      Format.fprintf ff "Switch(%a)" fmt_expr pe1 ;
      (loop label_bodys) ;
      (Format.fprintf ff "default(%i)" (SESP.Node.id default))
  | SESP.Expression(pe_o, None) ->
      Format.fprintf ff "%d:@ Expression(%a, NOHALT)"
        n_id (Option.fmt "None" fmt_expr) pe_o
  | SESP.Expression(pe_o, Some succ) ->
      Format.fprintf ff "%d:@ Expression(%a, %d)"
        n_id (Option.fmt "None" fmt_expr) pe_o (SESP.Node.id succ)
  | SESP.Return(pe, exit) ->
      Format.fprintf ff "%d:@ Return(%a, %d)"
        n_id fmt_expr pe (SESP.Node.id exit)
  | SESP.Assume(n, _) ->
      Format.fprintf ff "%d:@ Assume(%a)"
        n_id fmt_node n
  | SESP.Verify(n, _) ->
      Format.fprintf ff "%d:@ Verify(%a)"
        n_id fmt_node n
  | SESP.Asm(_succ) -> Format.fprintf ff "Asm"
  | SESP.Nop(succ) -> Format.fprintf ff "%d:Nop(%d)" n_id (SESP.Node.id succ)


(*****************************************************************************)
(* Hard-coded names                                                          *)
(*****************************************************************************)
let proc_is_agg_static_init n = n = "aggregate_static_initializer"
let proc_is_agg_dyn_init    n = n = "aggregate_dynamic_initializer"


(*****************************************************************************)
(* Heapification                                                             *)
(*****************************************************************************)

(* Calculate vars to heapify. We heapify:
   (1) globals of aggregate type;
   (2) locals of aggregate type;
   (3) locals whose address is taken.
   (4) Addresses of functions.
   (5) Formals whose address is taken
   (6) Structs that are passed by copying.*)
type heapify_tag = HGlobal | HLocalStruct | HAddrOfLocal | HAddrOfFunc | HFormal | HFormalStruct

let to_heapify_type ty =
  (fun b -> L.printf 6 "to_heapify_type (%a) = %b" fmt_type ty b)
  <&
  let open SESP in
  match get_type ty with
  | TypeStruct(_)
  | TypeUnion(_)
  | TypeCXXArray(_) -> true
  | _ -> false

let heapify_type ty =
  match Typ.desc ty with
  (* Heapification does not change the type of arrays,
     but ESP has already wrapped & around every array-typed expr. *)
  | Typ.Array(_) ->  ty
  | _ -> Typ.mkPointer (ty)

let to_heapify_acc global_pred cfg vars_to_heapify =
  let open SESP in
  let fn_symb symb tbl =
    if Symb.kind symb = ESP.ESP_IR_SK_GLOBAL && global_pred symb then (
        L.printf 6 "heapify %a G" fmt_symb symb ;
        SymbHMap.add tbl symb HGlobal
    )
    else if to_heapify_type (Symb.ctype symb) then (
        let open ESP in
        if     Symb.kind symb = ESP_IR_SK_FORMAL
           || (Symb.kind symb = ESP_IR_SK_RETURN_VALUE && Some(symb) = (Cfg.formal_return cfg))
        then (
            L.printf 6 "heapify %a FS" fmt_symb symb ;
            SymbHMap.add tbl symb HFormalStruct
        )
        else if Symb.kind symb = ESP_IR_SK_LOCAL
             || Symb.kind symb = ESP_IR_SK_RETURN_VALUE
             || Symb.kind symb = ESP_IR_SK_TEMPORARY
        then (
            L.printf 6 "heapify %a LS" fmt_symb symb ;
            SymbHMap.add tbl symb HLocalStruct
        )
        else
            L.printf 1 "heapify %a unexpected kind: %s" fmt_symb symb (Symb.kind_as_string symb)
    );
    tbl
  in
  let fn_expr expr tbl =
    (match Expr.refine expr with
    | ExprUNARY(ADDRESS, e0) ->
        (match Expr.refine e0 with
        | ExprLEAF(SYMBOL(symb)) ->
            let open ESP in
            (match Symb.kind symb with
            | ESP_IR_SK_LOCAL ->
                L.printf 6 "heapify %a &L" fmt_symb symb ;
                SymbHMap.add vars_to_heapify symb HAddrOfLocal
            | ESP_IR_SK_FUNCTION ->
                L.printf 6 "heapify %a &F" fmt_symb symb ;
                SymbHMap.add vars_to_heapify symb HAddrOfFunc
            | ESP_IR_SK_GLOBAL ->
                L.printf 6 "heapify %a &G" fmt_symb symb ;
                SymbHMap.add vars_to_heapify symb HGlobal
            | ESP_IR_SK_FORMAL ->
                L.printf 6 "heapify %a &FO" fmt_symb symb ;
                SymbHMap.add vars_to_heapify symb HFormal
            | _ ->
                failwithf "Unexpected address-of symbol %a of kind %s" fmt_symb symb (Symb.kind_as_string symb)
            )
        | _ ->
            ()
        )
    | _ ->
        ()
    );
    tbl
  in
  let fn_node _node tbl =
    tbl
  in
  fold_cfg fn_symb fn_expr fn_node cfg vars_to_heapify

let to_heapify global_pred cfg =
  to_heapify_acc global_pred cfg (SymbHMap.create 255)

let to_heapify_cfgs global_pred cfgs =
  StringHMap.fold (fun _ -> to_heapify_acc global_pred) cfgs (SymbHMap.create 255)

end (* module SESPExt *)


(*****************************************************************************)
(* Symbol management for linking                                             *)
(*****************************************************************************)

let of_symb mk =
  let tbl = StringHMap.create 1024 in
  fun symb meta ->
    let globally_unique_name = SESP.Symb.name symb in
    let name = SESP.Symb.undecoratedname symb in
    try
      StringHMap.find tbl globally_unique_name
    with Not_found ->
      let res = mk name meta in
      StringHMap.add tbl globally_unique_name res ;
      res

let var_of = of_symb Var.gensym

let proc_of =
  let closure = of_symb (fun name () -> Proc.Id.gensym name) in
  fun symb -> closure symb ()


(*****************************************************************************)
(* Program data structure functions.                                         *)
(*****************************************************************************)

let mkLoad x e p = I.mk (I.Load(x,e)) p
let mkStore e f p = I.mk (I.Store(e,f)) p

let mkKills vs pos blk = I.mk (I.Kill(vs)) pos :: blk

let mkKill v pos blk = mkKills (Vars.singleton v) pos blk

let mkMove x e p = I.mk (I.Move(x,e)) p

let mkAlloc pos v ty =
  L.printf 4 "heap_vars %a" Var.fmt v ;
  match Typ.desc ty with
  | Typ.Array _ ->
      [ I.mk (I.Alloc(v, E.mkNum(Typ.sizeof ty))) pos ]
  | _ ->
      [ I.mk (I.Alloc(v, E.mkNum(Typ.sizeof ty))) pos
      ; I.mk (I.Cast(v, Typ.mkPointer ty, E.mkVar v)) pos ]

let mkFree pos v = I.mk (I.Free(E.mkVar v)) pos

let no_insts = []
let mkNop pos = C.Inst(I.mk I.Nop pos)


(*****************************************************************************)
(* Translation of types                                                      *)
(*****************************************************************************)

let isStructUnion ety =
  match SESP.Type.refine ety with
  | SESP.TypeStruct(_,_) | SESP.TypeUnion(_,_) -> true
  | _ -> false

module TypeHMap = HashMap.Make (struct
  type t = SESP.ctype
  let equal x y =
    Pervasives. (==) x y || SESP.Type.types_equal x y
  let hash = Hashtbl.hash
end)

(* Keep track of recursive types. *)
let ctype_memo : Typ.t option TypeHMap.t = TypeHMap.create 101

let fld_tbl = PolyHMap.create 1024

let off_of_ctype_member member =
  let fo = Option.from_some (SESP.Symb.get_field_offset member) in
  if SESP.Symb.is_bit_field member then
      (fo, SESP.Symb.get_bit_field_offset member)
  else
      (fo, None)

let rec type_of_esp_type ety =
  L.incf 8 "( xlate_type %s" (SESP.Type.to_string ety) ;
  L.decf 8 ") xlate_type: %a" Typ.fmt
  <&
  match TypeHMap.tryfind ctype_memo ety with
  | None -> (* [ety] not yet xlated *)
      L.printf 10 "+ NotYetXlated" ;
      (* only memoize aggregates so that the hack that breaks cycles with Top does not hide Ptr types *)
      if(isStructUnion ety) then TypeHMap.add ctype_memo ety None ;
      let size = Int64.of_int (SESP.Type.sizeof_size ety) in
      let ty' =
        (match SESP.Type.refine ety with
        | SESP.TypeUnknown -> failwith "Can't xlate SESP.TypeUnknown"
        | SESP.TypeVoid -> Typ.mkTop
        | SESP.TypeBool -> Typ.mkBool
        | SESP.TypeChar(u,s)
        | SESP.TypeInteger(u,s) -> Typ.mkInt u s
        | SESP.TypeFloatingPoint(s) -> Typ.mkFloat s
        | SESP.TypeModifier((_,_),t) -> type_of_esp_type t
        | SESP.TypePointer p -> Typ.mkPointer(type_of_esp_type p)
        | SESP.TypeCXXReference p ->  Typ.mkPointer(type_of_esp_type p)
        | SESP.TypeCXXArray(a, len) ->
            Typ.mkArray (type_of_esp_type a) (Option.map Int64.of_int len) size
        |(SESP.TypeStruct(id,aggr_info)
        | SESP.TypeUnion(id,aggr_info)) as ty ->
            let aggr_name = SESP.Symb.name id in
            let fld_typs =
              Array.fold_right (fun member fld_typs ->
                let name = SESP.Symb.undecoratedname member in
                let off = off_of_ctype_member member in
                let fld = Fld.mk off name in
                let typ = type_of_esp_type (SESP.Symb.ctype member) in
                (fld, typ) :: fld_typs
              ) (SESP.Type.fields aggr_info) []
            in
            let typ =
              match ty with
              | SESP.TypeStruct _ -> Typ.mkStructure aggr_name fld_typs size
              | SESP.TypeUnion _ -> Typ.mkUnion aggr_name fld_typs size
              | _ -> assert false
            in
            (match Typ.desc typ with
            | Typ.Structure(_,fts,_) | Typ.Union(_,fts,_) ->
                List.iter (fun (f,_) ->
                  PolyHMap.add fld_tbl (ety, Fld.off f) f
                ) fts
            | _ -> ()
            );
            typ
        | SESP.TypeEnum(_, underlying_ty) -> type_of_esp_type underlying_ty
        | SESP.TypeAlias(_, t) -> type_of_esp_type t
        | SESP.TypeFunction(t) ->
            let (arguments_array, vararg) = SESP.Type.arguments t in
            let return_type = SESP.Type.returns t in
            Typ.mkFunction (type_of_esp_type return_type)
              (List.map (fun ty -> (type_of_esp_type ty))
                 (Array.to_list arguments_array))
              vararg
        | SESP.TypeClass _ | SESP.TypeMethod _ -> failwith "May call Type of C++"
        )
      in
      if (isStructUnion ety) then TypeHMap.add ctype_memo ety (Some ty') ;
      ty'

  | Some (None) -> (* ety being xlated, we're in a cycle. *)
      L.printf 10 "+ InACycle" ;
      Typ.mkTop (* break the recursion *)

  | Some (Some(ty)) -> (* ety already xlated *)
      L.printf 10 "+ AlreadyXlated" ;
      ty


(* ESP does not ensure uniqueness of names of field symbols,
   so identify them by their enclosing type and offset *)
let fld_of typ symb =
  let rec get_aggregate typ =
    match SESP.Type.refine typ with
    | SESP.TypeStruct _ | SESP.TypeUnion _ ->
        typ
    | SESP.TypePointer(typ) | SESP.TypeCXXReference(typ)
    | SESP.TypeModifier(_,typ) | SESP.TypeAlias(_,typ) ->
        get_aggregate typ
    | _ ->
        failwithf "aggregate type expected: %s" (SESP.Type.to_string typ)
  in
  (* ensure typ is translated before trying to look up its fields *)
  let _ = type_of_esp_type typ in
  let typ = get_aggregate typ in
  let off = off_of_ctype_member symb in
  PolyHMap.find fld_tbl (typ, off)


(*****************************************************************************)
(* Copy of SESP.Expr data structure.                                        *)
(*****************************************************************************)

(*
  I think I need to the heapification on some source (Esp.Expr) data
  structure. Both fe_slam and fe_cil did this on PExpr.Loc and Cil.Expr
  respectively. But I can't ctor SESP.Expr, or even ctor SESP.expr_schema,
  as SESP exposes no expr ctors for me. Hence this copy of SESP.Expr.
*)

module Expr = struct

  type convert_type =
    | Convert_PTR_UNRELATED
    | Convert_REINTERPRET
    | Convert_WIDEN
    | Convert_NARROW
    | Convert_FLOATING

  type expr_desc =
    | ExprConstInt of int64
    | ExprConstFloat of float
    | ExprConstString of int * string * bool
    | ExprSymb of SESP.symb
    | ExprSymbSlayer of Var.t  (* Used to allow creation of new intermediates *)
    | ExprAddr of expr
    | ExprDeref of expr
    | ExprDot of expr * SESP.symb
    | ExprNegate of expr
    | ExprUplus of expr
    | ExprNot of expr
    | ExprBitNot of expr
    | ExprConvert of convert_type * expr
    | ExprAssign of SESP.expr_schema_BINARY option * expr * expr
  (* SI: missing other ASSIGNs *)
    | ExprIncr of SESP.expr_schema_INCREMENT * expr * expr
    | ExprCompare of SESP.expr_schema_COMPARE * expr * expr
    | ExprBinary of SESP.expr_schema_BINARY * expr * expr
  (*     | SESP.ExprTERNARY(_, (_, _, _)) -> *)
    | ExprArrayInit of expr list
    | ExprStructInit of expr list
    (* Used for encoding some complex features *)
    | ExprSeq of expr list
  (* SI: There're segfaults if we immediately translate Expr.ctype to
     Typ.t. So, instead we keep SESP.ctype. [heapify_expr] sets
     var_heapified, so that [type_of_expr] returns the right type. *)
  and expr = {
    desc : expr_desc;
    var_heapified : bool;
    ty : SESP.ctype;
  (* ty : Typ.t *)
  }


  (* Printer *)
  let rec fmt_expr ff e =
    match e.desc with
    | ExprConstInt(i) -> Format.fprintf ff "(%Ld)" i
    | ExprConstFloat(f) -> Format.fprintf ff "(%f)" f
    | ExprConstString(_,s,_) -> Format.fprintf ff "(\"%s\")" s
    | ExprSymb(symb) -> Format.fprintf ff "(%a)" SESPExt.fmt_symb symb
    | ExprSymbSlayer(var) -> Format.fprintf ff "(%a)" Var.fmt var
    | ExprAddr(e0) -> Format.fprintf ff "(& %a)" fmt_expr e0
    | ExprDeref(e0) -> Format.fprintf ff "(*%a)" fmt_expr e0
    | ExprDot(e0, fld) -> Format.fprintf ff "(%a.%a)" fmt_expr e0 SESPExt.fmt_symb fld
    | ExprNegate(e0) -> Format.fprintf ff "(- %a)" fmt_expr e0
    | ExprUplus(e0) -> Format.fprintf ff "(+ %a)" fmt_expr e0
    | ExprNot (e0) -> Format.fprintf ff "(! %a)" fmt_expr e0
    | ExprBitNot(e0) -> Format.fprintf ff "(~ %a)" fmt_expr e0
    | ExprConvert(_t,e0) -> Format.fprintf ff "((T) %a)" fmt_expr e0
    | ExprAssign(op, lhs,rhs) -> Format.fprintf ff "(%a %a= %a)" fmt_expr lhs (Option.fmt "" (SESPExt.fmt_binary_op)) op fmt_expr rhs
    | ExprIncr(op,e0,_e1) -> (
        match op with
        | SESP.PREINCR | SESP.PREDECR ->
            Format.fprintf ff "(%a %a)"
              SESPExt.fmt_increment_op op fmt_expr e0
        | SESP.POSTINCR | SESP.POSTDECR ->
            Format.fprintf ff "(%a %a)"
              fmt_expr e0 SESPExt.fmt_increment_op op)
    | ExprCompare(op,e0,e1) -> Format.fprintf ff "(%a %a %a)"
        fmt_expr e0 SESPExt.fmt_compare_op op fmt_expr e1
    | ExprBinary(op,e0,e1) ->
        Format.fprintf ff "(%a %a %a)"
          fmt_expr e0 SESPExt.fmt_binary_op op fmt_expr e1
    | ExprArrayInit(ee) -> Format.fprintf ff "[%a]" (List.fmt "," fmt_expr) ee
    | ExprStructInit(ee) -> Format.fprintf ff "{%a}" (List.fmt "," fmt_expr) ee
    | ExprSeq(ee) -> Format.fprintf ff "(@[%a@])" (List.fmt ";@ " fmt_expr) ee
  let fmt_expr_typ ff e =
(*     Format.fprintf ff "%a : %s" fmt_expr e (SESP.Type.to_string e.ty) *)
    Format.fprintf ff "%a:" fmt_expr e


  (* Vanilla constructors and destructors *)
  let mk_expr desc ?(var_heapified=false) ty  = { desc; ty; var_heapified; }

  (* Make an assignment expression, deals with expanding a struct copy is required.  *)
  let mk_expr_assign op lhs rhs =
    let rec mk_expr_assign lhs rhs =
      let ty = lhs.ty in
      match SESPExt.get_type ty with
      | SESP.TypeStruct _ ->
          mk_member_assign lhs rhs
      | _ ->
          mk_expr (ExprAssign(op, lhs, rhs)) ty

    and mk_member_assign lhs rhs =
      let ty = lhs.ty in
      match SESPExt.get_type ty, rhs.desc with
      | SESP.TypeStruct _, ExprAssign _ ->
          failwith "Unsupported nested struct assignment"
      | SESP.TypeStruct(_id, aggr_info), _ ->
          assert( op = None );
          let ee =
            Array.fold_right (fun member assigns ->
              let lhs = mk_expr (ExprDot(lhs, member)) (SESP.Symb.ctype member) in
              let rhs = mk_expr (ExprDot(rhs, member)) (SESP.Symb.ctype member) in
              (mk_member_assign lhs rhs) :: assigns
            ) (SESP.Type.fields aggr_info) [] in
          mk_expr (ExprSeq ee) ty
      | SESP.TypeCXXArray _, ExprArrayInit _ ->
          mk_expr (ExprAssign(op, lhs, rhs)) ty
      | SESP.TypeCXXArray(ety, Some len), _ ->
          (* This only occurs as a part of structure copy *)
          let ee = ref [] in
          for i = len - 1 downto 0 do
            (* WARNING: The type of index should be integer, but there is no way to construct the appropriate
               symbol. The translation of ExprConstInt does not look at the type so we put in garbage here! *)
            let index = mk_expr (ExprConstInt(Int64.of_int i)) ety in
            let lhs = mk_expr (ExprBinary(SESP.INDEX, lhs, index)) ety in
            let rhs = mk_expr (ExprBinary(SESP.INDEX, rhs, index)) ety in
            ee := (mk_member_assign lhs rhs) :: !ee
          done;
          mk_expr (ExprSeq !ee) ty
      | _ ->
          mk_expr (ExprAssign(op, lhs, rhs)) ty
    in
    mk_expr_assign lhs rhs


  let type_of_expr (e:expr) : Typ.t =
    let typ = type_of_esp_type e.ty in
    if e.var_heapified then SESPExt.heapify_type typ
    else typ


  (* Translate Esp.Expr to expr *)
  let rec expr_of_EspExpr ee =
    L.incf 8 "( expr_of_Esp.Expr %a" SESPExt.fmt_expr ee ;
    let ty = SESP.Expr.ctype ee in
(*     let ty = type_of_esp_type ty in *)
    let e = (
      match SESP.Expr.refine ee with
      (* LEAF *)
      | SESP.ExprLEAF(SESP.NOP) ->  failwith("LEAF(NOP)")
      | SESP.ExprLEAF(SESP.CONSTANT i) -> mk_expr (ExprConstInt(i)) ty
      | SESP.ExprLEAF(SESP.FLOAT f) -> mk_expr (ExprConstFloat(f)) ty
      | SESP.ExprLEAF(SESP.STRING (len, str, w)) -> mk_expr (ExprConstString(len,str,w)) ty
      | SESP.ExprLEAF(SESP.FIELD) -> failwith("LEAF(FIELD)")
      | SESP.ExprLEAF(SESP.SYMBOL symb) -> mk_expr (ExprSymb(symb)) ty
      | SESP.ExprLEAF(SESP.CURRENTEXCEPTION) -> failwith("LEAF(CURRENTEXCEPTION)")
      | SESP.ExprLEAF(SESP.NEWOBJ) ->  failwith("LEAF(NEWOBJ)")
      | SESP.ExprLEAF(SESP.TYPEID_TYPE) -> failwith("LEAF(TYPEID_TYPE)")
      | SESP.ExprUNARY(SESP.ADDRESS, ee0) -> mk_expr (ExprAddr(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.DEREF, ee0) -> mk_expr (ExprDeref(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.DOT, ee0) -> (
          match SESP.Expr.field ee with
          | Some fld ->
              mk_expr (ExprDot(expr_of_EspExpr ee0, fld)) (SESP.Symb.ctype fld)
          | None -> failwith "Dot has no field")
      | SESP.ExprUNARY(SESP.ARROW, p) -> (
          match SESP.Expr.field ee with
          | Some fld ->
              let p_ty = SESP.Expr.ctype p in
              let p = expr_of_EspExpr p in
              let star_p = mk_expr (ExprDeref(p)) p_ty in
              mk_expr (ExprDot(star_p, fld)) (SESP.Symb.ctype fld)
          | None -> failwith "Arrow has no field")
      | SESP.ExprUNARY(SESP.CXXCATCHTEST, _ee0) -> failwith("UNARY(CXXCATCHTEST)")
      | SESP.ExprUNARY(SESP.NEGATE, ee0) -> mk_expr (ExprNegate(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.UPLUS, ee0) -> mk_expr (ExprUplus(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.NOT, ee0) -> mk_expr (ExprNot(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.BITNOT, ee0) -> mk_expr (ExprBitNot(expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.CONVERT_PTR_UNRELATED,ee0) ->
          mk_expr (ExprConvert(Convert_PTR_UNRELATED, expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.CONVERT_REINTERPRET, ee0) ->
          mk_expr (ExprConvert(Convert_REINTERPRET, expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.CONVERT_WIDEN,ee0) ->
          mk_expr (ExprConvert(Convert_WIDEN, expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.CONVERT_NARROW, ee0) ->
          mk_expr (ExprConvert(Convert_NARROW, expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.CONVERT_FLOATING,ee0) ->
          mk_expr (ExprConvert(Convert_FLOATING, expr_of_EspExpr ee0)) ty
      | SESP.ExprUNARY(SESP.NEWARRAY, _ee0)
      | SESP.ExprUNARY(SESP.ARRAYLENGTH, _ee0) -> failwith("UNARY(C# array)")
      | SESP.ExprUNARY(SESP.BOX, _ee0)
      | SESP.ExprUNARY(SESP.UNBOX, _ee0) -> failwith("UNARY(C# boxing)")
      | SESP.ExprUNARY(SESP.DYNAMICCAST, _ee0) -> failwith("UNARY(DYNAMICCAST)")
      | SESP.ExprUNARY(SESP.TYPEID_EXPR, _ee0) -> failwith("UANRY(TYPEID_EXPR)")
      | SESP.ExprUNARY(SESP.UNARY, _ee0) ->  failwith("UNARY(UNARY)")
      | SESP.ExprASSIGN(op, (lhs,rhs)) ->
          mk_expr_assign (SESPExt.assign_op_conv op) (expr_of_EspExpr lhs) (expr_of_EspExpr rhs)
      | SESP.ExprINCREMENT (op, (e0, e1)) ->
          mk_expr (ExprIncr(op, expr_of_EspExpr e0, expr_of_EspExpr e1)) ty
      | SESP.ExprCOMPARE(op, (ee0, ee1)) ->
          mk_expr (ExprCompare(op, expr_of_EspExpr ee0, expr_of_EspExpr ee1)) ty
      | SESP.ExprBINARY(op, (ee0, ee1)) ->
          mk_expr (ExprBinary(op,  expr_of_EspExpr ee0, expr_of_EspExpr ee1)) ty
      | SESP.ExprTERNARY(_, (_, _, _)) -> failwith("TERNARY")
      | SESP.ExprNARY(SESP.ARRAY_INIT, ee) ->
          mk_expr (ExprArrayInit(List.map expr_of_EspExpr (Array.to_list ee))) ty
      | SESP.ExprNARY(SESP.STRUCT_INIT, ee) ->
          mk_expr (ExprStructInit(List.map expr_of_EspExpr (Array.to_list ee))) ty
    ) in
    e
    &> (fun e -> L.decf 8 ") %a" fmt_expr e)


  (* Transform e so that vars_to_heap vars are accessed as though they
     are on the heap, not on the stack. Leave & in for now, xlate_expr_rv
     will translate only valid &*e exprs.
     SI: Review.
  *)
  let heapify_expr vars_to_heap e =
    L.incf 8 "( heapify %a" fmt_expr e ;
    let rec heapify e =
      match e.desc with
      | ExprConstInt(_) -> e
      | ExprConstFloat(_) -> e
      | ExprConstString(_) -> e (* SI: we'll come back to const string *)
      | ExprSymbSlayer(_) -> e (* Assume heapified correctly by creation *)
      | ExprSymb(symb) ->
          if SESPExt.SymbHMap.mem vars_to_heap symb then
              (* Transform s:T into s : Ptr(T); return Deref(s) : T. *)
              (* Heapification does not change the type of arrays,
                 but ESP has already wrapped & around every array-typed expr,
                 so add a Deref also around heapified array vars. *)
              mk_expr (ExprDeref({ e with var_heapified= true })) e.ty
          else
              e
      | ExprAddr(e0) -> { e with desc= ExprAddr(heapify e0) }
      | ExprDeref(e0) -> { e with desc= ExprDeref(heapify e0) }
      | ExprDot(e0, fld) ->
          { e with desc= ExprDot(heapify e0, fld) }
      | ExprNegate(e0) ->
          { e with desc= ExprNegate(heapify e0) }
      | ExprUplus(e0) ->
          { e with desc= ExprUplus(heapify e0) }
      | ExprNot(e0) ->
          { e with desc= ExprNot(heapify e0) }
      | ExprBitNot(e0) ->
          { e with desc= ExprBitNot(heapify e0) }
      | ExprConvert(t, e0) ->
          { e with desc= ExprConvert(t, heapify e0) }
      | ExprAssign(op, lhs, rhs) ->
          { e with desc= ExprAssign(op, heapify lhs, heapify rhs) }
      | ExprIncr(op, e0, e1) ->
          { e with desc= ExprIncr(op, heapify e0, heapify e1) }
      | ExprCompare(op, e0, e1) ->
          { e with desc= ExprCompare(op, heapify e0, heapify e1) }
      | ExprBinary(op, e0, e1) ->
          { e with desc= ExprBinary(op, heapify e0, heapify e1) }
      | ExprArrayInit(ee) ->
          { e with desc= ExprArrayInit(List.map heapify ee) }
      | ExprStructInit(ee) ->
          { e with desc= ExprStructInit(List.map heapify ee) }
      | ExprSeq(ee) ->
          { e with desc= ExprSeq(List.map heapify ee)}
    in
    heapify e
    &> (fun e -> L.decf 8 ") : %a" fmt_expr e)


end (* module Expr *)




(*****************************************************************************)
(* Translate ESP source_location to SIL position                             *)
(*****************************************************************************)
let xlate_pos = function
  | Some(file,line) ->
      let dir = Filename.dirname file in
      let file = Filename.basename file in
      let col = 0 in
      {Position.dir; file; line; col}
  | None ->
      {Position.dir= "none"; file= "none"; line= 0; col= 0}


(*****************************************************************************)
(* "Constants"                                                               *)
(*****************************************************************************)

(* Prefix for names of temporary variables introduced by frontend. *)
let temp_prefix = "temp"


(*****************************************************************************)
(* Temporary variables generated for RValue evaluation.                      *)
(*****************************************************************************)

let tmps : Var.t SESPExt.CfgHMMap.t = SESPExt.CfgHMMap.create 31

let mk_tmp ?(prefix=temp_prefix) cfg sort =
  let tmp = Var.gensym prefix sort in
  L.printf 8 "adding tmp %a for cfg %s" Var.fmt tmp (SESP.Symb.name cfg) ;
  SESPExt.CfgHMMap.add tmps cfg tmp ;
  tmp



(*****************************************************************************)
(* Translate esp identifiers                                                 *)
(*****************************************************************************)

(* (Memoized) translate ESP.symb to Var.t. *)

type var_kind = VarKFun | VarKGlobal | VarKFormal | VarKReturn | VarKLocal | VarKTmp

(* Translate symb:ty. Memoize in appropriate $ ([globals] or [locals]). *)
let var_of_symb globals locals ty symb = try
  match SESPExt.SymbHMap.tryfind locals symb with
  | Some (v,_k,ty') -> assert(Typ.equal ty' ty); v
  | None ->
    match SESPExt.SymbHMap.tryfind globals symb with
    | Some (v,_k,ty') -> assert(Typ.equal ty' ty); v
    | None ->
        let v = var_of symb (Var.sort_of_type ty) in
        let kind =
          match SESP.Symb.kind symb with
          | ESP.ESP_IR_SK_FUNCTION -> VarKFun
          | ESP.ESP_IR_SK_CTOR ->
              L.printf 4 "Ctor translated as function" ; VarKFun
          | ESP.ESP_IR_SK_DTOR ->
              L.printf 4 "Dtor translated as function" ; VarKFun
          | ESP.ESP_IR_SK_GLOBAL -> VarKGlobal
          | ESP.ESP_IR_SK_FORMAL -> VarKFormal
          | ESP.ESP_IR_SK_FORMAL_VARARGS ->
              failwith "var_of_symb of FORMAL_VARARGS"
          | ESP.ESP_IR_SK_LOCAL -> VarKLocal
          | ESP.ESP_IR_SK_TEMPORARY ->
          (* SI: These are ESP temps. We use mk_tmp for SLAyer's own tmps.*)
              VarKTmp
          | ESP.ESP_IR_SK_RETURN_VALUE -> VarKReturn
          | ESP.ESP_IR_SK_TYPENAME
          | ESP.ESP_IR_SK_FIELD
          | ESP.ESP_IR_SK_BASECLASS
          | ESP.ESP_IR_SK_VIRTUAL_BASECLASS ->
              failwith "var_of_symb of non-variable"
        in
        (match kind with
        | VarKFun | VarKGlobal -> SESPExt.SymbHMap.add globals symb (v,kind,ty)
        | VarKFormal | VarKLocal | VarKTmp -> SESPExt.SymbHMap.add locals symb (v,kind,ty)
        | VarKReturn -> ()
        );
        v

  with exc -> L.printf 0 "var_of_symb: %s" (SESP.Symb.name symb) ; raise exc


(* Not sure we need as much as espcu's sesp_symb_to_slam_pureName_aux.
   (We don't want to to do C++ right now.)
   But we should use linkage in this. *)
let symb_name symb =
  SESP.Symb.name symb



(*============================================================================

  Compile Expr to E.t.

  Also return the supporting set of insts that prefix the E.t, and the
  set of kills that'll post-fix it.

  ============================================================================*)

(** ExprStructInit has a list of exprs. We translate each of these,
    and store them temporarily in a E.OpN(E.UFun("name"),...).  *)

let struct_init_func_name = "StructInit"
let array_init_func_name = "ArrayInit"

(** is-a type *)
let rec is_array cty =
  let open SESP in
  L.printf 10 "is_array %s = %b" (Type.to_string cty) <&
  match Type.refine cty with
  | TypeCXXArray _ -> true
  | TypeAlias(_,cty) -> is_array cty
  | _ -> false

let rec is_ptr cty =
  let open SESP in
  L.printf 10 "is_ptr %s = %b" (Type.to_string cty) <&
  match Type.refine cty with
  | TypePointer _ -> true
  | TypeAlias(_,cty) -> is_ptr cty
  | _ -> false

let is_pchar cty =
  let open SESP in
  match SESPExt.get_type cty with
  | TypePointer(cty) ->
     (match SESPExt.get_type cty with
     | TypeChar _ -> true
     | _ -> false
     )
  | _ ->
     false

let get_struct_of_pstruct cty =
  let open SESP in
  match SESPExt.get_type cty with
  | TypePointer(cty) ->
      (match SESPExt.get_type cty with
      | TypeStruct _ | TypeUnion _ -> Some(cty)
      | _ -> None
      )
  | _ ->
     None


type lvalue = Reg of Var.t | Adr of Exp.t

let fmt_lvalue ff = function
  | Reg(v) -> Var.fmt ff v
  | Adr(a) -> Exp.fmt ff a


let fmt_blk = List.fmt ";@ " I.fmt


let xlate_binary_op op e0 e1 =
  match op with
  | SESP.INDEX ->
      E.mkIdx e0 e1
  | SESP.PLUS ->
      if E.is_pointer e0
      then E.mkIdx e0 e1
      else E.mkZAdd [|e0 ; e1|]
  | SESP.MINUS ->
      if E.is_pointer e0
      then E.mkIdx e0 (E.mkZMin e1)
      else E.mkZSub [|e0 ; e1|]
  | SESP.MULT ->
      E.mkZMul [|e0 ; e1|]
  | SESP.DIV ->
      E.mkZDiv  e0 e1
  | SESP.REM ->
      E.mkZRem  e0 e1
  | SESP.BITAND ->
      E.mkUFun ("&") [|e0;e1|]
  | SESP.BITOR ->
      E.mkUFun ("|") [|e0;e1|]
  | SESP.XOR ->
      E.mkUFun ("^") [|e0;e1|]
  | SESP.LSHIFT ->
      E.mkUFun ("<<") [|e0;e1|]
  | SESP.RSHIFT ->
      E.mkUFun (">>") [|e0;e1|]
  | SESP.RSHIFTU ->
      E.mkUFun (">>u") [|e0;e1|]
  | SESP.BINARY ->
      (* SI: don't know what ExprBINARY(BINARY,_,_) is *)
      failwith "xlate_binary:BINARY"
  | SESP.PLUS_OVF | SESP.MINUS_OVF | SESP.MULT_OVF ->
      failwith "Overflow arithmetic not supported"

(** [xlate_expr_lv ... p lv s] returns [p',ea,s'] where [ea] represents the "effective address" that an
    assignment [lv = rv] would modify.  This can be either a register [Reg], in which case the assignment
    should be implemented with [Move], or a memory address [Adr], in which case the assignment should be
    implemented with [Store].  The translation also extends command prefix [p] to [p'], which is a list of
    commands, in reverse order, to execute to set up the value of [ea]; and command suffix [s] to [s'], which
    is a list of commands to be executed after [ea] becomes dead. *)
let rec xlate_expr_lv cfg pos globals locals prefix expr suffix = try
  let open Expr in let open SESP in
    L.incf 4 "( xlate_expr_lv: %a" fmt_expr_typ expr ;
  (fun (prefix, lv, suffix) ->
    L.decf 4 ") : (@[%a;@  %a@  %a@])" fmt_blk (List.rev prefix) fmt_lvalue lv fmt_blk suffix )
  <&
  match expr.desc with

  | ExprSymb(symb) ->
      let ty = type_of_expr expr in
      let var = var_of_symb globals locals ty symb in
      (prefix, Reg(var), suffix)

  | ExprSymbSlayer(var) ->
      (prefix, Reg(var), suffix)

  (* This clause reverts ESP's bogus introduction of & around arrays in l-value position.  There are no valid
     l-value expressions of form &e, so this non-semantics-preserving translation does not mistranslate valid
     code. *)
  | ExprAddr(arr) ->
      assert( is_array arr.ty );
      xlate_expr_lv cfg pos globals locals prefix arr suffix

  | ExprDeref(ptr) ->
      (* Revert ESP's bogus introduction of & around arrays in l-value position. *)
      let ptr = match ptr.desc with ExprAddr(arr) when is_array arr.ty -> arr | _ -> ptr in
      let prefix, adr, suffix = xlate_expr_rv cfg pos globals locals prefix ptr suffix in
      (prefix, Adr(adr), suffix)

  | ExprBinary(INDEX, arr, idx) ->
      (* Revert ESP's bogus introduction of & around arrays in l-value position. *)
      let arr = match arr.desc with ExprAddr(arr) -> assert( is_array arr.ty ); arr | _ -> arr in
      let prefix, ea, suffix = xlate_expr_lv cfg pos globals locals prefix arr suffix in
      let arr = match ea with Adr(a) -> a | Reg(v) -> E.mkVar v in
      let prefix, idx, suffix = xlate_expr_rv cfg pos globals locals prefix idx suffix in
      (prefix, Adr(E.mkIdx arr idx), suffix)

  | ExprDot(pstruct, fld) ->
      (match xlate_expr_lv cfg pos globals locals prefix pstruct suffix with
      | prefix, Adr(ptr), suffix ->
          (prefix, Adr(E.mkAdd ptr (fld_of pstruct.ty fld)), suffix)
      | _ ->
          failwith "Unexpected stack-allocated struct access"
      )
  | ExprConvert(Convert_PTR_UNRELATED, {desc= ExprConvert(_,_)}) ->
      (* Not sure yet what to do with nested l-value casts of this form. *)
      failwithf "Unexpected l-value pointer cast: %a" fmt_expr expr

  | ExprConvert(conv, l) ->
      let r_ty = type_of_esp_type expr.ty in
      let l_ty = type_of_esp_type l.ty in
      let cfg_symb = Cfg.symb cfg in
      let tmp = mk_tmp ~prefix:"lval_cast_tmp" cfg_symb (Var.sort_of_type r_ty) in
      (match conv with
      | Convert_PTR_UNRELATED ->
          (* For pointer casts, the actual semantics of just copying the bit representation coincides with
             performing a coercion from a pointer of one type to a pointer of another, so translate the
             l-value cast to the corresponding r-value cast. *)
          (* xlate (r_ty)l = r where l:l_ty  to  { r_ty t = r; l = (l_ty)t; } *)
          let infix, lv, suffix = xlate_expr_lv cfg pos globals locals [] l suffix in
          let suffix =
            match lv with
            | Reg(v) ->
                let cast = I.mk (I.Cast(v, l_ty, E.mkVar tmp)) pos in
                (cast :: suffix)
            | Adr(a) ->
                let tmp2 = mk_tmp ~prefix:"store_cast_tmp" cfg_symb (Var.sort_of_type l_ty) in
                let cast = I.mk (I.Cast(tmp2, l_ty, E.mkVar tmp)) pos in
                let store = I.mk (I.Store(a, E.mkVar tmp2)) pos in
                (cast :: store :: suffix)
          in
          (prefix, Reg(tmp), List.rev_append infix suffix)
      | _ ->
          (* For non-pointer casts, the actual semantics of just copying the bit representation cannot be
             represented, so translate to non-deterministic assignment.  There may be more cases where the
             precise semantics can be expressed. *)
          let rec unwrap e =
            match e.desc with
            | ExprConvert(_,e) -> unwrap e
            | _ -> e
          in
          let l = unwrap l in
          let infix, lv, suffix = xlate_expr_lv cfg pos globals locals [] l suffix in
          let suffix =
            match lv with
            | Reg(v) ->
                mkKill v pos suffix
            | Adr _ ->
                (* Not sure yet what to do with non-pointer l-value casts of expressions that denote addresses. *)
                failwithf "Unexpected l-value non-pointer cast: %a" fmt_expr expr
          in
          (prefix, Reg(tmp), List.rev_append infix suffix)
      )
  | ExprSeq(ee) ->
      (* For unused expressions calculate rvalues *)
      let rec step prefix ee suffix =
        match ee with
        | [e] ->
            xlate_expr_lv cfg pos globals locals prefix e suffix
        | e::ee ->
            let prefix,_,suffix = xlate_expr_rv cfg pos globals locals prefix e suffix in
            step prefix ee suffix
        | [] ->
            failwith "Unexpected empty sequence expression"
      in
      step prefix ee suffix

  | ExprConstString _ ->
      let prefix, str, suffix = xlate_expr_rv cfg pos globals locals prefix expr suffix in
      (prefix, Adr(str), suffix)

  | _ ->
      failwithf "not an lvalue: %a" fmt_expr expr

  with exc -> L.printf 0 "xlate_expr_lv: %a" Expr.fmt_expr expr ; raise exc


(** [xlate_expr_rv ... p rv s] returns [p',e,s'] where [e] represents the value that an assignment [lv = rv]
    would move/store to [lv].  The translation also extends command prefix [p] to [p'], which is a list of
    commands, in reverse order, to execute to set up the value of [e]; and command suffix [s] to [s'], which
    is a list of commands to be executed after [e] becomes dead. *)
and xlate_expr_rv cfg pos globals locals prefix expr suffix = try
  let open Expr in let open SESP in
    L.incf 4 "( xlate_expr_rv: %a" fmt_expr_typ expr ;
  (fun (prefix,e,suffix) ->
    L.decf 4 ") : (@[%a;@  %a@  %a@])" fmt_blk (List.rev prefix) E.fmt e fmt_blk suffix )
  <&
  let cfg_symb = Cfg.symb cfg in
  match expr.desc with

  | ExprSymb(symb) ->
      let ty = type_of_expr expr in
      let var = var_of_symb globals locals ty symb in
      (prefix, E.mkVar var, suffix)

  | ExprSymbSlayer(var) ->
      (prefix, E.mkVar var, suffix)

  | ExprAddr(ptr) ->
      (match xlate_expr_lv cfg pos globals locals prefix ptr suffix with
      | prefix, Adr(a), suffix ->
          (prefix, a, suffix)
      | _ ->
          failwith "Unexpected address-of non-l-value expression"
      )
  | ExprDeref(ptr) ->
      let prefix, adr, suffix = xlate_expr_rv cfg pos globals locals prefix ptr suffix in
      let tmp = mk_tmp ~prefix:"load_tmp" cfg_symb (Var.sort_of_type (type_of_expr expr)) in
      (mkLoad tmp adr pos :: prefix, E.mkVar tmp, suffix)

  | ExprBinary(INDEX, arr, idx) ->
      let prefix, ea, suffix = xlate_expr_lv cfg pos globals locals prefix arr suffix in
      let prefix, idx, suffix = xlate_expr_rv cfg pos globals locals prefix idx suffix in
      let tmp = mk_tmp ~prefix:"load_tmp" cfg_symb (Var.sort_of_type (type_of_expr expr)) in
      let arr = match ea with Adr(a) -> a | Reg(v) -> E.mkVar v in
      (mkLoad tmp (E.mkIdx arr idx) pos :: prefix, E.mkVar tmp, suffix)

  | ExprDot _ ->
      let prefix, ea, suffix = xlate_expr_lv cfg pos globals locals prefix expr suffix in
      (match ea with
      | Reg(v) ->
          (prefix, E.mkVar v, suffix)
      | Adr(a) ->
          let tmp = mk_tmp ~prefix:"load_tmp" cfg_symb (Var.sort_of_type (type_of_expr expr)) in
          (mkLoad tmp a pos :: prefix, E.mkVar tmp, suffix)
      )
  (* CONTAINING_RECORD(address, pstruct_type, field) *)
  | ExprConvert(_, {desc= ExprBinary(MINUS,
                                     {desc= ExprConvert(_, address); ty= pchar_ty},
                                     {desc= ExprConstInt(o)})})
    when is_pchar pchar_ty && Option.is_some (get_struct_of_pstruct expr.ty) ->
      let prefix, adr, suffix = xlate_expr_rv cfg pos globals locals prefix address suffix in
      let ty = type_of_esp_type (Option.from_some (get_struct_of_pstruct expr.ty)) in
      (match Typ.paths_at_offset ty (Int64.to_int o, None) with
      | (path,_)::_ -> (prefix, E.mkSubs adr path, suffix)
      | [] -> L.printf 0 "No field found at offset %Li of type@ %a" o Typ.fmt ty ; failwith "xlate_expr_rv"
      )
  | ExprConstInt(0L) when is_ptr expr.ty ->
      (prefix, E.nil, suffix)

  | ExprConstInt(i) ->
      (prefix, E.mkNum i, suffix)

  | ExprConstFloat(f) ->
      (prefix, E.mkUFun "real_const" [| E.mkUFun (string_of_float f) [||] |], suffix)

  | ExprConstString(_len,str,_wide) ->
      (prefix, E.mkStr (String.escaped str), suffix)

  | ExprUplus(e0) ->
      xlate_expr_rv cfg pos globals locals prefix e0 suffix

  | ExprNegate(e0) ->
      let prefix, e0, suffix = xlate_expr_rv cfg pos globals locals prefix e0 suffix in
      (prefix, E.mkZMin e0, suffix)

  | ExprNot(e0) ->
      let prefix, e0, suffix = xlate_expr_rv cfg pos globals locals prefix e0 suffix in
      (prefix, E.mkNot e0, suffix)

  | ExprBitNot(e0) ->
      let prefix, e0, suffix = xlate_expr_rv cfg pos globals locals  prefix e0 suffix in
      (prefix, E.mkUFun ("~") [|e0|], suffix)

  | ExprCompare(op,e0,e1) ->
      let xlate_compare_op op e0 e1 =
        match op with
        | LT | LTU -> E.mkZLt e0 e1
        | LE | LEU -> E.mkZLe e0 e1
        | GT       -> E.mkZGt e0 e1
        | GE       -> E.mkZGe e0 e1
        | EQUALS   -> E.mkEq e0 e1
        | NE       -> E.mkDq e0 e1 in
      let prefix, e0, suffix = xlate_expr_rv cfg pos globals locals  prefix e0 suffix in
      let prefix, e1, suffix = xlate_expr_rv cfg pos globals locals prefix e1 suffix in
      let ce = xlate_compare_op op e0 e1 in
      (prefix, ce, suffix)

  | ExprBinary(op,e0,e1) ->
      let prefix, e0, suffix = xlate_expr_rv cfg pos globals locals prefix e0 suffix in
      let prefix, e1, suffix = xlate_expr_rv cfg pos globals locals prefix e1 suffix in
      let be = xlate_binary_op op e0 e1 in
      (prefix, be, suffix)

  | ExprConvert(_,e0) ->
      let prefix, e, suffix = xlate_expr_rv cfg pos globals locals prefix e0 suffix in
      let ty = type_of_esp_type expr.ty in
      let tmp = mk_tmp ~prefix:"cast_tmp" cfg_symb (Var.sort_of_type ty) in
      let cast = I.mk (I.Cast(tmp, ty, e)) pos in
      (cast :: prefix, E.mkVar tmp, suffix)

  | ExprAssign(op,lhs,rhs) ->
      let prefix, rv, suffix = xlate_expr_rv cfg pos globals locals prefix rhs suffix in
      let prefix, ea, suffix = xlate_expr_lv cfg pos globals locals prefix lhs suffix in
      (match E.desc rv with
      (* vector rv (array init) *)
      | E.OpN(E.UFun(f), ee) when f = array_init_func_name ->
          (* PS #658: Only catches x = {1;2;3;4;5} case right now, not x = {1}.
             We should get the size from the type of [lv]. *)
          assert( op = None );
          let _last_idx, prefix =
            List.fold_left (fun (idx, prefix) e ->
              let base = match ea with Adr(a) -> a | Reg(v) -> E.mkVar v in
              let base_idx = E.mkIdx base (E.mkNum idx) in
              let c = mkStore base_idx (E.name e) pos in
              (Int64.succ idx, c :: prefix)
            ) (Int64.zero, prefix) (Array.to_list ee) in
          (prefix, rv, suffix)
      (* vector rv (struct init) *)
      | E.OpN(E.UFun(f),_ee) when f = struct_init_func_name ->
          failwith "unimplemented"
      (* scalar rv *)
      | _ ->
          match ea with
          | Reg(v) ->
              let rv = Option.fold (fun op -> xlate_binary_op op (E.mkVar v)) op rv in
              ((mkMove v rv pos) :: prefix, rv, suffix)
          | Adr(a) ->
              match op with
              | None ->
                  ((mkStore a rv pos) :: prefix, rv, suffix)
              | Some op ->
                  let sort = Var.sort_of_type (type_of_expr lhs) in
                  let t = mk_tmp ~prefix:"fused_assign_load_tmp" cfg_symb sort in
                  let rv = xlate_binary_op op (E.mkVar t) rv in
                  ((mkStore a rv pos) :: (mkLoad t a pos) :: prefix, rv, suffix)
      )
  | ExprIncr(op,e,_) ->
      let incr exp =
        match E.sort_of exp with
        | Var.IntegerSort -> E.mkZAdd [|exp; E.one|]
        | Var.PointerSort -> E.mkIdx exp E.one
        | Var.BooleanSort | Var.OffsetSort -> failwith "cannot increment boolean or offset"
      and decr exp =
        match E.sort_of exp with
        | Var.IntegerSort -> E.mkZSub [|exp; E.one|]
        | Var.PointerSort -> E.mkIdx exp (E.mkNum(-1L))
        | Var.BooleanSort | Var.OffsetSort -> failwith "cannot decrement boolean or offset"
      in
      let prefix, ea, suffix = xlate_expr_lv cfg pos globals locals prefix e suffix in
      (match ea with
      | Reg(v) ->
          let vv = E.mkVar v in
          (match op with
          | PREINCR  -> (mkMove v (incr vv) pos :: prefix,      vv, suffix)
          | POSTINCR -> (mkMove v (incr vv) pos :: prefix, decr vv, suffix)
          | PREDECR  -> (mkMove v (decr vv) pos :: prefix,      vv, suffix)
          | POSTDECR -> (mkMove v (decr vv) pos :: prefix, incr vv, suffix)
          )
      | Adr(a) ->
          let t = mk_tmp ~prefix:"incr_load_tmp" cfg_symb (Var.sort_of_type (type_of_expr e)) in
          let tv = E.mkVar t in
          let prefix = mkLoad t a pos :: prefix in
          (match op with
          | PREINCR  -> (mkStore a (incr tv) pos :: prefix, incr tv, suffix)
          | POSTINCR -> (mkStore a (incr tv) pos :: prefix,      tv, suffix)
          | PREDECR  -> (mkStore a (decr tv) pos :: prefix, decr tv, suffix)
          | POSTDECR -> (mkStore a (decr tv) pos :: prefix,      tv, suffix)
          )
      )
  | ExprArrayInit(ee) ->
      let (prefix, suffix), ee_rev = List.fold_left (fun ((prefix,suffix),ee_rev) e ->
        let prefix,e,suffix = xlate_expr_rv cfg pos globals locals prefix e suffix in
        ((prefix,suffix), e :: ee_rev)
      ) ((prefix,suffix),[]) ee in
      let f = array_init_func_name in
      let ainit = E.mkUFun f (Array.of_list (List.rev ee_rev)) in
      (prefix, ainit, suffix)

  | ExprStructInit(ee) ->
      let (prefix, suffix), ee_rev = List.fold_left (fun ((prefix,suffix),ee_rev) e ->
        let prefix,e,suffix = xlate_expr_rv cfg pos globals locals prefix e suffix in
        ((prefix,suffix), e :: ee_rev)
      ) ((prefix,suffix),[]) ee in
      let f = struct_init_func_name in
      let ainit = E.mkUFun f (Array.of_list (List.rev ee_rev)) in
      (prefix, ainit, suffix)

  | ExprSeq ee ->
      let rec step prefix ee suffix =
        match ee with
        | [e] ->
            xlate_expr_rv cfg pos globals locals prefix e suffix
        | e::ee ->
            let prefix,_,suffix = xlate_expr_rv cfg pos globals locals prefix e suffix in
            step prefix ee suffix
        | [] ->
            failwith "Unexpected empty sequence expression"
      in
      step prefix ee suffix

  with exc -> L.printf 0 "xlate_expr_rv: %a" Expr.fmt_expr expr ; raise exc


(*============================================================================
  Translate SESP.node. Set the edges in the graph too.
  ============================================================================*)

module CFG = struct
  include CFG

  (* Add u--e--> w to g. *)
  let add_edge g u c w =
    L.printf 6 "added edge %a -%a-> %a" K.Id.fmt (K.id u) C.fmt c K.Id.fmt (K.id w) ;
    add_edge g u c w

  (* Replace v--Nop-->v' by v--blk-->v' in g. *)
  let replace_edge g (v,_,v') blk =
    L.printf 6 "replace_edge (%a,%a) e:%a" K.fmt v K.fmt v' fmt_blk blk ;
    remove_edge g v (mkNop (K.pos v')) v' ;
    add_block_edge g v blk v'

end


let xlate_node _cfgs (cfg:SESP.cfg)
    vars_to_heap node_to_edge globals locals (g:CFG.graph)
    (node:SESP.node)  : unit =

  L.incf 3 "( xlate_node: %a " SESPExt.fmt_node node ;

  (* "Constants" *)
  let cfg_symb, node_id = SESP.Cfg.symb cfg, SESP.Node.id node in
  let pos = xlate_pos (SESP.Node.source_location node) in

  (* xlate_node local functions *)

  (* Add node--blk-->succ in g *)
  let connect_to_succ blk succ =
    let (_,_,node_tgt) = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,node_id) in
    let succ_id = SESP.Node.id succ in
    let (succ_src,_,_) = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,succ_id) in
    CFG.add_block_edge g node_tgt blk succ_src
  in

  (* Translate Call's args. *)
  let xlate_call_args prefix args suffix =
    let prefix, args, suffix =
      Array.fold_right (fun arg (prefix, args, suffix) ->
        let arg = Expr.expr_of_EspExpr arg in
        let ty = type_of_esp_type arg.Expr.ty in
        let isStruct = match Typ.desc ty with | Typ.Structure _ -> true | _ -> false in
        let arg = if isStruct then Expr.mk_expr (Expr.ExprAddr(arg)) (arg.Expr.ty) else arg in
        let arg = Expr.heapify_expr vars_to_heap arg in
        let prefix, a, suffix = xlate_expr_rv cfg pos globals locals prefix arg suffix in
        (prefix, a :: args, suffix)
      ) args (prefix,[],suffix) in

    let prefix, args, suffix =
      List.fold_right (fun arg (prefix, args, suffix) ->
        match E.desc arg with
      (* no tmp needed if arg is a variable and distinct from other args *)
        | E.Var(v) when not (List.mem v args) ->
            (prefix, v :: args, suffix)
        | _ ->
            let v = mk_tmp ~prefix:"arg_tmp" cfg_symb (E.sort_of arg) in
            (mkMove v arg pos :: prefix, v :: args, suffix)
      ) args (prefix,[],suffix) in

    prefix, args, suffix
  in

  let areturn_from_CallReturn args node =
    match SESP.Node.refine node with
    | SESP.CallReturn(Some(ret),_) ->
        let ret = Expr.expr_of_EspExpr ret in
        (match ret.Expr.desc with
        | Expr.ExprSymb(symb) ->
            let ty = Expr.type_of_expr ret in
            L.printf 8 "Return type: %a" Typ.fmt ty;
            (match Typ.desc (Expr.type_of_expr ret) with
            | Typ.Structure _ ->
                (* Struct returning functions, are translating into taking an extra argument of the struct *)
                L.printf 8 "Structure returning function";
                ((var_of_symb globals locals (Typ.mkPointer ty) symb) :: args, None)
            | _ ->
                (args, Some (var_of_symb globals locals ty symb))
            )
        | _ ->
            failwith "return variable expected to be a variable"
        )
    | SESP.CallReturn(None,_) ->
        (args, None)
    | _ ->
        failwith "return node of Call expected to be a CallReturn"
  in
  let get_areturn_from_CallReturn node =
    match areturn_from_CallReturn [] node with
    | ([], Some(areturn)) -> areturn
    | _ -> failwith "No return variable for non-void returning function"
    (* Could also be an issue with Struct types, but not reachable from the code *)
  in

  (* xlate_node actual translation *)
  (match SESP.Node.refine node with

  | SESP.Entry(succ, _pre) ->
      connect_to_succ no_insts succ

  | SESP.Exit(_post) ->
      ()

  | SESP.ExceptionExit ->
      L.printf 8 "Ignoring ExceptionExit"
  | SESP.Pattern _ ->
      failwith "xlate_node: espCU doesn't implemented SESP.Pattern either"
  | SESP.Event ->
      failwith "xlate_node: espCU doesn't implement SESP.Event either"

  | SESP.Call(SESP.Direct(f), [|size|], callreturn,_,_,_) when symb_name f = "_SLAyer_malloc" ->
      let areturn = get_areturn_from_CallReturn callreturn in
      let size = Expr.expr_of_EspExpr size in
      let size = Expr.heapify_expr vars_to_heap size in
      let prefix, size, suffix = xlate_expr_rv cfg pos globals locals [] size [] in
      let prefix_alloc_suffix = List.rev_append (I.mk (I.Alloc(areturn, size)) pos :: prefix) suffix in
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      CFG.replace_edge g nedge prefix_alloc_suffix ;
      connect_to_succ no_insts callreturn

  | SESP.Call(SESP.Direct(f), [|ptr|], callreturn,_,_,_) when symb_name f = "_SLAyer_free" ->
      let ptr = Expr.expr_of_EspExpr ptr in
      let ptr = Expr.heapify_expr vars_to_heap ptr in
      let prefix, ptr, suffix = xlate_expr_rv cfg pos globals locals [] ptr [] in
      let prefix_free_suffix = List.rev_append (I.mk (I.Free(ptr)) pos :: prefix) suffix in
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      CFG.replace_edge g nedge prefix_free_suffix ;
      connect_to_succ no_insts callreturn

  | SESP.Call(SESP.Direct(f), [||],_,_,_,_) when symb_name f = "_SLAyer_error" ->
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      CFG.replace_edge g nedge [I.mk (I.Assert(E.ff)) pos]

  | SESP.Call(SESP.Direct(f), [||],_,_,_,_) when symb_name f = "_SLAyer_unreachable" ->
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      CFG.replace_edge g nedge [I.mk (I.Assume(E.ff)) pos]

  | SESP.Call(SESP.Direct(f), [||], callreturn,_,_,_) when symb_name f = "_SLAyer_nondet" ->
      let areturn = get_areturn_from_CallReturn callreturn in
      let kill = I.mk (I.Kill(Vars.singleton areturn)) pos in
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      CFG.replace_edge g nedge [kill] ;
      connect_to_succ no_insts callreturn

  | SESP.Call(call_target, actuals, callreturn,_,_,_) ->
      let xlate_actuals prefix suffix =
        let prefix, actuals, suffix = xlate_call_args prefix actuals suffix in
        (prefix, areturn_from_CallReturn actuals callreturn, suffix)
      in
      let prefix, call, suffix =
        match call_target with
        | SESP.Direct(f) ->
            let proc = proc_of f in
            let typ = type_of_esp_type (SESP.Symb.ctype f) in
            let prefix, (actuals, areturn), suffix = xlate_actuals [] [] in
            (prefix, C.Call{Call.proc; actuals; areturn; typ; targets= [proc]}, suffix)
        | SESP.Indirect(fp) ->
            let fp = Expr.expr_of_EspExpr fp in
            let fp = Expr.heapify_expr vars_to_heap fp in
            let fp =
              match fp.Expr.desc with
              | Expr.ExprDeref(f) -> f
              | _ -> failwith "Got ICall(pf), not ICall(*pf)" in
            let prefix, proc, suffix = xlate_expr_rv cfg pos globals locals [] fp [] in
            let typ = Expr.type_of_expr fp in
            let prefix, (actuals, areturn), suffix = xlate_actuals prefix suffix in
            (prefix, C.ICall{Call.proc; actuals; areturn; typ; targets= []}, suffix)
        | _ ->
            failwith "C++ Calls unimplemented"
      in
      (* Replace the node_src-->node_trg edge with: *)
      (*  (node_src)--prefix-->(pre_call)--call--(pre_kill)--suffix-->(node_trg) *)
      let node_src, node_lbl, node_trg = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      assert( C.equal node_lbl (mkNop (K.pos node_trg)) );
      let pre_call_vtx = CFG.add_vertex g (K.mk_label pos (K.proc node_src)) in
      let pre_kill_vtx = CFG.add_vertex g (K.mk_label pos (K.proc node_trg)) in
      CFG.remove_edge g node_src node_lbl node_trg ;
      CFG.add_block_edge g node_src (List.rev prefix) pre_call_vtx ;
      CFG.add_edge g pre_call_vtx call pre_kill_vtx ;
      CFG.add_block_edge g pre_kill_vtx suffix node_trg ;
      connect_to_succ no_insts callreturn

  | SESP.CallReturn(_, succo) ->
      Option.iter (fun succ -> connect_to_succ no_insts succ) succo

  | SESP.Return(ret, succ) ->
      (* This code converts returns into assignments.  This requires
         Returns to be findable, as they are always followed by an Exit.*)
      assert( match SESP.Node.refine succ with SESP.Exit _ -> true | _ -> false );
      let freturn =
        match SESP.Cfg.formal_return cfg with
        | Some(symb) -> symb
        | None ->
            failwithf "Unexpected return from procedure %a without a return variable" SESPExt.fmt_symb cfg_symb in
      let fty = SESP.Expr.ctype ret in
      let assign = Expr.mk_expr_assign None (Expr.mk_expr (Expr.ExprSymb(freturn)) fty) (Expr.expr_of_EspExpr ret) in
      let assign = Expr.heapify_expr vars_to_heap assign in
      let prefix, _assign_val, suffix = xlate_expr_rv cfg pos globals locals [] assign [] in

      let nsrc, nlbl, ntrg = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, node_id) in
      assert( C.equal nlbl (mkNop (K.pos ntrg)) );
      CFG.replace_edge g (nsrc,nlbl,ntrg) (List.rev_append prefix suffix);
      connect_to_succ no_insts succ

  | SESP.CallUnwind(succo) ->
      L.printf 8 "ignoring CallUnwind" ;
      (* But still hooking up to node to [succ] *)
      (match succo with
      | Some succ -> connect_to_succ no_insts succ
      | None -> ())
  | SESP.Decl(_symb, _ty, succ) ->
      L.printf 8 "ignoring Decl" ;
      (* But still hooking up to node to [succ] *)
      connect_to_succ no_insts succ

  | SESP.StaticDeclBegin(_symb, _ty, succ) ->
      L.printf 8 "ignoring StaticDeclBegin" ;
      (* But still hooking up to node to [succ] *)
      connect_to_succ no_insts succ

  | SESP.StaticDeclEnd(succ) ->
      L.printf 8 "ignoring StaticDeclEnd" ;
      (* But still hooking up to node to [succ] *)
      connect_to_succ no_insts succ

  | SESP.Endscope ->
      failwith "xlate_node: espCU doesn't implement SESP.Endscope either"

  | SESP.Throw(succ) ->
      L.printf 8 "Ignoring Throw" ;
      connect_to_succ no_insts succ

  | SESP.RaiseException ->
      failwith "xlate_node: espCU doesn't implement SESP.RaiseException either"
  | SESP.CatchBegin(succ) ->
      L.printf 8 "Ignoring CatchBegin" ;
      connect_to_succ no_insts succ
  | SESP.CatchEnd(succ) ->
      L.printf 8 "Ignoring CatchEnd" ;
      connect_to_succ no_insts succ

  | SESP.Branch(cond, nelse, nthen) ->

      let cond = Expr.expr_of_EspExpr cond in
      let cond = Expr.heapify_expr vars_to_heap cond in

      let prefix, ce, suffix = xlate_expr_rv cfg pos globals locals [] cond [] in
      let ce = Option.from_some (E.convert Var.BooleanSort ce) in
      let not_ce = E.mkNot ce in

        (* Replace current edge label with [cond_insts] *)
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,node_id) in
      CFG.replace_edge g nedge (List.rev prefix) ;

      let connect desc node =
        connect_to_succ (I.mk desc pos :: suffix) node
      in
      let connect_assert_assume desc node =
        let open! SESP in
        match Node.refine node with
        | Expression(Some(assign_pred_one), Some(nsucc)) ->
            assert( (* sanity check expression is as expected *)
              match Expr.refine assign_pred_one with
              | ExprASSIGN(ASSIGN,(pred,one)) ->
                  (match Expr.refine pred with
                  | ExprLEAF(SYMBOL(symb)) -> Str.string_match (Str.regexp "#Pred\\.[^#]+#.*") (Symb.name symb) 0
                  | _ -> false) &&
                  (match Expr.refine one with
                  | ExprLEAF(CONSTANT(1L)) -> true
                  | _ -> false)
              | _ -> false
            );
            connect desc nsucc
        | _ ->
            connect desc node
      in
      (match SESP.Node.refine nelse with
      | SESP.Call(SESP.Direct(f),[||],_,_,_,_) when symb_name f = "_SLAyer_error" ->
          connect_assert_assume (I.Assert(ce)) nthen

      | SESP.Call(SESP.Direct(f),[||],_,_,_,_) when symb_name f = "_SLAyer_unreachable" ->
          connect_assert_assume (I.Assume(ce)) nthen

      | _ ->
      match SESP.Node.refine nthen with
      | SESP.Call(SESP.Direct(f),[||],_,_,_,_) when symb_name f = "_SLAyer_error" ->
          connect_assert_assume (I.Assert(not_ce)) nelse

      | SESP.Call(SESP.Direct(f),[||],_,_,_,_) when symb_name f = "_SLAyer_unreachable" ->
          connect_assert_assume (I.Assume(not_ce)) nelse

      | _ ->
          connect (I.Assume(ce)) nthen ;
          connect (I.Assume(not_ce)) nelse
      )

  | SESP.Switch(test, label_bodys, default) ->

      let test = Expr.expr_of_EspExpr test in
      let test = Expr.heapify_expr vars_to_heap test in

      let prefix, te, suffix = xlate_expr_rv cfg pos globals locals [] test [] in
      let te = Option.from_some (E.convert Var.IntegerSort te) in

      (* Replace current edge label with [test]'s [prefix] *)
      let nedge = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,node_id) in
      CFG.replace_edge g nedge (List.rev prefix) ;

      (* label: body *)
      let (lbl_prefixes,lbl_es,lbl_suffixes) =
        List.fold_left (fun (pp,ee,ss) (label,body) ->
          let label = Expr.expr_of_EspExpr label in
          let label = Expr.heapify_expr vars_to_heap label in

          let lprefix, le, lsuffix = xlate_expr_rv cfg pos globals locals [] label [] in
          let le = Option.from_some (E.convert Var.IntegerSort le) in

          let assumption = I.mk (I.Assume(E.mkEq te le)) pos in
          let insts = prefix @ [assumption] @ lsuffix @ suffix in

          (* Connect node_tgt to src(body) *)
          connect_to_succ insts body ;

          (* And also keep all (lprefix,le,lsuffix)s for the default case. *)
          (lprefix::pp, le :: ee, lsuffix :: ss)

      ) ([],[],[]) (Array.to_list label_bodys) in

      (* default *)
      let assumption = E.mkAnd
        (Array.of_list (List.map (fun lbl -> E.mkNot (E.mkEq te lbl)) lbl_es)) in
      let insts =
        List.concat lbl_prefixes @
          [(I.mk (I.Assume(assumption)) pos)] @
          List.concat lbl_suffixes @
          suffix in
      connect_to_succ insts default

  | SESP.Expression (ceo, succo) ->
      let (nsrc,nlbl,ntrg) = SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,node_id) in
      (match ceo with
      | Some ce ->
          (* SI: xlate_expr_rv might do the replace_edge itself. See Note (1)
             in the function. *)
          let ce = Expr.expr_of_EspExpr ce in
          let ce = Expr.heapify_expr vars_to_heap ce in
          let ce_insts, _ce_e, suffix = xlate_expr_rv cfg pos globals locals [] ce [] in
          (* SI: Replace current edge label with [ce_insts].
             Ignore [ce_e] in this expr-statement context? *)
          CFG.replace_edge g (nsrc,nlbl,ntrg) (List.rev_append ce_insts suffix)
      | None -> ()
      ) ;
      Option.iter (fun succ -> connect_to_succ no_insts succ) succo

    (* SI: eSPCU doesn't expect Assume and Verify? *)
  | SESP.Assume(succ, _assumes) ->
      L.printf 8 "Ignoring Assume" ;
      connect_to_succ no_insts succ
  | SESP.Verify(succ, _verify) ->
      L.printf 8 "Ignoring Verify" ;
      connect_to_succ no_insts succ

  | SESP.Asm(succ) ->
      L.printf 8 "Ignoring Asm" ;
      connect_to_succ no_insts succ

  | SESP.Nop(succ) ->
      connect_to_succ no_insts succ
  );
  L.decf 3 ") xlate_node"


(*============================================================================
  In the initial stage of the translation, each esp node is mapped to
  a Program.CFG edge. We set up the edge here, with a dummy edge label
  (expecting xlate_node to over-write it).
  ============================================================================*)

let edge_of_node proc graph node =

  let pos = xlate_pos (SESP.Node.source_location node) in

  let vtx_src = CFG.add_vertex graph (K.mk_label pos proc) in

  let vtx_tgt = CFG.add_vertex graph (K.mk_label pos proc) in

  CFG.add_edge graph vtx_src (mkNop pos) vtx_tgt ;

  (vtx_src, mkNop pos, vtx_tgt)


(*============================================================================
  Convert esp cfg into Graph_sig representation.
  ============================================================================*)

(* Very light modules, don't need more at this point. *)
module NodeId = struct
  type t = int
  let compare = Pervasives.compare
  let equal x y = (x=y)
  let hash = Hashtbl.hash
  let fmt = Format.pp_print_int
end
module NodeStr = struct
  type t = string
  let compare = Pervasives.compare
  let equal x y = (x=y)
  let fmt = Format.pp_print_string
end
module NullEdge = struct
  type t = unit
  let compare = Pervasives.compare
  let equal x y = (x=y)
  let fmt ff () = Format.pp_print_string ff ""
end

module ESPGraph = Graph.Make(NodeId)(NodeStr)(NullEdge)

let graph_of_esp_cfg _filename cfg =

  let nodes = SESP.Cfg.nodes cfg in
  let cfg_symb = SESP.Cfg.symb cfg in

  let g = ESPGraph.create () in
  let node_to_vtx : ESPGraph.Vertex.t SESPExt.CfgNodeHMap.t =
    SESPExt.CfgNodeHMap.create (Array.length nodes) in

  (* Add all the vertices *)
  Array.iter (fun n ->
    let id = SESP.Node.id n in
    let s = Format.asprintf "%a" SESPExt.fmt_node n in
    let v = ESPGraph.add_vertex g (id, s) in
    SESPExt.CfgNodeHMap.add node_to_vtx (cfg_symb,id) v
  ) nodes ;
  (* Add all the edges *)
  Array.iter (fun n ->
    let id = SESP.Node.id n in
    let sso = SESP.Node.succs_optional n in
    Array.iter (fun so ->
      match so with
      | Some s ->
          let s_id = SESP.Node.id s in
          let v_n = SESPExt.CfgNodeHMap.find node_to_vtx (cfg_symb,id) in
          let v_s = SESPExt.CfgNodeHMap.find node_to_vtx (cfg_symb,s_id) in
          ESPGraph.add_edge g v_n () v_s
      | None -> ()) sso
    ) nodes ;


  let entry = SESP.Node.id (SESP.Cfg.entry cfg) in
  let entry_vtx = SESPExt.CfgNodeHMap.find node_to_vtx (cfg_symb,entry) in

  (g,entry_vtx)



(*===========================================================================
  Translate a cfg to a proc
  ===========================================================================*)

let proc_of_cfg cfgs globals heapified_globals (cfg:SESP.cfg) : Proc.t = try

  L.incf 3 "( proc_of_cfg: %s"  (SESP.Symb.name (SESP.Cfg.symb cfg)) ; (fun _ -> L.decf 3 ") proc_of_cfg: done") <&

  let cfg_symb = SESP.Cfg.symb cfg in
  let id = proc_of cfg_symb in
  let fty = type_of_esp_type (SESP.Symb.ctype cfg_symb) in

  (* Heapification *)
  let vars_to_heap = SESPExt.to_heapify heapified_globals cfg in
  L.printf 3 "Heapify {%a}"
    (List.fmt "," SESPExt.fmt_symb) (SESPExt.SymbHMap.fold (fun symb _tag symbs -> symb :: symbs) vars_to_heap []) ;

  (* Local variables used in this proc *)
  let locals : (Var.t * var_kind * Typ.t) SESPExt.SymbHMap.t =
    SESPExt.SymbHMap.create 255 in

  (* Translate the nodes to a cfg graph. *)
  let nodes = SESP.Cfg.nodes cfg in
  let graph =  CFG.create () in

  (* I. Initialize the edge the node will be translated to. *)
  let node_to_edge : (CFG.Vertex.t * CFG.e_label * CFG.Vertex.t)
      SESPExt.CfgNodeHMap.t = SESPExt.CfgNodeHMap.create (Array.length nodes) in

  Array.iter (fun node ->
    let node_id = SESP.Node.id node in
    let edge = edge_of_node id graph node in
    SESPExt.CfgNodeHMap.add node_to_edge (cfg_symb,node_id) edge
  ) nodes ;

  (* II. Translate, and connect the node to the rest of the graph. *)
  Array.iter (fun n ->
    xlate_node cfgs cfg vars_to_heap node_to_edge globals locals graph n
  ) nodes ;

  (* Find entry and exit points. *)
  let entry_vtx,_,_ =
    let entry_node = SESP.Node.id (SESP.Cfg.entry cfg) in
    SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb,entry_node) in

  (* ToDo: ESP's cfg.h includes GetExit which should be exposed as e.g. SESP.Cfg.exit *)
  let _,_,exit_vtx =
    match
      Array.fold_right (fun node exits ->
        match SESP.Node.refine node with
        | SESP.Exit _ -> node :: exits
        | _ -> exits
      ) nodes []
    with
    | [exit_node] -> SESPExt.CfgNodeHMap.find node_to_edge (cfg_symb, SESP.Node.id exit_node)
    | _ -> failwith "expected single exit node" in

  (* Malloc and free local heapified vars *)
  let heap_vars =
    SESPExt.SymbHMap.fold (fun hsymb htag vars ->
      match htag with
      | SESPExt.HAddrOfLocal | SESPExt.HLocalStruct ->
          let ty = type_of_esp_type (SESP.Symb.ctype hsymb) in
          let v = var_of_symb globals locals (SESPExt.heapify_type ty) hsymb in
          (v,ty) :: vars
      (* Ignore globals, they'll be malloced in main. *)
      | SESPExt.HGlobal
      | SESPExt.HAddrOfFunc
      | SESPExt.HFormal
      | SESPExt.HFormalStruct -> vars
    ) vars_to_heap [] in

  (* Translate formals *)
  let (formals, varargs) = SESP.Cfg.formals cfg in
  if varargs then L.printf 1 "Ignoring varags." ;

  let formals = Array.to_list formals in
  let formals, heap_vars, assigns =
    List.fold (fun f (formals, heap_vars, assigns) ->
      let espty = SESP.Symb.ctype f in
      let ty = type_of_esp_type espty in
      match SESPExt.SymbHMap.tryfind vars_to_heap f with
      | Some(SESPExt.HFormal) ->
          let v = var_of_symb globals locals (SESPExt.heapify_type ty) f in
          let newv = Var.gensym (Var.name v) (Var.sort_of_type ty) in
          let mv = I.mk (I.Store(E.mkVar v, E.mkVar newv)) (K.pos entry_vtx) in
          (newv :: formals, (v,ty) :: heap_vars, mv :: assigns)
      | Some(SESPExt.HFormalStruct) ->
          let hty = (SESPExt.heapify_type ty) in
          let v = var_of_symb globals locals hty f in
          let newv = Var.gensym (Var.name v) (Var.sort_of_type hty) in
          let lhs = Expr.mk_expr (Expr.ExprSymb f) espty in
          let rhs = Expr.mk_expr (Expr.ExprSymbSlayer newv) espty in
          let rhs = Expr.mk_expr (Expr.ExprDeref rhs) espty in
          let assign = Expr.mk_expr_assign None lhs rhs in
          let assign = Expr.heapify_expr vars_to_heap assign in
          let prefix, _, suffix = xlate_expr_rv cfg (K.pos entry_vtx) globals locals [] assign assigns in
          (newv :: formals, (v,ty) :: heap_vars, List.rev_append prefix suffix)
      | Some _ ->
          failwith "Unreachable"
      | None ->
          let v = var_of_symb globals locals ty f in
          (v :: formals, heap_vars, assigns)
    ) formals ([], heap_vars, []) in

  let formals = List.rev formals in
  L.printf 4 "formals: [%a]" (List.fmt "," (fun ff f -> Var.fmt ff f)) formals ;

  (* Deal with special case, where return type is a struct type *)
  let freturn, formals =
    match SESP.Cfg.formal_return cfg with
    | None ->
        (None, formals)
    | Some symb ->
        let ty = type_of_esp_type (SESP.Symb.ctype symb) in
        match Typ.desc ty with
        | Typ.Structure _ -> (None, (var_of_symb globals locals (Typ.mkPointer ty) symb) :: formals)
        | _ -> (Some(var_of_symb globals locals ty symb), formals)
  in

  let malloc_heap_vars =
    List.fold_left (fun mm (v,t) ->
      (mkAlloc (K.pos entry_vtx) v t) @ mm
    ) assigns heap_vars
  in
  let entry = CFG.add_vertex graph (K.mk_label (K.pos entry_vtx) id) in
  CFG.add_block_edge graph entry malloc_heap_vars entry_vtx ;

  let free_heap_vars =
    List.fold_left (fun ff (v,_t) ->
      mkFree (K.pos exit_vtx) v :: ff
    ) [] heap_vars
  in
  let exit = CFG.add_vertex graph (K.mk_label (K.pos exit_vtx) id) in
  CFG.add_block_edge graph exit_vtx free_heap_vars exit ;

  {Proc.
    id;
    fty;
    formals;
    freturn;
    locals= Vars.empty;
    modifs= Vars.empty;
    accessed= Vars.empty;
    cfg= graph;
    entry;
    exit;
  }

  with exc -> L.printf 0 "proc_of_cfg: %s" (SESP.Symb.name (SESP.Cfg.symb cfg)) ; raise exc


(*===========================================================================
  Find constants in prog.
  ===========================================================================*)
let prog_consts (main,non_mains) addr_taken_procs =

  let consts : unit Int64HMap.t = Int64HMap.create 31 in

  let num_in_e e =
        E.fold (fun e () ->
          match E.desc e with
          | E.Num(n) -> Int64HMap.add consts n ()
          | _ -> ()
        ) e () in

  let num_in_inst {I.desc} =
    match desc with
    | I.Load(_,e)
    | I.Alloc(_,e) | I.Free(e)
    | I.Move(_,e)
    | I.Cast(_,_,e)
    | I.Assume(e) | I.Assert(e) -> num_in_e e
    | I.Store(e,e') -> (num_in_e e) ; (num_in_e e')
    | I.Kill(_)
    | I.Nop -> ()
    | I.Generic(_) -> failwith "consts_in_generic unimplemented"  in

  (* Add ids of the &p's. *)
  List.iter (fun p ->
    Int64HMap.add consts (Int64.of_int (Proc.Id.id p)) ()
  ) addr_taken_procs ;

  (* Add const ints in cfg *)
  let f _name p =
    CFG.iter_edges (fun _v -> ()) (fun (_,c,_) ->
      match c with
      | C.Inst(i) -> num_in_inst i
      | C.Call _
      | C.ICall _ -> ()
    ) p.Proc.cfg (K.id p.Proc.entry) in

  f () main ; Proc.IdHMap.iter f non_mains ;

  (* Return [consts] keys as a list *)
  let ll = Int64HMap.fold (fun k _v ll -> k :: ll) consts [] in
  ll







(*===========================================================================
  Translate cfgs
  ===========================================================================*)

let remove_dead_cfgs cfgs0 =
  let open! SESP in
  let scan_symb symb todo =
    if Symb.kind symb = ESP.ESP_IR_SK_FUNCTION then
      (Symb.name symb) :: todo
    else
      todo
  in
  let cfgs = StringHMap.create 128
  in
  let rec loop = function
    | name :: todo ->
        if not (StringHMap.mem cfgs name) then
          match StringHMap.tryfind cfgs0 name with
          | Some(cfg) ->
              StringHMap.add cfgs name cfg ;
              loop (SESPExt.fold_cfg scan_symb (fun _ todo -> todo) (fun _ todo -> todo) cfg todo)
          | None ->
              loop todo
        else
          loop todo
    | [] ->
        cfgs
  in
  let todo =
    StringHMap.fold (fun name cfg todo ->
      let undecoratedname = Symb.undecoratedname (Cfg.symb cfg) in
      if (SESPExt.proc_is_agg_static_init undecoratedname ||
          SESPExt.proc_is_agg_dyn_init undecoratedname)
      then
        name :: todo
      else
        todo
    ) cfgs0 ["main"] in
  loop todo


let program_of_cfgs cfgs =

  L.incf 2 "( program_of_cfgs" ; (fun _ -> L.decf 2 ") program_of_cfgs") <& let()=()in

  (* Input cfg graphs, for debugging. *)
  if Config.write_cl_cfg then (
    StringHMap.iter (fun _ cfg ->
      let cfg_name = SESP.Cfg.name cfg in
      let filename =
        match SESP.Node.source_location (SESP.Cfg.entry cfg) with
        | Some(filename,_) -> Filename.basename filename
        | None -> Config.testname in
      let cfg_graph,cfg_entry = graph_of_esp_cfg filename cfg in
      Library.with_out (filename ^ "." ^ cfg_name ^ ".esp.cfg.dot")
        (ESPGraph.write_dot cfg_graph (ESPGraph.index_of cfg_entry))
    ) cfgs
  );

  let cfgs = remove_dead_cfgs cfgs in

  (* SI: check, I don't think I need the Type.t anymore. *)
  let globals : (Var.t * var_kind * Typ.t) SESPExt.SymbHMap.t =
    SESPExt.SymbHMap.create 255 in

  let procs = Proc.IdHMap.create (StringHMap.length cfgs - 1) in

  (* Heapification:
     Have to calculate heapified globals for all procedures, as address taken
     globals need boxing, this needs complete knowledge to
     calculate. *)
  let which_globals =
    if Config.optimize_boxing then
      fun sym -> SESPExt.to_heapify_type (SESP.Symb.ctype sym)
    else
      fun _ -> true
  in
  let vars_to_heap = SESPExt.to_heapify_cfgs which_globals cfgs in
  L.printf 3 "Heapify {%a}" (List.fmt "," SESPExt.fmt_symb)
    (SESPExt.SymbHMap.fold (fun symb _tag symbs -> symb :: symbs) vars_to_heap []) ;

  let heapified_globals = SESPExt.SymbHMap.mem vars_to_heap in

  let mains =
    StringHMap.fold (fun _ cfg mains ->
      let open Proc in
      let p = proc_of_cfg cfgs globals heapified_globals cfg in
      IdHMap.add procs p.id p ;
      if Id.name p.id = "main"
      then p :: mains
      else mains
    ) cfgs [] in

  let main =
    match mains with
    | [main] -> main
    | _ -> failwith "Not the one main"
  in

  let {Proc.entry= entry_vtx; exit= exit_vtx; formals; freturn} = main in

  (* Malloc and free global heapified vars *)
  let heap_vars =
    SESPExt.SymbHMap.fold (fun hsymb htag vars ->
      match htag with
      | SESPExt.HAddrOfLocal | SESPExt.HLocalStruct -> vars
      (* Ignore globals, they'll be malloced in main. *)
      | SESPExt.HGlobal
      | SESPExt.HAddrOfFunc -> (hsymb, SESPExt.SymbHMap.find globals hsymb) :: vars
      | SESPExt.HFormal | SESPExt.HFormalStruct -> vars
    ) vars_to_heap [] in

  (* Create procedure to allocate and deallocte the globals *)
  let mk_proc name pos insts =
    let id = Proc.Id.gensym name in
    let cfg = CFG.create() in
    let entry = CFG.add_vertex cfg (K.mk_label pos id) in
    let exit = CFG.add_vertex cfg (K.mk_label pos id)
    in
    CFG.add_block_edge cfg entry insts exit ;
    let p =
      {Proc.
        id; fty= Typ.mkFunction Typ.mkTop [] false; formals= []; freturn= None;
        locals= Vars.empty; modifs= Vars.empty; accessed= Vars.empty;
        cfg; entry; exit;
      } in
    Proc.IdHMap.add procs id p ;
    p in

  L.incf 6 "Create alloc and init procedure" ; (* Malloc and free global heapified vars *)
  let malloc_heap_vars =
    List.fold (fun (symb, (v,tag,_ty)) mm ->
      match tag with
      | VarKFun ->
          let pos = (K.pos entry_vtx) in
          let a = mkAlloc pos v (Typ.mkInt false 1) in
          let proc_id = Int64.of_int (Proc.Id.id (proc_of symb)) in
          let i = mkStore (E.mkVar v) (E.mkNum proc_id) pos in
          a @ [i] @ mm
      | _ ->
          let ty = type_of_esp_type (SESP.Symb.ctype symb) in
          (mkAlloc (K.pos entry_vtx) v ty) @ mm
    ) heap_vars []
  in
  let global_alloc = mk_proc "SLAyer_alloc_and_init_globals" (K.pos entry_vtx) malloc_heap_vars in

  let mkIdCall ({Proc.id} as proc) actuals areturn =
    C.Call({(Call.mk proc actuals areturn) with Call.proc= id})
  in

  (* Call aggregate_*_init. *)
  let call_inits, init_ids =
    Proc.IdHMap.fold (fun _ p (calls, ids) ->
      let open Proc in
      let name = Id.name p.id in
      if (SESPExt.proc_is_agg_static_init name ||
          SESPExt.proc_is_agg_dyn_init name)
      then
        (mkIdCall p [] None :: calls, p.id :: ids)
      else
        (calls, ids)
    ) procs ([],[]) in

  let {Proc.exit= ga_exit; cfg= ga_cfg; id= ga_id} = global_alloc in

  let add_call_after cfg id pos call prev_vtx =
    let return_vtx = CFG.add_vertex cfg (K.mk_label pos id) in
    CFG.add_edge cfg prev_vtx call return_vtx ;
    return_vtx (* is next prev_vtx *) in

  (* Add the static and dynamic initialises to the end of the alloc
     and init procedure *)
  let ga_last =
    let graph = ga_cfg in
    List.fold (add_call_after graph ga_id (K.pos entry_vtx))
      call_inits ga_exit in
  (* Add new exit node, as ga_last with be Return, so need a nop to Exit *)
  let ga_exit = CFG.add_vertex ga_cfg (K.mk_label (K.pos entry_vtx) ga_id) in
  CFG.add_edge ga_cfg  ga_last (mkNop (K.pos entry_vtx)) ga_exit ;

  (* Update the exit for the procedure *)
  let global_alloc = {global_alloc with Proc.exit= ga_exit} in
  Proc.IdHMap.add procs ga_id global_alloc ;
  L.decf 6 "" ;

  L.incf 6 "Create dealloc procedure" ;
  (* Deallocate all the globals. *)
  let free_heap_vars =
    List.fold (fun (_symb, (v,_tag,_ty)) ff ->
      mkFree (K.pos exit_vtx) v :: ff
    ) heap_vars []
  in
  let global_dealloc = mk_proc "SLAyer_dealloc_globals" (K.pos exit_vtx) free_heap_vars in
  L.decf 6 "" ;

  let global_setup = [global_alloc.Proc.id; global_dealloc.Proc.id] in

  L.incf 6 "Creating SLAyer main" ;
  let slayer_main = mk_proc "SLAyer_main" (K.pos entry_vtx) [] in
  let sm_cfg = slayer_main.Proc.cfg in
  let sm_id = slayer_main.Proc.id in
  let slayer_exit = slayer_main.Proc.exit in
  let slayer_last =
    List.fold (fun call vtx ->
      add_call_after sm_cfg sm_id (K.pos entry_vtx) call vtx
    ) [ mkIdCall global_alloc [] None
      ; mkIdCall main formals freturn
      ; mkIdCall global_dealloc [] None
      ]
      slayer_exit
  in

  let slayer_exit = CFG.add_vertex sm_cfg (K.mk_label (K.pos exit_vtx) sm_id) in
  CFG.add_edge sm_cfg slayer_last (mkNop (K.pos entry_vtx)) slayer_exit ;

  L.decf 6 "" ;

  (* Calculate other (meta-data) Prog components *)
  (* -- constants *)
  let addr_taken =
    SESPExt.SymbHMap.fold (fun symb (_v,tag,_ty) pp ->
      match tag with
      | VarKFun -> proc_of symb :: pp
      | _ -> pp
    ) globals [] in
  let constants = prog_consts (main,procs) addr_taken in

  (* -- globals *)
  let addr_taken_vars =
    SESPExt.SymbHMap.fold (fun _symb (v,tag,_ty) vv -> match tag with
      | VarKFun -> Vars.add v vv
      | _ -> vv
    ) globals Vars.empty in
  let globals = SESPExt.SymbHMap.fold (fun _ (v,tag,_) gs -> match tag with
    | VarKFun -> gs
    | _ -> Vars.add v gs
  ) globals addr_taken_vars in

  Proc.IdHMap.add procs sm_id {slayer_main with Proc.exit= slayer_exit; formals} ;

  {Prog.
    constants;
    globals;
    addr_taken;
    main= sm_id;
    procs;
    global_setup;
    inits = init_ids;
  }


(*============================================================================
  C compiler
  ============================================================================*)

let run_cl filenames =
  Unix.putenv "Esp.CfgPersist.ExpandLocalStaticInitializer" "1" ;

  let cl_exe = "cl" in
  let args =
    Array.of_list (
        [cl_exe]
      @ ["/nologo"]
      @ ["/DSLAyer=1"]
      @ ["/FIslayer_intrinsics.h"]
      @ (if Config.no_builtins then [] else ["/FIslayer.h"])
      @ ["/analyze:quiet"]
      @ ["/analyze:only"]
      @ ["/analyze:plugin"; "ESPPersist.dll"]
      @ Config.frontend_args
      @ filenames
    ) in
  L.printf 1 "%s@." (String.concat " " (Array.to_list args)) ;
  Pervasives.flush_all () ;
  let pid = Unix.create_process cl_exe args Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  if status <> Unix.WEXITED(0) then -1 else 0


(*============================================================================
  Entry point
  ============================================================================*)

let program_of_file filenames =
  let cfgs = StringHMap.create 128
  in
  let c_files, cfg_files =
    List.partition (fun filename ->
      if (Filename.check_suffix filename ".c" || Filename.check_suffix filename ".cpp") then
        true
      else if (Filename.check_suffix filename ".rawcfgf") then
        false
      else
        failwith "input file must be either C source (.c) or an esp cfg (.rawcfgf)"
    ) filenames
  in
  (* run cl to generate .rawcfgf from .c *)
  let cl_status = run_cl c_files
  in
  let cfg_files =
    if cl_status = 0 then
      List.fold (fun c_file cfg_files -> (c_file ^ ".rawcfgf") :: cfg_files) c_files cfg_files
    else
      failwith "cl died"
  in
  List.iter (fun cfg_file ->
    Array.iter (fun cfg ->
      StringHMap.add cfgs (SESP.Cfg.name cfg) cfg
    ) (SESP.Cfgs.cfgs cfg_file)
  ) cfg_files
  ;
  program_of_cfgs cfgs
