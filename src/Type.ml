(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Types for determining object layout *)

(* Review:
   1. Top currently used to break cycles in typedefs;
   2. Enum and Function probably not needed.
*)

open Library

open FLD
open TYP
module HC = HashCons

module L = (val Log.std Config.vTyp : Log.LOG)


(*============================================================================
                                     Fld
  ============================================================================*)

module rec Fld : sig
  type t = { id: int; off: int * int option; name: string; mutable typ: Typ.t }
  include FLD with type typ := Typ.t and type t := t
  val fmt_off : (int * int option) formatter
end =
struct

  type t = { id: int; off: int * int option; name: string; mutable typ: Typ.t }
  (* Fld.t values are cyclic: f.typ is the type of which f is a member *)

  include UniqueId.Make(struct
    type data = (int * int option) * string * Typ.t
    type uniq = t
    let get {id} = id
    let set id (off, name, typ) = {id; off; name; typ}
  end)

  let compare x y = let c = Pervasives.compare x.off y.off in if c <> 0 then c else compare x y

  let off  x = x.off
  let name x = x.name
  let typ  x = x.typ

  let is_first x =
    match x.off with
    | (0, None) | (0, Some(0)) -> true
    | _ -> false

  let mk off name = gensym (off, name, Typ.mkTop)
  let unsafe_create id off name typ = unsafe_create id (off, name, typ)


  let find_by_name ty name =
    try
      match Typ.desc ty with
      | Typ.Structure(_,fld_tys,_) | Typ.Union(_,fld_tys,_) ->
          Some(List.find (fun (f,_) -> f.name = name) fld_tys)
      | _ ->
          raise Not_found
    with Not_found -> None


  let fmt_off ff (byt_off, bit_off) =
    match bit_off with
    | Some(bit_off) -> Format.fprintf ff "%i.%i" byt_off bit_off
    | None          -> Format.fprintf ff "%i" byt_off

  let fmt ff v =
    match !Config.vTyp with
    | 0 -> Format.fprintf ff "%s:%a" (Hooks.var_name v.name) fmt_off v.off
    | 1 -> Format.fprintf ff "%s:%a" v.name fmt_off v.off
    | _ -> Format.fprintf ff "%s:%a!%i" v.name fmt_off v.off v.id

  let fmt_caml _ff _ =
    failwith "ToDo: Fld.fmt_caml"

end



(*============================================================================
                                     Typ
  ============================================================================*)

and Typ : (TYP with type fld := Fld.t) = struct

  type t = t_desc HC.hc

  and t_desc =
    | Top
    | Named of string
    | Bool
    | Int of bool * int
    | Float of int
    | Pointer of t
    | Array of t * int64 option * int64
    | Structure of string * (Fld.t * t) list * int64
    | Union of string * (Fld.t * t) list * int64
    | Enum of string * (string * int) list * int64
    | Function of t * t list * bool

  let id   t = t.HC.id
  let desc e = e.HC.desc


  (* Formatting ============================================================= *)

  let rec fmt_desc_ deep ff = function
    | Named s -> Format.fprintf ff "@[%s@]" s
    | Top -> Format.fprintf ff "@[Top@]"
    | Bool -> Format.fprintf ff "@[bool@]"
    | Int(u,s) -> Format.fprintf ff "@[%sint%i@]" (if u then "u" else "") s
    | Float(s) -> Format.fprintf ff "@[float%i@]" s
    | Pointer(t) -> Format.fprintf ff "@[%a *@]" (fmt_ false) t
    | Array(t,None,_) -> Format.fprintf ff "@[%a[]@]" (fmt_ deep) t
    | Array(t,Some(l),_) -> Format.fprintf ff "@[%a[%Ld]@]" (fmt_ deep) t l
    | ( Structure(id,fldty,_)
      | Union(id,fldty,_) ) as t ->
        let fldty_fmt ff (fld,ty) = Format.fprintf ff "@[<hov 2>%a@ %a@]" (fmt_ deep) ty Fld.fmt fld in
        if not deep || !Config.vTyp <= 1 then Format.fprintf ff "%s" id else
        Format.fprintf ff "@[%s %s@ {@[<hv 1> %a@]@ }@]"
          (match t with Structure _ -> "struct" | Union _ -> "union" | _ -> assert false)
          id
          (List.fmt ";@ " (fun ff fldty -> fldty_fmt ff fldty)) fldty
    | Enum(id,idval,_) ->
        let idval_fmt ff (id,v) = Format.fprintf ff "@[%s = %d@]" id v in
        if !Config.vTyp <= 1 then Format.fprintf ff "%s" id else
        Format.fprintf ff "@[enum %s { %a }@]"
          id (List.fmt ";@ " idval_fmt) idval
    | Function(retty,args,vararg) ->
        Format.fprintf ff "@[(%a)(FUNC)(%a%s)@]"
          (fmt_ deep) retty (List.fmt ";@ " (fmt_ deep)) args (if vararg then ",..." else "")

  and fmt_ deep ff t =
    fmt_desc_ deep ff (desc t)

  let fmt_desc ff t = fmt_desc_ true ff t

  let fmt ff t =
    match !Config.vTyp with
    | 0 -> ()
    | 1 -> Format.fprintf ff "@[(%a)@]" (fmt_ true) t
    | _ -> Format.fprintf ff "@[%i:(%a)@]" t.HC.id (fmt_ true) t


  let rec fmt_caml ff t =
    match desc t with
    | Named(s) -> Format.fprintf ff "@[(Typ.mkNamed %s)@]" s
    | Top -> Format.fprintf ff "@[Typ.mkTop@]"
    | Bool -> Format.fprintf ff "@[Typ.mkBool@]"
    | Int(u,s) -> Format.fprintf ff "@[Typ.mkInt %b %i@]" u s
    | Float(s) -> Format.fprintf ff "@[Typ.mkFloat %i@]" s
    | Pointer(t) -> Format.fprintf ff "@[(Typ.mkPointer %a)@]" fmt_caml t
    | Array(t,None,s) -> Format.fprintf ff "@[(Typ.mkArray %a None %Li)@]" fmt_caml t s
    | Array(t,Some(l),s) -> Format.fprintf ff "@[(Typ.mkArray %a (Some(%Li)) %Li)@]" fmt_caml t l s
    | (Structure(id,fldty,size)
    |  Union(id,fldty,size)) as t ->
        let fldty_fmt_caml ff (fld,ty) =
          Format.fprintf ff "@[(%a,@ %a)@]" Fld.fmt_caml fld fmt_caml ty in
        Format.fprintf ff "@[%s@ \"%s\"@ [@[%a@]]@ %Li@]"
          (match t with
          | Structure _ -> "Typ.mkStructure"
          | Union _     -> "Typ.mkUnion"
          | _ -> assert false)
          id
          (List.fmt ";@ " (fun ff fldty -> fldty_fmt_caml ff fldty)) fldty
          size
    | Enum(id,idval,size) ->
        let idval_fmt_caml ff (id,v) =
          Format.fprintf ff "@[(\"%s\",%d)@]" id v in
        Format.fprintf ff "@[(Typ.mkEnum \"%s\"@ %a@ %Li@]"
          id (List.fmt ";@ " (fun ff idval -> idval_fmt_caml ff idval)) idval size
    | Function(retty,args,vararg) ->
        Format.fprintf ff "@[(Typ.mkFunction %a@ [%a]@ %s)@]"
          fmt_caml retty (List.fmt ";@ " fmt_caml) args
          (if vararg then "true" else "false")


  (* Comparison ============================================================= *)

  let rec hash_desc x =
    match x with
    | Pointer(t) ->
        Hashtbl.hash (1 + hash t)
    | Array(t,l,s) ->
        Hashtbl.hash (hash t, l, s)
    | Structure(n,fs,s) | Union(n,fs,s) ->
        Hashtbl.hash (n, List.map (fun (f,t) -> (Fld.id f, hash t)) fs, s)
    | Function(r,ps,v) ->
        Hashtbl.hash (hash r, List.map hash ps, v)
    | Top | Bool | Int _ | Float _ | Enum _ | Named _ ->
        Hashtbl.hash x

  and hash x = x.HC.hash
    &> (fun n -> assert( n = hash_desc x.HC.desc || failwithf "mis-hashed: %a" fmt x ))

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
    | Top, Top
    | Named _, Named _
    | Bool, Bool
    | Int _, Int _
    | Float _, Float _
    | Enum _, Enum _ ->
        x = y
    | Pointer(s), Pointer(t) ->
        equal s t
    | Array(t,m,i), Array(u,n,j) ->
         i = j && m = n && equal t u
    | Structure(n,fs,i), Structure(m,gs,j)
    | Union(n,fs,i), Union(m,gs,j) ->
           i = j
        && n = m
        && List.equal (fun (f,s) (g,t) -> Fld.equal f g && equal s t) fs gs
    | Function(r,ps,u), Function(s,qs,v) ->
        u = v && equal r s && List.equal equal ps qs
    | ( Top | Bool | Int _ | Float _ | Pointer _
      | Array _ | Structure _ | Union _ | Enum _ | Function _ | Named _), _ ->
        false

  let equal x y = (x == y)
    &> (fun ptr_eq -> assert(
         let id_eq = (x.HC.id = y.HC.id) in
         let desc_eq = equal_desc x.HC.desc y.HC.desc in
         let hash_eq = (hash x = hash y) in
         (not ptr_eq || hash_eq
          || failwithf "@[hash %a = %i != %i = hash %a@]" fmt x (hash x) (hash y) fmt y) &&
         (ptr_eq = id_eq
          || failwithf "@[%a %s %a@]" fmt x (if ptr_eq then "== but id <>" else "!= but id =") fmt y) &&
         (id_eq = desc_eq
          || failwithf "@[%a %s %a@]" fmt x (if id_eq then "id = but desc <>" else "id <> but desc =") fmt y) &&
         (desc_eq = ptr_eq
          || failwithf "@[%a %s %a@]" fmt x (if ptr_eq then "== but desc <>" else "!= but desc =") fmt y) ))

  let compare x y = Pervasives.compare x.HC.id y.HC.id
    &> (fun cmp -> assert(
         (cmp <> 0 || hash x = hash y
          || failwithf "@[hash %a = %i <> %i = hash %a@]" fmt x (hash x) (hash y) fmt y) &&
         ((cmp = 0) = (equal_desc x.HC.desc y.HC.desc)
          || failwithf "@[%a@ %s@ %a@]" fmt x (if cmp = 0 then "= but <>" else "<> but =") fmt y) ))


  (* Hash-Consing =========================================================== *)

  module Desc = struct
    type t = t_desc
    let equal x y = equal_desc x y
    let hash x = hash_desc x
    let fmt = fmt_desc
  end

  module HCTbl = HC.Make(Desc)

  let tbl = HCTbl.create Config.typ_hc_initial_size

  let name d = HCTbl.intern tbl d


  (* Desc Constructors ====================================================== *)

  let mkTop = name Top
  let mkBool = name Bool
  let mkInt u s = name (Int(u,s))
  let mkFloat s = name (Float(s))
  let mkPointer t = name (Pointer(t))
  let mkNamed t = name (Named(t))
  let mkArray t l s = name (Array(t,l,s))
  let mkEnum n ts s = name (Enum(n,ts,s))
  let mkFunction r ps v = name (Function(r,ps,v))

  let set_field_typs ty fs = List.iter (fun (fld,_) -> fld.Fld.typ <- ty) fs

  let mkStructure n fs s =
    let ty = name (Structure(n,fs,s)) in
    set_field_typs ty fs ;
    assert( ty == name (Structure(n,fs,s)) );
    ty

  let mkUnion n fs s =
    let ty = name (Union(n,fs,s)) in
    set_field_typs ty fs ;
    assert( ty == name (Union(n,fs,s)) );
    ty


  (* Queries ================================================================ *)

  let fst_flds t =
    match desc t with
    | Structure(_,ftys,_) | Union(_,ftys,_) ->
        List.fold (fun (f,_) fs -> if Fld.is_first f then f :: fs else fs) ftys []
    | _ ->
        []


  let fmt_path ff path = Format.fprintf ff "%a" (List.fmt ";@ " Fld.fmt) path
  let fmt_path_t ff (off,path,t) = Format.fprintf ff "@[<hov 1>(%a,@ %a,@ %a)@]" Fld.fmt_off off fmt_path path fmt t

  let off_add (x,n) (y,i) =
    assert( n = None || failwithf "Unexpected aggregate bit-field" );
    (x+y, i)

  let all_paths t =
    assert(true$>( L.incf 10 "( all_paths:@ %a" fmt t ));
    (fun z -> assert(true$>( L.decf 10 ") @[<hv>%a@]" (List.fmt ";@ " fmt_path_t) z )))
    <&
    let rec path off prefix t results =
      match desc t with
      | Structure(_id,fty,_) | Union(_id,fty,_) ->
          List.fold_right (fun (f,t) pp -> path (off_add off (Fld.off f)) (f :: prefix) t pp) fty results
      | Bool | Int _ | Float _ | Pointer _ | Array _ | Enum _ | Function _ ->
          (off, prefix, t) :: results
      | Top | Named _ ->
          results
    in
    path (0,None) [] t []


  let all_offsets t =
    assert(true$>( L.shift_verb (!Config.vTyp - 2) (fun () -> L.incf 10 "( all_offsets:@ %a" fmt t )));
    (fun z -> assert(true$>( L.decf 10 ") @[<hv>%a@]" (List.fmt ";@ " fmt_path_t) z )))
    <&
    let rec path off prefix t results =
      match desc t with
      | Structure(_,fty,_) ->
          List.fold_right (fun (f,t) pp -> path (off_add off (Fld.off f)) (f :: prefix) t pp) fty results
      | Union(_,(f,t) :: _,_) ->
          path (off_add off (Fld.off f)) (f :: prefix) t results
      | Union(_,[],_) ->
          results
      | Bool | Int _ | Float _ | Pointer _ | Array _ | Enum _ | Function _ ->
          if List.exists (fun (o,_,_) -> off = o) results then
            results
          else
            (off, prefix, t) :: results
      | Top | Named _ ->
          results
    in
    path (0,None) [] t []


  let paths_at_offset ty off =
    List.fold (fun (o,path,ty) paths ->
      if o = off
      then (path, ty) :: paths
      else paths
    ) (all_offsets ty) []


  let sizeof typ =
    assert(true$>( L.incf 10 "( sizeof: %a" fmt typ )); (fun z -> assert(true$>( L.decf 10 ") %Li" z )))
    <&
    match desc typ with
    | Top | Named _ ->
        0L
    | Bool ->
        1L
    | Int(_,s) | Float(s) ->
        Int64.of_int s
    | Pointer(_) | Function(_) | Array(_,None,_) ->
        Int64.of_int Config.ptr_size
    | Array(_,_,s) | Enum(_,_,s) | Structure(_,_,s) | Union(_,_,s) ->
        s


  let of_fld typ fld =
    match desc typ with
    | Structure(_,fld_tys,_) | Union(_,fld_tys,_) ->
        (try Some(List.assoc fld fld_tys) with Not_found -> None)
    | _ ->
        None


  let fold_defined fn z =
    HCTbl.fold fn tbl z


  let find_by_name name =
    let module M = struct exception Found of Typ.t end in let open M in
    try
      fold_defined (fun ty () ->
        match desc ty with
        | Structure(ty_id,_,_) | Union (ty_id,_,_) when ty_id = name ->
            raise (Found(ty))
        | _ -> ()
      ) () ;
      raise Not_found
    with
    | Found(ty) -> Some(ty)
    | Not_found -> None

end
