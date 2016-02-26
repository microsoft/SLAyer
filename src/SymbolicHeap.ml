(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Symbolic Heap formulas *)

open Library

open Type
open Variable
open Expression
module E = Exp
module O = Off
module HC = HashCons
module S = Substitution
open SYMBOLIC_HEAP

module L = (val Log.std Config.vSH : Log.LOG)



(* Timing =================================================================== *)

let normalize_tmr = Timer.create "SH.normalize"
let normalize_stem_tmr = Timer.create "SH.normalize_stem"
let exists_elim_tmr = Timer.create "SH.exists_elim"
let pure_consequences_tmr = Timer.create "SH.pure_consequences"
let labeled_pure_consequences_tmr = Timer.create "SH.labeled_pure_consequences"



(*Pt==========================================================================
                              Points-To formulas
  ============================================================================*)

module Pt = struct

    type t = { loc: Exp.t; off: Off.t; cnt: Exp.t option }


    let fmtp fxt ff {loc; off; cnt} =
      let fmt_loc ff =
        if !Config.vSH > 3 then Exp.fmt ff loc else
        match O.desc off with
        | O.Var(v) -> Format.fprintf ff "%a-%a" (E.fmtp fxt) loc (Var.fmtp fxt) v
        | O.Path(_,fs) -> E.fmtp fxt ff (E.mkSubs loc fs)
      in
      Format.fprintf ff "@[<hov 2>(%t -%a-> %a)@]"
        fmt_loc (O.fmtp fxt) off (Option.fmt "-" (E.fmtp fxt)) cnt

    let fmt ff x = fmtp (Vars.empty,Vars.empty) ff x

    let fmt_caml ff {loc; off; cnt} =
      Format.fprintf ff "{Pt.loc= %a;@ off= %a;@ cnt= %a}"
        E.fmt_caml loc O.fmt_caml off
        (Option.fmt "None" (fun ff -> Format.fprintf ff "Some(%a)" E.fmt_caml))
        cnt

    let equal x y =
      E.equal x.loc y.loc && O.equal x.off y.off
      && Option.equal E.equal x.cnt y.cnt

    let compare x y =
      let cmp = E.compare x.loc y.loc in if cmp <> 0 then cmp else
      let cmp = O.compare x.off y.off in if cmp <> 0 then cmp else
                Option.compare E.compare x.cnt y.cnt

    let fold_exps fn {loc; cnt} z =
      E.fold fn loc (Option.fold (E.fold fn) cnt z)

    let map_exps fn {loc; off; cnt} =
      {loc= fn loc; off= Off.mk (fn (off :>Exp.t)); cnt= Option.map fn cnt}

    let fv {loc; off; cnt} =
      Vars.union (Vars.union
        (E.fv loc) (O.fv off)) (Option.option Vars.empty E.fv cnt)

    let may_allocs {loc} =
      Exps.singleton loc

end

module Pts : sig

  include IndexedSet.S with type elt := Pt.t and type idx := E.t
  val may_allocs : t -> Exps.t

end = struct

  (* Sets of points-tos are represented by sets indexed by the alloc
     expression to the Pt.t.  Note that normalization ensures that no Pts has
     more than one Pt per alloc expression. *)

  include IndexedSet.Make(struct
    type t = Pt.t
    let equal = Pt.equal
    let compare = Pt.compare
    type idx = E.t
    let index {Pt.loc} = loc
    let equal_idx = E.equal
    let compare_idx = E.compare
  end)

  let may_allocs s = Exps.of_list (keys s)

end

module ExpPtsMap = MultiMap.Make (Exp) (struct include Pts type elt = Pt.t end)



(*Params======================================================================
                  Formal Parameters of List Segment Patterns
  ============================================================================*)

module Params =
  BiEdge.Make (struct
    include Var
    let fold_exps _ _ z = z
    let map_exps _ v = v
    let fv v = Vars.singleton v
  end)



(*Args========================================================================
                  Actual Arguments of List Segment formulas
  ============================================================================*)

module Args = struct

  include BiEdge.Make (struct
    include Exp
    let fold_exps = fold
    let map_exps fn e = fn e
  end)

  let fmtp fxt ff {prev; frnt; back; next} =
    let args =
      if !Config.vSH > 3 then {prev; frnt; back; next} else
      let first = function [] -> [] | x::_ -> [x] in
      {prev= first prev; frnt= first frnt; back= first back; next= first next} in
    fmtp fxt ff args

  (** cycle_eqs (p, f;f', b, n;n') = f=n * f'=n' * b=p *)
  let cycle_eqs x =
    fold_links (fun (a,d) eql -> E.mkEq a d :: eql) x []

  let remove dir sub arg =
    if dir
    then remove_prefix sub arg
    else remove_suffix sub arg

end



(*Patn========================================================================
                            List Segment Patterns
  ============================================================================*)

module rec Patn : sig

  type t = private { params: Params.t; body: XSh.t; name: string }
  include TERM with type t := t

  val mk : ?name:string -> Params.t -> XSh.t -> t
  val instantiate : t -> Args.t -> XSh.t

end = struct
  type t = { params: Params.t; body: XSh.t; name: string }


  let fmtp fxt ff {params; body; name} =
    if name = "" || !Config.vDiscovery > 1 then
      Format.fprintf ff "@[(\\%a.@ %a)@]" Params.fmt params (XSh.fmtp fxt) body
    else
      Format.fprintf ff "%s" name

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml _ _ = failwith "Patn.fmt_caml unimplemented"


  let equal x y =
    x == y || (Params.equal x.params y.params && XSh.equal x.body y.body)

  let compare x y =
    if x == y then 0 else
    let c = Params.compare x.params y.params in if c <> 0 then c else
            XSh.compare x.body y.body


  let fold_exps _ _ z = z

  let map_exps _ p = p


  let bv x = Params.fv x.params

  let fv x = Vars.diff (XSh.fv x.body) (bv x)


  let mk ?(name="") params body =
    (* Note: Check this constraint.
    (fun ({params; body} as patn) -> assert(
      (* All variables in [may_allocs params] must be allocated in the body.
         In particular, f->_ * ls(L,k,p,f.u,b.v,n) implies k=0. *)
      let _, body = XSh.exists_bind Vars.empty body in
      let param_allocs =
        List.fold (fun a es ->
          Exps.add (CngRel.normalize body.Sh.fcr (E.mkVar a)) es
        ) (Params.may_allocs params) Exps.empty in
      let body_allocs = Sh.must_allocs body in
      Exps.subset param_allocs body_allocs ||
        failwithf "Patn.mk: may_allocs not allocated: %a" fmt patn ))
    <& *)
    let body = XSh.normalize body in
    {params; body; name}


  let instantiate patn args =
    assert(true$>
      L.incf 10 "( Patn.instantiate: %a@ (%a)" fmt patn Args.fmt args );
    (fun xsh -> assert(true$>
      L.decf 10 ") Patn.instantiate: %a" XSh.fmt xsh ))
    <&
    (* substitute arguments for parameters *)
    let s =
      Args.fold2 (fun x e s -> S.add (E.mkVar x) e s) patn.params args S.empty
    in
    XSh.subst s patn.body

end



(*Ls==========================================================================
                                List Segments
  ============================================================================*)

and Ls : sig

  type t = { pat: Patn.t; len: E.t; arg: Args.t }
  include TERM with type t := t
  val empty_eqs : t -> E.t list
  val may_allocs : t -> E.t list
  val fst_alloc : t -> E.t
  val direction : t -> E.t -> bool
  val split_on_fresh_point : t -> Vars.t * Args.t * Args.t

end = struct

  type t = { pat: Patn.t; len: E.t; arg: Args.t }


  (** We write ls(L, k, p, f;f'', b, n;n') for an Ls.t value
      (L, k, (p, f;f', b, n;n'), ms) *)
  let fmtp fxt ff {pat; len; arg} =
    Format.fprintf ff "@[<hov 3>ls(%a,@ %a,@ %a)@]"
      (Patn.fmtp fxt) pat (E.fmtp fxt) len (Args.fmtp fxt) arg

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml ff {pat; len; arg} =
    Format.fprintf ff "@[<hov 1>{Ls.pat=%a;@ len=%a;@ arg=%a}@]"
      Patn.fmt_caml pat E.fmt_caml len Args.fmt_caml arg


  let equal x y =
    Patn.equal x.pat y.pat && E.equal x.len y.len && Args.equal x.arg y.arg

  let compare x y =
    let c = Args.compare x.arg y.arg in if c <> 0 then c else
    let c =    E.compare x.len y.len in if c <> 0 then c else
            Patn.compare x.pat y.pat


  let fold_exps fn {len; arg} z =
    E.fold fn len (Args.fold_exps fn arg z)

  let map_exps fn {pat; len; arg} = {
    pat= Patn.map_exps fn pat;
    len= fn len;
    arg= Args.map_exps fn arg;
  }


  let fv {pat; len; arg} =
    Vars.union (Vars.union (Patn.fv pat) (E.fv len)) (Args.fv arg)


  (** empty_eqs ls(L, k, p0, f;f', b0, n;n') = k=0 * f=n * f'=n' * b0=p0 *)
  let empty_eqs {len; arg} =
    E.mkEq len E.zero :: Args.cycle_eqs arg


  let may_allocs {arg} =
    Args.may_allocs arg


  let fst_alloc ({arg= {frnt; back}} as ls) =
    match frnt, back with
    | f :: _, _ -> f
    | [], b :: _ -> b
    | [], [] ->
        (* Note: Enforce this requirement with a constructor? *)
        invalid_argf "Ls.fst_alloc: list must have at least one end: %a"
          fmt ls


  let direction {arg={frnt; back}} loc =
    (fun dir -> assert( dir || List.exists (fun a -> E.equal loc a) back ))
    <&
    List.exists (fun a -> E.equal loc a) frnt


  let split_on_fresh_point {pat= {Patn.params= {frnt; back}}; arg} =
    let freshen params vs =
      List.fold (fun param (vl,vs) ->
        let v = Var.gensym (Var.name param) (Var.sort param) in
        (E.mkVar v :: vl, Vars.add v vs)
      ) params ([],vs)
    in
    let fs, vs = freshen frnt Vars.empty in
    let bs, vs = freshen back vs in
    let fnt, bck = Args.split bs fs arg in
    (vs, fnt, bck)

end

and Lss : sig

  include MultiIndexedSet.S with type elt := Ls.t and type idx := E.t
  val may_allocs : t -> Exps.t

end = struct

  (* Sets of list segments are represented by sets of ls formulas indexed by
     _every_ may alloc expression in the ls formula.  Note that normalization
     ensures that no Lss has more than one Ls per may alloc expression. *)

  include MultiIndexedSet.Make(struct
    type t = Ls.t
    let equal = Ls.equal
    let compare = Ls.compare
    type idx = E.t
    let index = Ls.fst_alloc
    let indices = Ls.may_allocs
    let equal_idx = E.equal
    let compare_idx = E.compare
  end)

  let may_allocs s = Exps.of_list (keys s)

end

and LsB : sig

  include MultiIndexedMultiSet.S with type idx := E.t and type elt := Ls.t and type elts = Ls.t list

end = struct

  module IndexedLs = struct
    type t = Ls.t
    let equal = Ls.equal
    let compare = Ls.compare
    type idx = E.t
    let index = Ls.fst_alloc
    let indices = Ls.may_allocs
    let equal_idx = E.equal
    let compare_idx = E.compare
  end
  include MultiIndexedMultiSet.Make (IndexedLs) (List.Set(Ls))

end



(*Dj==========================================================================
                                 Disjunctions
  ============================================================================*)

and ShSet : (Set.S with type elt := Sh.t) = Set.Make(Sh)

(* disjunctions of symbolic heaps *)
and Dj : sig

  include TERM with type t = ShSet.t
  val fv : ?include_cng_rels:bool -> t -> Vars.t
  val may_allocs : t -> Exps.t
  include Set.S with type elt := Sh.t and type t := t
  val fold_commutative_semigroup : ('z -> 'z -> 'z) -> (Sh.t -> 'z) -> t -> 'z
(*   val may_allocs : t -> Exps.t *)
  val fmtsp : Sh.t -> Var.fxt -> t formatter

end = struct
  open Sh_t

  (* Disjunctions are represented by sets of Sh formulas, normalized with
     respect to identify of ff \/ -, associativity of \/, and equivalence of
     boolean and symbolic heap disjunction on pure formulas. *)

  include ShSet

  let fmtsp sup fxt ff x =
    let dts = to_list x in
    let compare_by_lbl x y = let o = Pervasives.compare x.lbl y.lbl in if o<>0 then o else Sh.compare x y in
    let dts = List.sort compare_by_lbl dts in
    Format.fprintf ff "@[<hov 2>{ %a }@]"
      (List.fmt " \\/@ " (Sh.fmtsp sup fxt)) dts

  let fmtp fxt = fmtsp (Sh.emp()) fxt

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml ff x =
    Format.fprintf ff "@[<hov 2>(Dj.of_list@ [@[%a@]]@])"
      (List.fmt ";@ " Sh.fmt_caml) (to_list x)

  let fv ?(include_cng_rels=true) s =
    fold (fun q vs -> Vars.union (Sh.fv ~include_cng_rels q) vs) s Vars.empty

  let map_exps fn dj = map (Sh.map_exps fn) dj

  let fold_exps fn = fold (Sh.fold_exps fn)

(*   let map_foldi fn (s,z) = *)
(*     foldi (fun i q (s,z) -> *)
(*       let q',z' = fn i (q,z) in *)
(*       (add q' s, z') *)
(*     ) s (empty,z) *)

  (* if the image of Dj.t under fn is a commutative semigroup with
     operation mul, then multiply the image of dj under fn *)
  let fold_commutative_semigroup mul fn dj =
    let q = choose dj in        (* dj must have at least one disjunct *)
    let qs = remove q dj in
    fold (fun r -> mul (fn r)) qs (fn q)

  let may_allocs s =
    fold (fun sh es -> Exps.union (Sh.may_allocs sh) es) s Exps.empty

end

(* *-conjunctions of disjunctions of symbolic heaps *)
and Djs : sig

  include Set.S with type elt := Dj.t
  val fold_semiring : ('z->'z->'z) -> ('z->'z->'z) -> (Sh.t->'z) -> t -> 'z->'z

end = struct
  include Set.Make(Dj)

(*   let map fn s = *)
(*     L.incf 0 "( Djs.map" ; (fun _ -> L.decf 0 ") Djs.map") <& *)
(*     map fn s *)

  (* if the image of Dj.t under fn is a semiring with operations add and mul,
     then sum the products of the image of each disjunct under fn *)
  let fold_semiring add mul fn =
    fold (fun dj -> add (Dj.fold_commutative_semigroup mul fn dj))

end



(*F===========================================================================
                                 subFormulas
  ============================================================================*)

and F : (sig
  include TERM with type t = Sh_t.f
  val fv : ?include_cng_rels:bool -> t -> Vars.t
  val may_allocs : t -> Exps.t
  val fmtsp : Sh.t -> Var.fxt -> t formatter
end) = struct

  open Sh_t

  type t = f

  let fv ?(include_cng_rels=true) = function
    | Pt(x) -> Pt.fv x
    | Ls(x) -> Ls.fv x
    | Dj(x) -> Dj.fv ~include_cng_rels x

  let may_allocs = function
    | Pt(x) -> Pt.may_allocs x
    | Ls(x) -> Exps.of_list (Ls.may_allocs x)
    | Dj(x) -> Dj.may_allocs x

  let map_exps fn = function
    | Pt(x) -> Pt(Pt.map_exps fn x)
    | Ls(x) -> Ls(Ls.map_exps fn x)
    | Dj(x) -> Dj(Dj.map_exps fn x)

  let fold_exps fn a z =
    match a with
    | Pt(x) -> Pt.fold_exps fn x z
    | Ls(x) -> Ls.fold_exps fn x z
    | Dj(x) -> Dj.fold_exps fn x z

  let compare x y =
    match x, y with
    | Pt(x), Pt(y) -> Pt.compare x y
    | Ls(x), Ls(y) -> Ls.compare x y
    | Dj(x), Dj(y) -> Dj.compare x y
    | (Pt _ | Ls _ | Dj _), _ -> Pervasives.compare x y

  let equal x y =
    match x, y with
    | Pt(x), Pt(y) -> Pt.equal x y
    | Ls(x), Ls(y) -> Ls.equal x y
    | Dj(x), Dj(y) -> Dj.equal x y
    | (Pt _ | Ls _ | Dj _), _ -> false

  let fmtsp sup fxt ff = function
    | Pt(x) -> Pt.fmtp fxt ff x
    | Ls(x) -> Ls.fmtp fxt ff x
    | Dj(x) -> Dj.fmtsp sup fxt ff x

  let fmtp fxt = fmtsp (Sh.emp()) fxt

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmt_caml ff = function
    | Pt(x) -> Pt.fmt_caml ff x
    | Ls(x) -> Ls.fmt_caml ff x
    | Dj(x) -> Dj.fmt_caml ff x

end


and Fs : sig

  type t = { p: Pts.t; l: Lss.t; d: Djs.t }

  val empty : t
  val is_empty : t -> bool
  val is_empty_djs : t -> bool
  val is_empty_pts_lss : t -> bool
  val singleton : F.t -> t
  val add : F.t -> t -> t
  val remove : F.t -> t -> t
  val union : t -> t -> t
(*   val of_list : F.t list -> t *)
  val to_list : t -> F.t list
  val fold : (F.t -> 'z -> 'z) -> t -> 'z -> 'z
  val clear_pts : t -> t
  val clear_lss : t -> t
  val clear_djs : t -> t
  val only_djs : t -> t
  val fold_pts : (Pt.t -> 'z -> 'z) -> t -> 'z -> 'z
  val fold_lss : (Ls.t -> 'z -> 'z) -> t -> 'z -> 'z
  val fold_djs : (Dj.t -> 'z -> 'z) -> t -> 'z -> 'z
  val exists_djs : (Dj.t -> bool) -> t -> bool
  val forall_djs : (Dj.t -> bool) -> t -> bool
  val trytake_djs : (Dj.t -> bool) -> t -> Dj.t option
(*   val extract_djs : t -> Dj.t * t *)
  val tryextract_djs : t -> (Dj.t * t) option
  val map_djs : (Dj.t -> Dj.t) -> t -> t
  val map_fold_djs : (Dj.t * 'z -> Dj.t * 'z) -> t * 'z -> t * 'z
  val extract_all_djs : t -> t * t
  val partition : (F.t -> bool) -> t -> t * t
  val map : (F.t -> F.t) -> t -> t
(*   val map_filter : (F.t -> F.t option) -> t -> t *)
  val fold_filter : (F.t -> 'z -> 'z option) -> t -> 'z -> t * 'z
  val find : E.t -> t -> F.t
  val tryfind : E.t -> t -> F.t option
  val may_allocs : t -> Exps.t
  val diff_inter_diff : t -> t -> t * t * t
(*   val fold_exps : (E.t -> 'z -> 'z) -> t -> 'z -> 'z *)
  val equal : t -> t -> bool
  val compare : t -> t -> int
(*   val mem : F.t -> t -> bool *)

end = struct

  (* Note: Revise this implementation to include a (partial?) order on f's
     for the purpose of expressing quantifier guarding, changing all the
     operations that enumerate f's to obey the order, and adding operations to
     dynamically change the order. *)

  open Sh_t

  type t = { p: Pts.t; l: Lss.t; d: Djs.t }

  let empty = { p= Pts.empty; l= Lss.empty; d= Djs.empty }

  let is_empty {p; l; d} =
    Pts.is_empty p && Lss.is_empty l && Djs.is_empty d

  let is_empty_pts_lss {p; l} =
    Pts.is_empty p && Lss.is_empty l

  let singleton = function
    | Pt(pt) -> {empty with p= Pts.singleton pt}
    | Ls(ls) -> {empty with l= Lss.singleton ls}
    | Dj(dj) -> {empty with d= Djs.singleton dj}

  let union x y = {
    p= Pts.union x.p y.p;
    l= Lss.union x.l y.l;
    d= Djs.union x.d y.d;
  }

  let add x fs =
    match x with
    | Pt(pt) -> {fs with p= Pts.add pt fs.p}
    | Ls(ls) -> {fs with l= Lss.add ls fs.l}
    | Dj(dj) -> {fs with d= Djs.add dj fs.d}

  let remove x fs =
    match x with
    | Pt(pt) -> {fs with p= Pts.remove pt fs.p}
    | Ls(ls) -> {fs with l= Lss.remove ls fs.l}
    | Dj(dj) -> {fs with d= Djs.remove dj fs.d}

(*   let mem x fs = *)
(*     match x with *)
(*     | Pt(pt) -> Pts.mem pt fs.p *)
(*     | Ls(ls) -> Lss.mem ls fs.l *)
(*     | Dj(dj) -> Djs.mem dj fs.d *)

  let fold fn {p; l; d} z =
     Djs.fold (fun dj z -> fn (Dj(dj)) z) d
    (Lss.fold (fun ls z -> fn (Ls(ls)) z) l
    (Pts.fold (fun pt z -> fn (Pt(pt)) z) p z))

  let map fn x =
    fold (fun f z -> add (fn f) z) empty x

(*   let fold_exps fn {p; l; d} z = *)
(*      Djs.fold (fun dj z -> Dj.fold_exps fn dj z) d *)
(*     (Lss.fold (fun ls z -> Ls.fold_exps fn ls z) l *)
(*     (Pts.fold (fun pt z -> Pt.fold_exps fn pt z) p z)) *)

  let clear_pts fs = {fs with p= empty.p}
  let clear_lss fs = {fs with l= empty.l}
  let clear_djs fs = {fs with d= empty.d}
  let only_djs fs = {fs with p= empty.p; l= empty.l}

  let fold_pts fn fs z = Pts.fold fn fs.p z
  let fold_lss fn fs z = Lss.fold fn fs.l z
  let fold_djs fn fs z = Djs.fold fn fs.d z

  let exists_djs fn fs = Djs.exists fn fs.d
  let forall_djs fn fs = Djs.for_all fn fs.d
  let trytake_djs fn fs = Djs.trytake fn fs.d
(*   let extract_djs fs = let dj,d = Djs.extract fs.d in (dj, {fs with d}) *)
  let tryextract_djs fs = match Djs.tryextract fs.d with Some(dj,d) -> Some(dj, {fs with d}) | None -> None
  let is_empty_djs fs = Djs.is_empty fs.d

  let map_djs fn fs = {fs with d= Djs.map fn fs.d}

  let extract_all_djs fs = ({fs with d= empty.d}, {empty with d= fs.d})

  let map_fold_djs fn (fs, z) =
    let d, z = Djs.map_fold fn (fs.d, z) in
    ({fs with d}, z)

(*   let fold_filter fn xs z = *)
(*     let rec loop rys xs z = *)
(*       match xs with *)
(*       | [] -> (List.rev rys, z) *)
(*       | x :: xs -> *)
(*        match fn x z with *)
(*        | None -> loop (x :: rys) xs z *)
(*        | Some(z') -> loop rys xs z' *)
(*     in *)
(*     loop [] xs z *)

  let partition fn fs =
    fold (fun f (ts,fs) ->
      if fn f then (add f ts, fs) else (ts, add f fs)
    ) fs (empty, empty)

(*   let map_filter fn fs = *)
(*     fold (fun f fs -> *)
(*       match fn f with *)
(*       | None -> fs *)
(*       | Some(f') -> add f' fs *)
(*     ) fs empty *)

  let fold_filter fn fs z =
    fold (fun f (fs, z) ->
      match fn f z with
      | None -> (fs, z)
      | Some(z') -> (remove f fs, z')
    ) fs (fs, z)

(*   let of_list fl = List.fold add fl empty *)
  let to_list fs = fold List.cons fs []

  let find e fs =
    try Pt(Pts.find e fs.p) with Not_found -> Ls(Lss.find e fs.l)

  let tryfind e fs =
    try Some(find e fs) with Not_found -> None


  let rec may_allocs fs =
    Djs.fold (fun dj es ->
      Dj.fold (fun dt es ->
        Exps.union (may_allocs dt.sfs) es
      ) dj es
    ) fs.d (Exps.union (Pts.may_allocs fs.p) (Lss.may_allocs fs.l))

  let diff_inter_diff q r =
    let q_r_pts, int_pts, r_q_pts = Pts.diff_inter_diff q.p r.p
    and q_r_lss, int_lss, r_q_lss = Lss.diff_inter_diff q.l r.l
    and q_r_djs, int_djs, r_q_djs = Djs.diff_inter_diff q.d r.d
    in
    let q_r = {p= q_r_pts; l= q_r_lss; d= q_r_djs}
    and int = {p= int_pts; l= int_lss; d= int_djs}
    and r_q = {p= r_q_pts; l= r_q_lss; d= r_q_djs}
    in
    (q_r, int, r_q)

  let equal p q =
       Pts.equal p.p q.p
    && Lss.equal p.l q.l
    && Djs.equal p.d q.d

  let compare p q =
    let o = Pts.compare p.p q.p in if o <> 0 then o else
    let o = Lss.compare p.l q.l in if o <> 0 then o else
            Djs.compare p.d q.d

end



(*Sh==========================================================================
                   (quantifier-free) Symbolic Heap formulas
  ============================================================================*)

and Sh_t : sig

  type f = Pt of Pt.t | Ls of Ls.t | Dj of Dj.t

  type t = {
    lbl: int;                   (** label identifies unique point in *-v tree *)
    fcr: CngRel.t;              (** full congruence relation                  *)
    tcr: CngRel.t;              (** trimmed congruence relation               *)
    pas: Exps.t;                (** pure subformulas                          *)
    sfs: Fs.t;                  (** spatial subformulas                       *)
    jnk: bool;                  (** arbitrary "junk" heap subformula          *)
  }

  type vs_t = Vars.t * t

end = Sh_t (* Contains only type definitions, so can be vacuously defined. *)


and Sh : sig
  include
    (QUANTIFIER_FREE_SYMBOLIC_HEAP
     with type t = Sh_t.t and type xsh := XSh.t
      and type pt := Pt.t and type ls := Ls.t and type dj := Dj.t)

  (* internal operations *)
  val fmtsp : t -> Var.fxt -> t formatter
(*   val is_disjunction : t -> bool *)
  val renaming : Vars.t -> Vars.t * S.t * S.t
(*   val rep_cmp : Exps.t -> Vars.t -> E.t -> E.t -> int *)

  (* shadow ground values that cannot be exported from recursive module *)
  val emp : unit -> t
  val tt : unit -> t
  val ff : unit -> t

end = struct

  include Sh_t


  (* Base Constructors ====================================================== *)

  let emp = {
    lbl= 0;
    fcr= CngRel.empty;
    tcr= CngRel.empty;
    pas= Exps.empty;
    sfs= Fs.empty;
    jnk= false;
  }

  let tt = {emp with jnk= true}

  let ff = {tt with pas= Exps.singleton E.ff; jnk= false}

  let dj_empty = Dj.of_list []

  let rec clear_cng sh =
    let sfs = Fs.map_djs (fun dj -> Dj.map clear_cng dj) sh.sfs in
    {sh with fcr= emp.fcr; tcr= emp.tcr; sfs}



  (* Folds ================================================================== *)

  let rec fold fn sh z =
    let z =
      Fs.fold_djs (fun dj z ->
        Dj.fold (fun dt z ->
          fold fn dt z
        ) dj z
      ) sh.sfs z in
    fn sh z


  let rec fold_sp dn up sh sa pa =
(*     L.incf 0 "( SH.fold_sp: %a" Sh.fmt sh ; *)
(*     L.decf 0 ") SH.fold_sp%a" (fun _ _ -> ()) <& *)
    let sa = dn sh sa in
    let pa =
      Fs.fold_djs (fun dj pa ->
        Dj.fold (fun dt pa ->
          fold_sp dn up dt sa pa
        ) dj pa
      ) sh.sfs pa in
    up sh sa pa


  let fold_fs fn sh z = fold (fun {sfs} z -> Fs.fold fn sfs z) sh z

  let fold_exps fn sh z =
    fold_fs (fun f z -> F.fold_exps fn f z) sh
      (Exps.fold (fun pa z -> E.fold fn pa z) sh.pas z)


  (* Note: Change to split cases based on list lengths? *)
  let fold_dnf ?(dnf=true) map red sh ca da =
    if not dnf then
      fold_sp map (fun _dt (c,_d) d -> red (c,d)) sh (ca,da) da
    else
(*     L.incf 4 "( SH.fold_dnf: %a" Sh.fmt sh ; *)
(*     L.decf 4 ") SH.fold_dnf%a" (fun _ _ -> ()) <& *)
    (* - pending_splits is a disjunction list of case splits which have yet
         to be made (none of their disjuncts are in cube_prefix)
       - dt is a disjunct in sh which is not represented by cube_prefix or
         pending_splits
       - ca is the result of applying map to the *-conjuncts in cube_prefix *)
    let rec add_disjunct pending_splits dt (ca,da) =
(*       L.incf 4 "( add_disjunct: @[[@[%a@]]@ %a@]" (List.fmt ";@ " Dj.fmt) pending_splits Sh.fmt dt ; *)
(*       L.decf 4 ") add_disjunct%a" (fun _ _ -> ()) <& *)
      split_case
        (Fs.fold_djs List.cons dt.sfs pending_splits)
        (map dt (ca,da))
    and split_case pending_splits (ca,da) =
(*       L.incf 4 "( split_case: [@[%a@]]" (List.fmt ";@ " Dj.fmt) pending_splits ; *)
(*       L.decf 4 ") split_case%a" (fun _ _ -> ()) <& *)
      match pending_splits with
      | dj :: pending_splits ->
          Dj.fold (fun dt da -> add_disjunct pending_splits dt (ca,da)) dj da
      | [] ->
          red (ca,da)
    in
    add_disjunct [] sh (ca,da)

(*   let fold_dnf map red sh ca da = debug_wrap5 Config.vSH 4 fold_dnf map red sh ca da *)



  (* Syntactic Operations =================================================== *)

  let equal p q =
    p == q ||
    (p.jnk = q.jnk && Exps.equal p.pas q.pas && Fs.equal p.sfs q.sfs)

  let compare p q =
    if p == q then 0 else
    let o = Pervasives.compare p.jnk q.jnk in if o <> 0 then o else
    let o = Exps.compare p.pas q.pas in if o <> 0 then o else
            Fs.compare p.sfs q.sfs


  let inconsistent q = Exps.mem E.ff q.pas


  (** syntactic *-conjunction *)
  let syntactic_star p q =
    { q with
      pas= Exps.union p.pas q.pas;
      sfs= Fs.union p.sfs q.sfs;
      jnk= p.jnk || q.jnk;
    }


  let partition fn q =
    let ts, fs = Fs.partition fn q.sfs in
    ({ q with sfs= ts }, { q with sfs= fs })


  (** syntactic intersection and differences.
      if [diff_inter_diff q r] = [(q_i,i,r_i)] then [union q r] =
      [union q_i (union i r_i)] *)
  let diff_inter_diff ?(pas=true) q r =
    let q_r_pas, int_pas, r_q_pas = if pas then Exps.diff_inter_diff q.pas r.pas else (q.pas, emp.pas, r.pas)
    and q_r_fs, int_fs, r_q_fs = Fs.diff_inter_diff q.sfs r.sfs
    and q_r_jnk, int_jnk, r_q_jnk = (q.jnk && not r.jnk, q.jnk && r.jnk, r.jnk && not q.jnk)
    in
    let i = emp in
    let q_r = {q with pas= q_r_pas; sfs= q_r_fs; jnk= q_r_jnk}
    and int = {i with pas= int_pas; sfs= int_fs; jnk= int_jnk}
    and r_q = {r with pas= r_q_pas; sfs= r_q_fs; jnk= r_q_jnk}
    in
    (q_r, int, r_q)

  let deep_diff_inter_diff sh0 sh1 =
    let m0 = fold (fun dt m0 -> IntMap.add dt.lbl dt m0) sh0 IntMap.empty in
    let m1 = fold (fun dt m1 -> IntMap.add dt.lbl dt m1) sh1 IntMap.empty in
    let m =
      IntMap.fold (fun lbl dt0 m ->
        let dt1 = try IntMap.find lbl m1 with Not_found -> emp in
        let dt0 = {dt0 with sfs= Fs.clear_djs dt0.sfs} in
        let dt1 = {dt1 with sfs= Fs.clear_djs dt1.sfs} in
        let did = diff_inter_diff dt0 dt1 in
        IntMap.add lbl did m
      ) m0 IntMap.empty in
    let replace_stem prj dt =
      let stem = try prj (IntMap.find dt.lbl m) with Not_found -> emp in
      let sfs = Fs.fold_djs (fun dj sh -> Fs.add (Dj(dj)) sh) dt.sfs stem.sfs in
      {stem with sfs}
    in
    let o = Sh.map (fun dt -> replace_stem fst3 dt) sh0 in
    let i = Sh.map (fun dt -> replace_stem snd3 dt) sh0 in
    let n = Sh.map (fun dt -> replace_stem thd3 dt) sh1 in
    (o,i,n)


  let find e q = Fs.find e q.sfs

  let tryfind e q = Fs.tryfind e q.sfs

  let is_empty sh =
    let rec loop sh =
         not sh.jnk
      && Fs.is_empty_pts_lss sh.sfs
      && Fs.forall_djs (Dj.for_all loop) sh.sfs
    in
    loop sh

  let is_pure sh =
    let rec loop sh =
         Fs.is_empty_pts_lss sh.sfs
      && Fs.forall_djs (Dj.for_all loop) sh.sfs
    in
    sh.jnk && loop sh
(*
  let is_disjunction sh =
       E.equal E.tt sh.bex
    && is_empty { sh with djs= Djs.empty }
    && Djs.cardinal sh.djs = 1
*)

  (* Very crude [sizeof(q)]: add one for each pt, ls and dj. *)
  (* Should use [max] for Djs "alternatives", though + is sound. *)
(*   let sizeof sh = *)
(*     let incr _ x = x + 1 in *)
(*     let incr' = { v_b= (fun _ x -> x); v_dj= incr; v_pt= incr; v_ls= incr } in *)
(*     fold incr' sh 0 *)
  let sizeof sh =
    let rec loop sh n =
      Fs.fold (fun f n ->
        match f with
        | Dj(dj) -> Dj.fold loop dj n
        | _ -> succ n
      ) sh.sfs n
    in loop sh 0


  let may_allocs_stem q =
    Fs.fold (fun f mas ->
      match f with
      | Pt({Pt.loc}) -> Exps.add loc mas
      | Ls(ls) -> List.fold Exps.add (Ls.may_allocs ls) mas
      | Dj _ -> mas
    ) q.sfs Exps.empty

  let may_allocs q =
    Fs.may_allocs q.sfs

  let fv ?(include_cng_rels=true) sh =
    fold_fs (fun f vs -> Vars.union (F.fv ~include_cng_rels f) vs) sh
      (Exps.fold (fun pa vs -> Vars.union (E.fv pa) vs) sh.pas
        (if include_cng_rels then
           Exps.fold (fun e vs -> Vars.union (E.fv e) vs)
             (CngRel.carrier sh.fcr) Vars.empty
         else
           Vars.empty))


  let count_occurrences sh =
    let occ_exp e occ m =
(*       L.printf 0 "occ_exp: %a" E.fmt e; *)
      let dn e occ =
        match E.desc e with
        | E.Var _ -> occ
        | _ -> Some(e)
      in
      let up e occ m =
        match E.desc e with
        | E.Var(x) ->
            (match occ with
            | Some(sup) when E.equal sup e -> m (* not proper subexp *)
            | _ -> VarMap.add x (try VarMap.find x m + 1 with Not_found -> 1) m
            )
        | _ -> m
      in
      E.fold_sp dn up e occ m
    in
    Exps.fold (fun e m ->
      occ_exp e (Some(e)) m
    ) sh.pas
    (fold_fs (fun f m ->
(*       L.printf 0 "occ: %a" F.fmt f; *)
      match f with
      | Pt({Pt.loc; cnt}) ->
          (* Note: Is ignoring off correct? *)
          occ_exp loc None (Option.fold (fun e m -> occ_exp e None m) cnt m)
      | Ls({Ls.len; arg}) ->
          occ_exp len None
            (Args.fold_links (fun (a,d) m ->
              occ_exp a None (occ_exp d None m)
             ) arg m)
      | Dj(_) ->
          m
    ) sh
    VarMap.empty)



  (* Formatting ============================================================= *)

  let mk_fxt (xs,sh) =
    let exists = xs in
    let uniques =
      Vars.inter exists
        (VarMap.fold (fun v n uniques ->
           if n <= 1 then Vars.add v uniques else uniques
         ) (count_occurrences sh) Vars.empty) in
    (exists, uniques)


  let fmtsp sup fxt ff q =
    let tfclss =
      CngRel.fold_classes (fun rep fcls clss ->
        let tcls = CngRel.class_of q.tcr rep in
        if !Config.vSH > 3 then (rep, Exps.remove rep tcls, Exps.remove rep fcls) :: clss else
        let fcls =
          let partns =
            List.classify (fun d e ->
              match E.desc d, E.desc e with
              | E.App(f,_), E.App(g,_) -> E.equal f g
              | _ -> false
            ) (Exps.to_list fcls) in
          List.fold (fun partn fcls ->
            if List.mem rep partn then
              Exps.add rep fcls
            else
              Exps.add (List.hd partn) fcls
          ) partns Exps.empty in
        let tcls, fcls = Exps.inter_diff fcls tcls in
        let tcls = Exps.diff tcls (CngRel.class_of sup.tcr rep) in
        let fcls = Exps.diff fcls (CngRel.class_of sup.fcr rep) in
        let fcls =
          Exps.fold (fun e fcls ->
            Exps.diff fcls (CngRel.class_of sup.fcr e)
          ) tcls fcls in
        let tcls = Exps.remove rep tcls in
        if Exps.is_empty fcls && Exps.is_empty tcls then clss else
        (rep, tcls, fcls) :: clss
      ) q.fcr []
    in
    let fmt_cngrels ff =
      Format.fprintf ff "@[%a@ %a@]@\n" CngRel.fmt q.fcr CngRel.fmt q.tcr
    in
    let fmt_cng ff =
      let fmt_cls ff cls =
        List.fmt " =@ " (Exp.fmtp fxt) ff cls
      in
      let fmt_fcls ff fcls =
        if not (Exps.is_empty fcls) then
          Format.fprintf ff " =@ %a" fmt_cls (Exps.to_list fcls)
      in
      let fmt_tfcls ff (rep,tcls,fcls) =
        Format.fprintf ff "@[<hov 1>((%a)%a)@]"
          fmt_cls (rep :: (Exps.to_list tcls)) fmt_fcls fcls
      in
      Format.fprintf ff "@[%a@]" (List.fmt " *@ " fmt_tfcls) (List.rev tfclss)
    in
    let rep e = Sh.Pf.normalize q e in
    let pas =
      if !Config.vSH > 3 then q.pas else
      Exps.fold (fun pa pas ->
        let pa = E.map rep pa in
        if E.equal E.tt pa then pas else Exps.add pa pas
      ) q.pas Exps.empty
    and sfs =
      if !Config.vSH > 3 then q.sfs else
      try
        Fs.map (fun f ->
          match f with
          | Dj _ -> f
          | _ -> F.map_exps rep f
        ) q.sfs
      with Invalid_argument _ ->
        q.sfs (* normalize might violate uniqueness of must allocs *)
    in
    (* group points-tos for the same base location together, and remove them from sfs *)
    let objs, sfs =
      if !Config.vSH > 3 then (ExpPtsMap.empty, sfs) else
      Fs.fold_pts (fun ({Pt.loc; off} as pt) (objs, sfs) ->
        match O.desc off with
        | O.Var _ ->
            (objs, sfs)
        | O.Path(_,fs) ->
            let base = E.mkSubs loc fs in
            let q = Sh.Pf.extend Vars.empty q base in
            let base' = Sh.Pf.normalize q base in
            (ExpPtsMap.add base' pt objs, Fs.remove (Pt(pt)) sfs)
      ) sfs (ExpPtsMap.empty, sfs)
    in
    let objs =
      ExpPtsMap.fold_keys (fun base pts objs ->
        let ty_fs_cnts =
          Pts.fold (fun {Pt.off; cnt} ty_fs_cnts ->
            match O.desc off with
            | O.Path(ty,fs) ->
                let rec loop ty fs =
                  let find ty fs =
                    let rec find_ gs =
                      match gs with
                      | g :: gs ->
                          (try
                            loop ty (g :: fs)
                          with Not_found ->
                            find_ gs
                          )
                      | [] ->
                          raise Not_found
                    in
                    find_ (Typ.fst_flds ty)
                  in
                  match fs with
                  | (f :: fs) as ffs ->
                      (match Typ.of_fld ty f with
                      | Some(fty) ->
                          (match fs with
                          | [] ->
                              (match Typ.fst_flds fty with
                              | [] ->
                                  ffs
                              | g :: _ ->
                                  f :: (loop fty [g])
                              )
                          | _ ->
                              f :: (loop fty fs)
                          )
                      | None ->
                          find ty ffs
                      )
                  | [] ->
                      []
                in
                let fs' = List.rev fs in
                let fs' =
                  try loop ty fs'
                  with Not_found ->
                    let module L = (val Log.std Config.vTyp : Log.LOG) in
                    L.shift_verb (!Config.vTyp - 2) (fun () ->
                      L.printf 0 "@[no path @[%a@]@ in type@ %a@]" (List.fmt ";@ " Fld.fmt) fs' Typ.fmt ty );
                    fs'
                in
                (ty, fs', cnt) :: ty_fs_cnts
            | _ -> failwith "unexpected offset"
          ) pts []
        in
        let ty_fs_cnts = List.sort (fun (_,n,_) (_,o,_) -> List.compare Fld.compare n o) ty_fs_cnts
        in
        (base, ty_fs_cnts) :: objs
      ) objs []
    in
    let fmt_objs ff =
      let fmt_obj ff (base, ty_fs_cnts) =
        let rec fmt_ptl ff ty_fs_cnts =
          match ty_fs_cnts with
          | [(ty,fs,cnt)] ->
              let off = O.mkPath ty (List.rev fs) in
              Format.fprintf ff "@[%a:@ %a@]" (O.fmtp fxt) off (Option.fmt "-" (E.fmtp fxt)) cnt
          | (_,f::_,_) :: _ ->
              Format.fprintf ff "@[%a.@;<0 2>@[%a@]@]" Fld.fmt f fmt_ptll
                (List.rev_map (fun (ty,fs,cnt) ->
                   match fs with
                   | f :: fs -> (Option.get (Typ.of_fld ty f), fs, cnt)
                   | [] -> (ty, fs, cnt)
                 ) ty_fs_cnts)
          | _ ->
              assert false
        and fmt_ptll ff ty_fs_cnts =
          Format.fprintf ff "[@[%a@]]" (List.fmt ";@ " fmt_ptl)
            (List.classify (fun (_,n,_) (_,o,_) ->
               match n, o with
               | f::_, g::_ -> Fld.equal f g
               | _ -> false
             ) ty_fs_cnts)
        in
        Format.fprintf ff "@[<hov 2>(%a ->@ @[%a@])@]" (E.fmtp fxt) base fmt_ptll ty_fs_cnts
      in
      Format.fprintf ff "@[%a@]" (List.fmt " *@ " fmt_obj) (List.rev objs)
    in
    let fmt_pas ff =
      List.fmt " *@ " (E.fmtp fxt) ff (Exps.to_list pas)
    in
    let fmt_sfs ff =
      List.fmt " *@ " (F.fmtsp q fxt) ff (List.rev (Fs.to_list sfs))
    in
    let fmt_jnk ff =
      Format.fprintf ff "true"
    in
    let fmt_emp ff =
      Format.fprintf ff "emp"
    in
    let fmtl =
      let fmtl =
        ( (if !Config.vSH > 30 then [fmt_cngrels] else [])
        @ (if tfclss = [] then [] else [fmt_cng])
        @ (if Exps.is_empty pas then [] else [fmt_pas])
        @ (if objs = [] then [] else [fmt_objs])
        @ (if Fs.is_empty sfs then [] else [fmt_sfs])
        @ (if q.jnk then [fmt_jnk] else []) )
      in if fmtl = [] then [fmt_emp] else fmtl
    in
    Format.fprintf ff "@[<hov 2>%i[ @[%a@] ]@]" q.lbl (List.fmtt " *@ ") fmtl

  let fmtp fxt ff q =
    let q = if !Config.vSH > 0 then q else snd (Sh.normalize (Vars.empty,q)) in
    fmtsp emp fxt ff q

  let fmt ff = fmtp (Vars.empty,Vars.empty) ff

  let fmtsp_xs super ((ys,_) as fxt) ff (xs,sh) =
    let xs, sh = if !Config.vSH > 0 then (xs,sh) else Sh.normalize (xs,sh) in
    let xs = Vars.union xs ys in
    let xs = if !Config.vSH > 0 then xs else Vars.inter xs (fv sh) in
    let fxt = if !Config.vSH > 0 then (Vars.empty, Vars.empty) else fxt in
    let uniques,_ = fxt in
    Format.fprintf ff "@[{ %a@[<hov 2>%a@] }@]"
      (Vars.fmtp_embrace "@[<hov 2>? " " .@]@ " fxt) (Vars.diff xs uniques)
      (fmtsp super fxt) sh

  let fmtp_xs fxt ff = fmtsp_xs emp fxt ff
  let fmt_xs ff = fmtp_xs (Vars.empty,Vars.empty) ff

  let fmt_did_xs ((xs,sh), (xs',sh')) =
    let o_sh, i_sh, n_sh = deep_diff_inter_diff sh sh' in
    let o_xs, i_xs, n_xs = Vars.diff_inter_diff xs xs' in
    let fxt  = (xs , Vars.empty) in
    let fxt' = (xs', Vars.empty) in
    ( (fun ff -> fmtsp_xs i_sh fxt  ff (o_xs, o_sh))
    , (fun ff -> fmtsp_xs i_sh fxt' ff (i_xs, i_sh))
    , (fun ff -> fmtsp_xs i_sh fxt' ff (n_xs, n_sh))
    )

  let fmt_did (sh, sh') = fmt_did_xs ((Vars.empty, sh), (Vars.empty, sh'))


  let fmt_caml _ = failwith "SH.fmt_caml unimplemented"
(*   let fmt_caml ff q = *)
(*     Format.fprintf ff *)
(*       "@[<hov 2>(SH.PtS.star@ [@[%a@]]@]@ \ *)
(*        @[<hov 2>(SH.LsS.star@ [@[%a@]]@]@ \ *)
(*        @[<hov 2>(SH.DjS.star@ [@[%a@]]@]@;<1 1>\ *)
(*               %s))))" *)
(*       (List.fmt ";@ " Pt.fmt_caml) (Pts.to_list q.sfs.Fs.p) *)
(*       (List.fmt ";@ " Ls.fmt_caml) (Lss.to_list q.sfs.Fs.l) *)
(*       (List.fmt ";@ " Dj.fmt_caml) (Djs.to_list q.sfs.Fs.d) *)
(*       (if q.jnk then "SH.tt" else "SH.emp") *)



  (* Labels ================================================================= *)

  let lbl q = q.lbl

  let labels q =
    fold_sp
      (fun _ () -> ())
      (fun q () ls -> IntSet.add q.lbl ls)
      q () IntSet.empty


  let relabel miss free q =
    assert( IntSet.equal free (labels q) )
    ;
    let rec map_fold fn sh z =
      let sfs, z =
        Fs.map_fold_djs (fun dj_z ->
          Dj.map_fold (fun (dt, z) ->
            map_fold fn dt z
          ) dj_z
        ) (sh.sfs, z) in
      fn {sh with sfs} z
    in
    let fresh = IntSet.empty
    and max = max (IntSet.max_elt miss) (IntSet.max_elt free)
    in
    let q, (fresh, _max) =
      map_fold (fun q (fresh, max) ->
        if IntSet.mem q.lbl miss then
          let lbl = max + 1 in
          ({q with lbl}, (IntSet.add lbl fresh, lbl))
        else
          (q, (fresh, max))
      ) q (fresh, max)
    in
    (q, fresh)

  let relabel_extend (sh, lbls) =
    let sh_lbls = labels sh in
    let sh, new_lbls = relabel lbls sh_lbls sh in
    let lbls = IntSet.union (IntSet.union new_lbls sh_lbls) lbls in
    (sh, lbls)

  let set_lbl lbl q =
    let q,_ = relabel (IntSet.singleton lbl) (labels q) q in
    {q with lbl}


  (* Syntactic Normalization ================================================ *)

  (* normalization uses an accumulator of type *)
  type u = { p: E.t list; s: F.t list; j: bool; }

  let uemp = { p= []; s= []; j= false; }

  let u_of q = { p= Exps.to_list q.pas; s= Fs.to_list q.sfs; j= q.jnk; }

  let u_fmt ff u =
    Format.fprintf ff "@[%a@,%a@,%s@]"
      (List.fmt " *@ " E.fmt) u.p
      (List.fmt " *@ " F.fmt) u.s
      (if u.j then " * true" else "")


  type cxt = { sub: E.t -> E.t; lss: LsB.t; locs: Exps.t }

  let id_cxt = { sub= (fun e -> e); lss= LsB.empty; locs= Exps.empty }

  (** [add_* s a (lbls0,q0,u0)] returns a tuple [(lbls1,q1,u1)] such that [q1]
      * [u1] iff [a] * [q0] * [u0].  Applies [s] to freshly constructed
      subformulas added to [(q1,u1)], and relabels such formulas to avoid
      [lbls0] (returning extended set of labels [lbls1]).  Progress is made
      (at least) in the sense that [a] is not in [u1]. *)

  let add_jnk (lbls, q, u, n) =
    assert(true$> L.printf 7 "add jnk" );
    (lbls, {q with jnk= true}, u, n)


  let add_empty_eqs {sub} ls z =
    List.map_append sub (Ls.empty_eqs ls) z


  let rec add_pa cxt pa (lbls, q, u, n) =
    assert(
      L.printf 7 "add pa: %a" E.fmt pa ;
      IntSet.subset (labels q) lbls
      || failwithf "missing labels: {@[%a@]}"
           (List.fmt ",@ " Format.pp_print_int)
             (IntSet.to_list (IntSet.diff (labels q) lbls)) );
    match E.desc pa with
    | _ when Config.sh_simplify && E.equal E.tt pa ->
        (lbls, q, u, n)

    | _ when Config.sh_simplify && E.equal E.ff pa ->
        (lbls, ff, uemp, false)

    | E.OpN(E.And,es) ->
        let p = Array.fold_right (fun e ufs -> E.name e :: ufs) es u.p in
        (lbls, q, {u with p}, n)

    | E.OpN(E.Or,es) ->
        let dj, n =
          Array.fold_right (fun e (dts, n) ->
            let dt, n' = norm cxt (emp, {uemp with p= [E.name e]}) in
            (Dj.add dt dts, n || n')
          ) es (Dj.empty, n) in
        let s = [Dj(dj)] in
        (lbls, q, {u with s}, n)

    | _ ->
        let pas = Exps.add pa q.pas in
        (lbls, {q with pas}, u, n)


  and add_sf ({lss; locs} as cxt) f (lbls, q, u, n) =
    assert (
      L.printf 7 "add f: %a" F.fmt f ;
      IntSet.subset (labels q) lbls
      || failwithf "missing labels: {@[%a@]}"
           (List.fmt ",@ " Format.pp_print_int) (IntSet.to_list (IntSet.diff (labels q) lbls))
    );
    match f with
    | Pt({Pt.loc} as pt) ->
        if E.equal loc E.nil then (
            (* 0->_  <=>  ff *)
            assert(true$> L.printf 8 "ff pt: %a" Pt.fmt pt );
            (lbls, ff, uemp, false)
        )
        else if Exps.mem loc locs || Pts.memi loc q.sfs.Fs.p then (
            (* E->_ * E->_  <=>  ff *)
            assert(true$> L.printf 8 "must-alloc conflict" );
            (lbls, ff, uemp, false)
        )
        else (match Option.fold List.cons (Lss.tryfind loc q.sfs.Fs.l) (LsB.find loc lss) with
        | [] ->
            assert(true$> L.printf 8 "add pt: %a" Pt.fmt pt );
            let sfs = Fs.add f q.sfs in
            (lbls, {q with sfs}, u, n)
        | lss ->
            (* E->_ * ls(L,K,P,E,B,N)  <=>  K==0 * E==N * P==B *)
            (* E->_ * ls(L,K,P,F,E,N)  <=>  K==0 * F==N * P==E *)
            let sfs, p =
              List.fold (fun ls (sfs, p) ->
                assert(true$> L.printf 8 "ls conflict: %a" Ls.fmt ls );
                let p = add_empty_eqs cxt ls p in
                let sfs = Fs.remove (Ls(ls)) sfs in
                (sfs, p)
              ) lss (q.sfs, u.p) in
            let sfs = Fs.add f sfs in
            (lbls, {q with sfs}, {u with p}, true)
        )
    | Ls({Ls.len} as ls) ->
        let may_allocs_ls = Exps.of_list (Ls.may_allocs ls) in
        if   E.equal E.zero len
             (* ls(L,0,P,F,B,N)  <=>  F==N * P==B *)
          || Exps.mem E.nil may_allocs_ls
             (* ls(L,K,P,0,B,N)  <=>  K==0 * 0==N * P==B *)
             (* ls(L,K,P,F,0,N)  <=>  K==0 * F==N * P==0 *)
          || Exps.intersect locs may_allocs_ls
             (* E->_ * ls(L,K,P,E,B,N)  <=>  K==0 * E==N * P==B *)
             (* E->_ * ls(L,K,P,F,E,N)  <=>  K==0 * F==N * P==E *)
        then (
            assert(true$> L.printf 8 "add empty ls: %a" Ls.fmt ls );
            let p = add_empty_eqs cxt ls u.p in
            (lbls, q, {u with p}, true)
        )
        else (
            let conflicts =
              Exps.fold (fun e conflicts ->
                match Fs.tryfind e q.sfs with
                | Some(Ls(ls)) when not (List.mem ls conflicts) -> ls :: conflicts
                | _ -> conflicts
              ) may_allocs_ls []
            in
            if conflicts = [] then (
                assert(true$> L.printf 8 "add ls: %a" Ls.fmt ls );
                let sfs = Fs.add f q.sfs in
                (lbls, {q with sfs}, u, n)
            )
            else (
                assert(true$> L.printf 8 "@[conflict: @[%a@]@]" (List.fmt ";@ " Ls.fmt) conflicts );
                let add_lss = List.fold (fun ls fs -> Fs.add (Ls(ls)) fs) in
                let rem_lss = List.fold (fun ls fs -> Fs.remove (Ls(ls)) fs) in
                (* Either ls is empty or all conflicting ls's are empty *)
                (* ls(_,J,_,E,_,_) * ls(_,K,_,_,E,_)  <=>  J==0 v K==0 *)
                let ls_empty =
                  let sfs = add_lss conflicts emp.sfs in
                  let p = add_empty_eqs cxt ls uemp.p in
                  fst (norm cxt ({emp with sfs}, {uemp with p})) in
                let conflicts_empty =
                  let sfs = Fs.singleton f in
                  let p = List.fold (add_empty_eqs cxt) conflicts uemp.p in
                  fst (norm cxt ({emp with sfs}, {uemp with p})) in
                let lbls = IntSet.add ls_empty.lbl lbls in
                let lbls = IntSet.add conflicts_empty.lbl lbls in
                let sfs = rem_lss conflicts q.sfs in
                let s = Dj(Dj.of_list [ls_empty; conflicts_empty]) :: u.s in
                (lbls, {q with sfs}, {u with s}, true)
            )
        )
    | Dj(dj) ->
        (* {tt \/ S}  <=>  {tt} *)
        let dj = if not Config.sh_simplify then dj else
          if Dj.exists (fun dt -> equal tt dt) dj
          then Dj.singleton tt
          else dj in
        (* {ff \/ S}  <=>  {S} *)
        let dj = if not Config.sh_simplify then dj else
          Dj.filter (fun dt -> not (inconsistent dt)) dj in
        (* {P \/ R} \/ S  <=>  {P \/ R \/ S} *)
        let dj = if not Config.sh_simplify then dj else
          Dj.fold (fun dt dj ->
            match Fs.tryextract_djs dt.sfs with
            | Some(dt_dj, sfs) when Fs.is_empty sfs ->
                let dt' = {dt with sfs} in
                Dj.fold (fun dt dj ->
                  Dj.add (star [dt'] dt) dj
                ) dt_dj dj
            | _ ->
                Dj.add dt dj
          ) dj Dj.empty in
        let contains_eq es = Exps.exists (fun e -> match E.desc e with E.Eq _ -> true | _ -> false) es in
        match Dj.to_list dj with
        | [] (* when !simplify *) ->
            assert(true$> L.printf 8 "ff dj" );
            (* \/{}  <=>  ff *)
            (lbls, ff, uemp, false)
        | [r] (* when !simplify *) ->
            assert(true$> L.printf 8 "add singleton dj: %a" Dj.fmt dj );
            (* \/{Q}  <=>  Q *)
            let lbls = IntSet.union (labels r) lbls in
            (lbls, star [r] q, u, n || contains_eq r.pas)
        | r::t::tl ->
            assert(true$> L.printf 8 "add dj: %a" Dj.fmt dj );
            (* P * {[R * Q] \/ [R * S]}  <=>  P * R * {Q \/ S} *)
            if Config.sh_hoist_common_subformulas then
              let r_i, i, t_i = diff_inter_diff r t in
              let raised, dj =
                List.fold (fun dt (raised, dj) ->
                  let dt, raised, lower = diff_inter_diff dt raised in
                  ( raised
                  , Dj.add dt (Dj.map (fun dt -> syntactic_star lower dt) dj) )
                ) tl (i, Dj.of_list [r_i; t_i]) in
              let n = n || contains_eq (Exps.diff raised.pas q.pas) in
              (* relabel raised disjunctions if needed to avoid lbls *)
              let lbls, raised =
                let d, lbls = Djs.map_fold (Dj.map_fold relabel_extend) (raised.sfs.Fs.d, lbls) in
                (lbls, {raised with sfs= {raised.sfs with Fs.d}}) in
              (* relabel subformulas of dj if needed to avoid lbls *)
              let dj, lbls =
                Dj.fold (fun dt (dj,lbls) ->
                  let dt, lbls = relabel_extend (dt, lbls) in
                  (Dj.add dt dj, lbls)
                ) dj (dj_empty, lbls) in
              let sfs = Fs.add (Dj(dj)) q.sfs in
              (lbls, star [raised] {q with sfs}, u, n)
            else
              let r_i_e, i, t_i_e = Exps.diff_inter_diff r.pas t.pas in
              let r_i = {r with pas= r_i_e} in
              let t_i = {t with pas= t_i_e} in
              let add_pas e q = {q with pas= Exps.union e q.pas} in
              let raised, dj =
                List.fold (fun dt (raised, dj) ->
                  let dt_pas, raised', lower = Exps.diff_inter_diff dt.pas raised in
                  let dt = {dt with pas= dt_pas} in
                  ( raised'
                  , Dj.add dt (Dj.map (fun dt -> add_pas lower dt) dj) )
                ) tl (i, Dj.of_list [r_i; t_i]) in
              let n = n || contains_eq (Exps.diff raised q.pas) in
              let q = add_pas raised q in
              (* relabel subformulas of dj if needed to avoid lbls *)
              let dj, lbls =
                Dj.fold (fun dt (dj,lbls) ->
                  let dt_lbls = labels dt in
                  let dt, new_lbls = relabel lbls dt_lbls dt in
                  let lbls = IntSet.union (IntSet.union new_lbls dt_lbls) lbls in
                  (Dj.add dt dj, lbls)
                ) dj (dj_empty, lbls) in
              let sfs = Fs.add (Dj(dj)) q.sfs in
              (lbls, {q with sfs}, u, n)
        (* Notes:
           - Define a well-guardedness check that requires the stem
             leading to a subformula to guard the existentials in the
             subformula.
           - Refine this normalization to only hoist subformulas that
             remain well-guarded after hoisting.
           - Prover.distrib_{pt,ls} should no longer be needed, remove
             them.
           - DjS.add no longer needs to violate normalization, remove it.
           - Revise DjS operations to not assume incoming Dj.t's are
             normalized
           - Move normalization done by Dj.add to here.
        *)


  (** Normalization is a fixed-point computation at type [t * u].  [norm
      (q,u)] selects the "first" atom of [u] and calls the appropriate
      [add_*] function on it, yielding a new [t * u] pair.  The argumet
      substitution is applied to subformulas added to the result that do not
      appear in [(q,u)]. *)
  and norm cxt (q,u) =
    let u = {u with p= List.filter (fun e -> not (E.equal E.tt e)) u.p} in
    if u = uemp then (q, false)
    else
    let reset = L.latch() in (fun (q,_) -> assert(true$> L.resetf 50 reset "> norm: %a" fmt q ))
    <&
    let rec loop (lbls, q, u, new_eqs) =
      assert(true$> L.incf 50 "< norm: @[<hv>q: %a@ u: %a@]" fmt q u_fmt u );
      match u with
      | {p= pa :: l} -> loop (add_pa cxt pa (lbls, q, {u with p= l}, new_eqs))
      | {s= sf :: l} -> loop (add_sf cxt sf (lbls, q, {u with s= l}, new_eqs))
      | {j= true   } -> loop (add_jnk       (lbls, q, {u with j= false}, new_eqs))
      | _            -> (q, new_eqs)
    in
    loop (labels q, q, u, false)



  (* Constructors =========================================================== *)

  (** [star ps q] returns the iterated star conjunction of the [ps] and [q],
      preserving the labels and congruence class representatives of [q]. *)
  and star ps q =
    assert(true$>
      L.incf 5 "( SH.star: @[<hv 1>[%a] *@ %a@]" (List.fmt " *@ " fmt) ps fmt q );
    (fun q -> assert(true$>
      L.decf 5 ") SH.star:@ %a" fmt q ))
    <&
    match ps with
    | [] -> q
    | [p] when equal p emp -> q
    | [p] when equal q emp -> {p with lbl= q.lbl}
    | p::ps -> fst (norm id_cxt (q, u_of (List.fold_left syntactic_star p ps)))


  (** [disj ps q] returns the iterated disjunction of the [ps] and [q],
      preserving the labels of [q]. *)
  let disj ps q =
    assert(true$>
      L.incf 5 "( SH.disj:@ @[<hv 1>[%a] \\/@ %a@]" (List.fmt " \\/@ " fmt) ps fmt q );
    (fun q -> assert(true$>
      L.decf 5 ") SH.disj:@ %a" fmt q ))
    <&
    (* relabel ps to avoid the labels of q, so that they are preserved *)
    let dj,_ =
      List.fold (fun p (dj, lbls) ->
        let p, lbls = relabel lbls (labels p) p in
        (Dj.add p dj, lbls)
      ) ps (Dj.singleton q, labels q) in
    fst (norm id_cxt (emp, {uemp with s= [Dj(dj)]}))



  (* Maps =================================================================== *)

  let rec map fn sh =
(*     L.incf 0 "( SH.map: %a" fmt sh ; L.decf 0 ") SH.map: %a" fmt <& *)
    let sh =
      Fs.fold (fun f sh ->
        let f' =
          match f with
          | Pt _ | Ls _ ->
              f
          | Dj(dj) ->
              let dj = Dj.map (fun dt -> map fn dt) dj in
              Dj(dj)
        in
        fst (norm id_cxt (sh, {uemp with s= [f']}))
      ) sh.sfs {sh with sfs= Fs.empty}
    in
    fn sh


  let rec map_fold fn sh z =
(*     L.incf 0 "( map_fold: %a" Sh.fmt sh ; (fun (sh,_) -> L.decf 0 ") map_fold: %a" Sh.fmt sh) <& *)
    let sh, z =
      Fs.fold (fun f (sh, z) ->
        let f', z =
          match f with
          | Pt _ | Ls _ ->
              (f, z)
          | Dj(dj) ->
              let dj, z = Dj.map_fold (fun (dt,z) -> map_fold fn dt z) (dj,z) in
              (Dj(dj), z)
        in
        (fst (norm id_cxt (sh, {uemp with s= [f']})), z)
      ) sh.sfs ({sh with sfs= Fs.empty}, z)
    in
    fn sh z


  let rec map_fold_sp dn up sh sa pa =
(*     L.incf 0 "( map_fold_sp: %a" Sh.fmt sh ; (fun (sh,_) -> L.decf 0 ") map_fold_sp: %a" Sh.fmt sh) <& *)
    let sh, sa = dn sh sa in
    let sh, pa =
      Fs.fold (fun f (sh, pa) ->
        let f', pa =
          match f with
          | Pt _ | Ls _ ->
              (f, pa)
          | Dj(dj) ->
              let dj, pa = Dj.map_fold (fun (dt,pa) -> map_fold_sp dn up dt sa pa) (dj,pa) in
              (Dj(dj), pa)
        in
        (fst (norm id_cxt (sh, {uemp with s= [f']})), pa)
      ) sh.sfs ({sh with sfs= Fs.empty}, pa)
    in
    up sh sa pa


  let map_fold_distrib fn sh z =
    let rec map_fold_distrib_ fn stem dt z =
      let dt_stem, dt_djs = Fs.extract_all_djs dt.sfs in
      (* star stem onto trimmed disjunct to preserve label of dt and associated metadata *)
      let stem = star [stem] {dt with sfs= dt_stem} in
      let stem, dt_djs, z =
        Fs.fold_djs (fun dj (stem, djs, z) ->
          let dj, z =
            Dj.map_fold (fun (dt, z) ->
              map_fold_distrib_ fn stem dt z
            ) (dj, z) in
          let dj,_ = norm id_cxt (emp, {uemp with s= [Dj(dj)]}) in
          let dj_stem, dj_djs = Fs.extract_all_djs dj.sfs in
          let stem = {stem with sfs= dj_stem} in
          let dj = {dj with sfs= dj_djs} in
          (stem, dj :: djs, z)
        ) dt_djs (stem, [], z) in
      let stem_s_dt = star dt_djs stem in
      fn stem_s_dt z
    in
    map_fold_distrib_ fn emp sh z



  (* Substitution, etc. ===================================================== *)

  (** [map_exps_denorm fn (q,u)] applies [fn] to every atom [a] of [q].  The
      input is not gratuitously copied, so if applying [fn] does not change
      any atoms of [q], then [map_exps_denorm fn qu == qu].  The spatial
      atoms changed by [fn] are moved to the accumulator [u], and the
      remainder of [q] is still normalized. *)
  let map_exps_denorm fn (q, u) =
    (fun (q',_) -> assert( q == q' || not (equal q q') ))
    <&
    let pas, p =
      Exps.fold (fun pa (pas, p) ->
        let pa' = E.map fn pa in
        if E.equal pa pa'
        then (pas, p)
        else (Exps.remove pa pas, pa' :: p)
      ) q.pas (q.pas, u.p)
    in
    let sfs, s =
      Fs.fold_filter (fun qf s ->
        let qf' = F.map_exps (fun x -> E.map fn x) qf in
        if F.equal qf qf'
        then None
        else Some(qf' :: s)
      ) q.sfs u.s
    in
    if p = [] && s = [] then (q, u) else ({q with pas; sfs}, {u with p; s})

  let map_exps fn q =
(*     L.incf 10 "( SH.map_exps:@ %a" fmt q ; *)
(*     L.decf 10 ") SH.map_exps:@ %a" fmt <& *)
    (* clear the congruences as the transformation may invalidate them *)
    let q0 = clear_cng q
    in
    let q1, u1 = map_exps_denorm fn (q0, uemp)
    in
    (* if nothing changed, keep the congruences *)
    if u1 = uemp then q
    else fst (norm {id_cxt with sub= fn} (q1, u1))


  let subst s q =
(*     L.incf 0 "( SH.subst:@ %a@ %a" S.fmt s fmt q ; *)
(*     L.decf 0 ") SH.subst:@ %a" fmt <& *)
    if S.is_empty s then q else
    map_exps (fun x -> try S.find x s with Not_found -> x) q


  let renaming vs =
    Vars.fold (fun v (fs,s,i) ->
      let sort = Var.sort v in
      let fresh = Var.gensym (Var.name v) sort in
      let s' = S.add (E.mkVar v) (E.mkVar fresh) s in
      let i' = S.add (E.mkVar fresh) (E.mkVar v) i in
      (Vars.add fresh fs, s', i')
    ) vs (Vars.empty, S.empty, S.empty)

  let rename_vs vs q =
(*     L.incf 0 "( SH.rename_vs:@ @[{%a}@]@ %a" Vars.fmt vs fmt q; *)
(*     (fun (q',_,s,_) -> L.decf 0 ") SH.rename_vs:@ %a@ %a" S.fmt s fmt q') *)
(*     <& *)
    let freshs, renaming, inverse = renaming vs in
    (subst renaming q, freshs, renaming, inverse)



  (* Pure Formulas ===========================================================*)

  module Pf = struct

    let default_preorder xs e f =
      let num_xs e = Vars.cardinal (Vars.inter xs (E.fv e)) in
      let o = Pervasives.compare (num_xs e) (num_xs f) in
      if o <> 0 then o < 0 else Exp.compare e f <= 0

    let star pas q =
      fst (norm id_cxt (q, {uemp with p= pas}))

    let term q =
      E.mkAnd (Exps.to_array q.pas)

    let normalize q e =
      CngRel.normalize q.fcr e

    let class_of q e =
      CngRel.class_of q.fcr e

    let classes q =
      Exps.fold (fun e' clss ->
        Expss.add (class_of q e') clss
      ) (CngRel.representatives q.fcr) Expss.empty

    let fold_classes fn q z =
      CngRel.fold_classes fn q.fcr z

    let mem_carrier e q =
      CngRel.mem_carrier e q.fcr

    let carrier q =
      CngRel.carrier q.fcr

    let empty q =
      {q with fcr= emp.fcr; tcr= emp.tcr; pas= emp.pas}

    let trim xs kills kill_to_keep q =
      let leq e f = default_preorder xs e f in
      let fcr = CngRel.remove_trivial q.fcr kills in
      let tcr = CngRel.remove_trivial q.tcr kills in
      let fcr = CngRel.subst leq fcr kill_to_keep in
      let tcr = CngRel.subst leq tcr kill_to_keep in
      {q with fcr; tcr}

    let union leq q r =
(*       L.incf 0 "( SH.Pf.union:@ %a@ %a" CngRel.fmt q.fcr CngRel.fmt r.fcr ; *)
(*       (fun {fcr} -> L.decf 0 ") SH.Pf.union:@ %a" CngRel.fmt fcr) <& *)
      {q with fcr= CngRel.union leq q.fcr r.fcr}

    let merge leq q e f =
(*       L.incf 0 "( SH.Pf.merge:@ %a = %a@ %a" E.fmt e E.fmt f CngRel.fmt q.fcr ; *)
(*       (fun {fcr} -> L.decf 0 ") SH.Pf.merge:@ %a" CngRel.fmt fcr) <& *)
      {q with fcr= CngRel.merge leq q.fcr e f}

    let extend xs q e =
      let leq e f = default_preorder xs e f in
      merge leq q e e

    let mem b q =
      let b' = E.map (normalize q) b in
      Exps.exists (fun e -> E.equal b' (E.map (normalize q) e)) q.pas

  end



  (* Normalization with respect to Disjunctive Congruence Closure =========== *)

  module ShFrm = struct
    include Sh
    module Exp = Exp
    module Exps = Exps
    module ExpMap = ExpMap

    let is_leaf q = Fs.is_empty_djs q.sfs

    let fold_rels _fn_scc fn q z =
      (* add all pointer and offset expressions to relation *)
      let z =
        fold_exps (fun e z ->
          (* Note: it should not be necessary to special case these *)
          match E.desc e with E.Add _ | E.Sub _ | E.Idx | E.App({HC.desc= E.Idx},_) -> z | _ ->
          match E.sort_of e with
          | Var.PointerSort | Var.OffsetSort -> fn e e z
          | _ -> z
        ) q z in
      (* add equations to relation *)
      let z =
        Exps.fold (fun e z ->
          match E.desc e with
          | E.Eq(e,f) ->
              fn f e (fn f f (fn e e z))
          | _ ->
              z
        ) q.pas z in
      z

    let fold_nrels fn q z =
      (* add disequations to relation *)
      Exps.fold (fun e z ->
        match E.desc e with
        | E.Op1(E.Not, E.Eq(e,f)) ->
            fn e f z
        | _ ->
            z
      ) q.pas z

  end

  (* Computes congruence closure logically, by expanding to DNF. *)
  module DCC = DisjCngClos.Make (ShFrm) (CngRel)

  let dcc x =
    let leq e f = Pf.default_preorder Vars.empty e f in
    DCC.dcc leq x


  let has_lit_off {Pt.off} sh =
    Exps.exists (fun e ->
      match Exp.desc e with
      | Exp.Var _ -> false
      | _ -> true
    ) (Sh.Pf.class_of sh (off :> Exp.t))

  let rec equates_off_to_lits pt dj =
    Dj.for_all (fun dt ->
      has_lit_off pt dt
      ||
      Fs.exists_djs (fun dj ->
        equates_off_to_lits pt dj
      ) dt.sfs
    ) dj

  let distrib_varoff_pts (sh, new_eqs) =
    let sfs, new_eqs =
      Fs.fold_pts (fun pt (sfs, n) ->
        if has_lit_off pt sh then
          (sfs, n)
        else match
          Fs.trytake_djs (fun dj ->
            equates_off_to_lits pt dj
          ) sfs
        with
        | Some(dj) ->
            let dj', n =
              Dj.map_fold (fun (dt, n) ->
                let dt', n' = norm id_cxt (dt, {uemp with s= [Pt(pt)]}) in
                (dt', n || n')
              ) (dj, n) in
            (Fs.add (Dj(dj')) (Fs.remove (Dj(dj)) (Fs.remove (Pt(pt)) sfs)), n)
        | None ->
            (sfs, n)
      ) sh.sfs (sh.sfs, new_eqs) in
    ({sh with sfs}, new_eqs)


  module LsG = struct

    module Edge = struct
      type t = Exp.t * Ls.t list * Exp.t

      let equal (u,k,v) (w,l,x) =
        Exp.equal u w && Exp.equal v x && List.equal Ls.equal k l

      let compare (u,k,v) (w,l,x) =
        let o = Exp.compare u w in if o<>0 then o else
        let o = Exp.compare v x in if o<>0 then o else
        List.compare Ls.compare k l

      let adjacent (_,_,v) (w,_,_) =
        Exp.equal v w

      let append (u,k,_) (_,l,x) =
        (u, k @ l, x)
    end

    include Set.Make(Edge)

    let add_with_closure edg g =
      let hds =
        fold (fun g_edg h ->
          if Edge.adjacent g_edg edg then
            add (Edge.append g_edg edg) h
          else
            h
        ) g empty
      in
      let tls =
        fold (fun g_edg h ->
          if Edge.adjacent edg g_edg then
            add (Edge.append edg g_edg) h
          else
            h
        ) g empty
      in
      let spn =
        fold_product (fun hd_edg tl_edg h ->
          add (Edge.append hd_edg tl_edg) h
        ) hds tls empty
      in
      union spn (union tls (union hds (add edg g)))

    let add_ls foreward ls g =
      match foreward, ls with
      | true,  {Ls.arg= {frnt= f::_; next= n::_}} -> add_with_closure (f, [ls], n) g
      | false, {Ls.arg= {back= b::_; prev= p::_}} -> add_with_closure (b, [ls], p) g
      | _ -> g

  end

  module LsGM = struct
    include Map.Make(Patn)

    let extend sh lsgm =
      Fs.fold_lss (fun ({Ls.pat} as ls) lsgm ->
        let flsg, blsg = Option.get_or (tryfind pat lsgm) (LsG.empty, LsG.empty) in
        let flsg = LsG.add_ls true ls flsg in
        let blsg = LsG.add_ls false ls blsg in
        add pat (flsg, blsg) lsgm
      ) sh.sfs lsgm
  end

  let must_allocs_ dnf sh lsgm locs =
    let must_allocs_pts {sfs} locs =
      Fs.fold_pts (fun {Pt.loc} locs ->
        Exps.add loc locs
      ) sfs locs
    in
    let must_allocs_nonempty_lss {fcr} lsgm locs =
      LsGM.fold (fun _pat (flsg, blsg) locs ->
        let add_nonempty foreward lsg locs =
          LsG.fold (fun (u,l,v) locs ->
            if CngRel.mem_dqs fcr u v then
              List.fold (fun {Ls.arg= {frnt; back}} locs ->
                if foreward then
                  Exps.adds frnt locs
                else
                  Exps.adds back locs
              ) l locs
            else
              locs
          ) lsg locs
        in
        locs |>
        add_nonempty true flsg |>
        add_nonempty false blsg
      ) lsgm locs
    in
    let rec must_allocs_terminated_lss ({sfs} as sh) locs0 =
      let locs =
        Fs.fold_lss (fun {Ls.arg} locs ->
          Args.fold_links (fun (l,r) locs ->
            if Exps.mem l locs then
              Exps.add r locs
            else
              locs
          ) arg locs
        ) sfs locs0
      in
      if not (Exps.equal locs locs0) then
          must_allocs_terminated_lss sh locs
      else
          locs
    in
    let close_wrt_tcr {tcr} locs =
      Exps.fold (fun e locs ->
        Exps.union (CngRel.class_of tcr e) locs
      ) locs locs
    in
    fold_dnf ~dnf
      (fun dt ((lsgm, clocs), dlocs) ->
         let lsgm = LsGM.extend dt lsgm in
         let clocs =
           clocs |>
           must_allocs_pts dt |>
           must_allocs_nonempty_lss dt lsgm |>
           must_allocs_terminated_lss dt |>
           close_wrt_tcr dt
         in
         ((lsgm, clocs), dlocs)
      )
      (fun ((_lsgm, clocs), dlocs) ->
         Some(Option.fold Exps.inter dlocs clocs)
      )
      sh (lsgm, locs) None |>
    Option.or_get Exps.empty

  let must_allocs sh =
    must_allocs_ true sh LsGM.empty Exps.empty


  let fmtn ff sh = fmtsp emp (Vars.empty,Vars.empty) ff sh

  let extend_lsgm_locs dnf sh sfl lsgm locs =
    let sh_sf,_ = norm id_cxt (sh, {uemp with s= sfl}) in
    let lsgm = LsGM.extend sh_sf lsgm in
    let locs = must_allocs_ dnf sh_sf lsgm locs in
    (lsgm, locs)


  let denorm_conflicts dnf sh ush lsgm locs =
    let sfs, ush, lsgm, locs =
      Fs.fold (fun sf (sfs, ush, lsgm, locs) ->
        if Exps.intersect (F.may_allocs sf) locs then
          let sfs = Fs.remove sf sfs in
          let ush = {ush with s= sf :: ush.s} in
          (sfs, ush, lsgm, locs)
        else
          let lsgm, locs = extend_lsgm_locs dnf emp [sf] lsgm locs in
          (sfs, ush, lsgm, locs)
      ) sh.sfs (sh.sfs, ush, lsgm, locs) in
    ({sh with sfs}, ush, lsgm, locs)


  let renorm dnf ({locs} as cxt) sh ush lsgm new_eqs =
    let sh, new_eqs' = norm cxt (sh, ush) in
    let new_eqs = new_eqs || new_eqs' in
    let locs = must_allocs_ dnf sh lsgm locs in
    (lsgm, locs, sh, new_eqs)


  let rec normalize_ ?(init=emp) dnf xs sh =
    let cm =
      let leq e f = Pf.default_preorder xs e f in
      DCC.dcc ~dnf leq ~init:init.fcr sh
    in
    let rec normalize_rec lss lsgm locs (dt, new_eqs) =
      assert(true$>(
        L.incf 20 "( normalize_rec: {@[%a@]}@ %a" Exps.fmt locs fmtn dt )) ;
      (fun (dt,_) -> assert(true$>(
        L.decf 20 ") normalize_rec: %a" fmtn dt )))
      <&
      (* find congruence relations for dt *)
      match IntMap.tryfind dt.lbl cm with
      | None ->
          (* the disjunction structure has changed during normalization
             due to expanding conflicting lists into a disjunction *)
          assert( new_eqs );
          (dt, new_eqs)
      | Some(fcr, tcr) ->
          let dt = {dt with fcr; tcr} in
          let sub = Pf.normalize dt in
          (* simplify inconsistent formulas *)
          if CngRel.inconsistent dt.fcr then
            ({ff with lbl= dt.lbl}, false)
          else
            let lsgm, locs, dt, new_eqs =
              (* denorm stem subformulas changed by normalizing wrt congruence *)
              let stem = {dt with sfs= Fs.clear_djs dt.sfs} in
              let stem, ustem = map_exps_denorm sub (stem, uemp) in
              (* conjoin implied (dis)equalities *)
              let ustem =
                let p =
                  ustem.p |>
                  CngRel.fold (fun e' e p -> E.mkEq e' e :: p) dt.fcr |>
                  CngRel.foldn (fun e' e p -> E.mkDq e' e :: p) dt.fcr in
                {ustem with p} in
              (* denorm stem pts and lss whose may-allocs intersect locs *)
              let stem, ustem, lsgm, locs = denorm_conflicts dnf stem ustem lsgm locs in
              (* normalize stem *)
              let lsgm, locs, stem, new_eqs = renorm dnf {sub; lss; locs} stem ustem lsgm new_eqs in
              (* recombine the stem and disjunctions *)
              let dt, new_eqs =
                let dt, new_eqs' = norm {sub; lss; locs} (stem, {uemp with s= Fs.to_list (Fs.only_djs dt.sfs)}) in
                let new_eqs = new_eqs || new_eqs' in
                (dt, new_eqs) in
              (lsgm, locs, dt, new_eqs) in
            (* distribute points-tos with variable offsets to literal equations *)
            let dt, new_eqs = distrib_varoff_pts (dt, new_eqs) in
            (* recurse over each disjunct of each disjunction individually *)
            let stem = {dt with sfs= Fs.clear_djs dt.sfs} in
            let ustem, lsgm, new_eqs =
              let djs, lsgm, new_eqs =
                (* extend stem lists *)
                let lss' = Fs.fold_lss LsB.add stem.sfs lss in
                let rec normalize_djs djs' lsgm new_eqs done_fs todo_djs =
                  match todo_djs with
                  | Dj(dj) :: todo_djs ->
                      (* extend locs with must-allocs of stem and other disjunctions *)
                      let lsgm', locs' = extend_lsgm_locs dnf done_fs todo_djs lsgm locs in
                      let dj', new_eqs' = Dj.map_fold (normalize_rec lss' lsgm' locs') (dj, new_eqs) in
                      let done_fs',_ = norm id_cxt (done_fs, {uemp with s= [Dj(dj)]}) in
                      normalize_djs (Djs.add dj' djs') lsgm' new_eqs' done_fs' todo_djs
                  | [] ->
                      (djs', lsgm, new_eqs)
                  | _ ->
                      assert false
                in
                let djs, lsgm, new_eqs =
                  let todo_djs = Fs.fold_djs (fun dj djs -> Dj(dj) :: djs) dt.sfs [] in
                  normalize_djs Djs.empty lsgm new_eqs stem todo_djs in
                (djs, lsgm, new_eqs) in
              (* add updated disjunctions to stem *)
              let ustem = {uemp with s= Djs.fold (fun dj ufs -> Dj(dj) :: ufs) djs []} in
              (ustem, lsgm, new_eqs) in
            let _,_, dt, new_eqs = renorm dnf {sub; lss; locs} stem ustem lsgm new_eqs in
            (dt, new_eqs)
    in
    let sh, new_eqs = normalize_rec LsB.empty LsGM.empty Exps.empty (sh, false)
    in
    if new_eqs then
      normalize_ ~init dnf xs sh
    else
      sh


  let normalize ?(dnf=true) ?init (xs, sh) =
    assert(true$>(
      L.incf 10 "( SH.normalize:@ %a" (fmtsp emp (mk_fxt (xs, sh))) sh ));
      Timer.start normalize_tmr ;
    (fun (xs',sh') ->
      Timer.stop normalize_tmr ; assert(true$>(
      L.decf 10 ") SH.normalize:@ %a" (fmtsp emp (mk_fxt (xs', sh'))) sh' )))
    <&
    let sh' = normalize_ ?init dnf xs sh in
    let xs' = Vars.inter xs (fv sh') in
    (xs', sh')

(*   let normalize ?(dnf=true) ?init (xs, sh) = *)
(*     debug_wrap3 Config.vSH 10 (fun dnf init xsh -> normalize ~dnf ?init xsh) *)
(*       dnf init (xs, sh) *)


  let normalize_stem ?(init=emp) (xs, sh) =
    assert(true$>(
      L.incf 10 "( SH.normalize_stem:@ %a"
        XSh.fmt (xs, {sh with fcr= init.fcr; tcr= init.tcr}) ));
      Timer.start normalize_stem_tmr ;
    (fun (xsh',_) ->
      Timer.stop normalize_stem_tmr ; assert(true$>(
      L.decf 10 ") SH.normalize_stem:@ %a" XSh.fmt xsh' )))
    <&
    let stem, djs = Fs.extract_all_djs sh.sfs in
    let stem' = normalize_ ~init false xs {sh with sfs= stem} in
    let sh' = {stem' with sfs= Fs.union stem'.sfs djs} in
    let xs' = Vars.inter xs (fv sh') in
    let eqs =
      CngRel.fold (fun e' e eqs ->
        if CngRel.mem init.fcr e' e || CngRel.mem sh.fcr e' e
        then eqs
        else Exps.add (E.mkEq e' e) eqs
      ) sh'.fcr Exps.empty in
    ((xs', sh'), eqs)



  (* Quantifier Operations ================================================== *)


  let exists_elim_id = ref 0

  (* Note: dnf could perhaps be false by default once subformulas are ordered
     by a guarding preorder *)
  let exists_elim ?(dnf=true) (xs, sh) =
    assert(true$>(
      incr exists_elim_id ;
      L.incf 10 "( SH.exists_elim %i:@ %a" !exists_elim_id XSh.fmt (xs, sh) ));
      Timer.start exists_elim_tmr ;
    (fun xsh' ->
      Timer.stop exists_elim_tmr ; assert(true$>(
      L.decf 10 ") SH.exists_elim:@ %a" XSh.fmt xsh' )))
    <&
    if Vars.is_empty xs || not Config.sh_simplify then (xs, sh)
    else
    let xs, sh = normalize ~dnf ~init:sh (xs, sh)
    in
    (* build substitution from existentials to equal, preferably universal, exps *)
    let extend_subst xs fcr (ks, subst) =
      let xs_ks = Vars.diff xs ks in
      Vars.fold (fun v (ks, subst) ->
        let x = E.mkVar v in
        let eqc = Exps.remove x (CngRel.class_of fcr x) in
        if Exps.is_empty eqc then (ks, subst) else
(*         L.printf 0 "v: %a eqc: {@[%a@]}" Var.fmt v Exps.fmt eqc ; let()=()in *)
        let vs =
          Exps.fold (fun e vs ->
            match Exp.desc e with
            | Exp.Var(v) when Vars.mem v xs_ks -> Vars.add v vs
            | _ -> vs
          ) eqc (Vars.singleton v)
        in
        let vs_ks = Vars.union vs ks
        in
        let map_vs_to_exp_disjoint_from zs =
          let e = Exps.take (fun e -> Vars.disjoint (E.fv e) zs) eqc in
          let subst =
            Vars.fold (fun v subst ->
              S.add (E.mkVar v) e subst
            ) vs subst in
          (vs_ks, subst)
        in
        try
          map_vs_to_exp_disjoint_from xs
        with Not_found -> try
          map_vs_to_exp_disjoint_from vs_ks
        with Not_found -> try
          map_vs_to_exp_disjoint_from ks
        with Not_found ->
          (ks, subst)
      ) xs_ks (ks, subst)
    in
    let rec subst_xs (ks, subst) (dt,sm) =
(*       L.incf 10 "( subst_xs: %i" dt.lbl ; (fun (sh,_) -> L.decf 10 ") subst_xs: %a" fmt sh) <& *)
      (* find congruence relations for dt *)
      let {fcr; tcr} = dt in
(*       L.printf 0 "fcr  : %a" CngRel.fmt fcr ; *)
(*       L.printf 0 "tcr  : %a" CngRel.fmt tcr ; *)
      (* construct substitution for existentials *)
      let ks, subst = extend_subst xs fcr (ks, subst) in
(*       L.printf 0 "subst: %a" S.fmt subst ; *)
      (* remember the substitution to trim congruences *)
      let sm = IntMap.add dt.lbl subst sm in
      (* recurse over each disjunct of each disjunction individually *)
      let sfs, sm =
        Fs.map_fold_djs (fun dj_sm ->
          Dj.map_fold (fun dt_sm ->
            subst_xs (ks, subst) dt_sm
          ) dj_sm
        ) (dt.sfs, sm) in
      (* substitute universal exps for existentials in stem *)
      let stem = {dt with fcr; tcr; sfs= Fs.clear_djs sfs} in
      let sub = (fun e -> S.subst subst e) in
      let stem', ustem' = map_exps_denorm sub (stem, uemp) in
(*       L.printf 0 "subst: %a" S.fmt subst ; *)
(*       L.printf 0 "stem : %a" fmt stem ; *)
(*       L.printf 0 "stem': %a" fmt stem' ; *)
      (* add updated disjunctions to stem *)
      let s = Fs.fold_djs (fun dj ufs -> Dj(dj) :: ufs) sfs ustem'.s in
      (fst (norm {id_cxt with sub} (stem', {ustem' with s})), sm)
    in
    (* substitute for xs *)
    let sh, sm = subst_xs (Vars.empty, S.empty) (sh, IntMap.empty) in
    (* kill equations only for xs that were eliminated *)
    let fs = fv ~include_cng_rels:false sh in
(*     L.printf 0 "fs: {@[%a@]}" Vars.fmt fs ; *)
    let kills = Vars.fold (fun k es -> Exps.add (E.mkVar(k)) es) (Vars.diff xs fs) Exps.empty in
    let sh', () =
      map_fold_sp
        (fun dt subst ->
          (* lookup subst to trim congruences, or use parent's *)
          (dt, try IntMap.find dt.lbl sm with Not_found -> subst)
        )
        (fun dt subst () ->
(*           L.incf 10 "( up: %a" fmt dt ; (fun (dt',_) -> L.decf 10 ") up: %a" fmt dt' ) <& let()=()in *)
          (* trim killed variables from congruences *)
(*           L.printf 0 "subst : %a" S.fmt subst ; *)
          let subst = S.remove_vs fs subst in
(*           L.printf 0 "subst': %a" S.fmt subst ; *)
          let dt = Pf.trim xs kills subst dt in
          (* conjoin implied equalities *)
          let p =
            CngRel.fold (fun e' e p ->
              Pf.normalize dt (E.mkEq e' e) :: p
            ) dt.fcr [] in
          let sub e = Pf.normalize dt e in
          (fst (norm {id_cxt with sub} (dt, {uemp with p})), ())
        )
        sh S.empty () in
    let xs' = Vars.inter xs (fv sh') in
    (xs', sh')

(*   let exists_elim ?(dnf=true) (xs, sh) = *)
(*     debug_wrap2 Config.vSH 10 (fun dnf xsh -> exists_elim ~dnf xsh) dnf (xs, sh) *)


  let exists_intro vs sh = XSh.exists_intro vs (XSh.inj sh)



  (* Sets of Subformulas ==================================================== *)

  module PtS = struct
    let embed pts sh =
      {sh with sfs=
          Pts.fold (fun pt fs -> Fs.add (Pt(pt)) fs) pts (Fs.clear_pts sh.sfs)}
    let project sh = Fs.fold_pts Pts.add sh.sfs Pts.empty
    include
      (Set.Lift
         (struct include Pts type elt = Pt.t end)
         (struct
           type t = Sh.t
           type s = Pts.t
           let embed = embed
           let project = project
          end)
      : (sig
          include Set.Q with type elt = Pt.t and type t = Sh.t
          val remove : elt -> t -> t
          val empty : t -> t
         end))
    let find l q = Pts.find l (project q)
    let may_allocs q = Pts.may_allocs (project q)
    let star pts q = fst (norm id_cxt (q, {uemp with s= List.map (fun x -> Pt(x)) pts}))
  end

  module LsS = struct
    let embed lss sh =
      {sh with sfs=
          Lss.fold (fun ls fs -> Fs.add (Ls(ls)) fs) lss (Fs.clear_lss sh.sfs)}
    let project sh = Fs.fold_lss Lss.add sh.sfs Lss.empty
    include
      (Set.Lift
         (struct include Lss type elt = Ls.t end)
         (struct
           type t = Sh.t
           type s = Lss.t
           let embed = embed
           let project = project
          end)
      : (sig
          include Set.Q with type elt = Ls.t and type t = Sh.t
          val remove : elt -> t -> t
          val empty : t -> t
         end))
    let find l q = Lss.find l (project q)
    let may_allocs q = Lss.may_allocs (project q)
    let star lss q = fst (norm id_cxt (q, {uemp with s= List.map (fun x -> Ls(x)) lss}))
  end

  module DjS = struct
    let embed djs sh =
      {sh with sfs=
          Djs.fold (fun dj fs -> Fs.add (Dj(dj)) fs) djs (Fs.clear_djs sh.sfs)}
    let project sh = Fs.fold_djs Djs.add sh.sfs Djs.empty
    include
      (Set.Lift
         (struct include Djs type elt = Dj.t end)
         (struct
           type t = Sh.t
           type s = Djs.t
           let embed = embed
           let project = project
          end)
      : (sig
          include Set.Q with type elt = Dj.t and type t = Sh.t
          val add : elt -> t -> t       (* Note: remove *)
          val map : (elt -> elt) -> t -> t
          val remove : elt -> t -> t
          val filter : (elt -> bool) -> t -> t
          val empty : t -> t
         end))

    let star djs q = fst (norm id_cxt (q, {uemp with s= List.map (fun x -> Dj(x)) djs}))

    let fold_semiring add mul fn q = Djs.fold_semiring add mul fn (project q)

    let extract_all q =
      let stem, djs = Fs.extract_all_djs q.sfs in
      ({q with sfs= stem}, {q with sfs= djs})
(*
    let map fn sh =
(*       L.incf 0 "( DjS.map: %a" fmt sh ; L.decf 0 ") DjS.map: %a" fmt <& *)
      star (Djs.to_list (Djs.map fn sh.djs)) (empty sh)

    let map_fold fn (sh,z) =
      let djs', z' = Djs.map_fold fn (sh.djs, z) in
      (star (Djs.to_list djs') (empty sh), z')

    let map_foldi fn (sh,z) =
      let djs',z' = Djs.map_foldi fn (sh.djs, z) in
      if Djs.equal djs' sh.djs then (sh, z') else
      (star (Djs.to_list djs') (empty sh), z')
*)
  end

  module Jnk = struct
    let star q = {q with jnk= true}
    let remove q = {q with jnk= false}
    let is_empty q = not (q.jnk)
  end



  (* Destructors ============================================================ *)

  let pure_sf q =
    let rec pure_sf_ q =
      E.mkAnd (Array.of_list
        (DjS.fold (fun dj cn ->
          E.mkOr (Array.of_list
             (Dj.fold (fun dt dn -> pure_sf_ dt :: dn) dj []))
          :: cn
         ) q (E.tt :: Exps.to_list q.pas)))
    in
    pure_sf_ q

  let rec spatial_sf q =
    DjS.map (Dj.map spatial_sf) {q with fcr= emp.fcr; tcr= emp.tcr; pas= emp.pas}



  (* Logical Normalization ================================================== *)

  module ExpIMMap = ImperativeMultiMap.Make(Exp)(Exps)


  (** [pure_consequences q] is [(ps,c,d)] where
      [?xs. q ==> (?ps,xs. c ^ d) /\ (!ps.?xs. c ==> d)]. *)
  let pure_consequences q =
    assert(true$>
      L.incf 10 "( pure_consequences:@ %a" fmt q);
      Timer.start pure_consequences_tmr ;
    (fun (ps,c,d) ->
      Timer.stop pure_consequences_tmr ; assert(true$>
      L.decf 10 ") pure_consequences:@ {@[%a@]}@ %a@ %a"
        Vars.fmt ps E.fmt c E.fmt d))
    <&
    let prtn_to_locs = ExpIMMap.create ()
    in
    let rec aux prtno vs xs sh =
(*       L.incf 0 "( aux sh: %a" fmt sh ; L.decf 0 ") to sh': %a" E.fmt <& *)
      let sh' =
        map (fun q ->
(*        L.incf 0 "( map q: %a" fmt q ; L.decf 0 ") to q': %a" fmt <& *)
          let bs = q.pas
          in
          let bs =
            PtS.fold (fun {Pt.loc} bs ->
              if Vars.intersect xs (E.fv loc) then
                bs
              else
                let prtn =
                  match prtno with
                  | Some(prtn) -> prtn
                  | None -> E.mkVar (Var.gensym "prtn" Var.BooleanSort) in
                ExpIMMap.add prtn_to_locs prtn loc ;
                Exps.add prtn (Exps.add (E.mkAllocd loc) bs)
            ) q bs
          in
          let do_one vs xs zs prtn cn pat ends =
            let ys, ls_one =
              XSh.exists_bind vs
                (XSh.exists_intro zs
                   (XSh.Pf.star cn (Patn.instantiate pat ends))) in
            let vs = Vars.union vs ys in
            let xs = Vars.union xs ys in
            let aux_one = aux (Some(prtn)) vs xs ls_one in
            (* Note: Avoid this weakening by adding exist'ls to E.boolean *)
(*            L.printf 0 "aux_one : %a" E.fmt_b aux_one ; *)
            let aux_one =
              E.remove (function
                | E.Var(x) -> Vars.mem x xs |_-> false
              ) aux_one in
(*            L.printf 0 "aux_one': %a" E.fmt_b aux_one ; *)
            (vs, xs, aux_one)
          in
          let bs =
            LsS.fold (fun ({Ls.pat; len; arg} as ls) bs ->
(*            L.incf 0 "( ls: %a" Ls.fmt ls ; *)
(*            (fun bl -> L.decf 0 ") to: %a" E.fmt (List.hd bl)) *)
(*            <& *)
              let len_zero = E.mkAnd (Array.of_list (Ls.empty_eqs ls))
              in
(*            L.printf 0 "len_zero: %a" E.fmt len_zero ; *)
              let sprtn = E.mkVar (Var.gensym "sprtn" Var.BooleanSort) in
              let fprtn = E.mkVar (Var.gensym "fprtn" Var.BooleanSort) in
              let bprtn = E.mkVar (Var.gensym "bprtn" Var.BooleanSort)
              in
              let vs, xs, len_one =
                do_one vs xs Vars.empty sprtn [E.mkEq len E.one] pat arg
              in
(*            L.printf 0 "len_one: %a" E.fmt len_one ; *)
              let zs, fnt, bck = Ls.split_on_fresh_point ls
              in
              let vs, xs, len_fnt =
                do_one vs xs zs fprtn [E.mkZGt len E.one] pat fnt
              in
              let _vs, _xs, len_many =
                do_one vs xs zs bprtn [len_fnt] pat bck
              in
(*            L.printf 0 "len_many: %a" E.fmt len_many ; *)
              Exps.add (E.mkOr [|len_zero; len_one; len_many|]) bs
            ) q bs
          in
          {q with pas= bs; sfs= Fs.only_djs q.sfs}
        ) sh
      in
      pure_sf sh'
    in
    let c = aux None (fv q) Vars.empty q
    in
    let ps, ites, _ =
      ExpIMMap.fold (fun prtn loc (ps, ites, i) ->
        let guard = E.mkAnd [|prtn; E.mkAllocd loc|] in
        ( Vars.union (E.fv prtn) ps
        , (E.mkIte guard loc (E.mkNum i)) :: ites
        , Int64.pred i )
      ) prtn_to_locs (Vars.empty, [E.nil], -1L)
    in
    let d = E.mkDistinct (Array.of_list ites)
    in
    (ps, c, d)


  let labeled_pure_consequences q =
    assert(true$>
      L.incf 10 "( labeled_pure_consequences:@ %a" fmt q );
      Timer.start labeled_pure_consequences_tmr ;
    (fun (c,_) ->
      Timer.stop labeled_pure_consequences_tmr ; assert(true$>
      L.decf 10 ") labeled_pure_consequences:@ %a" E.fmt c))
    <&
    let prtn_to_locs = ExpIMMap.create ()
    in
    let rec aux prtn vs xs q =
(*       L.incf 0 "( aux q: %a" fmt q ; L.decf 0 ") aux: %a" Exps.fmt <& *)
      let bs = q.pas
      in
      let bs =
        PtS.fold (fun {Pt.loc} bs ->
          if Vars.intersect xs (E.fv loc) then
            bs
          else (
            ExpIMMap.add prtn_to_locs prtn loc ;
            Exps.add prtn (Exps.add (E.mkAllocd loc) bs)
          )
        ) q bs
      in
      let do_one vs xs zs prtn cn pat ends =
        let ys, ls_one =
          XSh.exists_bind vs
            (XSh.exists_intro zs
               (XSh.Pf.star cn (Patn.instantiate pat ends))) in
        let vs = Vars.union vs ys in
        let xs = Vars.union xs ys in
        let aux_one = pure_sf (map (fun q -> {q with pas= aux prtn vs xs q}) ls_one) in
        (* Note: Avoid this weakening by adding exist'ls to E.boolean *)
(*         L.printf 0 "aux_one : %a" E.fmt aux_one ; *)
        let aux_one = E.remove (function E.Var(x) -> Vars.mem x xs |_-> false) aux_one in
(*         L.printf 0 "aux_one': %a" E.fmt aux_one ; *)
        (vs, xs, aux_one)
      in
      let bs =
        LsS.fold (fun ({Ls.pat; len; arg} as ls) bs ->
(*           L.incf 0 "( ls: %a" Ls.fmt ls ; L.decf 0 ") to: %a" Exps.fmt <& *)
          let len_zero = E.mkAnd (Array.of_list (Ls.empty_eqs ls))
          in
(*           L.printf 0 "len_zero: %a" E.fmt len_zero ; *)
          let sprtn = E.mkVar (Var.gensym "sprtn" Var.BooleanSort) in
          let fprtn = E.mkVar (Var.gensym "fprtn" Var.BooleanSort) in
          let bprtn = E.mkVar (Var.gensym "bprtn" Var.BooleanSort)
          in
          let vs, xs, len_one =
            do_one vs xs Vars.empty sprtn [E.mkEq len E.one] pat arg
          in
(*           L.printf 0 "len_one: %a" E.fmt len_one ; *)
          let zs, fnt, bck = Ls.split_on_fresh_point ls
          in
          let vs, xs, len_fnt =
            do_one vs xs zs fprtn [E.mkZGt len E.one] pat fnt
          in
          let _vs, _xs, len_many =
            do_one vs xs zs bprtn [len_fnt] pat bck
          in
(*           L.printf 0 "len_many: %a" E.fmt len_many ; *)
          Exps.add (E.mkOr [|len_zero; len_one; len_many|]) bs
        ) q bs
      in
      bs
    in
    let vs = fv q
    in
    let prop_q, (lbl_to_prop, implications) =
      map_fold_sp
        (fun q _ -> (q, q.lbl))
        (fun q lbl (lbl_to_prop, implications) ->
(*           L.incf 0 "( map q: %a" fmt q ; (fun (q',_) -> L.decf 0 ") to q': %a" fmt q') <& *)
          let prop = E.mkVar (Var.gensym "lbl" Var.BooleanSort) in
(*           let prop = E.mkVar (Var.gensym ("lbl_"^(string_of_int lbl)) Var.BooleanSort) in *)
          let pas, pure =
            if Config.weak_pure_consequences then
              (Exps.singleton prop, pure_sf q)
            else
              let pas = aux (E.mkVar (Var.gensym "prtn" Var.BooleanSort)) vs Vars.empty q in
              (pas, pure_sf {q with pas})
          in
          let q' = {q with pas; sfs= Fs.only_djs q.sfs} in
          let implications' = E.mkImp prop pure :: implications in
          let lbl_to_prop' = IntMap.add lbl prop lbl_to_prop in
          (q', (lbl_to_prop', implications'))
        )
        q q.lbl (IntMap.empty, [])
    in
    let _, ites, _ =
      ExpIMMap.fold (fun prtn loc (ps, ites, i) ->
        ( Vars.union (E.fv prtn) ps
        , (E.mkIte (E.mkAnd [|prtn; E.mkAllocd loc|]) loc (E.mkNum i)) :: ites
        , Int64.pred i
        )
      ) prtn_to_locs (Vars.empty, [E.nil], -1L)
    in
    (E.mkAnd (Array.of_list (E.mkDistinct (Array.of_list ites) :: pure_sf prop_q :: implications)), lbl_to_prop)



  (* shadow ground values that cannot be exported from recursive module *)
  let emp () = emp
  let tt () = tt
  let ff () = ff
end



(*XSh=========================================================================
               eXistentially quantified Symbolic Heap formulas
  ============================================================================*)

and XSh : sig
  include
    (EXISTENTIAL_SYMBOLIC_HEAP
     with type t = Vars.t * Sh.t and type sh := Sh.t
      and type pt := Pt.t and type ls := Ls.t and type dj := Dj.t)

  (* internal operations *)
  val inj : Sh.t -> t

  (* shadow ground values that cannot be exported from recursive module *)
  val emp : unit -> t
  val tt : unit -> t
  val ff : unit -> t

end = struct

  include (Sh_t : module type of Sh_t with type t := Sh_t.t)

  type t = Vars.t * Sh_t.t



  (* Formatting ============================================================= *)

  let fmtp fxt ff xsh = Sh.fmtp_xs fxt ff xsh

  let fmt ff xsh = fmtp (Sh.mk_fxt xsh) ff xsh

  let fmtp_xs = fmtp
  let fmt_xs = fmt

  let fmt_did = Sh.fmt_did_xs
  let fmt_did_xs = fmt_did


  let fmt_caml ff (xs,sh) =
    Format.fprintf ff
      "@[<hov 2>(SH.exists_intro@\n @[<hov 2>(Vars.of_list [@[%a@]])@\n \
       @[<hov 2>%a@])@]@]"
      (List.fmt ";@ " Var.fmt_caml) (Vars.to_list xs)
      Sh.fmt_caml sh



  (* Iterators ============================================================== *)

  (* Notes: Is ignoring the existentials in both map and fold here correct? *)
  let map_exps fn (xs,q) = (xs, Sh.map_exps fn q)

  let fold_exps fn (_,q) = Sh.fold_exps fn q



  (* Quantifier Operations ================================================== *)

  let inj q = (Vars.empty, q)


  let exists_bind cxt (xs,sh) =
    let ws, xs_m_cxt = Vars.inter_diff xs cxt in
    let sh', ws', _, _ = Sh.rename_vs ws sh in
    (Vars.union ws' xs_m_cxt, sh')


  let exists_binds cxt xshs =
    let _, fv_shs, xs, shs =
      List.fold (fun xsh (cxt, fv_shs, xs, shs) ->
        let ys, sh = exists_bind cxt xsh in
        let fv_sh = Sh.fv sh in
        let cxt' = Vars.union fv_sh cxt in
        let fv_shs' = Vars.union fv_sh fv_shs in
        let xs' = Vars.union xs ys in
        let shs' = sh :: shs in
        (cxt', fv_shs', xs', shs')
      ) xshs (cxt, Vars.empty, Vars.empty, [])
    in
    (fv_shs, xs, shs)


  let exists_intro vs (xs,sh as xsh) =
    assert(true$>(
      L.incf 10 "( XSH.exists_intro:@ {@[%a@]}@ %a" Vars.fmt vs fmt xsh )) ;
    (fun xsh' -> assert(true$>(
      L.decf 10 ") XSH.exists_intro:@ %a" fmt xsh' )))
    <&
    if Vars.is_empty vs then xsh else
    Sh.exists_elim (Vars.union vs xs, sh)



  (* Queries ================================================================ *)

  let inconsistent (_,sh) = Sh.inconsistent sh

  let is_empty (_,sh) = Sh.is_empty sh
  let is_pure (_,sh) = Sh.is_pure sh

  let sizeof (_,sh) = Sh.sizeof sh

  let fv (xs,q) = Vars.diff (Sh.fv q) xs

  let lbl (_,sh) = Sh.lbl sh
  let set_lbl lbl (xs,sh) = (xs, Sh.set_lbl lbl sh)



  (* Base Constructors ====================================================== *)

  let emp = inj (Sh.emp())
  let tt = inj (Sh.tt())
  let ff = inj (Sh.ff())



  (* Constructors =========================================================== *)

  let star pl q =
    assert(true$>
      L.incf 5 "( XSH.star: @[<hv 1>[%a] *@ %a@]"
        (List.fmt " *@ " fmt) pl fmt q );
    (fun q -> assert(true$>
      L.decf 5 ") XSH.star:@ %a" fmt q ))
    <&
    let fv_ps, xs, ps = exists_binds (fv q) pl in
    let ys, q = exists_bind fv_ps q in
    Sh.exists_intro (Vars.union xs ys) (Sh.star ps q)


  let disj pl q =
    assert(true$>
      L.incf 5 "( XSH.disj: @[<hv 1>[%a] v@ %a@]"
        (List.fmt " v@ " fmt) pl fmt q );
    (fun q -> assert(true$>
      L.decf 5 ") XSH.disj:@ %a" fmt q ))
    <&
    let fv_ps, xs, ps = exists_binds (fv q) pl in
    let ys, q = exists_bind fv_ps q in
    Sh.exists_intro (Vars.union xs ys) (Sh.disj ps q)


  let star_ats qf_star_ats at_fv = fun atl sh ->
    let vs = List.fold (fun at vs -> Vars.union (at_fv at) vs) atl Vars.empty in
    let xs, sh = exists_bind vs sh in
    Sh.exists_intro xs (qf_star_ats atl sh)


  module Pf = struct
    let star = star_ats Sh.Pf.star E.fv
  end

  module PtS = struct
    let star = star_ats Sh.PtS.star Pt.fv
  end
(*
  module LsS = struct
    let star = star_ats Sh.LsS.star Ls.fv
  end

  module DjS = struct
    let star = star_ats Sh.DjS.star Dj.fv
  end
*)
  module Jnk = struct
    let star (xs,sh) = (xs, Sh.Jnk.star sh)
    let remove (xs,sh) = (xs, Sh.Jnk.remove sh)
  end


  let normalize ?dnf ?(init=emp) xsh =
    assert(true$>
      L.incf 5 "( XSH.normalize:@ %a@]" fmt xsh);
    (fun xsh' -> assert(true$>
      L.decf 5 ") XSH.normalize:@ %a" fmt xsh'))
    <&
    Sh.normalize ?dnf ~init:(snd init) (exists_bind Vars.empty xsh)


  let normalize_stem ?(init=emp) xsh =
    assert(true$>
      L.incf 5 "( XSH.normalize_stem:@ %a@]" fmt xsh);
    (fun (xsh',_) -> assert(true$>
      L.decf 5 ") XSH.normalize_stem:@ %a" fmt xsh'))
    <&
    Sh.normalize_stem ~init:(snd init) (exists_bind Vars.empty xsh)


  let subst s q =
    let s = S.restrict (fv q) s in
    let xs, q = exists_bind (S.fv s) q in
    let q' = Sh.subst s q in
    exists_intro xs (inj q')


  let rename_vs vs q =
    let freshs, renaming, inverse = Sh.renaming vs in
    (subst renaming q, freshs, renaming, inverse)



  (* Comparison Operations ================================================== *)

  let equal q r = q == r || equal_tup2 Vars.equal Sh.equal q r

  let compare q r = if q==r then 0 else compare_tup2 Vars.compare Sh.compare q r

(*   (** [alpha_equiv p q] holds only if [p] and [q] are alpha-equivalent *) *)
(*   let alpha_equiv p q = *)
(* (*     L.incf 10 "( alpha_equiv: %a %a" fmt p fmt q; *) *)
(* (*     ( *) *)
(*     let xs, p = exists_bind Vars.empty p in *)
(*     let ys, q = exists_bind Vars.empty q in *)
(*     let exists_p, exists_q = Vars.diff_diff xs ys in *)
(*     if Vars.cardinal exists_p = Vars.cardinal exists_q then *)
(*       let to_exp_list vs = Vars.fold (fun v el -> E.mkVar v :: el) vs [] in *)
(*       let renamings = *)
(*         List.map S.of_assoc *)
(*           (List.fin_funs (to_exp_list exists_q) (to_exp_list exists_p)) in *)
(*       let rec loop = function *)
(*         | [] -> None *)
(*         | renaming :: renamings -> *)
(*             if Sh.equal p (Sh.subst renaming q) *)
(*             then Some(renaming) *)
(*             else loop renamings *)
(*       in *)
(*       loop renamings *)
(*     else *)
(*       None *)
(* (*     ) &> *) *)
(* (*     L.decf 10 ") alpha_equiv: %a" (fun ff -> function *) *)
(* (*       | None -> Format.fprintf ff "false" *) *)
(* (*       | Some(s) -> S.fmt ff s) *) *)

(*
  let equivalent p q =
    if inconsistent p && inconsistent q then
      Some(S.empty)
    else
      let q' = propagate p q in
      match alpha_equiv p q' with
      | None ->
          let p' = propagate q p in
          (* check p |- q' and q |- p' *)
          let _, p = exists_bind (fv q') p in
          let _, q' = exists_bind (Sh.fv p) q' in
          let _, q = exists_bind (fv p') q in
          let _, p' = exists_bind (Sh.fv q) p' in
          if   Pure.implies (Sh.pure_strong p) (Sh.pure q')
            && Sh.equal (Sh.spatial p) (Sh.spatial q')
            && Pure.implies (Sh.pure_strong q) (Sh.pure p')
            && Sh.equal (Sh.spatial q) (Sh.spatial p')
          then Some(S.empty)
          else None
      | some_renaming -> some_renaming
*)
  let equivalent p q = if equal p q then Some(S.empty) else None
(*
(*   let equal_coarse p q = equal p q || equivalent p q <> None *)

(*   let compare_coarse p q = *)
(*     let ord = compare p q in *)
(*     if ord = 0 || equivalent p q <> None then 0 else ord *)
*)


  (* shadow ground values that cannot be exported from recursive module *)
  let emp () = emp
  let tt () = tt
  let ff () = ff
end



(*============================================================================
                      Exported modules dethunking values
  ============================================================================*)

module SH = struct
  include Sh

  let emp = emp()
  let tt = tt()
  let ff = ff()

end


module XSH = struct
  include XSh

  let emp = emp()
  let tt = tt()
  let ff = ff()

end
