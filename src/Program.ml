(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Internal representation of programs *)

open Library

open Type
open Variable
open Expression
module E = Exp
module S = Substitution
module HC = HashCons
open SymbolicHeap

module L = (val Log.std Config.vPgm : Log.LOG)

let unmarshal_tmr = Timer.create "Program.unmarshal"



(*============================================================================
                            Source Code Positions
  ============================================================================*)

module Position = struct

  type t = { dir : string; file : string; line : int; col : int; }

  let compare = Pervasives.compare
  let equal = Pervasives.( = )

  let fmt ff {line; file} = Format.fprintf ff "line %i %s" line file

end



(*============================================================================
                                 Instructions
  ============================================================================*)

(** Instructions. The machine model is ideal RISC: an infinite number of
    word-sized registers (the Var.t's), memory only accessed via
    load/stores. *)
module Inst = struct

  type desc =
    | Load of Var.t * Exp.t
    | Store of Exp.t * Exp.t
    | Alloc of Var.t * Exp.t
    | Free of Exp.t
    | Kill of Vars.t
    | Move of Var.t * Exp.t
    | Cast of Var.t * Typ.t * Exp.t
    | Assume of Exp.t
    | Assert of Exp.t
    | Nop
    | Generic of spec

  (** Specifications, whose meaning is the Hoare triple [forall ghosts. {pre} insts {post}]. *)
  and spec = { ghosts : Vars.t; pre : XSH.t; insts : desc list; post : XSH.t; }


  type t = { desc : desc; pos : Position.t; }


  let compare_desc c0 c1 = match c0, c1 with
    | Load(x0,e0) , Load(x1,e1)  ->
        compare_tup2 Var.compare Exp.compare (x0,e0) (x1,e1)
    | Load(_)     , _            -> -1
    | _           , Load(_)      -> 1
    | Store(e0,f0), Store(e1,f1) ->
        compare_tup2 Exp.compare Exp.compare (e0,f0) (e1,f1)
    | Store(_)    , _            -> -1
    | _           , Store(_)     -> 1
    | Alloc(x0,e0), Alloc(x1,e1) ->
        let o = Var.compare x0 x1 in if o <> 0 then o else Exp.compare e0 e1
    | Alloc(_)    , _            -> -1
    | _           , Alloc(_)     -> 1
    | Free(e0)    , Free(e1)     -> Exp.compare e0 e1
    | Free(_)     , _            -> -1
    | _           , Free(_)      -> 1
    | Kill(x0)    , Kill(x1)     -> Vars.compare x0 x1
    | Kill(_)     , _            -> -1
    | _           , Kill(_)      -> 1
    | Move(x0,e0) , Move(x1,e1)  ->
        compare_tup2 Var.compare Exp.compare (x0,e0) (x1,e1)
    | Move(_)     , _            -> -1
    | _           , Move(_)      -> 1
    | Cast(v0,t0,e0) , Cast(v1,t1,e1)  ->
        compare_tup3 Var.compare Typ.compare Exp.compare (v0,t0,e0) (v1,t1,e1)
    | Cast(_)     , _            -> -1
    | _           , Cast(_)      -> 1
    | Assume(e0)  , Assume(e1)   -> Exp.compare e0 e1
    | Assume(_)   , _            -> -1
    | _           , Assume(_)    -> 1
    | Assert(e0)  , Assert(e1)   -> Exp.compare e0 e1
    | Assert(_)   , _            -> -1
    | _           , Assert(_)    -> 1
    | Nop         , Nop          -> 0
    | Nop         , _            -> -1
    | _           , Nop          -> 1
    | Generic({ghosts=g0; pre=p0; insts=cc0; post=q0}), Generic({ghosts=g1; pre=p1; insts=cc1; post=q1}) ->
        compare_tup4 Vars.compare XSH.compare (List.compare compare) XSH.compare (g0, p0, cc0, q0) (g1, p1, cc1, q1)

  let compare c0 c1 = compare_desc c0.desc c1.desc


  let rec equal_desc c0 c1 =
    match c0, c1 with
    | Load(x0,e0) , Load(x1,e1)  ->
        equal_tup2 Var.equal Exp.equal (x0,e0) (x1,e1)
    | Store(e0,f0), Store(e1,f1) ->
        equal_tup2 Exp.equal Exp.equal (e0,f0) (e1,f1)
    | Alloc(x0,e0), Alloc(x1,e1) -> Var.equal x0 x1 && Exp.equal e0 e1
    | Free(e0)    , Free(e1)     -> Exp.equal e0 e1
    | Kill(x0)    , Kill(x1)     -> Vars.equal x0 x1
    | Move(x0,e0) , Move(x1,e1)  ->
        equal_tup2 Var.equal Exp.equal (x0,e0) (x1,e1)
    | Cast(v0,t0,e0) , Cast(v1,t1,e1)  ->
        equal_tup3 Var.equal Typ.equal Exp.equal (v0,t0,e0) (v1,t1,e1)
    | Assume(e0)  , Assume(e1)   -> Exp.equal e0 e1
    | Assert(e0)  , Assert(e1)   -> Exp.equal e0 e1
    | Generic({ghosts=g0; pre=p0; insts=cc0; post=q0}), Generic({ghosts=g1; pre=p1; insts=cc1; post=q1}) ->
        equal_tup4 Vars.equal XSH.equal (List.equal equal_desc) XSH.equal (g0, p0, cc0, q0) (g1, p1, cc1, q1)
    | _ -> false

  let equal c0 c1 = equal_desc c0.desc c1.desc


  (** free variables *)
  let rec fv_desc = function
    | Kill(xs) -> xs
    | Move(x,e)
    | Cast(x,_,e)
    | Load(x,e)
    | Alloc(x,e) -> Vars.add x (Exp.fv e)
    | Store(e,f) -> Vars.union (Exp.fv e) (Exp.fv f)
    | Free(e)
    | Assert(e)
    | Assume(e) -> Exp.fv e
    | Nop -> Vars.empty
    | Generic({ghosts; pre; insts; post}) ->
        List.fold (fun c -> Vars.union (fv_desc c)) insts
          (Vars.diff (Vars.union (XSH.fv pre) (XSH.fv post)) ghosts)

  let fv inst = fv_desc inst.desc


  let rec mv_desc = function
    | Move(x,_)
    | Cast(x,_,_)
    | Load(x,_)
    | Alloc(x,_) -> Vars.singleton x
    | Free _
    | Assert _
    | Assume _
    | Nop
    | Store _ -> Vars.empty
    | Kill(vs) -> vs
    | Generic({insts}) -> List.fold (fun c ms -> Vars.union (mv_desc c) ms) insts Vars.empty

  let mv inst = mv_desc inst.desc


  let rec fmt_desc ff = function
    | Load(x,l) -> Format.fprintf ff "@[load(@[%a,@ %a@])@]" Var.fmt x Exp.fmt l
    | Store(l,r) -> Format.fprintf ff "@[store(@[%a,@ %a@])@]" Exp.fmt l Exp.fmt r
    | Alloc(x,e) -> Format.fprintf ff "@[alloc(@[%a,@ %a@])@]" Var.fmt x Exp.fmt e
    | Free(e) -> Format.fprintf ff "@[free(@[%a@])@]" Exp.fmt e
    | Kill(vs) -> Format.fprintf ff "@[kill(@[%a@])@]" Vars.fmt vs
    | Move(x,l) -> Format.fprintf ff "@[move(@[%a,@ %a@])@]" Var.fmt x Exp.fmt l
    | Cast(v,t,e) ->
        let vl = !Config.vTyp in Config.vTyp := max 1 vl;
        Format.fprintf ff "@[cast(@[%a,@ %a,@ %a@])@]" Var.fmt v Typ.fmt t Exp.fmt e ;
        Config.vTyp := vl
    | Assume(e) -> Format.fprintf ff "@[assume(@[%a@])@]" Exp.fmt e
    | Assert(e) -> Format.fprintf ff "@[assert(@[%a@])@]" Exp.fmt e
    | Nop -> Format.fprintf ff "nop"
    | Generic(s) ->
        Format.fprintf ff "@[<hov 7>generic%a%a@ %a%a@]"
          (Vars.fmt_embrace " ! " " .@ ") s.ghosts
          XSH.fmt s.pre
          (List.fmt ";@ " fmt_desc) s.insts
          XSH.fmt s.post

  let fmt_desc_c ff = function
    | Load(x,l) -> Format.fprintf ff "@[%a = *%a@]" Var.fmt x Exp.fmt l
    | Store(l,r) -> Format.fprintf ff "@[*%a = %a@]" Exp.fmt l Exp.fmt r
    | Alloc(x,e) -> Format.fprintf ff "@[%a = malloc(%a)@]" Var.fmt x Exp.fmt e
    | Free(e) -> Format.fprintf ff "@[free(@[%a@])@]" Exp.fmt e
    | Kill(vs) -> Format.fprintf ff "%a = nondet()" (List.fmt " = nondet();@ " Var.fmt) (Vars.to_list vs)
    | Move(x,l) -> Format.fprintf ff "@[%a = %a@]" Var.fmt x Exp.fmt l
    | Cast(v,t,e) ->
        let vl = !Config.vTyp in Config.vTyp := max 1 vl;
        Format.fprintf ff "@[%a = %a%a@]" Var.fmt v Typ.fmt t Exp.fmt e ;
        Config.vTyp := vl
    | Assume(e) -> Format.fprintf ff "@[assume(@[%a@])@]" Exp.fmt e
    | Assert(e) -> Format.fprintf ff "@[assert(@[%a@])@]" Exp.fmt e
    | Nop -> Format.fprintf ff ";"
    | Generic _ as i -> fmt_desc ff i

  let fmt ff k = if !Config.c_syntax then fmt_desc_c ff k.desc else fmt_desc ff k.desc


  let report_ill_sorted c =
    (if Config.check_sorts then failwithf else L.warnf) "ill-sorted: %a" fmt_desc c

  let mk desc pos =
    (fun {desc} -> assert(
      (match desc with
      | Alloc(x,e) -> Var.sort x = Var.PointerSort && Exp.sort_of e = Var.IntegerSort
      | Free(e)
      | Load(_,e)
      | Store(e,_) -> Exp.sort_of e = Var.PointerSort
      | Move(x,e) -> Var.sort x = Exp.sort_of e
      | Cast(x,t,_) -> Var.sort x = Var.sort_of_type t
      | Assert(e)
      | Assume(e) -> Exp.sort_of e = Var.BooleanSort
      | Kill _
      | Nop
      | Generic _ -> true
      ) || report_ill_sorted desc
    )) <&
    let desc =
      match desc with
      | Kill(vs) when Vars.is_empty vs ->
          Nop
      | Move(x,e) ->
          (match E.convert (Var.sort x) e with
          | Some(f) -> Move(x,f)
          | None -> desc
          )
      | Cast(x,t,l) ->
          (match Exp.convert (Var.sort x) l with
          | None ->
              Kill(Vars.singleton x)
          | Some(l) ->
              match Typ.desc t with
              | Typ.Pointer(ty) when Typ.equal Typ.mkTop ty ->
                  Move(x,l)
              | _ ->
                  desc
          )
      | _ ->
          desc
    in
    {desc; pos}

end
module I = Inst


(*============================================================================
                            Procedure Identifiers
  ============================================================================*)

module ProcId = struct

  type t = int * string

  include UniqueId.Make (struct
    type data = string
    type uniq = t
    let get (x,_) = x
    let set x s = (x, s)
  end)

  let name (_,s) = s

  let fmt ff (i,s) =
    if !Config.vPgm > 1 then
      Format.fprintf ff "%s_%i" s i
    else
      Format.fprintf ff "%s" s

end


(*============================================================================
                          Control-Flow Point Labels
  ============================================================================*)

module ControlPointLabel = struct

  module Id = struct

    type t = int

    include UniqueId.Make (struct
      type data = unit
      type uniq = t
      let get x = x
      let set x () = x
    end)

    let fmt ff i = Format.fprintf ff "k %i" i

  end

  type sort = Entry | Exit | Return | Cut | Join | Fork

  type label = { id : Id.t; sort : sort option; pos : Position.t; proc : ProcId.t; }

  let mk_label ?sort pos proc =
    let id = Id.gensym () in
    (id, {id; sort; pos; proc})

  let set_sort l sort =
    {l with sort}

  let compare x y = Id.compare x.id y.id
  let equal x y = Id.equal x.id y.id


  let fmt_sort ff = function
    | Entry -> Format.fprintf ff "Entry"
    | Exit -> Format.fprintf ff "Exit"
    | Return -> Format.fprintf ff "Return"
    | Cut -> Format.fprintf ff "Cut"
    | Join -> Format.fprintf ff "Join"
    | Fork -> Format.fprintf ff "Fork"

  let fmt ff k =
    Format.fprintf ff "%a: %a" Position.fmt k.pos (Option.fmt "" fmt_sort) k.sort

end


(*============================================================================
                               Procedure Calls
  ============================================================================*)

module Call0 = struct

  type 'a t = {
    proc    : 'a;
    actuals : Var.t list;
    areturn : Var.t option;
    typ     : Typ.t;
    targets : ProcId.t list;
  }

  let fv {actuals; areturn} = Option.fold Vars.add areturn (Vars.of_list actuals)

  let mv {areturn} = Option.fold Vars.add areturn Vars.empty

  let compare compare_proc x y =
    let {proc= proc0; actuals= actuals0; areturn= areturn0} = x in
    let {proc= proc1; actuals= actuals1; areturn= areturn1} = y in
    let c = compare_proc proc0 proc1 in if c <> 0 then c else
    let c = List.compare Var.compare actuals0 actuals1 in if c <> 0 then c else
            Option.compare Var.compare areturn0 areturn1

  let equal equal_proc x y =
    let {proc= proc0; actuals= actuals0; areturn= areturn0} = x in
    let {proc= proc1; actuals= actuals1; areturn= areturn1} = y in
       equal_proc proc0 proc1
    && List.equal Var.equal actuals0 actuals1
    && Option.equal Var.equal areturn0 areturn1

  let fmt fmt_proc ff {proc; actuals; areturn} =
    let fmt_ret ff rv = Option.fmt "" (fun ff v -> Format.fprintf ff "%a =@ " Var.fmt v) ff rv in
    let fmt_vs ff vs = List.fmt ",@ " Var.fmt ff vs in
    Format.fprintf ff "@[<hov 2>%a@,@[%a(@[%a@])@]@]" fmt_ret areturn fmt_proc proc fmt_vs actuals

end


(*============================================================================
                                   Commands
  ============================================================================*)

module Cmnd = struct

  type t =
    | Inst of Inst.t
    | Call of ProcId.t Call0.t
    | ICall of Exp.t Call0.t

  let append x y =
    let open I in
    match x, y with
    | x, Inst({desc= Nop}) -> Some(x)
    | Inst({desc= Nop}), y -> Some(y)
    | Inst({desc= Kill xs; pos}), Inst({desc= Kill ys}) -> Some(Inst(I.mk (Kill (Vars.union xs ys)) pos))
    | _ -> None

  let fv = function
    | Inst(i) -> I.fv i
    | Call(call) -> Call0.fv call
    | ICall({Call0.proc} as call) -> Vars.union (Exp.fv proc) (Call0.fv call)

  let mv = function
    | Inst(i) -> I.mv i
    | Call(call) -> Call0.mv call
    | ICall(call) -> Call0.mv call

  let compare e0 e1 =
    match e0, e1 with
    | Inst(x)  , Inst(y)   -> I.compare x y
    | Inst _   , _         -> -1
    | _        , Inst _    ->  1
    | Call(x)  , Call(y)   -> Call0.compare ProcId.compare x y
    | Call _   , _         -> -1
    | _        , Call _    ->  1
    | ICall(x) , ICall(y)  -> Call0.compare Exp.compare x y

  let equal x y = compare x y = 0

  let fmt ff t =
    match t with
    | Inst(i) -> I.fmt ff i
    | Call(call) -> Call0.fmt ProcId.fmt ff call
    | ICall(call) -> Call0.fmt Exp.fmt ff call

end
module C = Cmnd


(*============================================================================
                             Control-Flow Graphs
  ============================================================================*)

module CFG = struct
  include Graph.Make
            (ControlPointLabel.Id)
            (struct include ControlPointLabel type t = label end)
            (Cmnd)

  let rec add_block_edge g u blk w =
    match blk with
    | inst :: (_::_ as blk) ->
        let v = add_vertex g (ControlPointLabel.mk_label inst.I.pos ((label_of w).ControlPointLabel.proc)) in
        add_edge g u (C.Inst(inst)) v ;
        add_block_edge g v blk w
    | [inst] ->
        add_edge g u (C.Inst(inst)) w
    | [] ->
        add_edge g u (C.Inst(I.mk I.Nop ((label_of u).ControlPointLabel.pos))) w

end


(*============================================================================
                             Control-Flow Points
  ============================================================================*)

module ControlPoint = struct
  include ControlPointLabel
  include CFG.Vertex

  let id cp = CFG.index_of cp
  let sort cp = (CFG.label_of cp).sort
  let pos cp = (CFG.label_of cp).pos
  let proc cp = (CFG.label_of cp).proc

end
module K = ControlPoint


(*============================================================================
                                  Procedures
  ============================================================================*)

module Proc = struct

  module Id = ProcId
  module IdHMap = HashMap.Make(Id)

  type t = {
    id       : Id.t;
    fty      : Typ.t;
    formals  : Var.t list;
    freturn  : Var.t option;
    locals   : Vars.t;
    modifs   : Vars.t;
    accessed : Vars.t;
    cfg      : CFG.graph;
    entry    : K.t;
    exit     : K.t;
  }

  let fv p =
    CFG.fold_edges
      (fun _ vs -> vs)
      (fun (_, e, _) vs -> Vars.union (C.fv e) vs)
      p.cfg
      (K.id p.entry)
      Vars.empty

  let fmt ff {id; formals; freturn; locals; modifs; accessed} =
    Format.fprintf ff "@[<hv>%a@ locals: {@[%a@]}@ modifs: {@[%a@]}@ access: {@[%a@]}@]"
      C.fmt (C.Call{Call0.proc= id; actuals= formals; areturn= freturn; typ= Typ.mkTop; targets= []})
      Vars.fmt locals Vars.fmt modifs Vars.fmt accessed

  let compare x y = Id.compare x.id y.id
  let equal x y = Id.equal x.id y.id
  let hash x = Id.hash x.id

end


(*============================================================================
                               Procedure Calls
  ============================================================================*)

module Call = struct
  include Call0

  let mk ({Proc.id; fty} as proc) actuals areturn =
    {proc; actuals; areturn; typ= fty; targets= [id]}

  let args {proc= {Proc.id; formals; freturn}; actuals; areturn} =
    let rec loop frmls_to_actls rev_actls = function
      | frml::frmls, actl::actls ->
          loop (S.add (Exp.mkVar frml) (Exp.mkVar actl) frmls_to_actls) (actl :: rev_actls) (frmls, actls)
      | [], [] ->
          (frmls_to_actls, List.rev rev_actls)
      | [], actls ->
          L.printf 1 "ignoring extra actual parameters to @[%s(...,%a)@]"
            (Proc.Id.name id) (List.fmt ",@ " Var.fmt) actls ;
          (frmls_to_actls, List.rev rev_actls)
      | _::_, [] ->
          failwithf "exec_proc: too few arguments passed to %a" Proc.Id.fmt id
    in
    let formals, actuals =
      match freturn, areturn with
      | Some(freturn), Some(areturn) -> (freturn :: formals, areturn :: actuals)
      | _ -> (formals, actuals) in
    loop S.empty [] (formals, actuals)
end


(*============================================================================
                                   Programs
  ============================================================================*)

module Prog = struct

  type t = {
    globals      : Vars.t;
    main         : Proc.Id.t;
    procs        : Proc.t Proc.IdHMap.t;
    global_setup : Proc.Id.t list;
    inits        : Proc.Id.t list;
    addr_taken   : Proc.Id.t list;
    constants    : int64 list;
  }

  module CPHMap = HashMap.Make(K)
  module ProcHMap = HashMap.Make(Proc)


  let iter_procs fn prog =
    Proc.IdHMap.iter fn prog.procs

  let map_procs fn prog =
    {prog with procs= Proc.IdHMap.map fn prog.procs}

  let fold_procs fn prog z =
    Proc.IdHMap.fold (fun _ p z -> fn p z) prog.procs z


  (** fold inter ff cf fk ck x folds over x and applies ff to every proc, cf to every recursively called proc,
      fk to every control point, and ck to every backward jump destination control point.  If [inter] is set,
      also folds over called procedures. *)
  let fold_proc, fold_cp =
    let closure procso ff cf fk ck =
      let p_memo = ProcHMap.create 31
      in
      let rec fold_p p a =
        match ProcHMap.tryfind p_memo p with
        | Some(true) ->
            a
        | Some(false) ->
            ProcHMap.add p_memo p true ;
            cf p a
        | None ->
            ProcHMap.add p_memo p false ;
            fold_k p p.Proc.entry (ff p a)
      and fold_k p k =
        let cp_memo = CPHMap.create 31
        in
        let rec loop k a =
          match CPHMap.tryfind cp_memo k with
          | Some(true) ->
              a
          | Some(false) ->
              CPHMap.add cp_memo k true ;
              ck p k a
          | None ->
              CPHMap.add cp_memo k false ;
              let a = fk p k a in
              List.fold (fun (succ,edge) acc ->
                match edge with
                | C.Inst _ ->
                    loop succ acc
                | C.Call{Call.proc} ->
                    Option.fold (fun procs acc -> loop succ (fold_p (Proc.IdHMap.find procs proc) acc)) procso acc
                | C.ICall _ ->
                    acc (* Don't approx fptrs *)
              ) (CFG.successors k) a
        in
        loop k
      in
      fold_p, fold_k
    in
    ( (fun procs ff cf fk ck -> fst (closure procs ff cf fk ck))
    , (fun procs ff cf fk ck -> snd (closure procs ff cf fk ck))
    )


  (* Dump dot file per procedure. *)
  let write_dot file ext prog =
    let fn {Proc.cfg} kid =
      match CFG.vertices_for cfg kid with
      | [k] ->
          Option.map (fun p -> (Proc.Id.name p.Proc.id, fun ff -> Proc.fmt ff p))
            (Proc.IdHMap.tryfind prog.procs (CFG.label_of k).K.proc)
      | _ ->
          None
    in
    let write_proc p =
      let fname = file ^ "." ^ (Proc.Id.name p.Proc.id) ^ ext in
      let roots = [K.id p.Proc.entry; K.id p.Proc.exit] in
      Library.with_out fname (CFG.write_dot_partitioned (fn p) p.Proc.cfg roots)
    in
    Proc.IdHMap.iter (fun _ p -> write_proc p) prog.procs


  (* Marshalling ============================================================ *)

  (* copy Typ's to re-HashCons them *)
  let rec copy_typ t = Typ.(
    match desc t with
    | Top -> mkTop
    | Bool -> mkBool
    | Named(s) -> mkNamed s
    | Int(u,s) -> mkInt u s
    | Float(s) -> mkFloat s
    | Pointer(t) -> mkPointer (copy_typ t)
    | Array(t,io,s) -> mkArray (copy_typ t) io s
    | Structure(s,fs,i) -> mkStructure s (List.map (fun (f,t) -> (f, copy_typ t)) fs) i
    | Union(s,fs,i) -> mkUnion s (List.map (fun (f,t) -> (f, copy_typ t)) fs) i
    | Enum(s,ms,i) -> mkEnum s ms i
    | Function(t,ts,b) -> mkFunction (copy_typ t) (List.map copy_typ ts) b
  )

  let copy_fld f =
    let typ = copy_typ (Fld.typ f) in
    let fld = Fld.name f in
    try fst (Fld.find_by_name typ fld |> Option.get)
    with Not_found -> failwithf "copy_fld: no fld %a in ty %a" Fld.fmt f Typ.fmt typ


  (* copy Exp's to re-HashCons them *)
  let rec copy_exp e =
    copy_exp_desc (Exp.desc e)
  and desc_copy_exp_desc d =
    Exp.desc (copy_exp_desc d)
  and copy_exp_desc d = Exp.(
    match d with
    | Var(x) -> mkVar x
    | Nil -> nil
    | App({HC.desc=Add(f)},e) -> mkAdd (copy_exp e) (copy_fld f)
    | App({HC.desc=Sub(f)},e) -> mkSub (copy_exp e) (copy_fld f)
    | App({HC.desc=App({HC.desc=Idx},i)},a) -> mkIdx (copy_exp a) (copy_exp i)
    | Bas(t) -> mkBas (copy_typ t)
    | Eq(e,f) -> mkEq (copy_exp e) (copy_exp f)
    | Num(n) -> mkNum(n)
    | Str(s) -> mkStr(s)
    | Op1(o,a) -> mkOp1 o (desc_copy_exp_desc a)
    | Op2(o,a,b) -> mkOp2 o (desc_copy_exp_desc a) (desc_copy_exp_desc b)
    | Op3(o,a,b,c) -> mkOp3 o (desc_copy_exp_desc a) (desc_copy_exp_desc b) (desc_copy_exp_desc c)
    | OpN(o,a) -> mkOpN o (Array.map desc_copy_exp_desc a)
    | App _ | Add _ | Sub _ | Idx -> assert false             (* malformed *)
  )

  let copy_inst i =
    let copy_inst_desc i = I.(
      match i with
      | Load(x,l) -> Load(x, copy_exp l)
      | Store(l,r) -> Store(copy_exp l, copy_exp r)
      | Alloc(x,e) -> Alloc(x, copy_exp e)
      | Free(e) -> Free(copy_exp e)
      | Kill(_) -> i
      | Move(x,l) -> Move(x, copy_exp l)
      | Cast(x,t,e) -> Cast(x, copy_typ t, copy_exp e)
      | Assume(e) -> Assume(copy_exp e)
      | Assert(e) -> Assert(copy_exp e)
      | Nop -> Nop
      | Generic _ -> failwith "Unmarshaling Generic commands not supported"
    )
    in
    { I.desc= copy_inst_desc i.I.desc; pos= i.I.pos }

  let copy_cmnd ct = C.(
    match ct with
    | Inst(i) -> Inst(copy_inst i)
    | Call(_) -> ct
    | ICall({Call.proc; typ} as call) -> ICall({call with Call.proc= copy_exp proc; typ= copy_typ typ})
  )

  let marshal ch p =
    Var.marshal ch ;
    Fld.marshal ch ;
    K.Id.marshal ch ;
    Proc.Id.marshal ch ;
    Marshal.to_channel ch p []

  let unmarshal ch =
      L.incf 1 "( Program.unmarshal" ;
      Timer.start unmarshal_tmr ;
    (fun _ ->
      Timer.stop unmarshal_tmr ;
      L.decf 1 ") Program.unmarshal"
    )<& let()=()in
    Var.unmarshal ch ;
    Fld.unmarshal ch ;
    K.Id.unmarshal ch ;
    Proc.Id.unmarshal ch ;
    let p = Marshal.from_channel ch in
    let reinit_cfg_edges p =
      CFG.iter_edges
        (fun _ -> ())
        (fun (v,e,v') ->
          let e' = copy_cmnd e in
          CFG.remove_edge p.Proc.cfg v e v' ;
          CFG.add_edge p.Proc.cfg v e' v')
        p.Proc.cfg
        (K.id p.Proc.entry) ;
      {p with Proc.fty= copy_typ p.Proc.fty}
    in
    Proc.IdHMap.iter (fun name proc ->
      let proc = reinit_cfg_edges proc in
      Proc.IdHMap.add p.procs name proc
    ) p.procs ;
    p

end
