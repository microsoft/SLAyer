(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Discovery of patterns for parametric inductive predicates *)

open Library

open Type
open Variable
open Expression
module E = Exp
module S = Substitution
open SYMBOLIC_HEAP
open SymbolicHeap
open Program

module L = (val Log.std Config.vDiscovery : Log.LOG)
module LSH = (val Log.std Config.vSH : Log.LOG)


(*============================================================================
                                   Patterns
  ============================================================================*)

(** Patn constructor support *)

let deref_typ ty =
  match Typ.desc ty with
  | Typ.Pointer(t) -> t
  | _ -> invalid_arg "Discovery.deref_typ"


let name_of_path path =
  String.concat "." (List.rev_map Fld.name path)

let param_of_path path =
  let name = name_of_path path in
  let p = Var.gensym ("p"^name) Var.PointerSort in
  let f = Var.gensym ("f"^name) Var.PointerSort in
  let b = Var.gensym ("b"^name) Var.PointerSort in
  let n = Var.gensym ("n"^name) Var.PointerSort in
  (p, f, b, n)

let pt_of_path base typ cnts path =
  let loc = E.mkAdds base path in
  let off = Off.mkPath typ path in
  let cnt = try Some(List.assoc path cnts) with Not_found -> None in
  {Pt.loc; off; cnt}

let obj_of_typ typ cnts pts =
  let paths = Typ.all_offsets typ in
  let name =
    match paths with
    | [] -> "void"
    | (_,[],_)::_ -> "empty"
    | (_,[field],_)::_ -> name_of_path [field]
    | (_,_::prefix,_)::_ -> name_of_path prefix in
  let paths = if paths <> [] then paths else [((0,None), [], Typ.mkTop)] in
  let base = E.mkVar (Var.gensym name Var.PointerSort) in
  let pts =
    List.fold (fun (_,path,_) pts ->
      pt_of_path base typ cnts path :: pts
    ) paths pts in
  (base, pts)


let mk_sll id mk_body =
  L.printf 2 "Trying to add %s..." id ;
  (fun p -> L.shift_verb (-1) (fun () -> L.printf 2 "@[<hov 2>Adding %s =@ %a@]" id Patn.fmt p))
  <&
  let typ = Typ.find_by_name id |> Option.get in
  let (_,base,_), paths = List.take (fun _ -> true) (Typ.all_offsets typ) in
  let _pv, fv, _bv, nv = param_of_path (List.tl base) in
  let f, n = E.mkVar fv, E.mkVar nv in
  let us, eqs, cnts, pts = mk_body typ f n in
  let params, eqs, pts =
    List.fold_right (fun (_,path,_) ({prev; frnt; back; next}, eqs, pts) ->
      let _p', f', _b', n' = param_of_path path in
      ( {prev; frnt= f'::frnt; back; next= n'::next}
      , E.mkEq (E.mkVar f') (E.mkAdds f path) ::
        E.mkEq (E.mkVar n') (E.mkAdds n path) :: eqs
      , pt_of_path f typ cnts path :: pts
      )
    ) paths ({prev= []; frnt= []; back=[]; next= []}, eqs, pts) in
  let {prev; frnt; back; next} = params in
  let params = {prev; frnt= fv::frnt; back; next= nv::next} in
  let pts = pt_of_path f typ cnts base :: pts in
  let body = SH.Pf.star eqs (SH.PtS.star pts SH.emp) in
  Patn.mk ~name:id params
    (SH.exists_intro (Vars.diff (SH.fv body) (Vars.union us (Params.fv params)))
       body)


let mk_dll id mk_body =
  L.printf 2 "Trying to add %s..." id ;
  (fun p -> L.shift_verb (-1) (fun () -> L.printf 2 "@[<hov 2>Adding %s =@ %a@]" id Patn.fmt p))
  <&
  let typ = Typ.find_by_name id |> Option.get in
  let (_,base,_), paths = List.take (fun _ -> true) (Typ.all_offsets typ) in
  let pv, fv, bv, nv = param_of_path (List.tl base) in
  let p, f, b, n = E.mkVar pv, E.mkVar fv, E.mkVar bv, E.mkVar nv in
  let us, eqs, cnts, pts = mk_body typ p f b n in
  let params, eqs, pts =
    List.fold_right (fun (_,path,_) ({prev; frnt; back; next}, eqs, pts) ->
      let p', f', b', n' = param_of_path path in
      ( {prev= p'::prev; frnt= f'::frnt; back=b'::back; next= n'::next}
      , E.mkEq (E.mkVar p') (E.mkAdds p path) ::
        E.mkEq (E.mkVar f') (E.mkAdds f path) ::
        E.mkEq (E.mkVar b') (E.mkAdds b path) ::
        E.mkEq (E.mkVar n') (E.mkAdds n path) :: eqs
      , pt_of_path f typ cnts path :: pts
      )
    ) paths ({prev= []; frnt= []; back=[]; next= []}, eqs, pts) in
  let {prev; frnt; back; next} = params in
  let params = {prev= pv::prev; frnt= fv::frnt; back=bv::back; next= nv::next} in
  let pts = pt_of_path f typ cnts base :: pts in
  let body = SH.Pf.star eqs (SH.PtS.star pts SH.emp) in
  Patn.mk ~name:id params
    (SH.exists_intro (Vars.diff (SH.fv body) (Vars.union us (Params.fv params)))
       body)


(** Patn constructors *)

let mk_patn_sll id =
  mk_sll id (fun sll _f n ->
    let fFlink, _ = Fld.find_by_name sll "Flink" |> Option.get in
    let us = Vars.empty in
    let eqs = [] in
    let cnts = [([fFlink], n)] in
    let pts = [] in
    (us, eqs, cnts, pts)
  )


let mk_patn_dll id =
  mk_dll id (fun dll p f b n ->
    let fFlink, _ = Fld.find_by_name dll "Flink" |> Option.get in
    let fBlink, _ = Fld.find_by_name dll "Blink" |> Option.get in
    let us = Vars.empty in
    let eqs = [E.mkEq f b] in
    let cnts = [([fFlink], n); ([fBlink], p)] in
    let pts = [] in
    (us, eqs, cnts, pts)
  )


let mk_patn_crom_data id =
  mk_dll id (fun cd p f b n ->
    let fCromList, le = Fld.find_by_name cd "CromList" |> Option.get in
    let fFlink, _ = Fld.find_by_name le "Flink" |> Option.get in
    let fBlink, _ = Fld.find_by_name le "Blink" |> Option.get in
    let fBuffer, pvoid = Fld.find_by_name cd "Buffer" |> Option.get in
    let fpMdl, pmdl = Fld.find_by_name cd "pMdl" |> Option.get in
    let us = Vars.empty in
    let pts = [] in
    let mdl, pts = obj_of_typ (deref_typ pmdl) [] pts in
    let buffer, pts = obj_of_typ (deref_typ pvoid) [] pts in
    let eqs = [E.mkEq f b] in
    let cnts =
      [ ([fFlink;fCromList], n)
      ; ([fBlink;fCromList], p)
      ; ([fBuffer], buffer)
      ; ([fpMdl], mdl)
      ] in
    (us, eqs, cnts, pts)
  )


let mk_patn_async_address_data id _devExt =
  mk_dll id (fun aad p f b n ->
    let fAsyncAddressList, le = Fld.find_by_name aad "AsyncAddressList" |> Option.get in
    let fFlink, _ = Fld.find_by_name le "Flink" |> Option.get in
    let fBlink, _ = Fld.find_by_name le "Blink" |> Option.get in
    let fBuffer, pvoid = Fld.find_by_name aad "Buffer" |> Option.get in
    let fAddressRange, par = Fld.find_by_name aad "AddressRange" |> Option.get in
    let fpMdl, pmdl = Fld.find_by_name aad "pMdl" |> Option.get in
    let us = Vars.empty in
    let pts = [] in
    let mdl, pts = obj_of_typ (deref_typ pmdl) [] pts in
    let addressRange, pts = obj_of_typ (deref_typ par) [] pts in
    let buffer, pts = obj_of_typ (deref_typ pvoid) [] pts in
    let eqs = [E.mkEq f b] in
    let cnts =
      [ ([fFlink;fAsyncAddressList], n)
      ; ([fBlink;fAsyncAddressList], p)
      ; ([fBuffer], buffer)
      ; ([fAddressRange], addressRange)
      ; ([fpMdl], mdl)
      ] in
    (us, eqs, cnts, pts)
  )


let mk_patn_isoch_detach_data id devExt =
  mk_dll id (fun idd p f b n ->
    let fIsochDetachList, le = Fld.find_by_name idd "IsochDetachList" |> Option.get in
    let fFlink, _ = Fld.find_by_name le "Flink" |> Option.get in
    let fBlink, _ = Fld.find_by_name le "Blink" |> Option.get in
    let fDeviceExtension, _ = Fld.find_by_name idd "DeviceExtension" |> Option.get in
    let us = Vars.singleton devExt in
    let pts = [] in
    let eqs = [E.mkEq f b] in
    let cnts =
      [ ([fFlink;fIsochDetachList], n)
      ; ([fBlink;fIsochDetachList], p)
      ; ([fDeviceExtension], E.mkVar devExt)
      ] in
    (us, eqs, cnts, pts)
  )


let mk_patn_isoch_resource_data id =
  mk_dll id (fun idd p f b n ->
    let fIsochResourceList, le = Fld.find_by_name idd "IsochResourceList" |> Option.get in
    let fFlink, _ = Fld.find_by_name le "Flink" |> Option.get in
    let fBlink, _ = Fld.find_by_name le "Blink" |> Option.get in
    let us = Vars.empty in
    let pts = [] in
    let eqs = [E.mkEq f b] in
    let cnts =
      [ ([fFlink;fIsochResourceList], n)
      ; ([fBlink;fIsochResourceList], p)
      ] in
    (us, eqs, cnts, pts)
  )



(*============================================================================
                                Initialization
  ============================================================================*)

(** Patn ids *)

(* SI: the AAD and IDD patterns are hard-coded to pick up the
   harness.h-generated SL_Context_DEVICE_EXTENSION device extension. *)
let id_sll = "_SLL_ENTRY"
let id_dll = "_LIST_ENTRY"
let id_crom_data = "_CROM_DATA"
let id_async_address_data = "_ASYNC_ADDRESS_DATA"
let id_isoch_detach_data = "_ISOCH_DETACH_DATA"
let id_isoch_resource_data = "_ISOCH_RESOURCE_DATA"


(** patn.name->patn dictionary *)
(* Only use after [initialize] has run. *)
let patn_dict = ref []

let patn_dict_add (id,p) =
  patn_dict := (id,p) :: !patn_dict


let _ = Initialize.register (fun {Prog.globals} ->
  L.incf 1 "( Discovery.init: adding patns to discovery list" ;
  L.decf 1 ") Discovery.init"
  $>
  let sl_context_var =
    let the_context_var =
      Vars.fold (fun v result ->
        if ("SL_Context_DEVICE_EXTENSION" = (Var.name v)) then (Some v) else result
      ) globals None in
    match the_context_var with
    | Some(v) -> v
    | None ->
        L.printf 1 "no SL_Context declared." ;
        Var.gensym "DummySLContext" Var.PointerSort
  in
  try patn_dict_add (id_crom_data, mk_patn_crom_data id_crom_data)
  with Not_found -> () ;
  try patn_dict_add (id_async_address_data, mk_patn_async_address_data id_async_address_data sl_context_var)
  with Not_found -> () ;
  try patn_dict_add (id_isoch_detach_data, mk_patn_isoch_detach_data id_isoch_detach_data sl_context_var)
  with Not_found -> () ;
  try patn_dict_add (id_isoch_resource_data, mk_patn_isoch_resource_data id_isoch_resource_data)
  with Not_found -> () ;
  try patn_dict_add (id_sll, mk_patn_sll id_sll)
  with Not_found -> () ;
  try patn_dict_add (id_dll, mk_patn_dll id_dll)
  with Not_found -> () ;
)


(*============================================================================
                                   Queries
  ============================================================================*)

type result = Done | More of Patn.t * (unit -> result)

let discover _ =
  let rec aux = function
    | []          -> Done
    | (_,x) :: xl -> More(x, fun()-> aux xl)
  in
  aux !patn_dict

let fold fn q a =
  let rec fold_fn acc res =
    match res () with
    | Done          -> acc
    | More(pat,res) -> fold_fn (fn pat acc) res
  in
  fold_fn a (fun()-> discover q)
