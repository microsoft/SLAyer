(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Expression to expression substitutions *)

open Library

open Variable
open Expression
module E = Exp

module L = (val Log.std Config.vSubst : Log.LOG)



(*============================================================================
                                 Substitution
  ============================================================================*)

type t = E.t ExpMap.t


let equal x y = ExpMap.equal E.equal x y

let compare x y = ExpMap.compare E.compare x y


let fold fn x = ExpMap.fold fn x

let iter fn x = ExpMap.iter fn x

let to_assoc x = fold (fun k v al -> (k,v) :: al) x []

let to_exp s = E.mkAnd (ExpMap.map_to_array (fun x y -> E.mkEq x y) s)


let fmt ff s =
  let aux ff (x,e) = Format.fprintf ff "@[%a/%a@]" E.fmt e E.fmt x in
  Format.fprintf ff "@[<hov 1>[%a]@]" (List.fmt ",@ " aux)
    (to_assoc s)


let empty = ExpMap.empty

let is_empty x = ExpMap.is_empty x


let add k v s = if E.equal k v then s else ExpMap.add k v s

let add_id k v s = ExpMap.add k v s

let singleton k v = add k v empty


let find x s = let x' = ExpMap.find x s in if E.equal x x' then x else x'

let tryfind x s = try Some(find x s) with Not_found -> None


let subst s x = E.pmap (fun x -> tryfind x s) x


let of_assoc kvl = List.fold (fun (k,v) s -> add k v s) kvl empty


let in_dom x s = ExpMap.mem x s

let dom x = fold (fun k _ es -> Exps.add k es) x Exps.empty

let rng x = fold (fun _ v es -> Exps.add v es) x Exps.empty


let fv s =
  fold (fun e f -> Vars.union (Vars.union (E.fv e) (E.fv f))) s Vars.empty


let compose e_x f_y =
(*   L.incf 0 "( S.compose: %a %a" fmt e_x fmt f_y ; L.decf 0 ") S.compose: %a" fmt <& *)
  ExpMap.merge (fun _k eo fo ->
(*     L.printf 0 "k: %a  e: %a  f: %a  g: %a" *)
(*       E.fmt _k (Option.fmt "-" E.fmt) eo (Option.fmt "-" E.fmt) fo (Option.fmt "-" E.fmt) <& *)
    match eo with
    | Some(e) -> Some(subst f_y e)
    | None -> fo
  ) e_x f_y


let remove x s = ExpMap.remove x s


let remove_vs vs s =
  fold (fun x _ m ->
    if Vars.intersect (E.fv x) vs
    then remove x m
    else m
  ) s s


(* Note: Is this the right definition, or should Vars.subset be changed? *)
let restrict vs s =
  fold (fun x _ m ->
    if Vars.subset (E.fv x) vs
    then m
    else remove x m
  ) s s

let restrict_rng vs s =
  fold (fun x e m ->
    if Vars.subset (E.fv e) vs
    then m
    else remove x m
  ) s s


let meet s t =
  fold (fun x e u ->
    match tryfind x t with
    | Some(f) when E.equal e f -> add x e u
    | _                        -> u
  ) s empty


let diff s t =
  fold (fun x e u ->
    match tryfind x t with
    | Some(f) when E.equal e f -> u
    | _                        -> add x e u
  ) s empty
