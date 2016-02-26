(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib
open NSMap


module MultiMap = struct
  module type S = sig
    type k
    type v
    type vs
    type t

    val empty : t
    val add : k -> v -> t -> t
    val union : k -> vs -> t -> t
    val diff : k -> vs -> t -> t
    val replace : k -> vs -> t -> t
    val remove : k -> t -> t

    val is_empty : t -> bool
    val length : t -> int
    val mem : k -> t -> bool
    val find : k -> t -> vs

    val iter : (k -> v -> unit) -> t -> unit
    val iter_keys : (k -> vs -> unit) -> t -> unit
    val exists : (k -> v -> bool) -> t -> bool
    val existsi : k -> (v -> bool) -> t -> bool
    val filter : (k -> v -> bool) -> t -> t
    val filteri : k -> (v -> bool) -> t -> t
    val map : (v -> v) -> t -> t
    val mapi : (k -> v -> v) -> t -> t
    val fold : (k -> v -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_keys : (k -> vs -> 'a -> 'a) -> t -> 'a -> 'a
    val to_list : t -> (k * v) list

(*     val equal : t -> t -> bool *)
(*     val compare : t -> t -> int *)
  end

  module Make (Key: OrderedType) (ValSet: Set0) = struct
    module M = Map.Make(Key)
    type k = Key.t
    type v = ValSet.elt
    type vs = ValSet.t
    type t = vs M.t

    module S = ValSet

    let empty = M.empty

    let add k v t =
      match M.tryfind k t with
      | None -> M.add k (S.singleton v) t
      | Some(vs) -> M.add k (S.add v vs) t

    let union k vs t =
      if S.is_empty vs then t else
      match M.tryfind k t with
      | None -> M.add k vs t
      | Some(us) -> M.add k (S.union us vs) t

    let diff k vs t =
      if S.is_empty vs then t else
      match M.tryfind k t with
      | None -> t
      | Some(us) -> M.add k (S.diff us vs) t

    let replace k vs t = M.add k vs t

    let remove = M.remove

    let is_empty = M.is_empty

    let mem = M.mem

    let find k t = try M.find k t with Not_found -> S.empty

    let iter f t = M.iter (fun k vs -> S.iter (f k) vs) t

    let iter_keys f t = M.iter f t

    let exists p t = M.exists (fun k vs -> S.exists (p k) vs) t

    let existsi k p t = S.exists p (find k t)

    let filter p t =
      M.fold (fun k vs t ->
        let vs' = S.filter (p k) vs in
        if S.is_empty vs'
        then M.remove k t
        else M.add k vs' t
      ) t t

    let filteri k p t =
      match M.tryfind k t with
      | None -> t
      | Some(vs) ->
          let vs' = S.filter p vs in
          if S.is_empty vs'
          then M.remove k t
          else M.add k vs' t

    let map f t = M.map (fun vs -> S.map f vs) t

    let mapi f t = M.mapi (fun k vs -> S.map (f k) vs) t

    let fold f = M.fold (fun k -> S.fold (f k))

    let fold_keys f = M.fold f

    let length m = fold (fun _ _ n -> n+1) m 0

    let to_list m = fold (fun k v es -> (k,v) :: es) m []

(*     let compare = M.compare S.compare *)
(*     let equal = M.equal S.equal *)

  end
end
