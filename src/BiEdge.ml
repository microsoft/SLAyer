(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Labeled bi-directional multi-edges *)

open Library
open Variable

open SYMBOLIC_HEAP


module Poly = struct

  (* traversals *)

  let map fn {prev; frnt; back; next} =
    let prev = List.map fn prev in
    let frnt = List.map fn frnt in
    let back = List.map fn back in
    let next = List.map fn next in
    {prev; frnt; back; next}

  let map2 fn x y =
    let prev = List.map2 fn x.prev y.prev in
    let frnt = List.map2 fn x.frnt y.frnt in
    let back = List.map2 fn x.back y.back in
    let next = List.map2 fn x.next y.next in
    {prev; frnt; back; next}

  let fold fn x z =
     List.fold fn x.prev
    (List.fold fn x.frnt
    (List.fold fn x.back
    (List.fold fn x.next
     z)))

  let fold2 fn x y z =
     List.fold2 fn x.prev y.prev
    (List.fold2 fn x.frnt y.frnt
    (List.fold2 fn x.back y.back
    (List.fold2 fn x.next y.next
     z)))

  let fold_links fn {prev; frnt; back; next} z =
    let rec fold_links_ xs ys z =
      match xs, ys with
      | x::xs, y::ys -> fold_links_ xs ys (fn (x,y) z)
      | _ -> z
    in
    fold_links_ back prev (fold_links_ frnt next z)

  let fold_links2 fn x y z =
    let rec fold_links2_ vs ws xs ys z =
      match vs, ws, xs, ys with
      | v::vs, w::ws, x::xs, y::ys ->
          fold_links2_ vs ws xs ys (fn (v,w) (x,y) z)
      | _ ->
          z
    in
     fold_links2_ x.back x.prev y.back y.prev
    (fold_links2_ x.frnt x.next y.frnt y.next
     z)


  let may_allocs {frnt; back} =
    List.rev_append back frnt


  (* constructors *)

  (** [reverse] [p,f,b,n] = [n,b,f,p] *)
(*   let reverse {prev= p; frnt= f; back= b; next= n} *)
(*   = {prev= n; frnt= b; back= f; next= p} *)


  (** [between] [s, t;t', u, v;v'] [w, x;x', y, z;z']
      =                  [u, v;v',  w, x;x']          *)
  let between
     {prev=_s; frnt=_t; back= u; next= v} {prev= w; frnt= x; back=_y; next=_z}
  =                    {prev= u; frnt= v;  back= w; next= x}


  (** [append] [p, f;f', i, j;j'] [k, l;l', b, n;n']
             = [p, f;f',                    b, n;n'] *)
  (* Note: Check that args are adjacent? *)
  let append
    {prev= p; frnt= f; back=_i; next=_j} {prev=_k; frnt=_l; back= b; next= n}
  = {prev= p; frnt= f;                                      back= b; next= n}


  (** [split w x u_v_y_z] returns [(u_v_w_x, w_x_y_z)] such that
      [u_v_y_z = append (append u_v_w_x w_x_w_x) w_x_y_z] where
      [w_x_w_x] is the empty segment determined by [x] and [w]. *)
  let split                   w         x
     {prev= u; frnt= v;                    back= y; next= z} =
    ({prev= u; frnt= v; back= w; next= x},
                       {prev= w; frnt= x; back= y; next= z})


  (** [remove_prefix wy xz = (wx, yz)] such that if [wx] is empty, then
      [append wy yz = xz]. *)
  let remove_prefix
     {prev=s; frnt=t;                   back=u; next=v}
                     {prev=w; frnt=x;                   back=y; next=z} =
    ({prev=s; frnt=t; back=w; next=x}, {prev=u; frnt=v; back=y; next=z})


  (** [remove_suffix xz wy = (yz, wx)] such that if [yz] is empty, then
      [append wx xz = wy]. *)
  let remove_suffix xz wy =
    let wx, yz = remove_prefix wy xz in
    (yz, wx)

end


module Make (A: TERM) = struct

  include Poly

  type a = A.t
  type t = a edg


  (** [adjacent x y] holds if [x] and [y] are adjacent.  Eg:
      [adjacent] [p, f;f', i, j]
                          [k, l;l', b, n;n']
      holds if i = k & j = l *)
(*   let rec adjacent x y = *)
(*     match x.next, y.frnt with *)
(*     | n::next, f::frnt -> A.equal n f && adjacent {x with next} {y with frnt} *)
(*     | _ -> *)
(*     match y.prev, x.back with *)
(*     | p::prev, b::back -> A.equal p b && adjacent {x with back} {y with prev} *)
(*     | _ -> *)
(*     true *)


  let fv x =
    fold (fun a z -> Vars.union (A.fv a) z) x Vars.empty

  let map_exps fn x =
    map (fun a -> A.map_exps fn a) x

  let fold_exps fn x z =
    fold (fun a z -> A.fold_exps fn a z) x z

  let equal x y =
       List.equal A.equal x.prev y.prev
    && List.equal A.equal x.frnt y.frnt
    && List.equal A.equal x.back y.back
    && List.equal A.equal x.next y.next

  let compare x y =
    let o = List.compare A.compare x.frnt y.frnt in if o <> 0 then o else
    let o = List.compare A.compare x.back y.back in if o <> 0 then o else
    let o = List.compare A.compare x.prev y.prev in if o <> 0 then o else
            List.compare A.compare x.next y.next

  (** We write [p;p', f;f', b;b', n;n'] for
      [\{fore= \[(f,n);(f',n')\]; back= \[(b,p);(b',p')\]\}] *)
  let fmtp fxt ff {prev; frnt; back; next} =
    let fmtl f = List.fmt ";@ " f in
    Format.fprintf ff
      "@[<hv>@[<hv>@[%a@],@ @[%a@]@],@ @[<hv>@[%a@],@ @[%a@]@]@]"
      (fmtl (A.fmtp fxt)) prev (fmtl (A.fmtp fxt)) frnt
      (fmtl (A.fmtp fxt)) back (fmtl (A.fmtp fxt)) next

  let fmt ff x = fmtp (Vars.empty,Vars.empty) ff x

  let fmt_caml ff {prev; frnt; back; next} =
    Format.fprintf ff
      "@[{BiEdge.Poly.@,\
       prev= [@[%a@]];@ frnt= [@[%a@]];@ back= [@[%a@]];@ next= [@[%a@]]}@]"
      (List.fmt ";@ " A.fmt_caml) prev (List.fmt ";@ " A.fmt_caml) frnt
      (List.fmt ";@ " A.fmt_caml) back (List.fmt ";@ " A.fmt_caml) next

end
