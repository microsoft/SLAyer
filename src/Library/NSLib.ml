(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(* Non-Standard Library *)


(*============================================================================
                                 Combinators
  ============================================================================*)

(* Function combinators *)

let id x = x
let const f _ = f
let flip f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

let ( &> ) x f = f x ; x
let ( <& ) f x = f x ; x
let ( $> ) x y = y; x


(* Tuple combinators *)

let pair x y = (x,y)
let swap (x,y) = (y,x)
let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let thd3 (_,_,z) = z
let fst4 (w,_,_,_) = w
let snd4 (_,x,_,_) = x
let thd4 (_,_,y,_) = y
let fth4 (_,_,_,z) = z

let ( *** ) f g (x,y) = (f x, g y)


(* Predicate combinators *)

let ( &&& ) p q x = p x && q x
let ( ||| ) p q x = p x || q x


(* Equality combinators *)

let equal_tup2 equal0 equal1 (x0,x1) (y0,y1) = equal0 x0 y0 && equal1 x1 y1

let equal_tup3 equal0 equal1 equal2 (x0,x1,x2) (y0,y1,y2) = equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2

let equal_tup4 equal0 equal1 equal2 equal3 (x0,x1,x2,x3) (y0,y1,y2,y3) = equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3

let equal_tup5 equal0 equal1 equal2 equal3 equal4 (x0,x1,x2,x3,x4) (y0,y1,y2,y3,y4) =
  equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3 && equal4 x4 y4

let equal_tup6 equal0 equal1 equal2 equal3 equal4 equal5 (x0,x1,x2,x3,x4,x5) (y0,y1,y2,y3,y4,y5) =
  equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3 && equal4 x4 y4 && equal5 x5 y5

let equal_tup7 equal0 equal1 equal2 equal3 equal4 equal5 equal6 (x0,x1,x2,x3,x4,x5,x6) (y0,y1,y2,y3,y4,y5,y6) =
  equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3 && equal4 x4 y4 && equal5 x5 y5 && equal6 x6 y6

let equal_tup8 equal0 equal1 equal2 equal3 equal4 equal5 equal6 equal7 (x0,x1,x2,x3,x4,x5,x6,x7) (y0,y1,y2,y3,y4,y5,y6,y7) =
  equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3 && equal4 x4 y4 && equal5 x5 y5 && equal6 x6 y6 && equal7 x7 y7

let equal_tup9 equal0 equal1 equal2 equal3 equal4 equal5 equal6 equal7 equal8 (x0,x1,x2,x3,x4,x5,x6,x7,x8) (y0,y1,y2,y3,y4,y5,y6,y7,y8) =
  equal0 x0 y0 && equal1 x1 y1 && equal2 x2 y2 && equal3 x3 y3 && equal4 x4 y4 && equal5 x5 y5 && equal6 x6 y6 && equal7 x7 y7 && equal8 x8 y8


(* Comparison combinators *)

let compare_tup2 compare0 compare1 (x0,x1) (y0,y1) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else compare1 x1 y1

let compare_tup3 compare0 compare1 compare2 (x0,x1,x2) (y0,y1,y2) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
            compare2 x2 y2

let compare_tup4 compare0 compare1 compare2 compare3 (x0,x1,x2,x3) (y0,y1,y2,y3) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
            compare3 x3 y3

let compare_tup5 compare0 compare1 compare2 compare3 compare4 (x0,x1,x2,x3,x4) (y0,y1,y2,y3,y4) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
  let ord = compare3 x3 y3 in if ord <> 0 then ord else
            compare4 x4 y4

let compare_tup6 compare0 compare1 compare2 compare3 compare4 compare5 (x0,x1,x2,x3,x4,x5) (y0,y1,y2,y3,y4,y5) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
  let ord = compare3 x3 y3 in if ord <> 0 then ord else
  let ord = compare4 x4 y4 in if ord <> 0 then ord else
            compare5 x5 y5

let compare_tup7 compare0 compare1 compare2 compare3 compare4 compare5 compare6 (x0,x1,x2,x3,x4,x5,x6) (y0,y1,y2,y3,y4,y5,y6) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
  let ord = compare3 x3 y3 in if ord <> 0 then ord else
  let ord = compare4 x4 y4 in if ord <> 0 then ord else
  let ord = compare5 x5 y5 in if ord <> 0 then ord else
            compare6 x6 y6

let compare_tup8 compare0 compare1 compare2 compare3 compare4 compare5 compare6 compare7 (x0,x1,x2,x3,x4,x5,x6,x7) (y0,y1,y2,y3,y4,y5,y6,y7) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
  let ord = compare3 x3 y3 in if ord <> 0 then ord else
  let ord = compare4 x4 y4 in if ord <> 0 then ord else
  let ord = compare5 x5 y5 in if ord <> 0 then ord else
  let ord = compare6 x6 y6 in if ord <> 0 then ord else
            compare7 x7 y7

let compare_tup9 compare0 compare1 compare2 compare3 compare4 compare5 compare6 compare7 compare8 (x0,x1,x2,x3,x4,x5,x6,x7,x8) (y0,y1,y2,y3,y4,y5,y6,y7,y8) =
  let ord = compare0 x0 y0 in if ord <> 0 then ord else
  let ord = compare1 x1 y1 in if ord <> 0 then ord else
  let ord = compare2 x2 y2 in if ord <> 0 then ord else
  let ord = compare3 x3 y3 in if ord <> 0 then ord else
  let ord = compare4 x4 y4 in if ord <> 0 then ord else
  let ord = compare5 x5 y5 in if ord <> 0 then ord else
  let ord = compare6 x6 y6 in if ord <> 0 then ord else
  let ord = compare7 x7 y7 in if ord <> 0 then ord else
            compare8 x8 y8


(* File handling *)

let with_in_bin filename fn =
  let chan = open_in_bin filename in
  let res = fn chan in
  close_in chan ;
  res

let with_out_bin filename fn arg =
  let chan = open_out_bin filename in
  let res = fn chan arg in
  close_out chan ;
  res

let with_out filename outputter =
  let buf = Buffer.create 128 in
  let res = outputter buf in
  let chan = open_out filename in
  Buffer.output_buffer chan buf ;
  close_out chan ;
  res


(* Formatting *)

type 'a formatter = Format.formatter -> 'a -> unit
type 'a format_str = ('a formatter -> 'a -> unit, Format.formatter, unit) format

let ifbreakf fmt ff =
  Format.pp_print_if_newline ff () ;
  Format.fprintf ff fmt

let failwithf fmt =
  Format.kfprintf (fun _ -> failwith (Format.flush_str_formatter ()))
    Format.str_formatter ("@\n@["^^fmt^^"@]@\n")

let invalid_argf fmt =
  Format.kfprintf (fun _ -> invalid_arg (Format.flush_str_formatter ()))
    Format.str_formatter ("@\n@["^^fmt^^"@]@\n")

exception Nothing_to_fmt


(* Exception handling *)

exception Undef

let try_finally f g =
  let res =
    try
      f ()
    with e ->
      g () ;
      raise e
  in
  g () ;
  res

let finally_try g f =
  let res =
    try
      f ()
    with e ->
      prerr_endline (Printexc.to_string e) ;
      g () ;
      raise e
  in
  g () ;
  res

let debug_wrap1 verbose shift fn z =
  try fn z with _ -> verbose := !verbose + shift ; fn z

let debug_wrap2 verbose shift fn y z =
  try fn y z with _ -> verbose := !verbose + shift ; fn y z

let debug_wrap3 verbose shift fn x y z =
  try fn x y z with _ -> verbose := !verbose + shift ; fn x y z

let debug_wrap4 verbose shift fn w x y z =
  try fn w x y z with _ -> verbose := !verbose + shift ; fn w x y z

let debug_wrap5 verbose shift fn v w x y z =
  try fn v w x y z with _ -> verbose := !verbose + shift ; fn v w x y z


(* Module Types *)

module type EqualityType = sig
  type t
  val equal: t -> t -> bool
end

module type OrderedType = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module type HashedType = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module HashedTypeTup2 (H0: HashedType) (H1: HashedType) = struct
  type t = H0.t * H1.t
  let equal = equal_tup2 H0.equal H1.equal
  let hash (x,y) = Hashtbl.hash (H0.hash x, H1.hash y)
end

module type Set0 = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map_fold : (elt * 'z -> elt * 'z) -> t * 'z -> t * 'z
  val kfold : t -> (elt -> ('a->'b) -> 'a->'b) -> ('a->'b) -> 'a->'b
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val exists_unique : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val cardinal : t -> int
  val of_list : elt list -> t
  val to_list : t -> elt list
  val choose : t -> elt
  val union : t -> t -> t
  val diff : t -> t -> t
end

module type Set1 = sig
  include Set0
  include OrderedType with type t := t
  val remove : elt -> t -> t
  val diff_inter_diff : t -> t -> t * t * t
end
