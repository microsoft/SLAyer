(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open NSLib

(** Operations on ['a list].  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/List.html}List}. *)

module List : sig
  include module type of List

  val cons : 'a -> 'a list -> 'a list

  val tryfind : ('a -> bool) -> 'a list -> 'a option
  val no_duplicates : 'a list -> bool

  val exists_unique : ('a -> bool) -> 'a list -> bool

  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  (** Fold over the elements in the list.
      Eg: [fold f \[a0;a1;…;aN\] z] = [f aN (…(f a1 (f a0 z))…)] *)
  val foldi : (int -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b

  val map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list

  val map_to_array : ('a -> 'b) -> 'a list -> 'b array

  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
  val map3 : ('a -> 'b -> 'c -> 'z) -> 'a list -> 'b list -> 'c list -> 'z list
  val fold2 : ('a -> 'b -> 'z -> 'z) -> 'a list -> 'b list -> 'z -> 'z
  (** Fold over the corresponding elements in the two lists.  The lists must
      have the same length.
      Eg: [fold2 f \[a0;a1;…;aN\] \[b0;b1;…;bN\] z]
        = [f aN bN (…(f a1 b1 (f a0 b0 z))…)] *)

  val fold3 : ('a -> 'b -> 'c -> 'z -> 'z) -> 'a list -> 'b list -> 'c list -> 'z -> 'z

  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val compare_lex : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val compare_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> int

  val kfold : ('a -> ('b->'c) -> 'b->'c) -> 'a list -> ('b->'c) -> 'b->'c
  (** e.g. [kfold (fun x k () -> (-x) :: k ()) [1;2;3;4] (fun()-> []) ()] =
    [[-1; -2; -3; -4]] *)

  val kfold2 : ('a -> 'b -> ('c->'d) -> 'c->'d) -> 'a list -> 'b list -> ('c->'d) -> 'c->'d
  val kfold3 : ('a -> 'b -> 'c -> ('d->'e) -> 'd->'e) -> 'a list -> 'b list -> 'c list -> ('d->'e) -> 'd->'e
  val fold_pairs : ('a -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  (** e.g. [fold_pairs (fun x y l -> (x,y)::l) [1;2;3;4] []] =
      [[(1, 4); (1, 3); (1, 2); (2, 4); (2, 3); (3, 4)]] *)

  val kfold_pairs :
    ('a -> 'a -> ('b->'c) -> 'b->'c) -> ('b->'c) -> 'a list -> 'b->'c
  (** e.g. [kfold_pairs (fun x y k () -> (x,y)::k()) (fun()->[]) [1;2;3;4] ()] =
      [[(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)]] *)

  val fold_product : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  (** e.g. [fold_product (fun x y l -> (x,y)::l) [1;2;3] [4;5] []] =
      [[(1, 5); (1, 4); (2, 5); (2, 4); (3, 5); (3, 4)]] *)

  val kfold_product :
    'a list -> 'b list -> ('a -> 'b -> ('c->'d) -> 'c->'d) -> ('c->'d) -> 'c->'d
  (** e.g.
      [kfold_product (fun x y k () -> (x,y)::k()) (fun()->[]) [1;2;3] [4;5] ()]
      = [[(1, 5); (1, 4); (2, 5); (2, 4); (3, 5); (3, 4)]] *)

  val prefixes : 'a list -> 'a list list
  (** e.g. [prefixes [1;2;3]] = [[]; [1]; [1;2]; [1;2;3]] *)

  val infixes : 'a list -> 'a list list
  (** e.g. [infixes [1;2;3]] = [[[1;2;3]; [1;2]; [1]; [2;3]; [2]; [3]; []]] *)

  val powerlist : 'a list -> 'a list list
  (** e.g. [powerlist [1;2;3]] =
      [[1;2;3]; [1;2]; [1;3]; [1]; [2;3]; [2]; [3]; []] *)

  val combs :
    ('a -> 'b -> 'c list -> 'c list) -> 'a list -> 'b list -> 'c list list
  (** e.g., [combs (fun x y l -> (x,y)::l) [1;2;3] [4;5]] =
      [[[(3, 4); (2, 5)]; [(3, 4); (1, 5)]; [(2, 4); (1, 5)]; [(2, 4); (3, 5)];
        [(1, 4); (2, 5)]; [(1, 4); (3, 5)]]] *)

  val permutations :
    ('a -> 'b list list -> 'b list -> 'b list list) -> 'a list -> 'b list list
  (** [permutations xs] returns all the permutations of [xs], e.g.
      [permutations (fun x ps p -> (x :: p) :: ps) [1;2;3]] =
      [[3; 2; 1]; [3; 1; 2]; [2; 1; 3]; [2; 3; 1]; [1; 2; 3]; [1; 3; 2]] *)

  val fin_funs : 'a list -> 'b list -> ('a * 'b) list list
  (** [fin_funs [1;2;3] [4;5;6]] =
      [[(1, 4); (2, 5); (3, 6)]; [(1, 4); (2, 6); (3, 5)];
       [(1, 5); (2, 4); (3, 6)]; [(1, 5); (2, 6); (3, 4)];
       [(1, 6); (2, 4); (3, 5)]; [(1, 6); (2, 5); (3, 4)]] *)

  val map_partial : ('a -> 'b option) -> 'a list -> 'b list option

  val classify : ('a -> 'a -> bool) -> 'a list -> 'a list list
  (** [classify pred xs] partitions [xs] into classes equivalent modulo
      [pred].  Guarantees that no empty classes are returned. *)

  val divide : ('a -> 'a -> bool) -> 'a list -> 'a list list
  (** [divide pred xs] divides [xs] into the contiguous sublists of elements equivalent modulo [pred].  Guarantees
      that no empty sublists are returned. *)

  val range : int -> int -> int list
  val replicate : int -> 'a -> 'a list

  val inter : 'a list -> 'a list -> 'a list
  val union : 'a list -> 'a list -> 'a list
  val diff : 'a list -> 'a list -> 'a list

  val take : ('a -> bool) -> 'a list -> 'a * 'a list
  (** [take p l] returns the first element of [l] that satisfies [p], and the remainder of [l] *)

  val fmt :
    (unit,Format.formatter,unit)format -> 'a formatter -> 'a list formatter

  val fmtt :
    ('a, 'b, 'c, 'd, 'd, 'a) format6 ->
    Format.formatter -> (Format.formatter -> unit) list -> unit

  module Set (Elt: sig type t end) :
    (Set0 with type elt = Elt.t and type t = Elt.t list)

  module SetOrd (Elt: OrderedType) :
    (Set1 with type elt = Elt.t and type t = Elt.t list)

end
