(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(** Operations on sorted ['a list]. *)
module SortedList : sig
  val is_sorted : ('a->'a->int)-> 'a list -> bool
  val check_sorted : ('a->'a->int)-> 'a list -> 'a list

  (** Construct a SortedList from an unsorted list. *)
  val sort : ('a->'a->int)-> 'a list -> 'a list

  (** Adds an element into a sorted list, if it is not already a member. *)
  val add : ('a->'a->int)-> 'a -> 'a list -> 'a list

  (** Merges two sorted lists using given addition operation, keeping
      duplicates. *)
  val merge : ('a->'a->int)->
    ('a -> 'a list -> 'a list) -> 'a list -> 'a list -> 'a list

  (** Unions two sorted lists using given addition operation. *)
  val union : ('a->'a->int)->
    ('a -> 'a list -> 'a list) -> 'a list -> 'a list -> 'a list

  val inter : ('a->'a->int)-> 'a list -> 'a list -> 'a list

  val diff : ('a->'a->int)-> 'a list -> 'a list -> 'a list

  val intersect : ('a->'a->int)-> 'a list -> 'a list -> bool

  val diff_inter_diff : ('a->'a->int)->
    'a list -> 'a list -> 'a list * 'a list * 'a list

  val mem : ('a->'a->int)-> 'a -> 'a list -> bool

  (** Takes two sorted lists [xs], [ys] and returns a pair of booleans
      indicating whether [xs] is a subset of [ys], and whether [xs] is a
      superset of [ys]. *)
  val subsupset : ('a->'a->int)-> 'a list -> 'a list -> bool * bool
end
