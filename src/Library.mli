(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Extensions of the standard library.  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/index.html}Library}. *)


include module type of NSLib

include module type of NSArray
include module type of NSOption
include module type of NSList
include module type of NSSortedList
include module type of NSTuple
include module type of NSSet
include module type of NSMultiSet
(* include module type of NSPolySet *)
include module type of NSIndexedSet
include module type of NSMultiIndexedSet
include module type of NSMultiIndexedMultiSet
include module type of NSMap
include module type of NSMultiMap
include module type of NSImperativeMap
include module type of NSImperativeMultiMap
include module type of NSHashtbl
include module type of NSPolyHashMap
include module type of NSHashMap
include module type of NSHashMultiMap
include module type of NSImperativeSet
include module type of NSHashSet
include module type of NSBinaryRelation

(** {2 Extended Modules } *)
include module type of NSString
