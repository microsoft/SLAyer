(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


(** Operations on strings.  See also standard
    {{:file:../../../doc/ocaml%20manual/libref/String.html}String}. *)
module String : sig
  include module type of String
  val filteri : (int -> char -> bool) -> string -> string
end
