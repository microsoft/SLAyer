(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Variable

let log = Log.std Variable.verbose
let printf x = Log.printf log x


let test () =
  printf 0 "testing Variable" ;

  let old = Var.gensym "old" () in
  let young = Var.gensym "young" () in
  assert (Var.compare old young < 0)
