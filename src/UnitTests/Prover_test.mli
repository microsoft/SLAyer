(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Expression
open SymbolicHeap


val test : unit -> unit


val eq : Exp.value -> Exp.value -> XSH.t
val dq : Exp.value -> Exp.value -> XSH.t
val pt : Exp.value -> XSH.t
val ptV : Exp.value -> Exp.var -> XSH.t
val ptF : Exp.value -> Exp.value -> XSH.t
val sll : Exp.var -> Exp.value -> Exp.value -> XSH.t
