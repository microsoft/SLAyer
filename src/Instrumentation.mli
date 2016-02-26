(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Variable
open Expression
open SymbolicHeap
open Program


val instrument : Prog.t -> Analysis.t -> unit


val approximate : Vars.t -> XSH.t -> Vars.t * Exp.t
