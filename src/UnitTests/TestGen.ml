(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(* Utility code to generate stand-alone unit tests *)

open Library

open Variable
open Expression
module E = Exp
open Program


let constants = ref []
let globals = ref Vars.empty

let _ = Initialize.register (fun {Prog.constants= c; globals= g} ->
  constants := c ;
  globals := g ;
)


let gen name fmt_test =
  (* delay until exit to avoid clobbering the exception backtrace *)
  Pervasives.at_exit (fun()->
    let filename = name ^ "_" ^ Config.testname ^ ".ml" in
    let chan = open_out filename in
    let ff = Format.formatter_of_out_channel chan in
    Format.pp_set_margin ff Config.margin;
    Format.kfprintf (fun ff -> Format.pp_print_flush ff (); close_out chan)
      ff
      "open Library@\n\
       open Variable@\n\
       open Expression@\n\
       module E = Exp@\n\
       open SymbolicHeap@\n\
       open SIL@\n\
       module D = Discovery@\n\
       let _ =@\n\
       ignore( Config.parse () );@\n\
       @[<hv 2>let types = []@]@\nin@\n\
       @[<hv 2>let constants = [@,@[%a@]]@]@\nin@\n\
       @[<hv 2>let globals = @[%a@]@]@\nin@\n\
       @[<hv 2>let name = \"\"@]@\nin@\n\
       @[<hv 2>let formals = []@]@\nin@\n\
       @[<hv 2>let locals = Vars.empty@]@\nin@\n\
       @[<hv 2>let modifs = Vars.empty@]@\nin@\n\
       @[<hv 2>let cfg = Cont.dummy@]@\nin@\n\
       @[<hv 2>let exit = Cont.dummy@]@\nin@\n\
       @[<hv 2>let main = @[{Proc.name; formals; locals; modifs; cfg; exit@]@]@\nin@\n\
       @[<hv 2>let program = @[{Prog.types; constants; globals; main}@]@\nin@\n\
       Initialize.initialize program;@\n\
       %t@\n"
      (List.fmt ";@ " (fun ff c -> Format.pp_print_string ff (Int64.to_string c))) !constants
      Vars.fmt_caml !globals
      fmt_test
  )
