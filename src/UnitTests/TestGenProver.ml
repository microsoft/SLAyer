(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(* Utility code to generate stand-alone unit tests *)

open Library

open Variable
open Expression
module E = Exp
open SymbolicHeap


let gen_query (id, kind, (_us, minuend, xs, subtrahend)) =
  match kind with
  | "subtract" ->
      TestGen.gen ("sub_" ^ (string_of_int id)) (fun ff ->
        Format.fprintf ff
          "let tmr = Timer.create() in Timer.start tmr ;@\n\
           try let result =@\n\
           @[<hov 2>Prover.subtract_count@\n\
             @[<hov 2>%a@]@\n@\n\
             @[<hov 2>(Vars.of_list [@[%a@]])@]@\n@\n\
             @[<hov 2>%a@]@]@\n\
           in@\n\
           Timer.stop Timer.init ;@\n\
           Statistics.report (Timer.create()) tmr (Timer.create()) ;@\n\
           Printf.printf \"RESULT: %%i\\n\" result@\n\
           @\n\
           with exc ->@\n\
             print_endline (\"\\nRESULT: Internal Error: \"^\
                            (Printexc.to_string exc)) ;@\n\
             flush_all () ;@\n\
             raise exc"
          SH.fmt_caml minuend
          (List.fmt ";@ " Var.fmt_caml) (Vars.to_list xs)
          SH.fmt_caml subtrahend
      )
  | "entails" ->
      TestGen.gen ("ent_" ^ (string_of_int id)) (fun ff ->
        Format.fprintf ff
          "let tmr = Timer.create() in Timer.start tmr ;@\n\
           try let result =@\n\
           @[<hov 2>Prover.entails@\n\
             @[<hov 2>%a@]@\n@\n\
             @[<hov 2>(Vars.of_list [@[%a@]])@]@\n@\n\
             @[<hov 2>%a@]@]@\n\
           in@\n\
           Timer.stop Timer.init ;@\n\
           Statistics.report (Timer.create()) tmr (Timer.create()) ;@\n\
           Printf.printf \"RESULT: %%B\\n\" (None <> result)@\n\
           @\n\
           with exc ->@\n\
             print_endline (\"\\nRESULT: Internal Error: \"^\
                            (Printexc.to_string exc)) ;@\n\
             flush_all () ;@\n\
             raise exc"
          SH.fmt_caml minuend
          (List.fmt ";@ " Var.fmt_caml) (Vars.to_list xs)
          SH.fmt_caml subtrahend
      )
  | "inconsistent" ->
      TestGen.gen ("sat_" ^ (string_of_int id)) (fun ff ->
        Format.fprintf ff
          "let tmr = Timer.create() in Timer.start tmr ;@\n\
           try let result =@\n\
           @[<hov 2>Prover.inconsistent@\n\
             @[<hov 2>%a@]@]@\n\
           in@\n\
           Timer.stop Timer.init ;@\n\
           Statistics.report (Timer.create()) tmr (Timer.create()) ;@\n\
           Printf.printf \"RESULT: %%B\\n\" result@\n\
           @\n\
           with exc ->@\n\
             print_endline (\"\\nRESULT: Internal Error: \"^\
                            (Printexc.to_string exc)) ;@\n\
             flush_all () ;@\n\
             raise exc"
          SH.fmt_caml minuend
      )
  | _ -> ()
