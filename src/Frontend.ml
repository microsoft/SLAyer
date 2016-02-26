(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

open Library

open Program

module L = (val Log.std Config.vFE : Log.LOG)


let _ = try (
  (* turn on backtrace printing in debug mode *)
  assert(true$> Printexc.record_backtrace true );

  (* Set a handler for Ctrl-C. *)
  Sys.catch_break true ;

  L.printf 1 "%s@." (String.concat " " (Array.to_list Sys.argv)) ;
  Pervasives.flush_all () ;

  if Config.version_only then (

    Printf.printf "SLAyer frontend %s" Version.version ;

    exit 0

  ) else (

    let program = Frontend_esp.program_of_file Config.filenames in

    if Config.write_cfg then Prog.write_dot Config.testname ".fe.cfg.dot" program ;

    let program = if Config.norm_in_frontend then TransformProgram.normalize program else program in

    Library.with_out_bin (Config.testname^".sil") Prog.marshal program

  )
) with exc ->
  if Config.raise_exceptions then (
    prerr_endline (Printexc.to_string exc) ;
    raise exc
  ) else (
    print_endline ("\nRESULT: Internal Error: "^(Printexc.to_string exc)) ;
    flush_all () ;
    exit 1
  )
