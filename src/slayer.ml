(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Main routine of analyzer *)

open Library

open Program
module K = ControlPoint


type safety_result_t =
  | Safe
  | PossiblyUnsafe
  | Unsafe

type general_results = {
  (* ID *)
  tool_version: string;
  (* exn *)
  internal_error: exn option; (* if Some, then exn field is valid. If None, then the other fields are valid. *)
  (* results *)
  safety_proved: bool; unsafety_proved: bool;
  termination_proved: bool; nontermination_proved: bool;
  leaks: bool;
  hit_limit: bool;
  (* run time *)
  time_total: float;
  time_preanalysis: float; time_analysis: float; time_postanalysis: float;
  time_1: float; time_2: float; time_3: float; (* arbitrary *)
}


let success =
  { tool_version= Version.version;
    internal_error= None;
    safety_proved= false;  unsafety_proved= false;
    termination_proved= false; nontermination_proved= false;
    leaks= false;
    hit_limit= false;
    time_total= -0.; time_preanalysis= -0.; time_analysis= -0.; time_postanalysis= -0.;
    time_1= -0.; time_2= -0.; time_3= -0.;
  }


let str_of_general_results_old r =
  match r.internal_error with
  | Some(exn) ->
      "\nRESULT: Internal Error: "^(Printexc.to_string exn)
  | None ->
      "\nRESULT: " ^
        (if r.hit_limit then "HIT LIMIT" else
         (if r.safety_proved then "SAFE" else if r.unsafety_proved then "UNSAFE" else "POSSIBLY UNSAFE") ^
         (if r.safety_proved && r.leaks then ", MAY LEAK" else "") ^
         (if r.termination_proved then "" else if r.nontermination_proved then ", MUST DIVERGE" else ""))

let str_of_general_results r =
  Format.sprintf "(result (tool_version %s) (internal_error %b) (slayer_exn %s) (safety_proved %b) (unsafety_proved %b) (termination_proved %b) (nontermination_proved %b) (leaks %b) (hit_limit %b) (time_total %f) (time_preanalysis %f) (time_analysis %f) (time_postanalysis %f) (time_1 %f) (time_2 %f) (time_3 %f))"
    r.tool_version
    (r.internal_error <> None)
    (match r.internal_error with Some e -> Printexc.to_string e | _ -> "None")
    r.safety_proved r.unsafety_proved
    r.termination_proved r.nontermination_proved
    r.leaks
    r.hit_limit
    r.time_total
    r.time_preanalysis r.time_analysis r.time_postanalysis
    r.time_1 r.time_2 r.time_3


let pre_analysis_tmr = Timer.create "pre-analysis"
let analysis_tmr = Timer.create "analysis"
let post_analysis_tmr = Timer.create "post-analysis"


let results = try (
  (* turn on backtrace printing in debug mode *)
  assert(true$> Printexc.record_backtrace true );

  (* set size of minor heap *)
  Gc.set {(Gc.get()) with Gc.minor_heap_size= Config.minor_heap_size*1024*1024*8/Sys.word_size} ;

  (* Set a handler for Ctrl-C. *)
  Sys.catch_break true ;

  if Config.version_only then
    let dbg = ref "" in
    assert(true$>( dbg := " debug" ));
    let maj, min, bld, _rev = Z3.get_version () in
    Printf.printf "SLAyer %s%s (Z3 dll v%i.%i.%i)\n" Version.version !dbg maj min bld ;
    exit 0
  else (

    Timer.start pre_analysis_tmr ;

    let read_sil file =
      let program = Library.with_in_bin file Prog.unmarshal in
      if Config.norm_in_frontend then program else TransformProgram.normalize program
    in

    let program =
      try
        let first_fname = List.hd Config.filenames in
        if Filename.check_suffix first_fname ".sil" then
          read_sil first_fname
        else
          let args =
            Array.of_list (
                "frontend"
              :: Config.filenames
               @ "-fe_norm"
              :: (if Config.norm_in_frontend then "true" else "false")
              :: Config.frontend_args) in
          let test_sil = Config.testname^".sil" in
          if Sys.file_exists test_sil then Sys.remove test_sil ;
          match Unix.waitpid [] (Unix.create_process "frontend" args Unix.stdin Unix.stdout Unix.stderr) with
          | _,Unix.WEXITED(0) -> read_sil test_sil
          | _ -> exit 1
      with Sys_error(err) ->
        prerr_endline err ;
        exit 1
    in

    if Config.write_cfg then Prog.write_dot Config.testname ".cfg.dot" program ;

    if Config.compile_only then
      exit 0
    else (

      Initialize.initialize program ;

      Timer.stop pre_analysis_tmr ;
      Timer.start analysis_tmr ;

      let results = Analysis.exec_prog program (Analysis.init program) in

      Timer.stop analysis_tmr ;
      Timer.start post_analysis_tmr ;

      if Config.report_dead_code then (
        let dead_code = Analysis.dead results in
        if dead_code <> [] then
          Format.printf "@[<v 2>Detected dead code:@ @[%a@]@]@\n" (List.fmt "@\n" Position.fmt) dead_code
      );

      let safety =
        if CounterExample.disprove results then
          Unsafe
        else if Analysis.safe results then
          Safe
        else
          PossiblyUnsafe
      in

      Instrumentation.instrument program results ;

      { success with
        safety_proved= (safety = Safe);
        unsafety_proved= (safety = Unsafe);
        nontermination_proved= Analysis.must_diverge results;
        leaks= Analysis.leaks results <> [];
        hit_limit= Analysis.hit_limit results;
      }
    )
  )
) with exc ->
  if Config.raise_exceptions then (
    prerr_endline (Printexc.to_string exc) ;
    raise exc
  ) else (
    { success with
      internal_error= Some(exc);
      time_total= Timer.(init.uduration +. init.sduration);
    }
  )
;;
Timer.stop pre_analysis_tmr ;
Timer.stop analysis_tmr ;
Timer.stop post_analysis_tmr ;
Timer.stop Timer.init ;
(* Note: These timers miss the time spent producing the transition system file,
   which is executed on exit, after the statistics and results are printed. *)

let results =
  { results with
    time_total= Timer.(init.uduration +. init.sduration);
    time_preanalysis= Timer.(pre_analysis_tmr.uduration +. pre_analysis_tmr.sduration);
    time_analysis= Timer.(analysis_tmr.uduration +. analysis_tmr.sduration);
    time_postanalysis= Timer.(post_analysis_tmr.uduration +. post_analysis_tmr.sduration) ;
  } in

flush_all () ;

Statistics.report pre_analysis_tmr analysis_tmr post_analysis_tmr ;

(* Gc.print_stat stderr ; *)

print_endline ((if Config.full_results then str_of_general_results else str_of_general_results_old) results) ;

exit (if (results.internal_error = None) then 0 else 1)
