(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Configuration parameters *)

open Library


let next_lexicographic_permutation a =
  let rec loop1 k =
    if k <= 1 then raise Not_found
    else if a.(k-1) < a.(k) then k-1 else loop1 (k-1)
  in
  let rec loop2 k l =
    if a.(k) < a.(l) then l else loop2 k (l-1)
  in
  let n = Array.length a in
  try
    let k = loop1 (n-1) in
    let l = loop2 k (n-1) in
    Array.swap a k l ;
    Array.reverse a (k+1) n
  with Not_found ->
    ()

let tag_perm = [|10;2;0;3;4;5;1;6;7;8;9|]

let rec set_perm i =
  if i > 0 then (
    next_lexicographic_permutation tag_perm ;
    set_perm (i-1)
  )

let _ = Random.self_init ()

let set_perm_randomly () =
  let rec factorial n = if n < 2 then 1 else n * factorial (n-1) in
  let i = Random.int (factorial (Array.length tag_perm)) in
  Printf.printf "using permutation %i" i ;
  set_perm i


type precondition_order = Syntactic | Logical | WeakerSubheap

let precondition_order_of_string = function
  | "syntactic" -> Syntactic | "logical" -> Logical | "weaker-subheap" -> WeakerSubheap
  | _ -> raise (Arg.Bad "unrecognized precondition order")

let string_of_precondition_order = function
  | Syntactic -> "syntactic" | Logical -> "logical" | WeakerSubheap -> "weaker-subheap"


let abs_query_to_gen = ref 0
let c_syntax = ref false
let check_abs = ref false
let check_assumptions_time = ref infinity
let check_cng = ref false
let check_gie = ref false
let check_gie_vs_cc = ref false
let check_prv = ref false
let check_sorts = ref true
let check_scc = ref false
let compile_only = ref false
let continue = ref false
let dcc_gie = ref true
let distrib_pure = ref false
let entails_time = ref infinity
let exp_expand_ite = ref false
let exp_hc_initial_size = ref 16381 (* Note: tune, taken from http://oeis.org/A014234/list *)
let exp_simplify = ref true
let filenames = ref []
let font = ref (try Sys.getenv "SLAyer_FONT" with Not_found -> "")
let frontend_args = ref []
let full_results = ref false
let generalize_call_retn = ref true
let gie = ref 2
let gie_incremental = ref true
let gie_weak = ref false
let instrument = ref false
let join_powerset = ref true
let join_reduce = ref 0
let limit = ref 0
let limit_ghosts = ref (-1)
let margin = ref 176
let margin_frac = ref 64.0
let minor_heap_size = ref ((Gc.get()).Gc.minor_heap_size * Sys.word_size / (8*1024*1024))
let no_builtins = ref false
let norm_in_frontend = ref true
let norm_query_to_gen = ref 0
let optimize_frame = ref false
let optimize_icall_targets = ref false
let optimize_inline = ref 4
let optimize_unused = ref true
let optimize_liveness = ref true
let optimize_boxing = ref true
let precondition_order = ref WeakerSubheap
let preserve_consts = ref true
let propagate = ref false
let prv_gen_test = ref 0
let prv_simplify = ref true
let prv_strong_valid_check = ref false
let prv_valid_check = ref false
let prv_wbn = ref true
let ptr_size = ref 4
let pur_eager_qe = ref true
let quant_weight = ref 0
let raise_exceptions = ref false
let report_dead_code = ref false
let reset_freq = ref 0
let sh_simplify = ref true
let sh_hoist_common_subformulas = ref true
let show_unreachable = ref false
let stats = ref false
let subtract_time = ref infinity
let trust_casts = ref false
let tt_single_step = ref false
let typ_hc_initial_size = ref 1021              (* Note: tune, taken from http://oeis.org/A014234/list *)
let vATS = ref 0
let vAbs = ref 0
let vAbsH = ref 0
let vCEx = ref 0
let vCng = ref 0
let vDCC = ref 0
let vDiscovery = ref 0
let vExp = ref 0
let vFE = ref 0
let vFrame = ref 0
let vGraph = ref 0
let vHG = ref 0
let vInline = ref 0
let vInstr = ref 0
let vJoinPoint = ref 0
let vLiveness = ref 0
let vPrv = ref 0
let vPure = ref 0
let vRch = ref 0
let vSE = ref 0
let vSH = ref 0
let vPgm = ref 0
let vSubst = ref 0
let vTr = ref 2
let vTyp = ref 0
let vVar = ref 2
let vZ3 = ref 0
let version_only = ref false
let weak_pure_consequences = ref true
let write_ats = ref false
let write_cfg = ref false
let write_cl_cfg = ref false
let write_tt = ref false
let z3_distinct = ref false
let z3_ematching = ref true
let z3_log = ref false
let z3_memout = ref 0
let z3_model = ref false
let z3_print_mode = ref "full"
let z3_relevancy = ref 0
let z3_timeout = ref 0



let argspec = ref [
  (* options that select which results to compute *)
  ("-version", Arg.Set version_only,
   " Report version and exit");
  ("-c", Arg.Set compile_only,
   " Translate input to internal representation but do not analyze");
  ("-cfg", Arg.Set write_cfg,
   " Write internal representation of program as a control-flow graph to .cfg.dot file");
  ("-ats", Arg.Set write_ats,
   " Write abstract transition system to .ats.dot file");
  ("-tt", Arg.Set write_tt,
   " Write counter-examples to defect.tt to view with sdvdefect");
  ("-t2", Arg.Set instrument,
   " Generate instrumented arithmetic program for T2");

  ("-continue", Arg.Set continue,
   " Continue searching for errors after the first potential memory safety violation");
  ("-propagate", Arg.Set propagate,
   " Propagate error conditions beyond potential memory safety violations");

  (* options that affect external behavior *)
  ("-frame", Arg.Bool (fun x -> optimize_frame := x),
   "<bool>  Optimize: Frame \
    (default="^ (string_of_bool !optimize_frame) ^")");
  ("-generalize-call-retn", Arg.Bool (fun x -> generalize_call_retn := x),
   "<bool> Generalize assertions at procedure call and return sites \
    (default="^ (string_of_bool !generalize_call_retn) ^")");
  ("-join-powerset", Arg.Bool (fun x -> join_powerset := x),
   "<bool> Use powerset join instead of constructing disjunctive formulae \
    (default="^ (string_of_bool !join_powerset) ^")");
  ("-limit", Arg.Set_int limit,
   "<int> Limit length of chains at a program point (0 for unlimited) \
    (default="^ (string_of_int !limit) ^")");
  ("-limit-ghosts", Arg.Set_int limit_ghosts,
   " Limit on the number of stack frames worth of ghost variables kept \
    (default="^ (string_of_int !limit_ghosts) ^")");
  ("-precondition-order", Arg.String (fun s -> precondition_order := precondition_order_of_string s),
   " The order relation to use for procedure preconditions, one of syntactic, logical, weaker-subheap \
    (default="^ (string_of_precondition_order !precondition_order) ^")");
  ("-preserve-consts", Arg.Bool (fun x -> preserve_consts := x),
   "<bool> Preserve facts about program constants \
    (default="^ (string_of_bool !preserve_consts) ^")");
  ("-trust-casts", Arg.Bool (fun x -> trust_casts := x),
   "<bool> Trust casts that increase object size \
    (default="^ (string_of_bool !trust_casts) ^")");

  (* options that control progress reporting *)
  ("-results", Arg.Set full_results,
   " Report full results");
  ("-st", Arg.Set stats,
   " Report time and memory consumption statistics");
  ("-dead", Arg.Set report_dead_code,
   " Report dead code");

  ("-ATS-show-unreachable", Arg.Bool (fun x -> show_unreachable := x),
   "<bool> Show unreachable states in the abstract transition system \
    (default="^(string_of_bool !show_unreachable)^")");
  ("-ATS-reduce", Arg.Set_int join_reduce,
   "<int> Reduce the constructed abstract transition system by removing redundant vertices \
    (default="^ (string_of_int !join_reduce) ^")");
  ("-tt-single-step", Arg.Set tt_single_step,
   " Expand counter-example traces in defect.tt to take a step for each internal instruction, \
     rather than the default one step per source line");

  ("-c-syntax", Arg.Set c_syntax,
   " Generate output in as close to C syntax as possible");
  ("-margin", Arg.Set_int margin,
   "<int> The right margin used by the pretty printer \
    (default="^ (string_of_int !margin) ^")");
  ("-margin-frac", Arg.Set_float margin_frac,
   "<float> The target columns/lines fraction of pretty printed graph vertices \
    (default="^ (string_of_float !margin_frac) ^")");
  ("-font", Arg.Set_string font,
   "<string> The font used in dot graphs \
    (default="^ !font ^")");

  ("-vAbs", Arg.Set_int vAbs, "<int> Verbosity of Abstraction module");
  ("-vAbsH", Arg.Set_int vAbsH, "<int> Verbosity of HeapAbstraction module");
  ("-vATS", Arg.Set_int vATS, "<int> Verbosity of AbstractTransistionSystem module");
  ("-vCEx", Arg.Set_int vCEx, "<int> Verbosity of CounterExample module");
  ("-vCng", Arg.Set_int vCng, "<int> Verbosity of CngRel module");
  ("-vDCC", Arg.Set_int vDCC, "<int> Verbosity of DisjCngClos module");
  ("-vDiscovery", Arg.Set_int vDiscovery, "<int> Verbosity of Discovery module");
  ("-vExp", Arg.Set_int vExp, "<int> Verbosity of Expression module");
  ("-vFrame", Arg.Set_int vFrame, "<int> Verbosity of Frame module");
  ("-vGraph", Arg.Set_int vGraph, "<int> Verbosity of Graph module");
  ("-vHG", Arg.Set_int vHG, "<int> Verbosity of HeapGraph module");
  ("-vInstr", Arg.Set_int vInstr, "<int> Verbosity of Instrumentation module");
  ("-vPrv", Arg.Set_int vPrv, "<int> Verbosity of Prover module");
  ("-vPure", Arg.Set_int vPure, "<int> Verbosity of Pure module") ;
  ("-vRch", Arg.Set_int vRch, "<int> Verbosity of Reachability module");
  ("-vSE", Arg.Set_int vSE, "<int> Verbosity of SymbolicExecution module");
  ("-vSH", Arg.Set_int vSH, "<int> Verbosity of SymbolicHeap module");
  ("-vPgm", Arg.Set_int vPgm, "<int> Verbosity of Program module");
  ("-vSubst", Arg.Set_int vSubst, "<int> Verbosity of Substitution module");
  ("-vTr", Arg.Set_int vTr, "<int> Verbosity of reporting ATS transitions (default="^ (string_of_int !vTr) ^")");
  ("-vTyp", Arg.Set_int vTyp, "<int> Verbosity of Type module");
  ("-vVar", Arg.Set_int vVar, "<int> Verbosity of Variable module");
  ("-vZ3", Arg.Set_int vZ3, "<int> Verbosity of Z3 library") ;

  (* options that control internal behavior but should not meaningfully affect results *)
  ("-DCC-gie", Arg.Bool (fun x -> dcc_gie := x),
   "<bool> Call get_implied_equalities from DCC"
   ^" (default="^(string_of_bool !dcc_gie)^")");

  ("-Exp-simplify", Arg.Bool (fun x -> exp_simplify := x),
   "<bool> Perform syntactic simplification of Expressions"
   ^" (default="^(string_of_bool !exp_simplify)^")");
  ("-Exp-expand-ite", Arg.Bool (fun x -> exp_expand_ite := x),
   "<bool> Expand if-then-else Expressions into disjunctions"
   ^" (default="^(string_of_bool !exp_simplify)^")");
  ("-Exp-hc-size", Arg.Set_int exp_hc_initial_size,
   "<int> Initial size of table for hash-consing Expressions"
   ^" (default="^(string_of_int !exp_hc_initial_size)^")");
  ("-Exp-compare", Arg.Int set_perm,
   "<int> Use the ith lexicographic permutation for comparing expressions");
  ("-Exp-compare-random", Arg.Unit set_perm_randomly,
   " Use a random permutation for comparing expressions");

  ("-Pur-eager-qe", Arg.Bool (fun x -> pur_eager_qe := x),
   "<bool> Perform quantifier elimination when asserting formulas instead of when solving them \
    (default="^(string_of_bool !pur_eager_qe)^")");

  ("-Prv-simplify", Arg.Bool (fun x -> prv_simplify := x),
   "<bool> Simplify formulas during proof search \
    (default="^(string_of_bool !prv_simplify)^")");
  ("-Prv-svc", Arg.Set prv_strong_valid_check,
   " Use a strong (and expensive) check to fail proof search early \
    (default="^(string_of_bool !prv_strong_valid_check)^")");
  ("-Prv-vc", Arg.Set prv_valid_check,
   " Use a pure validity check to fail proof search early \
    (default="^(string_of_bool !prv_valid_check)^")");
  ("-Prv-wbn", Arg.Bool (fun x -> prv_wbn := x),
   "<bool> Compute existential witnesses using normalization \
    (default="^(string_of_bool !prv_wbn)^")");

  ("-SH-distrib-pure", Arg.Bool (fun x -> distrib_pure := x),
   "<bool> Distribute pure conjunction under disjunction \
    (default="^(string_of_bool !distrib_pure)^")");
  ("-SH-simplify", Arg.Bool (fun x -> sh_simplify := x),
   "<bool> Perform syntactic simplification of SymbolicHeaps"
   ^" (default="^(string_of_bool !sh_simplify)^")");
  ("-hcs", Arg.Unit (fun () -> sh_hoist_common_subformulas := not !sh_hoist_common_subformulas),
   " Hoist common subformulas out of disjunctions"
   ^" (default="^(string_of_bool !sh_hoist_common_subformulas)^")");
  ("-SH-weak-pure-consequences", Arg.Bool (fun x -> weak_pure_consequences := x),
   "<bool> Use a weak version of pure_consequences during normalization"
   ^" (default="^(string_of_bool !weak_pure_consequences)^")");

  ("-Typ-hc-size", Arg.Set_int typ_hc_initial_size,
   "<int> Initial size of table for hash-consing Types"
   ^" (default="^(string_of_int !typ_hc_initial_size)^")");

  ("-Z3-distinct", Arg.Bool (fun x -> z3_distinct := x),
   "<bool> Use 'distinct' formulas in Z3 encoding \
    (default="^ (string_of_bool !z3_distinct) ^")") ;
  ("-Z3-quant-weight", Arg.Set_int quant_weight,
   "<int> Set the weight of quantifiers, used by Z3 \
    (default="^ (string_of_int !quant_weight) ^")") ;
  ("-Z3-quant-inst", Arg.Bool (fun x -> z3_ematching := x),
   "<bool> Use heuristic quantifier instantiation \
    (default="^ (string_of_bool !z3_ematching) ^")") ;
  ("-Z3-relevancy", Arg.Set_int z3_relevancy,
   "<int> relevancy propagation heuristic \
    (default="^ (string_of_int !z3_relevancy) ^")") ;
  ("-Z3-timeout", Arg.Set_int z3_timeout,
   "<int> Set a time limit (in milliseconds) for calls to Z3") ;
  ("-Z3-memout", Arg.Set_int z3_memout,
   "<int> Set a memory limit (in megabytes) for calls to Z3") ;
  ("-Z3-rf", Arg.Set_int reset_freq,
   "<int> Z3 context reset frequency (0=never) \
    (default="^ (string_of_int !reset_freq) ^")") ;

  ("-gie", Arg.Set_int gie,
   "<int> Select 'get_implied_equalities' algorithm \
    (default="^ (string_of_int !gie) ^")") ;
  ("-gie-incremental", Arg.Bool (fun x -> gie_incremental := x),
   "<bool> Manage assertions incrementally for 'get_implied_equalities' \
    (default="^ (string_of_bool !gie_incremental) ^")") ;
  ("-gie-weak", Arg.Bool (fun x -> gie_weak := x),
   "<bool> Use weak encoding for 'get_implied_equalities' \
    (default="^ (string_of_bool !gie_weak) ^")") ;

  ("-minor-heap-size", Arg.Set_int minor_heap_size,
   "<int> Initial minor heap size in MB \
    (default="^ (string_of_int !minor_heap_size) ^")") ;

  (* options for debugging *)
  ("-checkCng", Arg.Bool (fun x -> check_cng := x),
   "<bool> Perform expensive checking of CngRel operations \
    (default="^ (string_of_bool !check_cng) ^")");
  ("-checkGIE", Arg.Bool (fun x -> check_gie := x),
   "<bool> Check soundness and completeness of get_implied_equalities \
    (default="^ (string_of_bool !check_gie) ^")") ;
  ("-checkGIEvsCC", Arg.Bool (fun x -> check_gie_vs_cc := x),
   "<bool> Check that get_implied_equalities and congruence closure have equal strength \
    (default="^ (string_of_bool !check_gie_vs_cc) ^")") ;
  ("-checkAbs", Arg.Bool (fun x -> check_abs := x),
   "<bool> Use prover to check soundness of abstraction \
    (default="^ (string_of_bool !check_abs) ^")");
  ("-checkPrv", Arg.Bool (fun x -> check_prv := x),
   "<bool> Use prover to check its own soundness \
    (default="^ (string_of_bool !check_prv) ^")");
  ("-checkSorts", Arg.Bool (fun x -> check_sorts := x),
   "<bool> Check well-sortedness \
    (default="^ (string_of_bool !check_sorts) ^")") ;
  ("-checkSCC", Arg.Bool (fun x -> check_scc := x),
   "<bool> Perform expensive checking of SCC operations \
    (default="^ (string_of_bool !check_scc) ^")");

  ("-exn", Arg.Set raise_exceptions,
   " Raise unhandled exceptions for internal errors");

  ("-gta", Arg.Set_int abs_query_to_gen,
   "<int> Generate standalone repro for Abstraction call number <int>");
  ("-gtn", Arg.Set_int norm_query_to_gen,
   "<int> Generate repro for SymbolicHeap normalization");
  ("-gtp", Arg.Set_int prv_gen_test,
   "<int> Generate standalone test for Prover query number <int>");

  ("-show-models", Arg.Set z3_model,
   " Display Z3 models") ;
  ("-Z3-print-mode", Arg.Set_string z3_print_mode,
   "<string> Set Z3 printing mode: full, low, smt, smt2") ;

  ("-tca", Arg.Set_float check_assumptions_time,
   "<float> Report check_assumptions calls exceeding time. Negative for running max");
  ("-te", Arg.Set_float entails_time,
   "<float> Report entails calls exceeding time. Negative for running max");
  ("-ts", Arg.Set_float subtract_time,
   "<float> Report subtract calls exceeding time. Negative for running max");

  ("-Z3-log", Arg.Set z3_log,
   " Log Z3 interactions") ;

  (* frontend args *)
  ("--", Arg.Rest (fun rest -> frontend_args := List.append !frontend_args [rest]),
   " Pass remaining arguments to frontend");
  ("-fe_norm", Arg.Bool (fun x -> norm_in_frontend := x),
   "<bool> Normalize internal representation of program before instead of after marshalling (default="^
     (string_of_bool !norm_in_frontend) ^")");
  ("-fe_cfg", Arg.Set write_cl_cfg,
   " Write cl-dropped representation of program to .fe.cfg.dot file");
  ("-no-builtins", Arg.Set no_builtins,
   " Do not include slayer.h when invoking C compiler");
  ("-ptr-size", Arg.Set_int ptr_size,
   "<int> Size (in bytes) of pointers, e.g. 4 for 32-bit code or 8 for 64-bit code \
    (default="^ (string_of_int !ptr_size) ^")");
  ("-Oicalls", Arg.Bool (fun x -> optimize_icall_targets := x),
   "<int> Optimize: Constrain static approximation of indirect call targets using types \
    (default="^ (string_of_bool !optimize_icall_targets) ^")");
  ("-Oinline", Arg.Set_int optimize_inline,
   "<int> Optimize: Inline function calls (0=none, 1=loop- and call-free, 2=loop-free \
    non-recursive, 3=as 2 with single call site, 4=non-recursive) (default="^
     (string_of_int !optimize_inline) ^")");
  ("-Oboxing", Arg.Bool (fun x -> optimize_boxing := x),
   "<int> Optimize: Only box address taken globals and global structs (default="^
     (string_of_bool !optimize_boxing) ^")");
  ("-Oliveness", Arg.Bool (fun x -> optimize_liveness := x),
   "<bool> Optimize: Apply liveness transformations \
    (default="^ (string_of_bool !optimize_liveness) ^")");
  ("-Ounused", Arg.Bool (fun x -> optimize_unused := x),
   "<bool> Optimize: Remove unused globals variables \
    (default="^ (string_of_bool !optimize_unused) ^")");
  ("-vFE", Arg.Set_int vFE,
   "<int> Verbosity of Frontend module");
  ("-vInline", Arg.Set_int vInline,
   "<int> Verbosity of Inline module");
  ("-vJoinPoint", Arg.Set_int vJoinPoint,
   "<int> Verbosity of JoinPoint module");
  ("-vLiveness", Arg.Set_int vLiveness,
   "<int> Verbosity of Liveness module");
]


let _ =
  let argspec = Arg.align !argspec
  and anon_arg_func fname = filenames := fname :: !filenames
  and usage = "Usage: "^Sys.argv.(0)^" <options> {-version | <file>.{c | sil}}"
  in
  Arg.parse argspec anon_arg_func usage
  ;
  Format.pp_set_margin Format.str_formatter !margin
  ;
  let good_usage =
    !version_only ||
    (match !filenames with
    | [] -> false
    | [fname] -> Filename.check_suffix fname ".c" || Filename.check_suffix fname ".sil"
    | fnames -> List.for_all (fun fname -> Filename.check_suffix fname ".c") fnames
    )
  in
  if not good_usage then (
    Arg.usage argspec usage ;
    exit 1
  )


let abs_query_to_gen = !abs_query_to_gen
let check_abs = !check_abs
let check_assumptions_time = !check_assumptions_time
let check_cng = !check_cng
let check_gie = !check_gie
let check_gie_vs_cc = !check_gie_vs_cc
let check_prv = !check_prv
let check_sorts = !check_sorts
let check_scc = !check_scc
let compile_only = !compile_only
let continue = !continue
let dcc_gie = !dcc_gie
let distrib_pure = !distrib_pure
let entails_time = !entails_time
let exp_expand_ite = !exp_expand_ite
let exp_hc_initial_size = !exp_hc_initial_size
let exp_simplify = !exp_simplify
let filenames = List.rev !filenames
let font = !font
let frontend_args = !frontend_args
let full_results = !full_results
let generalize_call_retn = !generalize_call_retn
let gie = !gie
let gie_incremental = !gie_incremental
let gie_weak = !gie_weak
let instrument = !instrument
let join_powerset = !join_powerset
let join_reduce = !join_reduce
let limit = !limit
let limit_ghosts = !limit_ghosts
let margin = !margin
let margin_frac = !margin_frac
let minor_heap_size = !minor_heap_size
let no_builtins = !no_builtins
let norm_in_frontend = !norm_in_frontend
let norm_query_to_gen = !norm_query_to_gen
let optimize_frame = !optimize_frame
let optimize_icall_targets = !optimize_icall_targets
let optimize_inline = !optimize_inline
let optimize_liveness = !optimize_liveness
let optimize_unused = !optimize_unused
let optimize_boxing = !optimize_boxing
let precondition_order = !precondition_order
let preserve_consts = !preserve_consts
let propagate = !propagate
let prv_gen_test = !prv_gen_test
let prv_simplify = !prv_simplify
let prv_strong_valid_check = !prv_strong_valid_check
let prv_valid_check = !prv_valid_check
let prv_wbn = !prv_wbn
let ptr_size = !ptr_size
let pur_eager_qe = !pur_eager_qe
let quant_weight = !quant_weight
let raise_exceptions = !raise_exceptions
let report_dead_code = !report_dead_code
let reset_freq = !reset_freq
let sh_simplify = !sh_simplify
let sh_hoist_common_subformulas = !sh_hoist_common_subformulas
let show_unreachable = !show_unreachable
let stats = !stats
let subtract_time = !subtract_time
let trust_casts = !trust_casts
let tt_single_step = !tt_single_step
let typ_hc_initial_size = !typ_hc_initial_size
let version_only = !version_only
let weak_pure_consequences = !weak_pure_consequences
let write_ats = !write_ats
let write_cfg = !write_cfg
let write_cl_cfg = !write_cl_cfg
let write_tt = !write_tt
let z3_distinct = !z3_distinct
let z3_ematching = !z3_ematching
let z3_log = !z3_log
let z3_memout = !z3_memout
let z3_model = !z3_model
let z3_print_mode = !z3_print_mode
let z3_relevancy = !z3_relevancy
let z3_timeout = !z3_timeout

let testname =
  match filenames with
  | [fname] -> Filename.chop_extension fname
  | _ -> Filename.basename (Sys.getcwd ())
