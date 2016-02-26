(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

module SH = SymbolicHeap


let report pre_analysis_tmr analysis_tmr post_analysis_tmr =

  if Config.stats then (
    let duration = Timer.init.Timer.uduration +. Timer.init.Timer.sduration in
    let fmt_tmr {Timer.uduration; sduration; max; count; name} =
      if count <> 0 then
      let usduration = uduration +. sduration in
      Printf.printf "        %-40s\
                   \ %3.f (%2.f) %%\
                   \ %12.6f (%12.6f) sec\
                   \ %7i calls %12.6f ms/call %15.3f ms max\n"
        name
        (100. *. (usduration /. duration))
        (100. *. (sduration /. duration))
        usduration
        sduration
        count
        (1000. *. usduration /. (float count))
        (1000. *. max)
    in
    let words_to_bytes n = n / (Sys.word_size / 8) in
    let bytes_to_MB n = n /. (1024.*.1024.) in

    Printf.printf "STATISTICS:\n" ;
    Printf.printf "Time:   total         %12.6f (%12.6f) sec\n"
      duration Timer.init.Timer.sduration ;
    Printf.printf "        pre-analysis  %12.6f (%12.6f) sec\n"
      (pre_analysis_tmr.Timer.uduration +. pre_analysis_tmr.Timer.sduration)
      pre_analysis_tmr.Timer.sduration ;
    Printf.printf "        analysis      %12.6f (%12.6f) sec\n"
      (analysis_tmr.Timer.uduration +. analysis_tmr.Timer.sduration)
      analysis_tmr.Timer.sduration ;
    Printf.printf "        post-analysis %12.6f (%12.6f) sec\n"
      (post_analysis_tmr.Timer.uduration +. post_analysis_tmr.Timer.sduration)
      post_analysis_tmr.Timer.sduration ;
    Printf.printf "\n" ;
    fmt_tmr Pure.z3_assert_tmr ;
    fmt_tmr Pure.z3_push_tmr ;
    fmt_tmr Pure.z3_pop_tmr ;
    fmt_tmr Pure.z3_check_tmr ;
    fmt_tmr Pure.z3_check_assumptions_tmr ;
    fmt_tmr Pure.z3_eval_tmr ;
    fmt_tmr Pure.z3_get_implied_equalities_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Pure.conjoin_tmr ;
    fmt_tmr Pure.inconsistent_tmr ;
    fmt_tmr Pure.implies_tmr ;
    fmt_tmr Pure.find_provable_equality_tmr ;
    fmt_tmr Prover.pure_normalize_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Prover.inconsistent_tmr ;
    fmt_tmr Prover.sub_inconsis_m_tmr ;
    fmt_tmr Prover.sub_inconsis_s_tmr ;
    fmt_tmr Prover.ent_pure_tmr ;
    fmt_tmr SH.pure_consequences_tmr ;
    fmt_tmr SH.labeled_pure_consequences_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Prover.entails_tmr ;
    fmt_tmr Prover.subtract_tmr ;
    Printf.printf "\n" ;
    fmt_tmr DisjCngClos.cc_tmr ;
    fmt_tmr DisjCngClos.gie_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Pure.get_implied_equalities_tmr ;
    fmt_tmr SH.normalize_tmr ;
    fmt_tmr SH.normalize_stem_tmr ;
    fmt_tmr SH.exists_elim_tmr ;
    fmt_tmr Prover.sh_normalize_tmr ;
    fmt_tmr Abstraction.normalize_tmr ;
    fmt_tmr SymbolicExecution.normalize_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Reachability.reachability_graphs_tmr ;
    fmt_tmr HeapGraph.add_with_closure_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Abstraction.abs_junk_tmr ;
    fmt_tmr Abstraction.abs_ls_tmr ;
    fmt_tmr Abstraction.abs_arith_tmr ;
    fmt_tmr Abstraction.abs_pure_tmr ;
    fmt_tmr Abstraction.abstract_tmr ;
    Printf.printf "\n" ;
    fmt_tmr TransRel.add_edge_tmr ;
    fmt_tmr TransRel.add_scc_tmr ;
    fmt_tmr TransRel.preds_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Frame.frame_tmr ;
    Printf.printf "\n" ;
    fmt_tmr Program.unmarshal_tmr ;
    fmt_tmr TransformProgram.normalize_tmr ;
    Printf.printf "\n" ;

    let len, num, sum, min, med, max = Expression.stats () in
    Printf.printf "Expression HashCons table length: %i entries: %i \
                   bucket lengths: sum: %i min: %i median: %i max: %i\n"
      len num sum min med max ;
    Printf.printf "\n" ;
    Printf.printf "Memory: total %16.3f MB\n"
      (bytes_to_MB (Gc.allocated_bytes())) ;
    Printf.printf "        maximum %14.3f MB\n"
      (bytes_to_MB (float (words_to_bytes
        (Gc.get()).Gc.minor_heap_size + (Gc.quick_stat()).Gc.top_heap_words))) ;
    Printf.printf "        rate %17.3f MB/sec\n"
      ((bytes_to_MB (Gc.allocated_bytes()))
       /. (Timer.init.Timer.uduration +. Timer.init.Timer.sduration)) ;
  )
