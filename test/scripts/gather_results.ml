(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

let t2 = ref false
let recurse = ref true

let fnames = ref []
let test_results = ref []
let starting_dir = Sys.getcwd ()


(** Read the results from a file *)
let read_results fname =
  if Sys.file_exists fname then begin
    let in_ch = open_in fname in
    let rec go l =
      let (s, has_more) =
        try (input_line in_ch, true) with End_of_file -> ("", false) in
      if has_more then
        let (s1,s2) =
          try
            let n = String.index s ':' in
            let test = String.sub s 0 n in
            let result = String.sub s (n+1) (String.length s - n - 1) in
            (test, result)
          with Not_found -> ("",s)
        in
        go ((s1,s2)::l)
      else
        List.fast_sort (fun (x,_) (y,_) -> String.compare x y) l
    in
    let res = go [] in
    close_in in_ch;
    res
  end else begin
    prerr_endline ("Could not find " ^ fname ^ "!");
    []
  end


let expected = read_results "EXPECTED.txt"


(** One-line summary of results *)
let summarize_res res =
  let incr_on_match s t i = if (s=t) then i+1 else i in
  let initial_summary = (0,0,0,0,0) in
  List.fold_left
    (fun (ee,ff,ll,ss,uu) (_,_,_,what,_) ->
       let ee' = incr_on_match "error" what ee in
       let ff' = incr_on_match "fail" what ff in
       let ll' = incr_on_match "limit" what ll in
       let ss' = incr_on_match "succ" what ss in
       let uu' = if (ee=ee' && ff=ff' && ll=ll' && ss=ss') then (uu+1) else uu in
       (ee',ff',ll',ss',uu'))
    initial_summary res


let with_out filename outputter =
  let buf = Buffer.create 128 in
  let res = outputter buf in
  let chan = open_out filename in
  Buffer.output_buffer chan buf ;
  close_out chan ;
  res


(** Write the results to a txt file *)
let produce_txt fname res =
  let ch = open_out fname in
  try
    let errors, fails, limits, succs, unknowns = summarize_res res in
    Printf.fprintf ch "Summary: errors %d, fails %d, limits %d, succs %d, unknowns %d\n"
      errors fails limits succs unknowns;
    List.iter (fun (name,_what,_time,_mem,msg) ->
      Printf.fprintf ch "%s:%s\n" name msg
    ) res;
    close_out ch
  with exn ->
    close_out ch;
    raise exn


(** Write the results to a tsv file *)
let produce_tsv fname res =
  with_out fname (fun buf ->
    List.iter (fun (name, (utime, stime, ttime), mem, _what, msg) ->
      Format.bprintf buf
        "%s\t%f\t%f\t%f\t%f\t%s@\n"
        name ttime utime stime mem (String.sub msg 0 (min 100 (String.length msg)))
    ) res
  )


(** Write the results to a html file *)
let produce_html fname res =
  (* SI: precompute Summary *)
  let errors, fails, limits, succs, unknowns = summarize_res res in
  let out_ch = open_out fname in
  let pr x = Printf.fprintf out_ch x in
  try
    pr "<html><head><title>Test results</title><style>\n" ;
    pr ".error { background-color: #ffcccc; padding: 5px; }\n" ;
    pr ".fail { background-color: #ffcccc; padding: 5px; }\n" ;
    pr ".limit { background-color: #ffffcc; padding: 5px; }\n" ;
    pr ".succ { background-color: #ccffcc; padding: 5px; }\n" ;
    pr "</style></head>\n" ;
    pr "<body>" ;
    pr "<h1>Test results</h1>\n" ;

    let module U = Unix in
    let tm = U.localtime (U.time ()) in
    let yy = tm.U.tm_year + 1900
    and mm = tm.U.tm_mon + 1
    and dd = tm.U.tm_mday
    and hh = tm.U.tm_hour
    and nn = tm.U.tm_min
    in
    pr "<p>%d:%02d:%02d:%02d:%02d</p>\n"
      yy mm dd hh nn ;
    pr "<table><tr><td>Summary:</td><td>errors %d, </td>\
                   <td>fails %d, </td><td>limits %d, </td><td>succs %d, </td>\
                   <td>unknowns %d</td></tr>\n"
      errors fails limits succs unknowns ;
    pr "<table><tr><th>Test</th><th>&nbsp;</th>\
                   <th>total<br>(sec)</th>\
                   <th>usr<br>(sec)</th>\
                   <th>sys<br>(sec)</th>\
                   <th>Memory<br>(MB)</th>\
                   <th>Result</th></tr>\n" ;
    List.iter (fun (name, (utime, stime, ttime), mem, what, msg) ->
      let x = Filename.chop_extension name in
      pr "<tr>\
          <td class=\"%s\"><a href=\"%s\">%s</a></td>\
          <td class=\"%s\">\
            <a href=\"%s.cfg.dot\">[cfg]</a>\
            <a href=\"%s.dot\">[ats]</a>\
            <a href=\"%s.slayer.out\">[out]</a></td>"
         what name name what x x x ;
      pr "<td align=\"right\" class=\"%s\">%12.6f</td>\n\
          <td align=\"right\" class=\"%s\">%12.6f</td>\n\
          <td align=\"right\" class=\"%s\">%12.6f</td>\n\
          <td align=\"right\" class=\"%s\">%12.6f</td>\n\
          <td class=\"%s\">%s</td></tr>\n"
         what ttime what utime what stime
         what mem what (String.sub msg 0 (min 100 (String.length msg)))
    ) res ;
    pr "<table>\n" ;
    pr "</body></html>\n" ;
    close_out out_ch
  with exn ->
    close_out out_ch ;
    raise exn


(** Compare results *)
let compare_results res1 res2 =
  let no_found (x,time,mem,what,m) =
    if what = "error" then (x,time,mem,what,m)
    else (x,time,mem,"fail",m ^ " No expected result found.") in
  let rec go res res1 res2 =
    match res1, res2 with
    | [], _ -> List.rev res
    | x::res1, [] -> go (no_found x :: res) res1 []
    | (x,time,mem,what,m)::res1', (x2,m2)::res2' ->
        let n = String.compare x x2 in
        if n < 0 then
          go (no_found (x,time,mem,what,m) :: res) res1' res2
        else if n = 0 then
          let res' =
            if m = m2 then
              (x, time, mem, "succ", m) :: res
            else if what = "error" || what = "limit" then
              (x, time, mem, what, m^" Expected:"^m2) :: res
            else
              (x, time, mem, "fail", m^" Expected:"^m2) :: res
          in
          go res' res1' res2'
        else
          go res res1 res2'
  in
  go [] res1 res2


let grep num_groups rex fname =
  let inch = open_in fname in
  let rec go l =
    let s, has_more =
      try (input_line inch, true) with End_of_file -> ("", false) in
    let l =
      try
        let _ = Str.search_forward rex s 0 in
        let rec loop i z =
          if i > num_groups then z else loop (i+1) (Str.matched_group i s :: z)
        in
        (List.rev (loop 1 [])) :: l
      with Not_found -> l in
    (if has_more then go l else l)
  in
  let res = go [] in
  close_in inch;
  res

(* Parse a t2 output file *)
let t2_test_file test_t2 =
  let test = Filename.chop_extension test_t2 in
  let test_c = test ^ ".c" in
  let test_out = test ^ ".t2.out" in

  if Sys.file_exists test_out then
  let t_rex = Str.regexp "T2 time: \\([0-9.]+\\)s"
  in
  let t =
    match grep 1 t_rex test_out with
    | [[t]] ->
        let t = float_of_string t in (t, 0., t)
    | _ ->
        (0.,0.,0.)
  in
  let mem = 0. in
  let res_rex = Str.regexp "RESULT:\\(.*\\)$" in
  let r1, r2  =
    (match grep 1 res_rex test_out with
    | [[" Error: TIMEOUT"]] -> ("error", "TIMEOUT")
    | [[" Error: MEMOUT"]]  -> ("error", "MEMOUT")
    | [s] :: _ ->
        let rex = Str.regexp "[^:]*Error:" in
        (try ignore (Str.search_forward rex s 0); ("error", s)
         with Not_found -> ("succ", s))
    | [] -> ("error", "Error: No result")
    | ([] | _::_) :: _ -> assert false  (* grep 1 returns lists of singletons *)
    )
  in
  test_results := (test_c, t, mem, r1, r2) :: !test_results

(** Parse a slayer output file *)
let test_file test_li =
  let test = Filename.chop_extension test_li in
  let test_c = test ^ ".c" in
  let test_out = test ^ ".slayer.out" in

  if Sys.file_exists test_out then

  let t_rex = Str.regexp "Time:   total *\\([0-9.]+\\) *( *\\([0-9.]+\\)) sec"
  in
  let t =
    match grep 2 t_rex test_out with
    | [[t;s]] ->
        let t = float_of_string t and s = float_of_string s in
        (t -. s, s, t)
    | _ ->
        (0.,0.,0.)
  in
  let mem_rex = Str.regexp "Memory: total *\\([0-9.]+\\) MB" in
  let mem =
    match grep 1 mem_rex test_out with
    | [[s]] -> float_of_string s
    | _ -> 0.
  in
  let res_rex = Str.regexp "RESULT:\\(.*\\)$" in
  let r1, r2  =
    (match grep 1 res_rex test_out with
    | [[" TIMEOUT"]] -> ("limit", " TIMEOUT")
    | [[" MEMOUT"]]  -> ("limit", " MEMOUT")
    | [[" HIT LIMIT"]]  -> ("limit", " HIT LIMIT")
    | [s] :: _ ->
        let rex = Str.regexp "[^:]*Error:" in
        (try ignore (Str.search_forward rex s 0); ("error", s)
         with Not_found -> ("succ", s))
    | [] -> ("limit", "Error: No result")
    | ([] | _::_) :: _ -> assert false  (* grep 1 returns lists of singletons *)
    )
  in
  test_results := (test_c, t, mem, r1, r2) :: !test_results


(** Parse arguments *)
let parse_args () =
  let usage = "\nUsage: gather_results test1.li test2.li ..." in
  Arg.parse [("-t2", Arg.Unit (fun () -> t2 := true), ": work over t2.out files instead of .li")] (fun s -> fnames := s :: !fnames) usage;
  List.sort String.compare !fnames

let () =
  let _ = parse_args() in
  let handle_file = if !t2 then t2_test_file else test_file in
  List.iter handle_file !fnames;
  let res =
    List.fast_sort (fun (x,_,_,_,_) (y,_,_,_,_) -> String.compare x y)
      !test_results in
  let res2 = compare_results res expected in
  produce_txt "RESULT.curr.txt" res2 ;
  produce_tsv "RESULT.curr.tsv" res2 ;
  produce_html "RESULT.curr.html" res2
