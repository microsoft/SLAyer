(* Copyright (c) Microsoft Corporation.  All rights reserved. *)


type record = {
  name: string;
  result: string;
  total: float;
  user: float;
  system: float;
  memory: float;
}

let labels = ref [] ;;
let timeout = ref 0 ;;
let pdf = ref false ;;

Arg.parse [
  ("-timeout", Arg.Set_int timeout, " Time limit (sec)");
  ("-pdf", Arg.Set pdf, " Generate plot as a PDF file");
] (fun label -> labels := label :: !labels) ""

let label1, label2 =
  match !labels with
  | [label2; label1] -> label1, label2
  | _ -> invalid_arg "expected two data file labels"
;;

let read_file label tbl =
  let chan = open_in ("RESULT."^label^".tsv") in
  let rec loop () =
    try
      let mk_record name total user system memory result = {name; total; user; system; memory; result} in
      let record = Scanf.fscanf chan "%s %f %f %f %f %[^\n]\n" mk_record in
      let record =
        if Str.string_match (Str.regexp_string "TIMEOUT") record.result 0
        then {record with total= float_of_int !timeout}
        else record in
      Hashtbl.add tbl record.name record ;
      loop ()
    with End_of_file -> ()
  in
  loop () ;
  close_in chan

let tbl1 = Hashtbl.create 256
let tbl2 = Hashtbl.create 256

let read_files () =
  match !labels with
  | [label2; label1] ->
      read_file label1 tbl1 ;
      read_file label2 tbl2 ;
  | _ ->
      invalid_arg "must pass two labels determining data files"
;;

module SS = Set.Make(String)

let write_merged_results () =
  let chan = open_out "RESULT.compare.tsv"
  in
  let add_names tbl names =
    Hashtbl.fold (fun name _ names -> SS.add name names) tbl names
  in
  let names = add_names tbl1 (add_names tbl2 SS.empty)
  in
  Printf.fprintf chan "Test\tResult1\tResult2\tTime1\tTime2\n"
  ;
  SS.iter (fun name ->
    try
      let {result= result1; total= time1} = Hashtbl.find tbl1 name in
      let {result= result2; total= time2} = Hashtbl.find tbl2 name in
      Printf.fprintf chan "%s\t%s\t%s\t%f\t%f\n" name result1 result2 time1 time2
    with Not_found -> ()
  ) names ;
  close_out chan
;;

let generate_plot () =
  let gp_commands =
    ["set xlabel '"^label1^"'"] @
    ["set ylabel '"^label2^"'"] @
    (if not !pdf then [] else
    ["set terminal pdf enhanced font 'Times' size 20cm, 20cm linewidth 1"] @
    ["set output 'RESULT."^label1^"."^label2^".pdf'"] )
  in
  let args = [
    "gnuplot" ;
    "-p" ;
    "-e \""^(String.concat "; " gp_commands)^"\"" ;
    "scripts/compare_results.gp"
  ] in
  ignore( Sys.command (String.concat " " args) );
;;

read_files () ;
write_merged_results () ;
generate_plot () ;
