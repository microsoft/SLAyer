#!/usr/bin/env ocaml
(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

#load "str.cma" ;;

let try_finally f g =
  let res =
    try
      f ()
    with e ->
      g () ;
      raise e
  in
  g () ;
  res

let with_in file reader =
  let chan = open_in file in
  try_finally
    (fun () -> reader chan)
    (fun () -> close_in chan)


let tsv_file = ref "RESULT.curr.tsv"
let fnames = ref []
let prev_times = Hashtbl.create 512


let read_tsv () =
  with_in !tsv_file (fun chan ->
    try
      while true do
        let name, ttime, result =
          Scanf.fscanf chan "%s %f %f %f %f %[^\n]\n"
            (fun name ttime _utime _stime _mem result -> (name, ttime, result)) in
        let name = (Filename.chop_extension name) ^ ".sil" in
        let time = if Str.string_match (Str.regexp_string "TIMEOUT") result 0 then infinity else ttime in
        Hashtbl.replace prev_times name time
      done
    with End_of_file ->
      ()
  )


let cmp x y =
  let c = Pervasives.compare (Hashtbl.find prev_times x) (Hashtbl.find prev_times y) in
  if c<>0 then c else String.compare x y


let () =
  Arg.parse
    [("-tsv", Arg.String (fun f -> tsv_file := f), "<file.tsv> Read previous run-times from <file.tsv>")]
    (fun s -> fnames := s :: !fnames)
    "sort_tests {file1.sil}+"
  ;
  List.iter (fun test -> Hashtbl.add prev_times test max_float) !fnames
  ;
  (try read_tsv ()
  with _ -> ())
  ;
  let sorted_fnames = List.sort cmp !fnames
  in
  List.iter (fun fname -> print_string (fname ^ " ")) sorted_fnames
