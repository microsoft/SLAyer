(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Debug message logging *)

open Library


(* For each channel, there is one indent ref, but multiple flag refs. *)
type log = {
  flag: int ref;
  indent: int ref;
  formatter: Format.formatter;
  buffer: Buffer.t;
  channel: out_channel;
}


let set_indent log i =
  log.indent := i ;
  assert (!(log.indent) >= 0) ;
  Format.pp_set_margin log.formatter (Config.margin - 2 * (!(log.indent) + 2))

let inc_indent log = set_indent log (!(log.indent) + 1)


let flush_and_reprint log () =
  let pr_indent () =
    for _i = !(log.indent) downto 0 do output_string log.channel "| " done
  in
  Format.pp_print_flush log.formatter () ;
  set_indent log !(log.indent) ;
  let n = Buffer.length log.buffer - 1 in
  if n >= 0 then (
    let prev = ref (-1) in
    for curr = 0 to n do
      if Buffer.nth log.buffer curr = '\n' then (
        pr_indent () ;
        output_string log.channel
          (Buffer.sub log.buffer (!prev + 1) (curr - !prev)) ;
        prev := curr ;
      )
    done;
    if !prev < n then (
      pr_indent () ;
      output_string log.channel
        (Buffer.sub log.buffer (!prev + 1) (n - !prev)) ;
      output_string log.channel "\n" ;
      flush log.channel ;
    )
  ) ;
  Buffer.clear log.buffer


let printf log vlevel =
  if vlevel = 0 || vlevel <= !(log.flag)
  then Format.kfprintf (fun _ -> flush_and_reprint log ()) log.formatter
  else Format.ifprintf log.formatter


let incf log vlevel =
  if vlevel = 0 || vlevel <= !(log.flag) then
    Format.kfprintf (fun _ ->
      flush_and_reprint log () ;
      inc_indent log
    ) log.formatter
  else
    Format.ifprintf log.formatter

let resetf log vlevel ilevel fmt =
  (* decrement indent before printing anything, but after accepting all args *)
  printf log vlevel ("%t"^^fmt) (fun _ -> set_indent log ilevel)

let decf log vlevel fmt =
  resetf log vlevel (!(log.indent) - 1) fmt


let latch log =
  !(log.indent)

let latch_incf log vlevel start_msg fmt x =
  let save_indent = !(log.indent) in
  incf log vlevel start_msg fmt x ;
  save_indent


let warnf log =
  Format.kfprintf (fun _ -> flush_and_reprint log () ; true) log.formatter


let shift_verb verbose shift thunk =
  let save = !verbose in
  verbose := !verbose - shift;
  let res = thunk() in
  verbose := save;
  res


module type LOG = sig
  val printf : int -> ('a,Format.formatter,unit)format -> 'a
  val incf : int -> ('a,Format.formatter,unit)format -> 'a
  val decf : int -> ('a,Format.formatter,unit)format -> 'a

  val warnf  : ('a,Format.formatter,unit,bool)format4 -> 'a

  val latch : unit -> int
  val latch_incf : int -> 'a format_str -> 'a formatter -> 'a -> int
  val resetf : int -> int -> ('a,Format.formatter,unit)format -> 'a

  val shift_verb : int -> (unit -> 'a) -> 'a
end


let mk_raw channel =
  let indent = ref 0 in
  let buffer = Buffer.create 512 in
  let formatter = Format.formatter_of_buffer buffer in
  fun flag ->
    let log = {flag; indent; formatter; buffer; channel} in
    set_indent log 0 ;
    let module L = struct
      let printf     l   x = printf log l x
      let incf       l   x = incf   log l x
      let decf       l   x = decf   log l x
      let warnf          x = warnf  log   x
      let latch         () = latch  log
      let resetf     l   x = resetf log l x
      let latch_incf l m x = latch_incf log l m x
      let shift_verb l t   = shift_verb flag l t
    end in
    (module L : LOG)

let mk channel =
  let mkL = mk_raw channel in
  fun flag ->
    let module L = (val mkL flag : LOG) in
    (module struct
      include L
      let printf l x = printf l ("@[<hov 0>"^^x^^"@]")
      let incf   l x = incf   l ("@[<hov 2>"^^x^^"@]")
      let decf   l x = decf   l ("@[<hov 2>"^^x^^"@]")
      let warnf    x = warnf    ("@[<hov 2>"^^x^^"@]")
     end : LOG)

let raw = mk_raw Pervasives.stdout

let std = mk Pervasives.stdout
