(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Timers for runtime statistics *)

open Library


type t = {
  mutable ustart: float;
  mutable sstart: float;
  mutable ufinish: float;
  mutable sfinish: float;
  mutable uduration: float;
  mutable sduration: float;
  mutable running: bool;
  mutable pending: int;
  mutable count: int;
  mutable max: float;
  mutable enabled: bool;
  mutable name: string;
}


let init =
  let {Unix.tms_utime; tms_stime} = Unix.times() in {
    ustart= tms_utime;
    sstart= tms_stime;
    ufinish= 0.;
    sfinish= 0.;
    uduration= tms_utime;
    sduration= tms_stime;
    running= true;
    pending= 0;
    count= 0;
    max= 0.;
    enabled= true;
    name= "init";
  }

let create name = {
  ustart= 0.; ufinish= 0.; uduration= 0.;
  sstart= 0.; sfinish= 0.; sduration= 0.;
  running= false; pending= 0; count= 0; max= 0.;
  enabled= true; name;
}

let enable t = t.enabled <- true
let disable t = t.enabled <- false

let start t =
  if not t.enabled then () else
  if t.running then (
    t.pending <- t.pending + 1 ;
  ) else (
    t.running <- true ;
    let {Unix.tms_utime; tms_stime} = Unix.times() in
    t.ustart <- tms_utime ;
    t.sstart <- tms_stime ;
  )

let stop t =
  if not t.enabled then () else
  if t.pending > 0 then (
    t.pending <- t.pending - 1 ;
  ) else if t.running then (
    let {Unix.tms_utime; tms_stime} = Unix.times() in
    t.running <- false ;
    t.ufinish <- tms_utime;
    t.sfinish <- tms_stime;
    let ud = t.ufinish -. t.ustart
    and sd = t.sfinish -. t.sstart in
    t.uduration <- t.uduration +. ud ;
    t.sduration <- t.sduration +. sd ;
    let usd = ud +. sd in
    if t.max < usd then t.max <- usd ;
    t.count <- t.count + 1 ;
  )

let stop_report t bound printf =
  stop t ;
  let {ustart; sstart; ufinish; sfinish; uduration; sduration} = t in
  let elapsed = (ufinish +. sfinish) -. (ustart +. sstart) in
  if elapsed > bound then printf elapsed (uduration +. sduration)

let log tmr chan prefix =
  let {ustart; sstart; ufinish; sfinish; count} = tmr in
  output_string chan prefix ;
  output_char chan '\t' ;
  output_string chan (string_of_int count) ;
  output_char chan '\t' ;
  output_string chan (string_of_float (1000. *. ((ufinish +. sfinish) -. (ustart +. sstart)))) ;
  output_char chan '\n' ;
  flush chan

let stop_ tmr () = stop tmr

let time tmr fn =
  start tmr ; try_finally fn (stop_ tmr)
