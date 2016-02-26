(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(** Timers for runtime statistics *)

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

val init : t
val create : string -> t
val enable : t -> unit
val disable : t -> unit
val start : t -> unit
val stop : t -> unit
val stop_report : t -> float -> (float->float->unit) -> unit
val log : t -> out_channel -> string -> unit
val time : t -> (unit -> 'a) -> 'a
