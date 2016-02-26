(* Copyright (c) Microsoft Corporation.  All rights reserved. *)

(********************
  This does not deal optimally with the following case:

     T foo;
     PT pfoo = &foo;

  If foo is used in the program, but pfoo is never used. The analysis
  overaproximates flows, and thus will not remove pfoo, even though it
  would be safe to do so.

*********************)

open Library

open Variable
open Program
module I = Inst
module C = Cmnd
module K = ControlPoint

let rec remove_vars_from_proc procs vars id =
  let open Proc in
  try
    let proc = IdHMap.find procs id in
    let entry = proc.entry in
    let entry = K.id entry in
    let cfg = proc.cfg in
    let to_nop =
      CFG.fold_edges (fun _ m -> m)
        (fun (s,c,e) to_nop ->
           match c with
           | C.Call{Call.proc} ->
               remove_vars_from_proc procs vars proc ;
               to_nop
           | C.ICall _ -> failwith "Not expecting this"
           | C.Inst _ ->
               if Vars.intersect (C.fv c) vars then
                   (s, c, e) :: to_nop
               else
                   to_nop
       ) cfg entry [] in
    let nop_it (s,c,e) =
(*      Format.printf "Remove@ @[(%a,@ %a,@ %a)@]@\n" K.fmt s C.fmt c K.fmt e;*)
      CFG.remove_edge cfg s c e ;
      CFG.add_edge cfg s (C.Inst(I.mk I.Nop (K.pos s))) e
    in
    List.iter nop_it to_nop
  with
    Not_found -> ()

let remove_unused_globals ({Prog.procs; globals; main; global_setup; inits} as prog) =
  if not Config.optimize_unused then prog else
  (* This assumes main is not called by something else, but I think a lot of code makes that assumption. If it
     is called by something else, this analysis will not find any unused globals. *)
  (* This should be called before inlining, as the allocate and deallocate global routines will be inline. *)
  let open Proc in
  (* Calculate used globals *)
  let mainproc = Proc.IdHMap.find procs main in

  let get_proc id = Option.get (Proc.IdHMap.tryfind procs id) in
  let accessed id = (get_proc id).Proc.accessed in
  (* Initially assume all globals are unused, and remove everything that is used *)
  let remove_used id m =
    if List.mem id global_setup then
       (* If these are the calls to do the creatation/initialisation and disposal, then ignore them *)
        m
    else
        Vars.diff m (accessed id)
  in
  (* Traverse main removing everything that is accessed *)
  let unused =
    CFG.fold_edges (fun _ m -> m) (fun (_,c,_) m ->
      let m = Vars.diff m (C.fv c) in
      match c with
      | C.Inst _ ->
          m
      | C.Call{Call.targets}
      | C.ICall{Call.targets} ->
          List.fold remove_used targets m
    ) mainproc.cfg (K.id mainproc.entry) globals
  in
  (* To deal correctly with initialisation of globals, that uses other
     globals, e.g.
        T foo;
        PT pfoo = &foo;
     For any initialiser, that accesses a used variable, make all
     other variables it accesses used as well.  *)
  let up = ref false in                 (* Used to find fixedpoint *)
  let remove_used_if_accessed_used proc unused =
    let access_used, access_unused,_ = Vars.diff_inter_diff (accessed proc) unused in
    (* Second disjunct is require for termination, as if this is empty, no work will be done *)
    if Vars.is_empty access_used || Vars.is_empty access_unused then
      unused
    else (
      up := true ;
      Vars.diff unused access_unused
    ) in
  (* The interation here is potentially expensive, if there is a long initialisation chain*)
  let rec work unused =
    let unused =
      List.fold (fun id unused ->
        let proc = get_proc id in
        CFG.fold_edges (fun _ m -> m) (fun (_,c,_) m ->
          match c with
          | C.Inst {Inst.desc = Inst.Nop} -> m
          | C.Inst _ ->
              (* MJP: the static and dynamic initialisers should just call the initialisers for each variable,
                 they don't do their own work. *)
              assert false
          | C.Call{Call.targets}
          | C.ICall{Call.targets} ->
              List.fold remove_used_if_accessed_used targets m
        ) proc.cfg (K.id proc.entry) unused
      ) inits unused in
    if !up then (
      up := false ;
      work unused
    )
    else
      unused
  in
  let unused = work unused in

  (* Filter unused globals *)
  List.iter (remove_vars_from_proc procs unused) global_setup ;
  {prog with Prog.globals = Vars.diff globals unused}
