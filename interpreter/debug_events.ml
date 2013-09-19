(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let event_of_pc pc vm =
  match vm.Vm.debug.(pc) with
  | Some ev -> ev, false
  | None ->
    (* less: too many None, better understand debug_event? look
     * ocamldebug? find best ev like in backtrace.c?
     *)
    let i = ref pc in
    while vm.Vm.debug.(!i) = None && !i >= 0 do
      i := !i - 1;
    done;
    if !i < 0
    then failwith (spf "no event found for %d" pc)
    else
      (match vm.Vm.debug.(!i) with
      | Some ev -> ev, true
      | None -> raise Impossible
      )


let parse_debug_section n xs =
  let arr = Array.create n None in
  xs +> List.iter (fun (orig, events) ->
    events +> List.iter (fun ev ->
      let pos = ev.Instruct.ev_pos in
      (* relocate *)
      let final = (pos + orig) / 4 in
      if (pos + orig) mod 4 <> 0
      then failwith (spf "event should be 4-bytes aligned, got %d + %d"
                       pos orig);
      arr.(final) <- Some ev;
    );
  );
  arr

(*
let location_of_pc pc =
  let pos = (pc * 4) in
  try 
    let ev = Hashtbl.find hdebug_events pos in
    let pos = ev.Instruct.ev_loc.Location.loc_start in
    Some pos
  with Not_found -> 
    None

let print_location_of_pc pc =
  location_of_pc pc +> Common.do_option (fun pos ->
    pr (spf "%s %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum)
  )
*)

let location_of_pc pc vm =
  let (ev, approx) = event_of_pc pc vm in
  let pos = ev.Instruct.ev_loc.Location.loc_start in
  (spf "%s %d%s" 
     pos.Lexing.pos_fname pos.Lexing.pos_lnum
     (if approx then "(=~)" else ""))

let print_backtrace vm =
  let pc = vm.Vm.code_pointer in
  pr2 (location_of_pc pc vm);
  vm.Vm.stack +> List.iter (fun v ->
    match v with
    | Value.Code_pointer _ 
    | Value.Closure _
    | Value.Infix _
        ->
      let pc = Conv_obj_value.get_code v in
      pr2 (location_of_pc pc vm);
    | _ -> ()
  )

