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

let event_of_pc pc =
  raise Todo

let parse_debug_section n xs =
  let arr = Array.create n None in
  xs +> List.iter (fun (orig, events) ->
    events +> List.iter (fun ev ->
      let pos = ev.Instruct.ev_pos in
      (* relocate *)
      let final = (pos + orig) / 4 in
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

let print_backtrace vm =
  raise Todo
