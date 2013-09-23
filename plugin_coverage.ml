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

module I = Instructions
module Conv = Conv_obj_value

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let coverage = ref false

let g = ref [||]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Plugin *)
(*****************************************************************************)

let step vm instruction =
  let pc = vm.Vm.code_pointer in
    
  (match instruction with
  | I.C_CALL1 _
  | I.C_CALL2 _
  | I.C_CALL3 _
  | I.C_CALL4 _
  | I.C_CALL5 _
  | I.C_CALLN _
    ->
    (!g).(pc) <- (!g).(pc) + 1;
  | I.APPLY _
  | I.APPLY1
  | I.APPLY2
  | I.APPLY3

  | I.APPTERM _
  | I.APPTERM1 _
  | I.APPTERM2 _
  | I.APPTERM3 _
    ->
    let _dst = Conv.get_code vm.Vm.accumulator in
    (!g).(pc) <- (!g).(pc) + 1;

  | _ -> ()
  )
  
let init data =
  g := Array.make (Array.length data.Bytecode_loader.code_section) 0;
  ()

let finalise vm =
  let hfiles = Hashtbl.create 101 in
  !g +> Array.iteri (fun i cnt ->
    if cnt > 0 then begin
      let (ev, approx) = Debug_events.event_of_pc i vm in
      if (*not approx*) true then begin

        let pos = ev.Instruct.ev_loc.Location.loc_start in
        let file = pos.Lexing.pos_fname in
        (* the debug events filename is relative to the place where
         * the module was compiled (e.g. common.ml, but also ocollection/oset.ml)
         * so let's normalize and just use the basename, we will need
         * later to reconstruct the coverage information to go from a
         * basename to a readable path.
         *)
        let file = Filename.basename file in
        let line = pos.Lexing.pos_lnum in
    
        (* hfiles{file}{line} = true; *)
        let hlines =
          try Hashtbl.find hfiles file
          with Not_found ->
            let h = Hashtbl.create 101 in
            Hashtbl.add hfiles file h;
            h
        in
        Hashtbl.replace hlines line true;
      end
    end
  );
  hfiles +> Common.hash_to_list +> List.iter (fun (file, hlines) ->
    let lines = hlines +> Common.hashset_to_list in
    pr (spf "%s: %s" file (Common.join " " (lines +> List.map string_of_int)));
  )


let _ =
  Plugin.register_plugin init step finalise
    ("-coverage", Arg.Unit (fun s -> coverage := true),
     "  print coverage information")
