(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: pluginDebug.ml                                                *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common

module I = Instructions

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* TODO different instances of the VM should not write in the same file *)

let debug = ref false
let name = ref ""
let debug_c = ref stdout

let prims = ref (Array.make 0 "")

let depth = ref 0

let (hdebug_events: (int, Instruct.debug_event) Hashtbl.t) = 
  Hashtbl.create 101

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Plugin *)
(*****************************************************************************)

let step vm instruction =
  (* print debug infomations *)
  if !debug then begin
    (* Interpreter.dump_state vm; *)
    (* Interpreter.dump_env vm.environment; *)
    (* Interpreter.dump_stack vm.stack vm.stack_pointer; *)
  end;
  (match instruction with
  | I.C_CALL1 _
  | I.C_CALL2 _
  | I.C_CALL3 _
  | I.C_CALL4 _
  | I.C_CALL5 _
  | I.C_CALLN _
    ->
    pr (spf "%s: (%04d) %s" (String.make !depth ' ') vm.Vm.code_pointer
          (Instructions.string_of_instructions !prims instruction));
   print_location_of_pc (vm.Vm.code_pointer);
    ()

  | I.APPLY _
  | I.APPLY1
  | I.APPLY2
  | I.APPLY3

  | I.APPTERM _
  | I.APPTERM1 _
  | I.APPTERM2 _
  | I.APPTERM3 _
      ->
    (*depth := !depth + 1;*)
    let dst = Utils.get_code vm.Vm.accumulator in
    pr (spf "%s: (%04d) -> %d (%s)" (String.make !depth ' ') 
          vm.Vm.code_pointer
          dst
          (Instructions.string_of_instructions !prims instruction));
    print_location_of_pc (vm.Vm.code_pointer);
    print_location_of_pc dst (*(vm.Vm.code_pointer)*);
    ()
  | I.RETURN _ ->
    (*depth := !depth -1;*)
    ()
  | _ -> ()
  )
  

let init data =
  prims := data.Bytecode_loader.primitive_section;
  data.Bytecode_loader.debug_section +> List.iter (fun (orig, events) ->
    events +> List.iter (fun ev ->
      let pos = ev.Instruct.ev_pos in
      (* relocate *)
      Hashtbl.replace hdebug_events (pos + orig) ev;
    );
  );
  if !debug then debug_c := open_out !name

let finalise () = if !debug then close_out !debug_c

let _ =
  Plugin.register_plugin init step finalise
    ("-debug", Arg.String (fun s -> debug := true; name := s),
     "file  print debug statements in file")
