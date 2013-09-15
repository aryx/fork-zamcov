(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: pluginDebug.ml                                                *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common

module I = Instructions

(* TODO different instances of the VM should not write in the same file *)

let debug = ref false
let name = ref ""
let debug_c = ref stdout
let prims = ref (Array.make 0 "")

let (hdebug_events: (int, Instruct.debug_event) Hashtbl.t) = 
  Hashtbl.create 101

let print_location_of_pc pc =
  let pos = (pc * 4) in
  try 
    let ev = Hashtbl.find hdebug_events pos in
    let pos = ev.Instruct.ev_loc.Location.loc_start in
    pr (spf "%s %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum)
  with Not_found -> 
    pr (spf "Not found %d" pos)

let step vm instruction =
  (* print debug infomations *)
  if !debug then begin
      (* print the execution trace *)
    output_string !debug_c (string_of_int vm.Vm.code_pointer^"\t--> executing "^
                              Instructions.string_of_instructions !prims instruction^"\n");
    flush !debug_c;
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
    pr (spf "%d\t--> executing %s" vm.Vm.code_pointer
          (Instructions.string_of_instructions 
             !prims instruction));
    print_location_of_pc 
      ((vm.Vm.code_pointer) - 2);

  | I.APPLY _
  | I.APPLY1
  | I.APPLY2
  | I.APPLY3

  | I.APPTERM _
  | I.APPTERM2 _
  | I.APPTERM3 _
      ->
    let dst = Utils.get_code vm.Vm.accumulator in
    pr (spf "%d --> %d (%s)"
          vm.Vm.code_pointer 
          dst
          (Instructions.string_of_instructions !prims instruction));
    print_location_of_pc 
      dst (*(vm.Vm.code_pointer)*);
    
    

  | I.RETURN _ ->
    pr (spf "%d\t--> executing %s" vm.Vm.code_pointer
          (Instructions.string_of_instructions 
             !prims instruction));
    
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
