(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: pluginDebug.ml                                                *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

(* TODO different instances of the VM should not write in the same file *)

let debug = ref false
let name = ref ""
let debug_c = ref stdout
let prims = ref (Array.make 0 "")

let step vm instruction =
    (* print debug infomations *)
  if !debug then
    begin
    (* print the execution trace *)
      output_string !debug_c (string_of_int vm.Vm.code_pointer^"\t--> executing "^
                              Instructions.string_of_instructions !prims instruction^"\n");
      flush !debug_c;
      (* Interpreter.dump_state vm; *)
      (* Interpreter.dump_env vm.environment; *)
      (* Interpreter.dump_stack vm.stack vm.stack_pointer; *)
    end

let init data =
  prims := data.Bytecode_loader.primitive_section;
  if !debug then debug_c := open_out !name

let finalise () = if !debug then close_out !debug_c

let _ =
  Plugin.register_plugin init step finalise
    ("-debug", Arg.String (fun s -> debug := true; name := s),
     "file  print debug statements in file")
