(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: pluginDebug.ml                                                *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
module I = Instructions

let spf = Printf.sprintf

(* TODO different instances of the VM should not write in the same file *)

let debug = ref false
let name = ref ""
let debug_c = ref stdout
let prims = ref (Array.make 0 "")

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
    output_string stdout (string_of_int vm.Vm.code_pointer^"\t--> executing "^
                            Instructions.string_of_instructions 
                            !prims instruction^"\n");

  | I.APPLY _
  | I.APPLY1
  | I.APPLY2
  | I.APPLY3

  | I.APPTERM _
  | I.APPTERM2 _
  | I.APPTERM3 _
      ->
    let dst = Utils.get_code vm.Vm.accumulator in
    output_string stdout 
      (spf "%d --> %d (%s)\n" 
         vm.Vm.code_pointer 
         dst
         (Instructions.string_of_instructions !prims instruction));

  | I.RETURN _ ->
    output_string stdout (string_of_int vm.Vm.code_pointer^"\t--> executing "^
                            Instructions.string_of_instructions 
                            !prims instruction^"\n");
    
  | _ -> ()
  )
  

let init data =
  prims := data.Bytecode_loader.primitive_section;
  if !debug then debug_c := open_out !name

let finalise () = if !debug then close_out !debug_c

let _ =
  Plugin.register_plugin init step finalise
    ("-debug", Arg.String (fun s -> debug := true; name := s),
     "file  print debug statements in file")
