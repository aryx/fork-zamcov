(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type plugin = {
  init: Bytecode_loader.sections -> unit;
  step: Vm.virtual_machine -> Instructions.instruction -> unit;
  finalise: Vm.virtual_machine -> unit;
  cmd_args: Arg.key * Arg.spec * Arg.doc;
}

let plugin_list = ref ([]: plugin list)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let register_plugin init step finalise cmd_args =
  plugin_list := {
    init = init;
    step = step;
    finalise = finalise;
    cmd_args = cmd_args
  }::!plugin_list

let init data = 
  List.iter (fun p -> p.init data) !plugin_list
let step vm instruction = 
  List.iter (fun p -> p.step vm instruction) !plugin_list
let finalise vm = 
  List.iter (fun p -> p.finalise vm) !plugin_list
let cmd_args () = 
  List.map (fun p -> p.cmd_args) !plugin_list
