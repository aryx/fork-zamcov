(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: pluginRun.ml                                                  *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

(* Plugin run interface *)

type plugin = {
  init: Bytecode_loader.sections -> unit;
  step: Vm.virtual_machine -> Instructions.instruction -> unit;
  finalise: unit -> unit;
  cmd_args: Arg.key * Arg.spec * Arg.doc;
}

let plugin_list = ref ([]: plugin list)

let register_plugin init step finalise cmd_args =
  plugin_list := {
    init = init;
    step = step;
    finalise = finalise;
    cmd_args = cmd_args
  }::!plugin_list

let init data = List.iter (fun p -> p.init data) !plugin_list
let step vm instruction = List.iter (fun p -> p.step vm instruction) !plugin_list
let finalise () = List.iter (fun p -> p.finalise ()) !plugin_list
let cmd_args () = List.map (fun p -> p.cmd_args) !plugin_list
