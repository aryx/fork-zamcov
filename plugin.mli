type plugin = {
  init : Bytecode_loader.sections -> unit;
  step : Vm.virtual_machine -> Instructions.instruction -> unit;
  finalise : Vm.virtual_machine -> unit;
  cmd_args : Arg.key * Arg.spec * Arg.doc;
}
val plugin_list : plugin list ref

val register_plugin :
  (Bytecode_loader.sections -> unit) ->
  (Vm.virtual_machine -> Instructions.instruction -> unit) ->
  (Vm.virtual_machine -> unit) -> 
  Arg.key * Arg.spec * Arg.doc -> unit

val init : Bytecode_loader.sections -> unit
val step : Vm.virtual_machine -> Instructions.instruction -> unit
val finalise : Vm.virtual_machine -> unit
val cmd_args : unit -> (Arg.key * Arg.spec * Arg.doc) list
