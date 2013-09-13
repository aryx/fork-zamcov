exception EXIT_ON_STOP
exception Exception_raised
val string_of_global_data : Value.value array -> string
val raise_our_exception : Vm.virtual_machine -> Value.value -> 'a
val raise_exception : Vm.virtual_machine -> 'a -> 'b
val not_yet_implemented : string -> unit
val dump_state : Vm.virtual_machine -> unit
val dump_globals : Value.value -> unit
val print_block : Value.value -> unit
val dump_env : Value.value -> unit
val dump_stack : Value.value array -> int -> unit
val execute_step : Vm.virtual_machine -> Instructions.instruction -> unit
