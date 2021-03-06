
type virtual_machine = {
  code : Instructions.instruction array;

  mutable code_pointer : int;
  mutable accumulator : Value.value;
  mutable environment : Value.value;
  mutable extra_arguments : int;

  mutable stack : Value.value list;
  global_data : Value.value;

  mutable caml_trap_pointer : Value.value list ref option;

  plugin_step : virtual_machine -> Instructions.instruction -> unit;
  execute_step : virtual_machine -> Instructions.instruction -> unit;

  prim_table : prim_table;

  debug: Instruct.debug_event option array;
  prims: string array;

  name : string;
}

and prim_table = {
  tbl1 : (virtual_machine -> Value.value -> Value.value) array;
  tbl2 : (virtual_machine -> Value.value -> Value.value -> Value.value) array;
  tbl3 :
    (virtual_machine ->
     Value.value -> Value.value -> Value.value -> Value.value)
    array;
  tbl4 :
    (virtual_machine ->
     Value.value -> Value.value -> Value.value -> Value.value -> Value.value)
    array;
  tbl5 :
    (virtual_machine ->
     Value.value ->
     Value.value -> Value.value -> Value.value -> Value.value -> Value.value)
    array;
  tbln : (virtual_machine -> Value.value array -> int -> Value.value) array;
}

val exec : string ref
val args : string array ref

val pop : virtual_machine -> Value.value
val push : virtual_machine -> Value.value -> unit
val peek : virtual_machine -> int -> Value.value

val assign : virtual_machine -> int -> Value.value -> unit

exception Fatal_error of string
exception Vm_error of string

val fatal_error: string -> 'a
val vm_error: string -> 'a

val run : virtual_machine -> unit

val init :
  string ->
  Instructions.instruction array ->
  Value.value ->
  (virtual_machine -> Instructions.instruction -> unit) ->
  (virtual_machine -> Instructions.instruction -> unit) ->
  prim_table -> 
  Instruct.debug_event option array ->
  string array ->
  virtual_machine

val copy :
  virtual_machine ->
  string -> Instructions.instruction array -> virtual_machine
