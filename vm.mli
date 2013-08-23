val stack_size : int
type prim_table = {
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
and virtual_machine = {
  name : string;
  code : Instructions.instruction array;
  mutable extra_arguments : int;
  mutable environment : Value.value;
  mutable accumulator : Value.value;
  mutable stack : Value.value list;
  mutable code_pointer : int;
  mutable caml_trap_pointer : Value.value list ref option;
  global_data : Value.value;
  plugin_step : virtual_machine -> Instructions.instruction -> unit;
  execute_step : virtual_machine -> Instructions.instruction -> unit;
  prim_table : prim_table;
}
val exec : string ref
val args : string array ref
val pop : virtual_machine -> Value.value
val push : virtual_machine -> Value.value -> unit
val peek : virtual_machine -> int -> Value.value
val assign : virtual_machine -> int -> Value.value -> unit
val run : virtual_machine -> unit
val init :
  string ->
  Instructions.instruction array ->
  Value.value ->
  (virtual_machine -> Instructions.instruction -> unit) ->
  (virtual_machine -> Instructions.instruction -> unit) ->
  prim_table -> virtual_machine
val copy :
  virtual_machine ->
  string -> Instructions.instruction array -> virtual_machine
