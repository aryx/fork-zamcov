val int_of_tag : Value.tag -> int
val get_block_tag_int : Value.value -> int
val string_of_tag : Value.tag -> string
val string_of_value : Value.value -> string
val unbox_string : Value.value -> string
val unbox_float : Value.value -> float
val unbox_custom : Value.value -> Obj.t
val unbox_stack_pointer : Value.value -> Value.value list ref option
val unbox_block : Value.value -> Value.block
val unbox_closure : Value.value -> Value.closure
val int_of_value : Value.value -> int
val float_of_value : Value.value -> float
val string_of_value_array : Value.value array -> string
val is_block : Value.value -> bool
val is_infix : Value.value -> bool
val is_int : Value.value -> bool
val is_float : Value.value -> bool
val block_size : Value.value -> int
val get_block_tag : Value.value -> Value.tag
val set_block_tag : Value.value -> Value.tag -> unit
val get_data : Value.value -> Value.value array
val get_parent : Value.value -> Value.closure
val get_code : Value.value -> int
val obj_of_block : Value.block -> Obj.t
val obj_of_value : Value.value -> Obj.t
val set_field : Value.value -> int -> Value.value -> unit
val create_block : int -> int -> Value.value
val create_closure : int -> int -> int -> Value.closure
val create_infix : Value.closure -> int -> int -> Value.value
val value_of_obj : Obj.t -> Value.value
val get_field : Value.value -> int -> Value.value
val offset : Value.value -> int -> Value.value
val ult : int -> int -> bool
val string_of_exception : Value.value -> string
val pequal : Value.value -> Value.value -> bool
val cnum : int ref
val create_callback : Vm.virtual_machine -> Value.value -> 'a -> unit
