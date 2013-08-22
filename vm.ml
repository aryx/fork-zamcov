(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: vm.ml                                                         *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

(* size of the stack of the execution enviornment *)
let stack_size = 1024*512*2

type prim_table = {
  tbl1 : (virtual_machine -> Value.value -> Value.value) array;
  tbl2 : (virtual_machine -> Value.value -> Value.value -> Value.value) array;
  tbl3 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value) array;
  tbl4 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) array;
  tbl5 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) array;
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

let exec = ref ""
let args : string array ref = ref [||]

let pop vm =
  let res = List.hd vm.stack in
  vm.stack <- List.tl vm.stack;
  res

let push vm x =
  vm.stack <- x :: vm.stack

let peek vm i =
  List.nth vm.stack i

let assign vm i x =
  let rec aux l ac = function
    | 0 -> List.rev_append ac (x :: List.tl l)
    | n -> aux (List.tl l) (List.hd l::ac) (n-1) in
  vm.stack <- aux vm.stack [] i

let run vm =
  (* the main loop of interpretation *)
  (* exit when all the CODE section has been read *)
  let code_size = Array.length vm.code in
  while vm.code_pointer < code_size do
    let instruction = vm.code.(vm.code_pointer) in
      vm.plugin_step vm instruction;
	(* interpretation of the current instruction *)
      vm.execute_step vm instruction;
  done

let init name code global execute_step plugin_step prim_table = {
  name = name;
  code = code;
  extra_arguments = 0;
  environment = Value.atom;
  accumulator = (Value.Int 0);
  stack = [];
  code_pointer = 0;
  caml_trap_pointer = None;
  global_data = global;
  plugin_step = plugin_step;
  execute_step = execute_step;
  prim_table = prim_table
}

let copy vm name code = { vm with
  name = name;
  code = code;
  extra_arguments = 0;
  environment = Value.atom;
  accumulator = (Value.Int 0);
  stack = [];
  code_pointer = 0;
  caml_trap_pointer = None;
}
