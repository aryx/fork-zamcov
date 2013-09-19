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
(* Types *)
(*****************************************************************************)

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
  (* for string_of_instruction *)
  prims: string array;

  (* can be useful to debug, to know in which vm we are currently (the 
   * initial one is called 'main' in main_zamcov.ml) as we create different
   * vms in Conv.create_callback
   *)
  name : string;
}
  and prim_table = {
    tbl1 : (virtual_machine -> Value.value -> Value.value) array;
    tbl2 : (virtual_machine -> Value.value -> Value.value -> Value.value) array;
    tbl3 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value) array;
    tbl4 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) array;
    tbl5 : (virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) array;
    tbln : (virtual_machine -> Value.value array -> int -> Value.value) array;
  }

(* size of the stack of the execution enviornment *)
let stack_size = 1024*512*2

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

exception Fatal_error of string
exception Vm_error of string

let fatal_error msg =
  raise (Fatal_error msg)

let vm_error msg =
  raise (Vm_error msg)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

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

let init name code global execute_step plugin_step prim_table debug prims = {
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
  prim_table = prim_table;
  debug; 
  prims;
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
