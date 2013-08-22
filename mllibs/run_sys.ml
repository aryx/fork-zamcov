(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_sys.ml                                                    *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_sys_exit vm = function
  | Value.Int code -> vm.Vm.code_pointer <- Array.length vm.Vm.code + 1; Value.Int 0 (* TODO return the right exit code *)
  | _ -> ccall_failwith "error sys_exit"

let caml_sys_get_argv vm = function _ ->
  let exec_name = Value.String !Vm.exec in
  let list_args = Value.Block {
    Value.tag = Value.Abstract_tag; 
    Value.data = Array.map (fun s -> Value.String s) !Vm.args
  }
  in
  let r = {
    Value.tag = Value.Abstract_tag;
    Value.data = Array.make 2 Value.atom;
  } in
    r.Value.data.(0) <- exec_name;
    r.Value.data.(1) <- list_args;
    Value.Block r

let prims () =
  add1 "caml_sys_get_argv" caml_sys_get_argv;
  add1 "caml_sys_exit" caml_sys_exit

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
