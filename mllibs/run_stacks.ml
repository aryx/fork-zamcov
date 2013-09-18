(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_stacks.ml                                                 *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

module Conv = Conv_obj_value
open Ffi

let prims () =
  (* TODO *)
  add1 "caml_ensure_stack_capacity" (fun vm -> fun _ -> Value.Int 0)

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
