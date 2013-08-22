(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_callback.ml                                               *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

(* TODO these values should be accessible by the C function caml_named_value *)
let named_values = Hashtbl.create 100

let caml_register_named_value vm arg1 arg2 = match arg1 with
  | Value.String name ->
      begin
        if not (Hashtbl.mem named_values name) then
          Hashtbl.add named_values name arg2
        else
        Hashtbl.replace named_values name arg2;
        Value.Int 0
      end
  | e -> ccall_failwith ("error caml_register_named_value "^Utils.string_of_value e)

let prims () =
  add2 "caml_register_named_value" caml_register_named_value

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
