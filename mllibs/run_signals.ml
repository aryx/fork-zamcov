(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_signal.ml                                                 *)
(* authors: Alexis Darrasse                                            *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_install_signal_handler vm arg1 arg2 =
  let res = match (arg1, arg2) with
    | (Value.Int s, Value.Int 0) -> Sys.signal s Sys.Signal_default
    | (Value.Int s, Value.Int 1) -> Sys.signal s Sys.Signal_ignore
    | (Value.Int s, Value.Block b) ->
        Sys.signal s (Sys.Signal_handle (Utils.create_callback vm (Utils.get_field arg2 0)))
    | _ -> ccall_failwith "install_signal_handler: Wrong arguments"
  in
  match res with
  | Sys.Signal_default -> Value.Int 0
  | Sys.Signal_ignore -> Value.Int 1
  | Sys.Signal_handle f -> print_endline "## TODO signal"; Value.Int 0

let prims () =
  add2 "caml_install_signal_handler" caml_install_signal_handler

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
