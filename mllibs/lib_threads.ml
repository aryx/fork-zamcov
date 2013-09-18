(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: lib_threads.ml                                                *)
(* authors: Alexis Darrasse                                            *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

module Conv = Conv_obj_value
open Ffi

external thread_new : (unit -> unit) -> Obj.t = "caml_thread_new"

let caml_thread_new vm arg =
  Value.Custom (thread_new (Conv.create_callback vm arg))

let prims () =
  add1 "caml_thread_new" caml_thread_new

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
