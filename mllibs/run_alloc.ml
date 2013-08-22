(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_alloc.ml                                                  *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_alloc_dummy vm = function
  | Value.Int i -> 
      let b = {
        Value.tag = Value.Structured_tag 0;
        Value.data = Array.make i (Value.Int 0);
      } in
      Value.Block b
  | _ -> ccall_failwith "caml_alloc_dummy"

external ext_caml_update_dummy : Obj.t -> Obj.t -> Obj.t = "caml_update_dummy"

let caml_update_dummy vm arg1 arg2 = match arg1, arg2 with
  | Value.Block b1, Value.Block b2 -> 
      b1.Value.tag <- b2.Value.tag;
      for i = 0 to (Array.length b1.Value.data) - 1 do
        b1.Value.data.(i) <- b2.Value.data.(i)
      done;
      Value.Int 0
  | Value.Block _, Value.Closure _
  | Value.Block _, Value.String _ -> (* TODO: verify *)
      ignore (ext_caml_update_dummy (Obj.repr arg1) (Obj.repr arg2));
      Value.Int 0
  | _ -> ccall_failwith ("caml_update_dummy "^Utils.string_of_value arg2)

let prims () =
  add1 "caml_alloc_dummy" caml_alloc_dummy;
  add2 "caml_update_dummy" caml_update_dummy

let initialize dlls_section =
  prims ()
;;
init_list := initialize:: !init_list
