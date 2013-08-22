(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_lexing.ml                                                 *)
(* authors: Alexis Darrasse                                            *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

external lex_engine : Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_lex_engine"
external new_lex_engine : Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_new_lex_engine"

let caml_lex_engine f vm arg1 arg2 v =
  let buf = Obj.new_block (Utils.get_block_tag_int v) 12 in
  Obj.set_field buf 0 (Obj.repr (Utils.create_callback vm (Utils.get_field v 0)));
  for i = 1 to 11 do
    Obj.set_field buf i (Utils.obj_of_value (Utils.get_field v i))
  done;
  let res = Utils.value_of_obj (f (Utils.obj_of_value arg1)
                                  (Utils.obj_of_value arg2) buf) in
  for i = 1 to 11 do
    Utils.set_field v i (Utils.value_of_obj (Obj.field buf i))
  done;
  res

let prims () =
    add3 "caml_lex_engine" (caml_lex_engine lex_engine);
    add3 "caml_new_lex_engine" (caml_lex_engine new_lex_engine)

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
