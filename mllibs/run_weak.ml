(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_weak.ml                                                   *)
(* authors: Alexis Darrasse                                            *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_weak_set vm arg1 arg2 arg3 = match (arg1, arg2) with
  | (Value.Block tbl, Value.Int idx) -> (match arg3 with
    | Value.Int 0 -> Weak.set (Obj.obj (Utils.unbox_custom tbl.Value.data.(0))) idx None
    | Value.Block {Value.data = [|x|]} ->
        Weak.set (Obj.obj (Utils.unbox_custom tbl.Value.data.(0))) idx (Some x)
    | _ -> ccall_failwith "error caml_weak_set");
    Value.Int 0
  | _ -> ccall_failwith "error caml_weak_set"

let caml_weak_get vm arg1 arg2 = match (arg1, arg2) with
  | (Value.Block tbl, Value.Int idx) ->
      (match Weak.get (Obj.obj (Utils.unbox_custom tbl.Value.data.(0))) idx with
        | None -> Value.Int 0
        | Some x ->
            Value.Block {
              Value.tag = Value.Structured_tag 0;
              Value.data = [| x |];
            })
  | _ -> ccall_failwith "error caml_weak_get"

let caml_weak_get_copy vm arg1 arg2 = match (arg1, arg2) with
  | (Value.Block tbl, Value.Int idx) ->
      (match Weak.get_copy (Obj.obj (Utils.unbox_custom tbl.Value.data.(0))) idx with
        | None -> Value.Int 0
        | Some (Value.Block x) ->
            Value.Block {
              Value.tag = Value.Structured_tag 0;
              Value.data = [| Obj.obj (Obj.dup (Obj.repr x)) |];
            }
        | _ -> ccall_failwith "error caml_weak_get_copy")
  | _ -> ccall_failwith "error caml_weak_get_copy"

let caml_weak_check vm arg1 arg2 = match (arg1, arg2) with
  | (Value.Block tbl, Value.Int idx) ->
      (match Weak.check (Obj.obj (Utils.unbox_custom tbl.Value.data.(0))) idx with
        | true -> Value.Int 1
        | false -> Value.Int 0)
  | _ -> ccall_failwith "error caml_weak_check"

let prims () =
  add3 "caml_weak_set" caml_weak_set;
  add2 "caml_weak_get" caml_weak_get;
  add2 "caml_weak_get_copy" caml_weak_get_copy;
  add2 "caml_weak_check" caml_weak_check

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
