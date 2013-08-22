(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_obj.ml                                                    *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_obj_is_block vm arg =
  if Utils.is_int arg then
    Value.Int 0
  else
    Value.Int 1

let caml_obj_is_int vm = function
  | Value.Int i -> Value.Int 1
  | _ -> Value.Int 0

let caml_obj_tag vm v =
  Value.Int (Utils.get_block_tag_int v)

let caml_obj_set_tag vm arg1 arg2 = match (arg1, arg2) with
  | Value.Block b, Value.Int i -> b.Value.tag <-
      if i = Obj.object_tag       then Value.Object_tag
      else if i = Obj.forward_tag      then Value.Forward_tag
      else if i = Obj.abstract_tag     then Value.Abstract_tag
      (* TODO decide what to do if our representation is not a block *)
      else ccall_failwith "caml_obj_set_tag";
      Value.Int 0
  | _ -> ccall_failwith "caml_obj_set_tag"

let caml_obj_size vm arg = match arg with
  | Value.Block b -> 
      Value.Int (Utils.block_size arg)
  | _ -> ccall_failwith "caml_obj_size"

let caml_obj_field vm arg1 arg2 = match (arg1, arg2) with
  | Value.Block b, Value.Int idx -> 
      Utils.get_field arg1 idx
  | _ -> ccall_failwith "caml_obj_field"

let caml_obj_set_field vm arg1 arg2 arg3 = match (arg1, arg2, arg3) with
  | Value.Block b, Value.Int idx, _ -> 
      Utils.set_field arg1 idx arg3;
      Value.Int 0
  | _ -> ccall_failwith "caml_obj_field"

let caml_obj_block vm arg1 arg2 = match (arg1, arg2) with
  | Value.Int tag, Value.Int size -> 
      Utils.create_block size tag
  | _ -> ccall_failwith "caml_obj_block"

let caml_obj_dup vm = function
  | Value.Int i -> Value.Int i
  | Value.Block b ->
      let block = {
        Value.tag = b.Value.tag;
        Value.data = Array.make (Array.length b.Value.data) (Value.Int 0);
      } in
      for i = 0 to (Array.length b.Value.data) - 1 do
        block.Value.data.(i) <- b.Value.data.(i)
      done;
      Value.Block block
  | _ -> ccall_failwith "caml_obj_dup"

let caml_obj_truncate vm arg1 arg2 = match (arg1, arg2) with
  | Value.Block b, Value.Int l -> 
      let block = {
        Value.tag = b.Value.tag;
        Value.data = Array.make l (Value.Int 0);
      } in
      for i = 0 to l - 1 do
        block.Value.data.(i) <- b.Value.data.(i)
      done;
      Value.Block block
  | _ -> ccall_failwith "caml_obj_truncate"

let prims () =
  add1 "caml_obj_is_block" caml_obj_is_block;
  add1 "caml_obj_is_int" caml_obj_is_int;
  add1 "caml_obj_tag" caml_obj_tag;
  add2 "caml_obj_set_tag" caml_obj_set_tag;
  add1 "caml_obj_size" caml_obj_size;
  add2 "caml_obj_field" caml_obj_field;
  add3 "caml_obj_set_field" caml_obj_set_field;
  add2 "caml_obj_block" caml_obj_block;
  add1 "caml_obj_dup" caml_obj_dup;
  add2 "caml_obj_truncate" caml_obj_truncate

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
