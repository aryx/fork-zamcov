(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_array.ml                                                  *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

module Conv = Conv_obj_value
open Ffi

let caml_array_get vm a ind =
  let i = Conv.int_of_value ind in
  if i >= Conv.block_size a || i < 0 then
    raise (Invalid_argument "index out of bounds")
  else 
    Conv.get_field a i

let caml_array_unsafe_get vm a i =
  Conv.get_field a (Conv.int_of_value i)

let caml_array_set vm a ind v =
  let i = Conv.int_of_value ind in
  if i >= Conv.block_size a || i < 0 then
    raise (Invalid_argument "index out of bounds")
  else
    begin
      Conv.set_field a i v;
      Value.Int 0
    end

let caml_array_unsafe_set vm a i v =
  Conv.set_field a (Conv.int_of_value i) v;
  Value.Int 0

(* TODO review & test *)
let caml_make_vect vm arg1 init = 
  match (arg1, init) with
  | Value.Int len, Value.Float _ ->
      Value.Double_array (Array.make len (Conv.unbox_float init))
  | Value.Int len, _ ->
      Value.Block {
        Value.tag = Value.Zero_tag;
        Value.data = Array.make len init;
      }
  | _ -> ccall_failwith "error caml_make_vect"

(* TODO review & test *)
let caml_make_array vm arg = 
  match arg with
  | Value.Block block ->
      let size = Conv.block_size arg in
      let v = Conv.get_field arg 0 in
      if size = 0 then
        arg
      else
        if Conv.is_float v then
          begin
            let res = Array.make size 0. in
            for i=0 to (size-1) do
              res.(i) <- Conv.unbox_float (Conv.get_field arg i)
            done;
            Value.Double_array res
          end
        else
          arg
  | _ -> ccall_failwith "error caml_make_array"

(* external concat : 'a array list -> 'a array = "caml_array_concat" *)
let caml_array_concat vm (xs: Value.value) =
  raise Common.Todo

let prims () =
  add2 "caml_array_get" caml_array_get;
  add2 "caml_array_get_addr" caml_array_get;
  add2 "caml_array_get_float" caml_array_get;

  add2 "caml_array_unsafe_get" caml_array_unsafe_get;
  add2 "caml_array_unsafe_get_float" caml_array_unsafe_get;

  add3 "caml_array_set" caml_array_set;
  add3 "caml_array_set_addr" caml_array_set;
  add3 "caml_array_set_float" caml_array_set;

  add3 "caml_array_unsafe_set" caml_array_unsafe_set;
  add3 "caml_array_unsafe_set_addr" caml_array_unsafe_set;
  add3 "caml_array_unsafe_set_float" caml_array_unsafe_set;

  add2 "caml_make_vect" caml_make_vect;

  add1 "caml_make_array" caml_make_array;
  add1 "caml_array_concat" caml_array_concat;
  ()


let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
