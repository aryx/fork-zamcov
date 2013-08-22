(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_compare.ml                                                *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

exception Break

let compare_data cmp a b =
  let t = ref 0 in
  (try
    for i = 0 to (min (Array.length a) (Array.length b) - 1) do
      t := (cmp a.(i) b.(i));
      if !t <> 0 then raise Break
    done
  with 
  | Break -> ());
  if !t = 0 then
    if Array.length a < Array.length b then -1
    else 1
  else
    !t

let rec compare a b =
  if a = b then 0
  else 
    match a, b with
      | Value.Block x, Value.Block y ->
          if x.Value.tag <> y.Value.tag then
            Utils.get_block_tag_int a - Utils.get_block_tag_int b
          else
            compare_data compare x.Value.data y.Value.data
      | Value.Double_array x, Value.Double_array y ->
          compare_data (Pervasives.compare : float -> float -> int) x y
      | _, Value.Block y ->
        (match y.Value.tag with
          | Value.Forward_tag -> compare a y.Value.data.(0)
          | _ -> -1)
      | Value.Block x, _ ->
        (match x.Value.tag with
          | Value.Forward_tag -> compare x.Value.data.(0) b
          | _ -> 1)
      | Value.Int x, Value.Int y -> if x < y then -1 else 1
      | Value.Float x, Value.Float y -> (Pervasives.compare : float -> float -> int) x y
      | Value.String x, Value.String y -> (Pervasives.compare : string -> string -> int) x y
      | Value.Custom o1, Value.Custom o2 -> Pervasives.compare o1 o2;
      | _ -> ccall_failwith ("compare "^Utils.string_of_value a^" "^Utils.string_of_value b)

let caml_compare vm arg1 arg2 =
  Value.Int (compare arg1 arg2)

let caml_equal vm arg1 arg2 =
  if (compare arg1 arg2) = 0 then
    Value.Int 1
  else
    Value.Int 0

let caml_notequal vm arg1 arg2 =
  if (compare arg1 arg2) <> 0 then
    Value.Int 1
  else
    Value.Int 0

let caml_lessthan vm arg1 arg2 =
  if (compare arg1 arg2) < 0 then
    Value.Int 1
  else
    Value.Int 0

let caml_lessequal vm arg1 arg2 =
  if (compare arg1 arg2) <= 0 then
    Value.Int 1
  else
    Value.Int 0

let caml_greaterthan vm arg1 arg2 =
  if (compare arg1 arg2) > 0 then
    Value.Int 1
  else
    Value.Int 0

let caml_greaterequal vm arg1 arg2 =
  if (compare arg1 arg2) >= 0 then
    Value.Int 1
  else
    Value.Int 0

let prims () =
  add2 "caml_float_compare"  caml_compare;
  add2 "caml_int_compare"    caml_compare;
  add2 "caml_string_compare" caml_compare;
  add2 "caml_compare"        caml_compare;

  add2 "caml_equal"        caml_equal;
  add2 "caml_eq_float"     caml_equal;
  add2 "caml_string_equal" caml_equal;

  add2 "caml_notequal"        caml_notequal;
  add2 "caml_neq_float"       caml_notequal;
  add2 "caml_string_notequal" caml_notequal;

  add2 "caml_lessthan"        caml_lessthan;
  add2 "caml_string_lessthan" caml_lessthan;
  add2 "caml_lt_float"        caml_lessthan;

  add2 "caml_lessequal"        caml_lessequal;
  add2 "caml_string_lessequal" caml_lessequal;
  add2 "caml_le_float"         caml_lessequal;

  add2 "caml_greaterthan"        caml_greaterthan;
  add2 "caml_string_greaterthan" caml_greaterthan;
  add2 "caml_gt_float"           caml_greaterthan;

  add2 "caml_greaterequal"        caml_greaterequal;
  add2 "caml_string_greaterequal" caml_greaterequal;
  add2 "caml_ge_float"            caml_greaterequal

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
