(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: utils.ml                                                      *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Utils

(*****************************************************************************)
(* Value -> (int, string, Obj) *)
(*****************************************************************************)

let int_of_tag = function
  | Value.Zero_tag -> 0
  | Value.Structured_tag t -> t
  | Value.Raw_tag t -> t
  | Value.Object_tag -> Obj.object_tag
  | Value.Forward_tag -> Obj.forward_tag
  | Value.Abstract_tag -> Obj.abstract_tag

let get_block_tag_int = function
  | Value.Block b -> int_of_tag b.Value.tag
  | Value.Infix _ -> Obj.infix_tag
  | Value.Closure _ -> Obj.closure_tag
  | Value.Double_array _ -> Obj.double_array_tag
  | Value.Float _ -> Obj.double_tag
  | Value.String _ -> Obj.string_tag
  | Value.Custom _ -> Obj.custom_tag
  | Value.Int _ | Value.Code_pointer _ | Value.Stack_pointer _ -> 
    Vm.vm_error "error get_block_tag_int : not a block"

let string_of_tag = function
  | Value.Zero_tag -> "Zero"
  | Value.Structured_tag t -> string_of_int t
  | Value.Raw_tag t -> string_of_int t
  | Value.Object_tag -> "Object"
  | Value.Forward_tag -> "Forward"
  | Value.Abstract_tag -> "Abstract"

let string_of_value = function
  | Value.Code_pointer p -> "Code_pointer "^(string_of_int p)
  | Value.Stack_pointer _ -> "Stack_pointer"
  | Value.Int i -> if i >= 0 then "Int "^(string_of_int i) else
      "Int -"^(string_of_int (abs i))
  | Value.Float f -> "Float "^(string_of_float f)
  | Value.String _ -> "String"
  | Value.Double_array a -> "Double Array "^string_of_int (Array.length a)
  | Value.Block b -> "Block "^string_of_tag b.Value.tag
  | Value.Infix _ -> "Infix"
  | Value.Closure _ -> "Closure"  
  (* | Value.Closure c -> Printf.sprintf "Closure(%d)" (Obj.magic c)  *)
  | Value.Custom _ -> "Custom"

let unbox_string = function
  | Value.String s -> s
  | _ -> Vm.vm_error "unbox_string"

(* get the value of a float from a block *)
let rec unbox_float = function
  | Value.Float f1 -> f1
  | f -> Vm.vm_error ("error unbox_float "^(string_of_value f))

let unbox_custom = function
  | Value.Custom o -> o
  | _ -> Vm.vm_error "unbox_custom"

let unbox_stack_pointer = function
  | Value.Stack_pointer p -> p
  | _ -> Vm.vm_error "unbox_stack_pointer"

let unbox_block = function
  | Value.Block b -> b
  | _ -> Vm.vm_error "unbox_block"

let unbox_closure = function
  | Value.Closure c -> c
  | _ -> Vm.vm_error "unbox_closure"

let rec int_of_value v =
  match v with 
    | Value.Int i -> i
    | v -> Vm.vm_error ("error int_of_value "^(string_of_value v))

let float_of_value v =
  match v with 
    | Value.Float f -> f
    | v -> Vm.vm_error ("error float_of_value "^(string_of_value v))

let string_of_value_array vect =
  Array.fold_left (fun r e -> r ^ string_of_value e ^ " ; ") "[| " vect  ^ " |]"
    
let is_block = function
  | Value.Block _ | Value.Infix _ -> true
  | _ -> false

let is_infix = function
  | Value.Infix _ -> true
  | _ -> false
  
let is_int = function
    Value.Int _ -> true
  | _ -> false

let is_float = function
    Value.Float _ -> true
  | _ -> false
 
let block_size = function
  | Value.Block x -> Array.length x.Value.data
  | Value.Double_array a -> Array.length a
  | Value.Closure c -> 2*c.Value.nfuncs + Array.length c.Value.vars - 1
  | value -> Vm.vm_error ("error block_size : "^string_of_value value)
 
let get_block_tag = function
  | Value.Block b -> b.Value.tag
  | _ -> Vm.vm_error "error get_block_tag : not a Block"
 
let set_block_tag block tag =  
  match block with 
    | Value.Block b -> b.Value.tag <- tag
    | _ -> Vm.vm_error "error set_block_tag"

let get_data = function
    Value.Block b -> b.Value.data
  | _ -> Vm.vm_error "get_data"

let get_parent = function
  | Value.Infix i -> i.Value.parent
  | _ -> Vm.vm_error "get parent"

let rec get_code = function
  | Value.Code_pointer i -> i
  | Value.Closure c -> get_code c.Value.funcs.(0)
  | Value.Infix i -> i.Value.icode
  | value -> Vm.vm_error ("error get_code : "^(string_of_value value))
	
let rec obj_of_block b =
  let seen = ref [] in
  let rec aux b = match b.Value.tag with
    | Value.Raw_tag i -> unbox_custom b.Value.data.(0)
    | t' ->
        let t = int_of_tag t' in
        let d = b.Value.data in
        if t >= Obj.no_scan_tag then Vm.vm_error "error obj_of_value not completely implemented";
        let res = Obj.new_block t (Array.length d) in
        seen := (d, res)::!seen;
        for i = 0 to Array.length d - 1 do
          if is_block d.(i) then
            (try
              Obj.set_field res i (List.assq (get_data d.(i)) !seen);
            with
              | Not_found -> Obj.set_field res i (aux (unbox_block d.(i))))
          else
            Obj.set_field res i (obj_of_value d.(i))
        done;
        res
  in
  aux b
and obj_of_value = function
  | Value.Code_pointer _ 
  | Value.Stack_pointer _ -> Vm.vm_error "obj_of_value of pointer"
  | Value.Int n -> Obj.repr n
  | Value.Float f -> Obj.repr f
  | Value.String s -> Obj.repr s
  | Value.Double_array a -> Obj.repr a
  | Value.Custom o -> o
  | Value.Block b -> obj_of_block b
  | Value.Closure _ -> Vm.vm_error "obj_of_value: closures not yet handled"
  | Value.Infix _ -> Vm.vm_error "obj_of_value: infix not yet handled"

(*****************************************************************************)
(* Obj -> Value *)
(*****************************************************************************)

let create_closure pointer nfuncs nvars =
  let res = { 
    Value.funcs = Array.make nfuncs (Value.Int 0);
    Value.vars = Array.make nvars (Value.Int 0);
    Value.nfuncs = nfuncs;
  } in
  res.Value.funcs.(0) <- Value.Code_pointer pointer;
  res
    
let value_of_obj o =
  let seen = ref [] in
  let rec aux o =
    if Obj.is_int o
    then Value.Int (Obj.obj o)
    else
      try
        let data = List.assq o !seen in
        Value.Block {
          Value.tag = Value.Structured_tag (Obj.tag o);
          Value.data = data;
        }
      with
        | Not_found ->
          if Obj.tag o = Obj.double_tag then Value.Float (Obj.obj o)
          else if Obj.tag o = Obj.custom_tag then Value.Custom o
          else if Obj.tag o = Obj.string_tag then Value.String (Obj.obj o)
          else if Obj.tag o = Obj.double_array_tag then Value.Double_array (Obj.obj o)
          else if Obj.tag o = Obj.closure_tag then
            begin
              print_endline "!!! TODO value_of_obj closure";
              Value.Closure (create_closure (-1) 1 0) (* TODO *)
            end
         (* TODO deal all known tags separately *)
          else if Obj.tag o < Obj.no_scan_tag then
            begin
              let res = Array.make (Obj.size o) (Value.Int 0)  in
              seen := (o,res)::!seen;
              for i = 0 to Obj.size o - 1 do
                res.(i) <- aux (Obj.field o i)
              done;
              Value.Block {
                Value.tag = Value.Structured_tag (Obj.tag o);
                Value.data = res;
              }
            end
          else
            Value.Block {
              Value.tag = Value.Raw_tag (Obj.tag o);
              Value.data = Array.make 1 (Value.Custom o);
            }
  in
  aux o

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* set the value of the field data of a block at index idx *)      
(* TODO review & test *)
let set_field block idx v =
  match block, v with
    | Value.Infix _, _ -> Vm.vm_error "set_field Infix"
    | Value.Block b, _ -> b.Value.data.(idx) <- v
    | Value.Double_array a, Value.Float f -> a.(idx) <- f
    | Value.String s, Value.Int i when i >= 0 -> String.set s idx (char_of_int (i mod 256))
    | Value.String s, Value.Int i when i mod 256 = 0 -> String.set s idx '\000'
    | Value.String s, Value.Int i -> String.set s idx (char_of_int ((i mod 256) + 256))
    | v, _ -> Vm.vm_error ("can't set_field of "^(string_of_value v))

let create_block size tag = (* TODO deal with known tags *)
  assert (size >= 0);
  Value.Block { 
    Value.tag = Value.Structured_tag tag;
    Value.data = Array.make (size) (Value.Int 0);
  }

let create_infix parent pointer idx = 
  Value.Infix {
    Value.icode = pointer;
    Value.parent = parent;
    Value.idx = idx;
  }



(* get the value of the field data of a block at index idx *)      
let get_field block idx =  
  match block with
    | Value.Closure c when idx < 2*c.Value.nfuncs - 1 ->
        if idx mod 2 = 1 then
          Vm.vm_error ("get_field closure "^string_of_int idx)
        else
          c.Value.funcs.(idx/2)
    | Value.Closure c -> c.Value.vars.(idx - 2*c.Value.nfuncs + 1)
    | Value.Infix b when idx = 0 -> Value.Code_pointer b.Value.icode
    | Value.Infix b ->
        let parent = b.Value.parent in
        if 2*b.Value.idx + idx < 2*parent.Value.nfuncs - 1 then
          parent.Value.funcs.(b.Value.idx + idx/2)
        else
          parent.Value.vars.(2*b.Value.idx + idx - 2*parent.Value.nfuncs + 1)
    | Value.Block b -> b.Value.data.(idx)
    | Value.Double_array a -> Value.Float a.(idx)
    | Value.String s -> Value.Int (int_of_char (String.get s idx))
    | v -> Vm.vm_error ("can't get_field of "^(string_of_value v))  

let offset block ofs =
  match block with
    | Value.Infix b ->
        let parent = b.Value.parent in
        let idx = b.Value.idx + (ofs/2) in
        if idx = 0 then Value.Closure parent
        else parent.Value.funcs.(idx)
    | Value.Closure c ->
	get_field block ofs (* TODO verify *)
    | Value.Block b when b.Value.tag <> Value.Object_tag ->
	get_field block ofs (* TODO verify *)
    | _ -> Vm.vm_error "error offset" 
 
let ult a b =
  if a >= 0 then
    ((b < 0) || (a < b))
  else
    ((b < 0) && (a > b))

let string_of_exception e =
  let name = unbox_string (get_field (get_field e 0) 0) in
  let args =
    if block_size e >= 2 then
      let l =
        if block_size e = 2 && is_block (get_field e 1) && get_block_tag_int (get_field e 1) = 0
        then Array.to_list (get_data (get_field e 1))
        else List.tl (Array.to_list (get_data e))
      in
      let string_of_val = function
        | Value.String s -> "\""^s^"\""
        | Value.Int i -> string_of_int i
        | _ -> "_"
      in
      "("^List.fold_left (fun s -> fun v -> s^", "^string_of_val v)
                         (string_of_val (List.hd l))
                         (List.tl l)^")"
    else ""
  in
    name^args

let pequal x y = match x,y with
  | Value.Block b1, Value.Block b2 -> b1.Value.data == b2.Value.data
  | a, b -> a = b

let cnum = ref 1

let create_callback vm orig_clos = match orig_clos with
  | Value.Closure _ ->
    let num = !cnum in
    incr cnum;
    fun arg ->
      let code_size = Array.length vm.Vm.code in
      let code = Array.make (code_size + 7) (Instructions.STOP) in
      Array.blit vm.Vm.code 0 code 0 (code_size);
      code.(code_size) <- Instructions.ACC (1 + 3);
      code.(code_size + 1) <- Instructions.Param 4;
      code.(code_size + 2) <- Instructions.APPLY 1;
      code.(code_size + 3) <- Instructions.Param 1;
      code.(code_size + 4) <- Instructions.POP 1;
      code.(code_size + 5) <- Instructions.Param 1;
      code.(code_size + 6) <- Instructions.STOP;
      let vm' = Vm.copy vm ("callback"^string_of_int num) code in
      vm'.Vm.code_pointer <- code_size;
      Vm.push vm' orig_clos;
      Vm.push vm' (Value.Int 0); (* extra_args (0) *)
      Vm.push vm' (Value.Int 0); (* enivornment (Unit) *)
      Vm.push vm' (Value.Code_pointer (code_size + 4));
      Vm.push vm' (value_of_obj (Obj.repr arg));
      Vm.run vm';
  | _ -> Vm.vm_error "create_callback: Wrong argument"
