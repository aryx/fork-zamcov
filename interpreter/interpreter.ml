(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common
module Conv = Conv_obj_value

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception EXIT_ON_STOP
exception Exception_raised

let rec string_of_global_data gb = 
  let s = ref "[| " in
    for i=0 to Array.length gb -1 do
      match gb.(i) with
	| Value.Block b as c -> s := !s^" "^(Conv.string_of_value c)^" "^(string_of_global_data b.Value.data)^";";
	| c -> s := !s^" "^(Conv.string_of_value c)^";";
    done; 
    s := !s^" |]";
    !s

let raise_our_exception vm e = match vm.Vm.caml_trap_pointer with
  | None -> Vm.fatal_error ("exception "^Conv.string_of_exception e)
  | Some p ->
      vm.Vm.stack <- !p;
      vm.Vm.code_pointer <- Conv.get_code (Vm.pop vm);
      vm.Vm.caml_trap_pointer <- Conv.unbox_stack_pointer (Vm.pop vm);
      vm.Vm.environment <- Vm.pop vm;
      vm.Vm.extra_arguments <- Conv.int_of_value (Vm.pop vm);
      raise Exception_raised
 
let raise_exception vm e =
  let v = Conv.value_of_obj (Obj.repr e) in
  let t = Conv.get_field v 0 in
    for i = 0 to Conv.block_size vm.Vm.global_data - 1 do
      if Conv.get_field vm.Vm.global_data i = t then
      begin
	Conv.set_field v 0 (Conv.get_field vm.Vm.global_data i);
	vm.Vm.accumulator <- v;
	raise_our_exception vm v
      end
    done;
    Vm.vm_error ("unknown exception "^Conv.string_of_exception v)

let not_yet_implemented : string -> unit = fun s ->
  Vm.fatal_error (s ^ " is not yet implemented\n")

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let dump_state vm =
  flush stdout;
  Printf.printf
    "==============================\nextra_arguments : %d\naccumulator : %s\n
\ncode_pointer : %d\nglobal_data length : %d\n\n==============================\n"
    (vm.Vm.extra_arguments)
    (Conv.string_of_value vm.Vm.accumulator)
    (vm.Vm.code_pointer)
    (match vm.Vm.global_data with 
       | Value.Block b -> Array.length b.Value.data
       | _ -> -1);
  flush stdout
 
let dump_globals gb =
  print_string "globals : \n [ ";
  let rec aux l =
    match l with 
      | Value.Block b ->
	  print_string " Value.Block : ";
	  for i=0 to (Array.length b.Value.data)-1 do
	    aux b.Value.data.(i)
	  done;
      | v -> print_string (Conv.string_of_value v); print_string "; "
  in
    aux gb;
    print_string "]\n"

let print_block block =
  print_string "block : \n [ ";
  let rec aux l =
    match l with 
      | Value.Block b ->
	  print_string " Block : ";
	  for i=0 to (Array.length b.Value.data)-1 do
	    aux b.Value.data.(i)
	  done;
      | v -> print_string (Conv.string_of_value v); print_string "; "
  in
    aux block;
    print_string "]\n"

let dump_env env =
  print_string "env : \n [ ";
  let rec aux l =
    match l with 
      | Value.Block b ->
	  print_string " Value.Block : [ ";
	  for i=0 to (Array.length b.Value.data)-1 do
	    aux b.Value.data.(i)
	  done;
	  print_string " ] ";
      | v -> print_string (Conv.string_of_value v); print_string "; "
  in
    aux env;
    print_string "]\n"

let dump_stack stack sp =
  print_string "stack : \n [ ";
  let rec aux l =
    match l with 
      | Value.Block b ->
	  print_string " Value.Block : [ ";
	  for i=0 to (Array.length b.Value.data) - 1 do
	    aux b.Value.data.(i)
	  done;
	  print_string " ]; ";
      | v -> print_string (Conv.string_of_value v); print_string "; "
  in
    for i= sp to 1048574 do
      aux stack.(i)
    done;
    print_string "]\n"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let execute_step vm instruction = 
  vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
  try (
    match instruction with
    (* ------------------------------------------------------------------ *)
    (* Basic stack operations *)
    (* ------------------------------------------------------------------ *)
      
    | Instructions.ACC0 -> 
      vm.Vm.accumulator <- Vm.peek vm 0
	
    | Instructions.ACC1 -> 
      vm.Vm.accumulator <- Vm.peek vm 1
	
    | Instructions.ACC2 -> 
      vm.Vm.accumulator <- Vm.peek vm 2

    | Instructions.ACC3 -> 
      vm.Vm.accumulator <- Vm.peek vm 3

    | Instructions.ACC4 -> 
      vm.Vm.accumulator <- Vm.peek vm 4

    | Instructions.ACC5 -> 
      vm.Vm.accumulator <- Vm.peek vm 5

    | Instructions.ACC6 ->
      vm.Vm.accumulator <- Vm.peek vm 6

    | Instructions.ACC7 ->
      vm.Vm.accumulator <- Vm.peek vm 7

    | Instructions.ACC n -> 
      vm.Vm.accumulator <- Vm.peek vm n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.PUSH
    | Instructions.PUSHACC0 ->
      Vm.push vm vm.Vm.accumulator

    | Instructions.PUSHACC1 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 1

    | Instructions.PUSHACC2 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 2

    | Instructions.PUSHACC3 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 3

    | Instructions.PUSHACC4 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 4

    | Instructions.PUSHACC5 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 5

    | Instructions.PUSHACC6 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 6

    | Instructions.PUSHACC7 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm 7

    | Instructions.PUSHACC n -> 
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Vm.peek vm n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.POP n ->
      for i = 1 to n do ignore (Vm.pop vm) done;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.ASSIGN n ->
      Vm.assign vm n vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int 0;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1


    (* ------------------------------------------------------------------ *)
    (* Access in heap-allocated environment *)
    (* ------------------------------------------------------------------ *)


    | Instructions.ENVACC1 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 1
	
    | Instructions.ENVACC2 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 2

    | Instructions.ENVACC3 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 3
	
    | Instructions.ENVACC4 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 4

    | Instructions.ENVACC n ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.PUSHENVACC1 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 1

    | Instructions.PUSHENVACC2 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 2

    | Instructions.PUSHENVACC3 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 3

    | Instructions.PUSHENVACC4 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment 4

    | Instructions.PUSHENVACC n ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.environment n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1



    (* ------------------------------------------------------------------ *)
    (* Function application *)
    (* ------------------------------------------------------------------ *)


    | Instructions.PUSH_RETADDR n ->
      Vm.push vm (Value.Int vm.Vm.extra_arguments);
      Vm.push vm vm.Vm.environment;
      Vm.push vm (Value.Code_pointer (vm.Vm.code_pointer + n));
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.APPLY n ->
      vm.Vm.extra_arguments <- n - 1;
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator

    | Instructions.APPLY1 -> 
      let arg1 = Vm.pop vm in	
      Vm.push vm (Value.Int vm.Vm.extra_arguments);
      Vm.push vm vm.Vm.environment; 
      Vm.push vm (Value.Code_pointer vm.Vm.code_pointer);
      Vm.push vm arg1; 
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator;
      vm.Vm.extra_arguments <- 0
	
    | Instructions.APPLY2 ->
      let arg1 = Vm.pop vm in 
      let arg2 = Vm.pop vm in
      Vm.push vm (Value.Int vm.Vm.extra_arguments);
      Vm.push vm vm.Vm.environment; 
      Vm.push vm (Value.Code_pointer vm.Vm.code_pointer);
      Vm.push vm arg2; 
      Vm.push vm arg1; 
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator;
      vm.Vm.extra_arguments <- 1

    | Instructions.APPLY3 -> 
      let arg1 = Vm.pop vm in 
      let arg2 = Vm.pop vm in
      let arg3 = Vm.pop vm in
      Vm.push vm (Value.Int vm.Vm.extra_arguments);
      Vm.push vm vm.Vm.environment; 
      Vm.push vm (Value.Code_pointer vm.Vm.code_pointer);
      Vm.push vm arg3; 
      Vm.push vm arg2; 
      Vm.push vm arg1; 
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator;
      vm.Vm.extra_arguments <- 2

    | Instructions.APPTERM (nargs,slotsize) ->
      let sp_dist = slotsize - nargs in
      for i = nargs - 1 downto 0 do
        Vm.assign vm (sp_dist + i) (Vm.peek vm i);
      done;
      for i = 1 to sp_dist do ignore (Vm.pop vm) done;
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator; 
      vm.Vm.extra_arguments <- vm.Vm.extra_arguments + nargs - 1
	
    | Instructions.APPTERM1 n ->
      let arg1 = Vm.pop vm in 
      for i = 2 to n do ignore (Vm.pop vm) done;
      Vm.push vm arg1;
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator

    | Instructions.APPTERM2 n ->
      let arg1 = Vm.pop vm in
      let arg2 = Vm.pop vm in 
      for i = 3 to n do ignore (Vm.pop vm) done;
      Vm.push vm arg2;
      Vm.push vm arg1;
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator;
      vm.Vm.extra_arguments <- vm.Vm.extra_arguments + 1

    | Instructions.APPTERM3 n ->
      let arg1 = Vm.pop vm in
      let arg2 = Vm.pop vm in 
      let arg3 = Vm.pop vm in
      for i = 4 to n do ignore (Vm.pop vm) done;
      Vm.push vm arg3;
      Vm.push vm arg2;
      Vm.push vm arg1;
      vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
      vm.Vm.environment <- vm.Vm.accumulator; 
      vm.Vm.extra_arguments <- vm.Vm.extra_arguments + 2

    | Instructions.RETURN n ->
      for i = 1 to n do ignore (Vm.pop vm) done;
      if vm.Vm.extra_arguments > 0 then
	begin
	  vm.Vm.extra_arguments <- vm.Vm.extra_arguments - 1;
	  vm.Vm.code_pointer <- Conv.get_code vm.Vm.accumulator;
	  vm.Vm.environment <- vm.Vm.accumulator;
	end
      else
	begin
	  vm.Vm.code_pointer <- Conv.get_code (Vm.pop vm);
	  vm.Vm.environment <- Vm.pop vm;
	  vm.Vm.extra_arguments <- Conv.int_of_value (Vm.pop vm);
	end
	  
    | Instructions.RESTART ->
      let num_args = (Conv.block_size vm.Vm.environment) - 2 in
      for i = 1 to num_args do
        Vm.push vm (Conv.get_field vm.Vm.environment (num_args - i + 2));
      done;
      vm.Vm.environment <- Conv.get_field vm.Vm.environment 1;
      vm.Vm.extra_arguments <- vm.Vm.extra_arguments + num_args

    | Instructions.GRAB required ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      if vm.Vm.extra_arguments >= required then
        vm.Vm.extra_arguments <- vm.Vm.extra_arguments - required
      else 
        begin
          let c = Conv.create_closure (vm.Vm.code_pointer - 3) 1
            (vm.Vm.extra_arguments + 2) in
          vm.Vm.accumulator <- Value.Closure c;
          c.Value.vars.(0) <- vm.Vm.environment;
          for i = 1 to vm.Vm.extra_arguments + 1 do
            c.Value.vars.(i) <- Vm.pop vm
          done;
          vm.Vm.code_pointer <- Conv.get_code (Vm.pop vm);
          vm.Vm.environment <- Vm.pop vm;
          vm.Vm.extra_arguments <- Conv.int_of_value (Vm.pop vm);
        end

    | Instructions.CLOSURE (nvars,ofs) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      if nvars > 0 then Vm.push vm vm.Vm.accumulator;
      let c = Conv.create_closure (vm.Vm.code_pointer + ofs) 1 nvars in
      for i = 0 to (nvars - 1) do
        c.Value.vars.(i) <- (Vm.pop vm)
      done;
      vm.Vm.accumulator <- Value.Closure c;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.CLOSUREREC (nfuncs,nvars,o,t) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 2;
      if nvars > 0 then Vm.push vm vm.Vm.accumulator;
      let c = Conv.create_closure (vm.Vm.code_pointer + o) nfuncs nvars in
      for i = 0 to nvars - 1 do
        c.Value.vars.(i) <- Vm.pop vm
      done;
      vm.Vm.accumulator <- Value.Closure c;
      Vm.push vm vm.Vm.accumulator;
      for i = 1 to (nfuncs - 1) do
        c.Value.funcs.(i) <- Conv.create_infix c (vm.Vm.code_pointer + t.(i-1)) i;
        Vm.push vm c.Value.funcs.(i)
      done;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + nfuncs
	
    | Instructions.OFFSETCLOSUREM2 -> 
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment (-2)
	
    | Instructions.OFFSETCLOSURE0 -> 
      vm.Vm.accumulator <- vm.Vm.environment
	
    | Instructions.OFFSETCLOSURE2 -> 
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment 2
	
    | Instructions.OFFSETCLOSURE n ->
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.PUSHOFFSETCLOSUREM2 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment (-2)
	
    | Instructions.PUSHOFFSETCLOSURE0 -> 
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- vm.Vm.environment
	
    | Instructions.PUSHOFFSETCLOSURE2 -> 
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment 2

    | Instructions.PUSHOFFSETCLOSURE n ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.offset vm.Vm.environment n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    (* ------------------------------------------------------------------ *)
    (* Access to global variables *)
    (* ------------------------------------------------------------------ *)
        
    | Instructions.GETGLOBAL n ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.global_data n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.PUSHGETGLOBAL n ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.global_data n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.GETGLOBALFIELD (n,p) ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.global_data n;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator p;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 2

    | Instructions.PUSHGETGLOBALFIELD (n,p) ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.global_data n;
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator p;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 2
	
    | Instructions.SETGLOBAL n ->
      Conv.set_field vm.Vm.global_data n vm.Vm.accumulator;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      vm.Vm.accumulator <- Value.Int 0
	
    (* ------------------------------------------------------------------ *)
    (*  Allocation of blocks *)
    (* ------------------------------------------------------------------ *)

    | Instructions.ATOM0 ->
      vm.Vm.accumulator <- Value.atom
	
    | Instructions.PUSHATOM0 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.atom
	
    | Instructions.ATOM n ->
      vm.Vm.accumulator <- Conv.create_block 0 n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.PUSHATOM n ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Conv.create_block 0 n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.MAKEBLOCK (wosize,tag) ->
      let block = Conv.create_block wosize tag in
      Conv.set_field block 0 vm.Vm.accumulator;
      for i = 1 to wosize - 1 do
        Conv.set_field block i (Vm.pop vm);
      done;
      vm.Vm.accumulator <- block;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 2

    | Instructions.MAKEBLOCK1 tag ->
      let block = Conv.create_block 1 tag in
      Conv.set_field block 0 vm.Vm.accumulator;
      vm.Vm.accumulator <- block;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.MAKEBLOCK2 tag ->
      let block = Conv.create_block 2 tag in
      Conv.set_field block 0 vm.Vm.accumulator;
      Conv.set_field block 1 (Vm.pop vm);
      vm.Vm.accumulator <- block;        
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.MAKEBLOCK3 tag ->
      let block = Conv.create_block 3 tag in
      Conv.set_field block 0 vm.Vm.accumulator;
      Conv.set_field block 1 (Vm.pop vm);
      Conv.set_field block 2 (Vm.pop vm);
      vm.Vm.accumulator <- block;        
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

        (* TODO test *)
    | Instructions.MAKEFLOATBLOCK size ->
      let a = Array.make size 0. in
      a.(0) <- Conv.unbox_float vm.Vm.accumulator;
      for i = 1 to size - 1 do
        a.(i) <- Conv.unbox_float (Vm.pop vm);
      done;
      vm.Vm.accumulator <- Value.Double_array a;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    (* ------------------------------------------------------------------ *)
    (* Access to components of blocks *)
    (* ------------------------------------------------------------------ *)

    | Instructions.GETFIELD0 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator 0

    | Instructions.GETFIELD1 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator 1

    | Instructions.GETFIELD2 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator 2

    | Instructions.GETFIELD3 ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator 3

    | Instructions.GETFIELD n | Instructions.GETFLOATFIELD n ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      
    | Instructions.SETFIELD0 ->
      Conv.set_field vm.Vm.accumulator 0 (Vm.pop vm);
      vm.Vm.accumulator <- Value.Int 0

    | Instructions.SETFIELD1 ->
      Conv.set_field vm.Vm.accumulator 1 (Vm.pop vm);
      vm.Vm.accumulator <- Value.Int 0

    | Instructions.SETFIELD2 ->
      Conv.set_field vm.Vm.accumulator 2 (Vm.pop vm);
      vm.Vm.accumulator <- Value.Int 0

    | Instructions.SETFIELD3 ->
      Conv.set_field vm.Vm.accumulator 3 (Vm.pop vm);
      vm.Vm.accumulator <- Value.Int 0

    | Instructions.SETFIELD n | Instructions.SETFLOATFIELD n -> 
      Conv.set_field vm.Vm.accumulator n (Vm.pop vm);
      vm.Vm.accumulator <- Value.Int 0;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    (* ------------------------------------------------------------------ *)
    (* Array & String operations *)
    (* ------------------------------------------------------------------ *)

    | Instructions.VECTLENGTH ->
      let size = match vm.Vm.accumulator with
        | Value.Block b ->
          (match b.Value.tag with
          | Value.Raw_tag _ -> Obj.size (Conv.unbox_custom b.Value.data.(0))
          | _ -> Array.length b.Value.data)
        | Value.Double_array a -> Array.length a
	| e -> Vm.fatal_error ("VECTLENGTH : "^(Conv.string_of_value e))
      in
      vm.Vm.accumulator <- Value.Int size
	
    | Instructions.GETVECTITEM | Instructions.GETSTRINGCHAR ->
      vm.Vm.accumulator <- Conv.get_field vm.Vm.accumulator (Conv.int_of_value (Vm.pop vm))
	
    | Instructions.SETVECTITEM | Instructions.SETSTRINGCHAR ->
      let n = Conv.int_of_value (Vm.pop vm) in
      Conv.set_field vm.Vm.accumulator n (Vm.pop vm)

    (* ------------------------------------------------------------------ *)
    (* Branches and conditional branches *)
    (* ------------------------------------------------------------------ *)

    | Instructions.BRANCH n -> 
      vm.Vm.code_pointer <- vm.Vm.code_pointer + n

    | Instructions.BRANCHIF n ->
      if vm.Vm.accumulator <> Value.Int 0 then
	vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      else
        vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	  
    | Instructions.BRANCHIFNOT n -> 
      if vm.Vm.accumulator = Value.Int 0 then
	vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      else
	vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	  
    | Instructions.SWITCH (sizes,tab) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      let index =
        if Conv.is_block vm.Vm.accumulator then
          (sizes land 0xFFFF) + Conv.get_block_tag_int vm.Vm.accumulator
        else 
          Conv.int_of_value vm.Vm.accumulator
      in
      vm.Vm.code_pointer <- vm.Vm.code_pointer + tab.(index)
	
    | Instructions.BOOLNOT -> 
      vm.Vm.accumulator <- 
	(match vm.Vm.accumulator with
	  Value.Int i -> 
	    if i = 0 then Value.Int 1
	    else Value.Int 0
	| _ -> Vm.fatal_error "error BOOLNOT")
	
    (* ------------------------------------------------------------------ *)
    (* Exceptions *)
    (* ------------------------------------------------------------------ *)
	
    | Instructions.PUSHTRAP n ->
      Vm.push vm (Value.Int (vm.Vm.extra_arguments));
      Vm.push vm vm.Vm.environment;
      Vm.push vm (Value.Stack_pointer vm.Vm.caml_trap_pointer);
      Vm.push vm (Value.Code_pointer (vm.Vm.code_pointer + n));
      vm.Vm.caml_trap_pointer <- Some (ref vm.Vm.stack);
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1

    | Instructions.POPTRAP ->
      vm.Vm.caml_trap_pointer <- Conv.unbox_stack_pointer (Vm.peek vm 1);
      for i = 1 to 4 do ignore (Vm.pop vm) done

    | Instructions.RAISE -> 
      raise_our_exception vm vm.Vm.accumulator

    (* ------------------------------------------------------------------ *)
    (* Stack checks *)
    (* ------------------------------------------------------------------ *)
	
    (* ------------------------------------------------------------------ *)
    (* Signal handling *)
    (* ------------------------------------------------------------------ *)
	
    | Instructions.CHECK_SIGNALS -> ()
      
    (* ------------------------------------------------------------------ *)
    (* Calling C functions *)
    (* ------------------------------------------------------------------ *)

        (* TODO are we sure we don't need to push the environment as specified? *)
    | Instructions.C_CALL1 n ->
      let a0 = vm.Vm.accumulator in
      (try 
         vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbl1.(n) vm a0;
	 vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;		   
       with
       | e -> raise_exception vm e)
	
    | Instructions.C_CALL2 n ->
      let a0 = vm.Vm.accumulator in
      let a1 = Vm.pop vm in
      (try 
         vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbl2.(n) vm a0 a1;
	 vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
       with
       | e -> raise_exception vm e)

    | Instructions.C_CALL3 n -> 
      let a0 = vm.Vm.accumulator in
      let a1 = Vm.pop vm in
      let a2 = Vm.pop vm in
      (try 
         vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbl3.(n) vm a0 a1 a2;
	 vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
       with
       | e -> raise_exception vm e)
	
    | Instructions.C_CALL4 n ->
      let a0 = vm.Vm.accumulator in
      let a1 = Vm.pop vm in
      let a2 = Vm.pop vm in
      let a3 = Vm.pop vm in
      (try 
         vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbl4.(n) vm a0 a1 a2 a3;
	 vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
       with
       | e -> raise_exception vm e)
	
    | Instructions.C_CALL5 n ->
      let a0 = vm.Vm.accumulator in
      let a1 = Vm.pop vm in
      let a2 = Vm.pop vm in
      let a3 = Vm.pop vm in
      let a4 = Vm.pop vm in
      (try
         vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbl5.(n) vm a0 a1 a2 a3 a4;
	 vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
       with
       | e -> raise_exception vm e)

    | Instructions.C_CALLN (nargs,p) ->
      Vm.push vm vm.Vm.accumulator;
      let args = Array.make nargs (Value.Int 0) in
      for i = 0 to (nargs - 1) do
	args.(i) <- Vm.pop vm;
      done;
      begin
	try
          vm.Vm.accumulator <- vm.Vm.prim_table.Vm.tbln.(p) vm args nargs;
	  vm.Vm.code_pointer <- vm.Vm.code_pointer + 2;
	with
	| e -> raise_exception vm e
      end
	
    (* ------------------------------------------------------------------ *)
    (*  Integer constants *)
    (* ------------------------------------------------------------------ *)
	
    | Instructions.CONST0 -> 
      vm.Vm.accumulator <- Value.Int 0
	
    | Instructions.CONST1 ->
      vm.Vm.accumulator <- Value.Int 1
	
    | Instructions.CONST2 ->
      vm.Vm.accumulator <- Value.Int 2

    | Instructions.CONST3 ->
      vm.Vm.accumulator <- Value.Int 3

    | Instructions.PUSHCONST0 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int 0
	
    | Instructions.PUSHCONST1 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int 1

    | Instructions.PUSHCONST2 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int 2
	
    | Instructions.PUSHCONST3 ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int 3

    | Instructions.PUSHCONSTINT n ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.CONSTINT n ->
      vm.Vm.accumulator <- Value.Int n;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    (* ------------------------------------------------------------------ *)
    (* Integer arithmetic *)
    (* ------------------------------------------------------------------ *)

    | Instructions.NEGINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator with 
	  Value.Int i -> Value.Int (-i)
	| _ -> Vm.fatal_error "wrong arguments NEGINT")
	
    | Instructions.ADDINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 + i2)
	| _ -> Vm.fatal_error "wrong arguments ADDINT")
	
    | Instructions.SUBINT ->  
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 - i2)
	| _ -> Vm.fatal_error "wrong arguments SUBINT")
	
    | Instructions.MULINT ->  
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 * i2)
	| _ -> Vm.fatal_error "wrong arguments MULINT")

    | Instructions.DIVINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> 
	    (try Value.Int (i1 / i2)
	     with Division_by_zero -> raise_exception vm Division_by_zero);
	| _ -> Vm.fatal_error "wrong arguments DIVINT")
	
    | Instructions.MODINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> 
	    (try Value.Int (i1 mod i2)
	     with Division_by_zero -> raise_exception vm Division_by_zero);
	| _ -> Vm.fatal_error "wrong arguments MODINT")
	
    | Instructions.ANDINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 land i2)
	| _ -> Vm.fatal_error "wrong arguments ANDINT")

    | Instructions.ORINT -> 
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 lor i2)
	| _ -> Vm.fatal_error "wrong arguments ORINT")
	
    | Instructions.XORINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 lxor i2)
	| _ -> Vm.fatal_error "wrong arguments XORINT")

    | Instructions.LSLINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 lsl i2)
	| _ -> Vm.fatal_error "wrong arguments LSLINT")

    | Instructions.LSRINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 lsr i2)
	| _ -> Vm.fatal_error "wrong arguments LSRINT")

    | Instructions.ASRINT ->
      vm.Vm.accumulator <-
	(match vm.Vm.accumulator, Vm.pop vm with 
	  Value.Int i1, Value.Int i2 -> Value.Int (i1 asr i2)
	| _ -> Vm.fatal_error "wrong arguments ASRINT")

    | Instructions.EQ ->
      vm.Vm.accumulator <-
	if Conv.pequal vm.Vm.accumulator (Vm.pop vm)
	then Value.Int 1
	else Value.Int 0

    | Instructions.NEQ ->
      vm.Vm.accumulator <- 
	if Conv.pequal vm.Vm.accumulator (Vm.pop vm)
	then Value.Int 0
	else Value.Int 1

    | Instructions.LTINT ->
      vm.Vm.accumulator <- 
	(match vm.Vm.accumulator, Vm.pop vm with 
	| Value.Int i1, Value.Int i2 -> if i1 < i2 then Value.Int 1 else Value.Int 0
	| _ -> Vm.fatal_error "wrong arguments LTINT")

    | Instructions.LEINT ->
      vm.Vm.accumulator <- 
	(match vm.Vm.accumulator, Vm.pop vm with 
	| Value.Int i1, Value.Int i2 -> if i1 <= i2 then Value.Int 1 else Value.Int 0
	| _ -> Vm.fatal_error "wrong arguments LEINT")

    | Instructions.GTINT ->
      vm.Vm.accumulator <- 
	(match vm.Vm.accumulator, Vm.pop vm with 
	| Value.Int i1, Value.Int i2 -> if i1 > i2 then Value.Int 1 else Value.Int 0
	| _ -> Vm.fatal_error "wrong arguments GTINT")

    | Instructions.GEINT ->
      vm.Vm.accumulator <- 
	(match vm.Vm.accumulator, Vm.pop vm with 
	| Value.Int i1, Value.Int i2 -> if i1 >= i2 then Value.Int 1 else Value.Int 0
	| _ -> Vm.fatal_error "wrong arguments GEINT")

    | Instructions.ULTINT ->
      if Conv.ult (Conv.int_of_value vm.Vm.accumulator)
	(Conv.int_of_value (Vm.pop vm)) then
	vm.Vm.accumulator <- Value.Int 1
      else 
	vm.Vm.accumulator <- Value.Int 0

    | Instructions.UGEINT ->
      if not (Conv.ult (Conv.int_of_value vm.Vm.accumulator)
		(Conv.int_of_value (Vm.pop vm))) then
	vm.Vm.accumulator <- Value.Int 1
      else 
	vm.Vm.accumulator <- Value.Int 0

    | Instructions.BEQ (temp,n) -> 
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i1 = i2 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1)

    | Instructions.BNEQ (temp,n) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i1 <> i2 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | Value.Int _, _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
      | _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n)
	
    | Instructions.BLTINT (temp,n) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i2 < i1 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | Value.Int _, _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
      | _ -> Vm.fatal_error "wrong arguments BLTINT")
	
    | Instructions.BLEINT (temp,n) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i2 <= i1 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | Value.Int _, _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
      | _ -> Vm.fatal_error "wrong arguments BLEINT")
	
    | Instructions.BGTINT (temp,n) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i2 > i1 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | Value.Int _, _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
      | _ -> Vm.fatal_error "wrong arguments BGTINT ")
	
    | Instructions.BGEINT (temp,n) ->
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1;
      (match vm.Vm.accumulator, temp with 
      | Value.Int i1, i2 when i2 >= i1 -> vm.Vm.code_pointer <- vm.Vm.code_pointer + n
      | Value.Int _, _ -> vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
      | _ -> Vm.fatal_error "wrong arguments BGEINT")
	
    | Instructions.BULTINT (n,ofs) ->
      if Conv.ult n (Conv.int_of_value vm.Vm.accumulator) then
        vm.Vm.code_pointer <- vm.Vm.code_pointer + 1 + ofs
      else
	vm.Vm.code_pointer <- vm.Vm.code_pointer + 2
	  
    | Instructions.BUGEINT (n,ofs) ->
      if not (Conv.ult n (Conv.int_of_value vm.Vm.accumulator)) then
        vm.Vm.code_pointer <- vm.Vm.code_pointer + 1 + ofs
      else
	vm.Vm.code_pointer <- vm.Vm.code_pointer + 2

    | Instructions.OFFSETINT n ->
      vm.Vm.accumulator <- Value.Int (Conv.int_of_value vm.Vm.accumulator + n);
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.OFFSETREF n ->
      Conv.set_field vm.Vm.accumulator 0
	(Value.Int (Conv.int_of_value (Conv.get_field vm.Vm.accumulator 0) + n));
      vm.Vm.accumulator <- Value.Int 0;
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 1
	
    | Instructions.ISINT ->
      (match vm.Vm.accumulator with 
      | Value.Int x -> vm.Vm.accumulator <- Value.Int 1
      | _ -> vm.Vm.accumulator <- Value.Int 0)
	
    (* ------------------------------------------------------------------ *)
    (* Object-oriented operations *)
    (* ------------------------------------------------------------------ *)
	
    | Instructions.GETMETHOD -> 
      vm.Vm.accumulator <- 
	Conv.get_field (Conv.get_field (Vm.peek vm 0) 0) (Conv.int_of_value vm.Vm.accumulator)
	
    | Instructions.GETPUBMET (tag,_) ->
      Vm.push vm vm.Vm.accumulator;
      vm.Vm.accumulator <- Value.Int tag;
      let meths = Conv.get_field (Vm.peek vm 0) 0 in
      let li = ref 3 in
      let hi = ref (Conv.int_of_value (Conv.get_field meths 0) * 2 + 1) in
      while !li < !hi do
	let mi = (((!li + !hi) lsr 1) lor 1) in 
	if (Conv.int_of_value vm.Vm.accumulator) < (Conv.int_of_value (Conv.get_field meths mi)) then
	  hi := mi - 2
	else
	  li := mi
      done;
      vm.Vm.accumulator <- Conv.get_field meths (!li - 1);
      vm.Vm.code_pointer <- vm.Vm.code_pointer + 2;
      
    | Instructions.GETDYNMET ->
      let meths = Conv.get_field (Vm.peek vm 0) 0 in
      let li = ref 3 in
      let hi = ref (Conv.int_of_value (Conv.get_field meths 0) * 2 + 1) in
      while !li < !hi do
	let mi = (((!li + !hi) lsr 1) lor 1) in 
	if (Conv.int_of_value vm.Vm.accumulator) < (Conv.int_of_value (Conv.get_field meths mi)) then
	  hi := mi - 2
	else
	  li := mi
      done;
      vm.Vm.accumulator <- Conv.get_field meths (!li - 1);

    (* ------------------------------------------------------------------ *)
    (* Debugging and machine control *)
    (* ------------------------------------------------------------------ *)
      
    | Instructions.STOP -> ()
      
    | Instructions.EVENT ->  not_yet_implemented "EVENT"
      
    | Instructions.BREAK ->  not_yet_implemented "BREAK"
      
    | Instructions.Param n -> 
      Vm.fatal_error (spf "error Not an instruction Parameter %d" n)
  )
  with
  | Exception_raised -> ()
  | (Vm.Fatal_error s | Vm.Vm_error s) as exn ->
    Debug_events.print_backtrace vm;
    raise exn
  | e -> raise e
