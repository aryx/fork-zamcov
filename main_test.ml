(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * todo: how avoid interpreter unknown flag and just pass them
 * in Vm.args? Use Arg.Rest ?
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*
 * The main loop of execution:
 * - reading of the different sections (CODE, DATA, DEBUG...) of the bytecode
 * - interpretation of all the instructions of the CODE section
 *)
let main_action file args =
  if not (Sys.file_exists file) 
  then Utils.fatal_error ("cannot find file "^file);

  Vm.exec := file;
  Vm.args := Array.of_list args;

  (* load the OCaml bytecode exe *)
  let data = Bytecode_loader.load_file file in

  (* the CODE section of the exe *)
  let code_section = data.Bytecode_loader.code_section in
  (* the DATA section of the exe converted in a value type *)
  let data_section = Utils.value_of_obj data.Bytecode_loader.data_section in
  (* the PRIM section of the exe *)
  let primitive_section = data.Bytecode_loader.primitive_section in 

  (* initialisation of C primitives using the information of the DLLS section
   * of the exe *)
  Ffi.init data.Bytecode_loader.dlls_section;

  Plugin.init data;

  (* initialisation of the virtual machine environment *)
  Vm.run 
    (Vm.init 
       "main" 
       code_section 
       data_section 
       Interpreter.execute_step
       (match !Plugin.plugin_list with
       | [] -> (fun _ -> fun _ -> ())
       | [p] -> p.Plugin.step
       | _ -> Plugin.step
       )
       (Ffi.load primitive_section)
    );

  Plugin.finalise ();
  exit 0
    
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* Extraction of headers from the bytecode, for primitive handling.
 * Was in mainExtract.ml.
 *)
let dump_bytecode section file =
  if not (Sys.file_exists file) 
  then failwith "executable file does not exist";

  let data = Bytecode_loader.load_file file in
  match section with
  | "code" ->
      let code_section = data.Bytecode_loader.code_section in
      let primitive_section = data.Bytecode_loader.primitive_section in 
      for i = 0 to Array.length code_section - 1 do
        match code_section.(i) with
        | Instructions.Param _ -> ()
        | inst -> pr (spf "%d %s" i 
                        (Instructions.string_of_instructions primitive_section
                           inst))
      done
  | "prim" ->
      let primitive_section = data.Bytecode_loader.primitive_section in 
      for i = 0 to Array.length primitive_section - 1 do
        print_endline primitive_section.(i)
      done
  | "dlls" ->
      List.iter print_endline data.Bytecode_loader.dlls_section
  | "dlpts" ->
      List.iter print_endline data.Bytecode_loader.dlpt_section
  | "crcs" ->
      List.iter (function (s, d) -> 
        print_endline (s^" "^Digest.to_hex d)) data.Bytecode_loader.crcs_section
  | "debug" ->
    data.Bytecode_loader.debug_section +> List.iter (fun (i, s) ->
      pr (spf "%d" i);
    )
  | _ -> failwith (spf "section not recognized: %s" section)

let cmo_magic_number = "Caml1999O007"

module Cmo = Cmo_loader

let dump_cmo file =
  let chan = open_in file in
  let res = String.make 12 ' ' in
  Pervasives.really_input chan res 0 12;

  if res <> cmo_magic_number
  then failwith ("not a cmo, could not find magic number, found: " ^ res);

  let ofs_compunit = Pervasives.input_binary_int chan in
  Pervasives.seek_in chan ofs_compunit;
  let (unit: Cmo.compilation_unit) = Pervasives.input_value chan in
  let v = Cmo.vof_compilation_unit unit in
  let s = Ocaml.string_of_v v in
  pr s;

  Pervasives.seek_in chan (12 + 4);
  let codesize = unit.Cmo.cu_codesize in
  let code = String.make  codesize ' ' in
  Pervasives.really_input chan code 0 codesize;
  let instr = Instructions.parse_code_section code in
  pr "";
  let primitive_section = [||] in
  for i = 0 to Array.length instr - 1 do
    match instr.(i) with
    | inst -> pr (spf "%d %s" i 
                    (Instructions.string_of_instructions primitive_section
                       inst))
  done;

  Pervasives.seek_in chan (12 + 4 + unit.Cmo.cu_codesize);
  let (debug : Instruct.debug_event list) = Pervasives.input_value chan in
  pr "";
  debug +> List.iter (fun ev ->
    pr (spf "%d %s %s %s" 
          ev.Instruct.ev_pos
          ev.Instruct.ev_module
          (match ev.Instruct.ev_info with
          | Instruct.Event_function -> "Event_function"
          | Instruct.Event_return i -> spf "Event_return %d" i
          | Instruct.Event_other -> "Event_other"
          )
          (let pos = ev.Instruct.ev_loc.Location.loc_start in
           spf "%s %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum)
    );
  );

  ()
  

(* ---------------------------------------------------------------------- *)
let extra_actions () = [
  "-dump_bytecode", " <section> <file>",
  Common.mk_action_2_arg dump_bytecode;
  "-dump_cmo", " <file>",
  Common.mk_action_1_arg dump_cmo;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions () ++
  []

(* I use --- long version for the flag below to avoid conflicts
 * with the executable under test. We could also use the '--'
 * trick used by many tools but I dunno how to do that with
 * Arg
 *)
let options () = 
  [
    "---verbose", Arg.Set verbose, 
    " ";
  ] ++
  (Plugin.cmd_args ()) ++
  Common.options_of_actions action (all_actions()) ++
  [
  "---version",   Arg.Unit (fun () -> 
    pr2 (spf "zamcov version: %d" 1);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "---date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2013/09/13 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
      " [options] <ocaml bytecode program> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action x xs

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | _ -> 
        Common.usage usage_msg (options()); 
        failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
