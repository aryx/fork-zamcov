(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: mainRun.ml                                                    *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * The main loop of execution.
 * - reading of the different section (CODE, DATA, DBUG...) of the bytecode
 * executable.
 * - Interpretation of all the instructions of the CODE section
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

let main_action s =
  if not (Sys.file_exists s) then Utils.fatal_error ("cannot find file "^s);
  Vm.exec := s;
  Vm.args := Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current);
  (* load the OCaml bytecode exe *)
  let data = Bytecode_loader.load_file !Vm.exec in (* TODO try .. with ? *)
    (* the CODE section of the exe *)
  let code_section = data.Bytecode_loader.code_section in
    (* the DATA section of the exe converted in a value type *)
  let data_section = Utils.value_of_obj data.Bytecode_loader.data_section in
    (* the PRIM section of the exe *)
  let primitive_section = data.Bytecode_loader.primitive_section in 
    (* initialisation of C primitives using the information of the DLLS section of the exe *)
    Ffi.init data.Bytecode_loader.dlls_section;
    (* the size in byte of the bytecode *)
    Plugin.init data;
    (* initialisation of the virtual machine environment *)
    Vm.run (Vm.init "main" code_section data_section Interpreter.execute_step
                 (match !Plugin.plugin_list with
                   | [] -> (fun _ -> fun _ -> ())
                   | [p] -> p.Plugin.step
                   | _ -> Plugin.step)
                 (Ffi.load primitive_section));
    Plugin.finalise ();
    exit 0

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  (Plugin.cmd_args ()) ++
  Common.options_of_actions action (all_actions()) ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "zamcov version: %d" 1);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
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
    | [x] -> 
        main_action x

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
