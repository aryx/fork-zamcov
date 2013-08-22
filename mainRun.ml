(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: mainRun.ml                                                    *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
(*
  The main loop of execution.
  - reading of the different section (CODE, DATA, DBUG...) of the bytecode
  executable.
  - Interpretation of all the instructions of the CODE section
*)
(***********************************************************************)

let cmd_args = []

let init_exec s =
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

let _ =
  Arg.parse (cmd_args @ Plugin.cmd_args ()) init_exec (Sys.argv.(0)^" [options] executable args");
  Arg.usage (cmd_args @ Plugin.cmd_args ()) (Sys.argv.(0)^" [options] executable args")
