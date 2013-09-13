(*
*)
(***********************************************************************)

let code = ref false
let prim = ref false
let dlls = ref false
let dlpt = ref false
let crcs = ref false
let debug = ref false
let nb_instr = ref 0
let trace_name = ref ""
let exec = ref ""
  
let init_exec s =
  if !exec = ""
  then exec := s

let print_code data =
  let code_section = data.Bytecode_loader.code_section in
  let primitive_section = data.Bytecode_loader.primitive_section in 
  for i = 0 to Array.length code_section - 1 do
    match code_section.(i) with
    | Instructions.Param _ -> ()
    | inst -> print_endline (string_of_int i^" "^Instructions.string_of_instructions primitive_section inst)
  done

let print_prim data =
  let primitive_section = data.Bytecode_loader.primitive_section in 
  for i = 0 to Array.length primitive_section - 1 do
    print_endline primitive_section.(i)
  done

let print_dlls data =
  List.iter print_endline data.Bytecode_loader.dlls_section

let print_dlpt data =
  List.iter print_endline data.Bytecode_loader.dlpt_section

let print_crcs data =
  List.iter (function (s, d) -> print_endline (s^" "^Digest.to_hex d)) data.Bytecode_loader.crcs_section

let print_debug data =
  print_endline data.Bytecode_loader.debug_section

let _ =
  Arg.parse
    [
      ("-code", Arg.Set code, "print code instructions");
      ("-prim", Arg.Set prim, "print primitives");
      ("-dlls", Arg.Set dlls, "print DLLS");
      ("-dlpt", Arg.Set dlpt, "print DLPT");
      ("-crcs", Arg.Set crcs, "print CRCS");
      ("-debug", Arg.Set crcs, "print DEBUG");
      ("--", Arg.Rest init_exec, "no options follow")
    ]
    init_exec
    "mainRun [options] executable";
  if not (Sys.file_exists !exec) then failwith "executable file does not exist";
  let data = Bytecode_loader.load_file !exec in
  if !code then print_code data;
  if !prim then print_prim data;
  if !dlls then print_dlls data;
  if !dlpt then print_dlpt data;
  if !crcs then print_crcs data;
  if !debug then print_debug data;
;;
