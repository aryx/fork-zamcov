(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: bytecode_loader.ml                                            *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This module allows the reading of the executable file.
 * Mostly the reverse of bytecomp/Bytelink.link_bytecode
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* module StringSet = Set.Make(String) *)
module StringList = struct
  type t = string list
  let empty = []
  let add e l = l@[e]
  let rec get l i =
    match l with
      | [] -> (output_string stderr "StringList.get\n"; assert false)
      | h::t -> 
          if i = 0 then h
          else get t (i-1)
  let to_array l = Array.of_list l
end
module StringSet = StringList

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* list of magic numbers for the identification of the exe*)
let exec_magic_number = "Caml1999X008"

let real_code_location = ref 0

exception Break
exception Bytecode_loader_Error of string
 
let check_point (condition, error_handler) =
  if condition then () else error_handler ()

let error msg = 
  fun () -> raise (Bytecode_loader_Error (msg^"\n"))

(* the different sections of the bytecode program *)
type section = 
  | CODE 
  | DATA 
  | PRIM 
  | DLLS 
  | DLPT 
  | DBUG 
  | SYMB 
  | CRCS

let section_of_string = function
  | "CODE" -> CODE
  | "DATA" -> DATA
  | "PRIM" -> PRIM
  | "DLLS" -> DLLS
  | "DLPT" -> DLPT
  | "DBUG" -> DBUG
  | "SYMB" -> SYMB
  | "CRCS" -> CRCS
  | bad ->
      assert(bad<>"CODE");
      check_point(false, error (Printf.sprintf "section_of_string <%s>" bad));
      assert false

let string_of_section = function
  | CODE -> "CODE"
  | DATA -> "DATA"
  | PRIM -> "PRIM"
  | DLLS -> "DLLS"
  | DLPT -> "DLPT"
  | DBUG -> "DBUG"
  | SYMB -> "SYMB"
  | CRCS -> "CRCS"

type sections = {
  name : string;
  code_section : Instructions.instruction array;
  data_section : Obj.t;
  primitive_section : string array;
  dlls_section : StringSet.t;
  dlpt_section : StringSet.t;
  debug_section : (int * string (* TODO *)) list;
  crcs_section : (string * Digest.t) list;
}

let cut_zero_terminated_strings s =
  let res = ref StringSet.empty in
  let p = ref 0 in
    for i = 0 to String.length s - 1 do
      if s.[i] = '\000' then
        ( res := StringSet.add (String.sub s !p (i - !p)) !res;
          p := i + 1 )
    done;
    !res

let parse_debug_section raw_debug_section =
  let tmpfile = Common.new_temp_file "zamcov" "raw" in
  Common.write_file ~file:tmpfile raw_debug_section;
  let chan = open_in tmpfile in
  let n = input_binary_int chan in
  pr2 (spf "%d" n);
  let res = ref [] in
  for i = 0 to n - 1 do
    let ofs = input_binary_int chan in
    let _v = input_value chan in
    Common.push2 (ofs, "") res;
  done;
  !res

let sections_of_raw_sections name 
    raw_code_section raw_data_section raw_primitive_section
    raw_dlls_section raw_dlpt_section raw_debug_section raw_crcs_section 
 =
  {
    name = name;
    code_section = Instructions.parse_code_section raw_code_section;
    data_section = Marshal.from_string raw_data_section 0;
    primitive_section = StringSet.to_array (cut_zero_terminated_strings raw_primitive_section);
    dlls_section = cut_zero_terminated_strings raw_dlls_section;
    dlpt_section = cut_zero_terminated_strings raw_dlpt_section;
    debug_section = parse_debug_section raw_debug_section;
    crcs_section = Marshal.from_string raw_crcs_section 0;
  }


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* Load the given bytecode program *)
let load_file file =
  let channel = Pervasives.open_in file in
  let file_size = Pervasives.in_channel_length channel in
  check_point (file_size >= 12, error "not a bytecode executable file");
  (* Reading of the trailer that contains the magic number *)
  Pervasives.seek_in channel (file_size - 12);
  let res = String.make 12 ' ' in
  Pervasives.really_input channel res 0 12;
  check_point (res = exec_magic_number, error "wrong magic code");
  let number_of_sections = 
    (* Reading of the number of section wich has a 4 caracter size*)
    Pervasives.seek_in channel (file_size - 16);
    let res =
      (* (we can assume number of sections will never be > max_int) *)
      Pervasives.input_binary_int channel
    in
    check_point (res > 0, error "sections");
    res
  in
  let toc_size = number_of_sections * 8 in
  let sections : (section * int) array =
    Pervasives.seek_in channel (file_size - 16 - toc_size);
    let res = Array.make number_of_sections (CODE, 0) in
    for i = 0 to number_of_sections - 1 do
      res.(i) <-
        let a = String.make 4 ' ' in
        Pervasives.really_input channel a 0 4;
        let b = Pervasives.input_binary_int channel in 
        section_of_string a, b
    done;
    res
  in
  let get_section section =
    let ofs = ref (file_size - 16 - toc_size) in
    let res = ref "" in
    let () =
      try
        for i = number_of_sections - 1 downto 0 do
          match sections.(i) with
          | s, l ->
            ofs := !ofs - l;
            if s = section then
              begin
                if section = CODE then
                  real_code_location := l;
                Pervasives.seek_in channel (!ofs);
                res := String.make l ' ';
                Pervasives.really_input channel (!res) 0 l;
                raise Break
              end
        done;
        check_point(false,
                    error
                      (Printf.sprintf
                         "cannot get some bytecode section <%s>"
                         (string_of_section section)));
      with Break -> ()
    in
    !res
  in
  let get_optional_section section =
    try get_section section with _ -> ""
  in
  let raw_code_section = get_section CODE in (* bytecode to be executed *)
  let raw_data_section = get_section DATA in (* global data for the program *)
  let raw_primitive_section = get_section PRIM in (* primitive *)
  let raw_dlls_section = get_optional_section DLLS in (* linked libraries *)
  let raw_dlpt_section = get_optional_section DLPT in (* libraries path *)
  let raw_debug_section = get_optional_section DBUG in (* debug information *)
  let raw_crcs_section = get_section CRCS in
  sections_of_raw_sections file raw_code_section raw_data_section raw_primitive_section
    raw_dlls_section raw_dlpt_section raw_debug_section raw_crcs_section
