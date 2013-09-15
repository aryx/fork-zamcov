(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* copy paste of cmo_format.mli, not in compiler-libs :( *)

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     debugging information if any
     compilation unit descriptor 

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type reloc_info =
    Reloc_literal of Lambda.structured_constant    (* structured constant *)
  | Reloc_getglobal of Ident.t              (* reference to a global *)
  | Reloc_setglobal of Ident.t              (* definition of a global *)
  | Reloc_primitive of string               (* C primitive number *)
 (* with tarzan *)

(* Descriptor for compilation units *)

type compilation_unit =
  { cu_name: string;                    (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
    cu_primitives: string list;         (* Primitives declared inside *)
    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int;                 (* Length of debugging info *)
  }

(* with tarzan *)

(*****************************************************************************)
(* Vof *)
(*****************************************************************************)

module Ident = struct
    let vof_t _x = Ocaml.VTODO "Ident.vof_t"
end

let vof_reloc_info =
  function
  | Reloc_literal v1 ->
      let v1 = Ocaml.VTODO "Lambda" (*Lambda.vof_structured_constant v1 *)
      in Ocaml.VSum (("Reloc_literal", [ v1 ]))
  | Reloc_getglobal v1 ->
      let v1 = Ident.vof_t v1 in Ocaml.VSum (("Reloc_getglobal", [ v1 ]))
  | Reloc_setglobal v1 ->
      let v1 = Ident.vof_t v1 in Ocaml.VSum (("Reloc_setglobal", [ v1 ]))
  | Reloc_primitive v1 ->
      let v1 = Ocaml.vof_string v1
      in Ocaml.VSum (("Reloc_primitive", [ v1 ]))

let vof_compilation_unit {
                           cu_name = v_cu_name;
                           cu_pos = v_cu_pos;
                           cu_codesize = v_cu_codesize;
                           cu_reloc = v_cu_reloc;
                           cu_imports = v_cu_imports;
                           cu_primitives = v_cu_primitives;
                           cu_force_link = v_cu_force_link;
                           cu_debug = v_cu_debug;
                           cu_debugsize = v_cu_debugsize
                         } =
  let bnds = [] in
  let arg = Ocaml.vof_int v_cu_debugsize in
  let bnd = ("cu_debugsize", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_cu_debug in
  let bnd = ("cu_debug", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_bool v_cu_force_link in
  let bnd = ("cu_force_link", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_list Ocaml.vof_string v_cu_primitives in
  let bnd = ("cu_primitives", arg) in
  let bnds = bnd :: bnds in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = Ocaml.vof_string v1
         and v2 = (*Digest.vof_t v2*) Ocaml.VTODO "Digest"
         in Ocaml.VTuple [ v1; v2 ])
      v_cu_imports in
  let bnd = ("cu_imports", arg) in
  let bnds = bnd :: bnds in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_reloc_info v1
         and v2 = Ocaml.vof_int v2
         in Ocaml.VTuple [ v1; v2 ])
      v_cu_reloc in
  let bnd = ("cu_reloc", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_cu_codesize in
  let bnd = ("cu_codesize", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_cu_pos in
  let bnd = ("cu_pos", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_string v_cu_name in
  let bnd = ("cu_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
