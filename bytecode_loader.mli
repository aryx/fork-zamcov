module StringList :
  sig
    type t = string list
    val empty : 'a list
    val add : 'a -> 'a list -> 'a list
    val get : 'a list -> int -> 'a
    val to_array : 'a list -> 'a array
  end
module StringSet :
  sig
    type t = string list
    val empty : 'a list
    val add : 'a -> 'a list -> 'a list
    val get : 'a list -> int -> 'a
    val to_array : 'a list -> 'a array
  end
val exec_magic_number : string

val real_code_location : int ref
exception Break
exception Bytecode_loader_Error of string
val check_point : bool * (unit -> unit) -> unit
val error : string -> unit -> 'a
type section = CODE | DATA | PRIM | DLLS | DLPT | DBUG | SYMB | CRCS
val section_of_string : string -> section
val string_of_section : section -> string
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
val cut_zero_terminated_strings : string -> string list
val sections_of_raw_sections :
  string ->
  string ->
  string -> string -> string -> string -> string -> string -> sections
val load_file : string -> sections
