(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: utils.ml                                                      *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

let fatal_error msg =
  output_string stderr ("Fatal error: "^msg^"\n");
  exit 2

let vm_error msg =
  output_string stderr ("!!! VM error: "^msg^"\n");
  exit 3

