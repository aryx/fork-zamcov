(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: value.ml                                                      *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
(*
  The value type definition and the different block tag values
*)
(***********************************************************************)

(* Block tags *)  
type tag =
  | Zero_tag
  | Structured_tag of int
  | Raw_tag of int (* TODO remplacer *)
  | Object_tag
  | Forward_tag
  | Abstract_tag

type value =
  | Code_pointer of int
  | Stack_pointer of value list ref option
  | Int of int
  | Float of float
  | String of string
  | Double_array of float array
  | Block of block
  | Closure of closure
  | Infix of infix
  | Custom of Obj.t
and block = {
  mutable tag : tag; (* block identification (see above) *)
  data : value array; 
}
and closure = {
  vars : value array;
  funcs : value array;
  nfuncs : int;
}
and infix = {
  icode : int;
  idx : int;
  parent : closure;
}
    
let atom = Block { 
  tag = Zero_tag;
  data = [||]
}
