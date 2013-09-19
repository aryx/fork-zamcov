(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

(* The value type definition and the different block tag values *)

type value =
  | Int of int
  | Float of float
  | String of string

  | Block of block
  | Double_array of float array

  | Code_pointer of int
  | Stack_pointer of value list ref option
  | Closure of closure
  | Infix of infix

  | Custom of Obj.t

  and block = {
    mutable tag : tag; (* block identification (see above) *)
    data : value array; 
  }
   (* Block tags *)  
    and tag =
      | Zero_tag
      | Structured_tag of int
      | Raw_tag of int (* TODO remplacer *)
      | Object_tag
      | Forward_tag
      | Abstract_tag

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
