type tag =
    Zero_tag
  | Structured_tag of int
  | Raw_tag of int
  | Object_tag
  | Forward_tag
  | Abstract_tag
type value =
    Code_pointer of int
  | Stack_pointer of value list ref option
  | Int of int
  | Float of float
  | String of string
  | Double_array of float array
  | Block of block
  | Closure of closure
  | Infix of infix
  | Custom of Obj.t
and block = { mutable tag : tag; data : value array; }
and closure = { vars : value array; funcs : value array; nfuncs : int; }
and infix = { icode : int; idx : int; parent : closure; }
val atom : value
