(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: ffi.ml                                                        *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)
  
module Conv = Conv_obj_value

let ccall_failwith s = Vm.vm_error ("CCALL "^s)

let obj_of_value v =
  try Conv.obj_of_value v with Failure s -> ccall_failwith ("Obj_of_value: "^s)

let unavailable1 f _ _ = ccall_failwith ("primitive "^f^" not found")
let unavailable2 f _ _ _ = ccall_failwith ("primitive "^f^" not found")
let unavailable3 f _ _ _ _ = ccall_failwith ("primitive "^f^" not found")
let unavailable4 f _ _ _ _ _ = ccall_failwith ("primitive "^f^" not found")
let unavailable5 f _ _ _ _ _ _ = ccall_failwith ("primitive "^f^" not found")
let unavailablen f _ _ _ = ccall_failwith ("primitive "^f^" not found")
 
let warp1 f vm arg = Conv.value_of_obj (f (obj_of_value arg))

let warp2 f vm arg1 arg2 = Conv.value_of_obj (f (obj_of_value arg1) (obj_of_value arg2))

let warp3 f vm arg1 arg2 arg3 = Conv.value_of_obj (f (obj_of_value arg1)
                                                      (obj_of_value arg2)
                                                      (obj_of_value arg3))

let warp4 f vm arg1 arg2 arg3 arg4 = Conv.value_of_obj (f (obj_of_value arg1)
                                                           (obj_of_value arg2)
							   (obj_of_value arg3)
							   (obj_of_value arg4))

let warp5 f vm arg1 arg2 arg3 arg4 arg5 = Conv.value_of_obj (f (obj_of_value arg1)
                                                                (obj_of_value arg2)
								(obj_of_value arg3)
								(obj_of_value arg4)
								(obj_of_value arg5))

let warpn f vm argv argc = Conv.value_of_obj (f (Array.map obj_of_value argv) argc)

let tbl1 : (string, Vm.virtual_machine -> Value.value -> Value.value) Hashtbl.t = Hashtbl.create 10
let tbl2 : (string, Vm.virtual_machine -> Value.value -> Value.value -> Value.value) Hashtbl.t = Hashtbl.create 10
let tbl3 : (string, Vm.virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value) Hashtbl.t = Hashtbl.create 10
let tbl4 : (string, Vm.virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) Hashtbl.t= Hashtbl.create 10
let tbl5 : (string, Vm.virtual_machine -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value -> Value.value) Hashtbl.t = Hashtbl.create 10
let tbln : (string, Vm.virtual_machine -> Value.value array -> int -> Value.value) Hashtbl.t = Hashtbl.create 10

let add1 s f = if not (Hashtbl.mem tbl1 s) then Hashtbl.add tbl1 s f else ()
let add2 s f = if not (Hashtbl.mem tbl2 s) then Hashtbl.add tbl2 s f else ()
let add3 s f = if not (Hashtbl.mem tbl3 s) then Hashtbl.add tbl3 s f else ()
let add4 s f = if not (Hashtbl.mem tbl4 s) then Hashtbl.add tbl4 s f else ()
let add5 s f = if not (Hashtbl.mem tbl5 s) then Hashtbl.add tbl5 s f else ()
let addn s f = if not (Hashtbl.mem tbln s) then Hashtbl.add tbln s f else ()
(* when executing the VM inside the VM C_CALLN is transformed into a C_CALL2 *)

let replace1 = Hashtbl.add tbl1
let replace2 = Hashtbl.add tbl2
let replace3 = Hashtbl.add tbl3
let replace4 = Hashtbl.add tbl4
let replace5 = Hashtbl.add tbl5
let replacen = Hashtbl.add tbln

let init_list = ref ([]:(string list -> unit) list)

let init dlls_section =
  List.iter (fun f -> 
    f dlls_section
  ) !init_list

let load primitive_section =
  let n = Array.length primitive_section in
  let res = {
    Vm.tbl1 = Array.make n (unavailable1 "");
    Vm.tbl2 = Array.make n (unavailable2 "");
    Vm.tbl3 = Array.make n (unavailable3 "");
    Vm.tbl4 = Array.make n (unavailable4 "");
    Vm.tbl5 = Array.make n (unavailable5 "");
    Vm.tbln = Array.make n (unavailablen "");
  } in
  for i = 0 to n - 1 do
    let s = primitive_section.(i) in
    (* todo: detect if is not present in any table *)
    res.Vm.tbl1.(i) <- (try Hashtbl.find tbl1 s with Not_found -> unavailable1 s);
    res.Vm.tbl2.(i) <- (try Hashtbl.find tbl2 s with Not_found -> unavailable2 s);
    res.Vm.tbl3.(i) <- (try Hashtbl.find tbl3 s with Not_found -> unavailable3 s);
    res.Vm.tbl4.(i) <- (try Hashtbl.find tbl4 s with Not_found -> unavailable4 s);
    res.Vm.tbl5.(i) <- (try Hashtbl.find tbl5 s with Not_found -> unavailable5 s);
    res.Vm.tbln.(i) <- (try Hashtbl.find tbln s with Not_found -> unavailablen s);
  done;
  res
