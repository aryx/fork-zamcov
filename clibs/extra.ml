external ext_caml_ba_set_1 : Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_ba_set_1"

let prims () =
  Ffi.add3 "caml_ba_set_1" (Ffi.warp3 ext_caml_ba_set_1)

let init dlls_section =
  prims ()
;;
Ffi.init_list := init::!Ffi.init_list
