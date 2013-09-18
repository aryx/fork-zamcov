external ext_caml_ba_set_1 : Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_ba_set_1"
external ext_caml_ba_get_1 : Obj.t -> Obj.t -> Obj.t = "caml_ba_get_1"

(* in commons/ *)
external ext_caml_realpath: Obj.t -> Obj.t = "caml_realpath"

let prims () =
  Ffi.add2 "caml_ba_get_1" (Ffi.warp2 ext_caml_ba_get_1);
  Ffi.add3 "caml_ba_set_1" (Ffi.warp3 ext_caml_ba_set_1);

  Ffi.add1 "caml_realpath" (Ffi.warp1 ext_caml_realpath);
  ()

let init dlls_section =
  prims ()
;;
Ffi.init_list := init::!Ffi.init_list
