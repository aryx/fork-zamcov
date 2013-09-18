(* Primitives of threads *)

external ext_caml_thread_self : Obj.t -> Obj.t = "caml_thread_self"
external ext_caml_thread_sigmask : Obj.t -> Obj.t -> Obj.t = "caml_thread_sigmask"
external ext_caml_mutex_new : Obj.t -> Obj.t = "caml_mutex_new"
external ext_caml_thread_yield : Obj.t -> Obj.t = "caml_thread_yield"
external ext_caml_thread_join : Obj.t -> Obj.t = "caml_thread_join"
external ext_caml_thread_uncaught_exception : Obj.t -> Obj.t = "caml_thread_uncaught_exception"
external ext_caml_thread_id : Obj.t -> Obj.t = "caml_thread_id"
external ext_caml_thread_exit : Obj.t -> Obj.t = "caml_thread_exit"
external ext_caml_thread_initialize : Obj.t -> Obj.t = "caml_thread_initialize"
external ext_caml_wait_signal : Obj.t -> Obj.t = "caml_wait_signal"
external ext_caml_thread_new : Obj.t -> Obj.t -> Obj.t = "caml_thread_new"

let prims () =
  Ffi.add1 "caml_thread_self" (Ffi.warp1 ext_caml_thread_self);
  Ffi.add2 "caml_thread_sigmask" (Ffi.warp2 ext_caml_thread_sigmask);
  Ffi.add1 "caml_mutex_new" (Ffi.warp1 ext_caml_mutex_new);
  Ffi.add1 "caml_thread_yield" (Ffi.warp1 ext_caml_thread_yield);
  Ffi.add1 "caml_thread_join" (Ffi.warp1 ext_caml_thread_join);
  Ffi.add1 "caml_thread_uncaught_exception" (Ffi.warp1 ext_caml_thread_uncaught_exception);
  Ffi.add1 "caml_thread_id" (Ffi.warp1 ext_caml_thread_id);
  Ffi.add1 "caml_thread_exit" (Ffi.warp1 ext_caml_thread_exit);
  Ffi.add1 "caml_thread_initialize" (Ffi.warp1 ext_caml_thread_initialize);
  Ffi.add1 "caml_wait_signal" (Ffi.warp1 ext_caml_wait_signal);
  Ffi.add2 "caml_thread_new" (Ffi.warp2 ext_caml_thread_new)

let init dlls_section =
  prims ()
;;
Ffi.init_list := init::!Ffi.init_list
