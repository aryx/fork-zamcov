val ccall_failwith : string -> 'a
val obj_of_value : Value.value -> Obj.t
val unavailable1 : string -> 'a -> 'b -> 'c
val unavailable2 : string -> 'a -> 'b -> 'c -> 'd
val unavailable3 : string -> 'a -> 'b -> 'c -> 'd -> 'e
val unavailable4 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val unavailable5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val unavailablen : string -> 'a -> 'b -> 'c -> 'd
val warp1 : (Obj.t -> Obj.t) -> 'a -> Value.value -> Value.value
val warp2 :
  (Obj.t -> Obj.t -> Obj.t) ->
  'a -> Value.value -> Value.value -> Value.value
val warp3 :
  (Obj.t -> Obj.t -> Obj.t -> Obj.t) ->
  'a -> Value.value -> Value.value -> Value.value -> Value.value
val warp4 :
  (Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t) ->
  'a ->
  Value.value -> Value.value -> Value.value -> Value.value -> Value.value
val warp5 :
  (Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t) ->
  'a ->
  Value.value ->
  Value.value -> Value.value -> Value.value -> Value.value -> Value.value
val warpn :
  (Obj.t array -> 'a -> Obj.t) ->
  'b -> Value.value array -> 'a -> Value.value
val tbl1 :
  (string, Vm.virtual_machine -> Value.value -> Value.value) Hashtbl.t
val tbl2 :
  (string, Vm.virtual_machine -> Value.value -> Value.value -> Value.value)
  Hashtbl.t
val tbl3 :
  (string,
   Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value)
  Hashtbl.t
val tbl4 :
  (string,
   Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value)
  Hashtbl.t
val tbl5 :
  (string,
   Vm.virtual_machine ->
   Value.value ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value)
  Hashtbl.t
val tbln :
  (string, Vm.virtual_machine -> Value.value array -> int -> Value.value)
  Hashtbl.t
val add1 :
  string -> (Vm.virtual_machine -> Value.value -> Value.value) -> unit
val add2 :
  string ->
  (Vm.virtual_machine -> Value.value -> Value.value -> Value.value) -> unit
val add3 :
  string ->
  (Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val add4 :
  string ->
  (Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val add5 :
  string ->
  (Vm.virtual_machine ->
   Value.value ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val addn :
  string ->
  (Vm.virtual_machine -> Value.value array -> int -> Value.value) -> unit
val replace1 :
  string -> (Vm.virtual_machine -> Value.value -> Value.value) -> unit
val replace2 :
  string ->
  (Vm.virtual_machine -> Value.value -> Value.value -> Value.value) -> unit
val replace3 :
  string ->
  (Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val replace4 :
  string ->
  (Vm.virtual_machine ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val replace5 :
  string ->
  (Vm.virtual_machine ->
   Value.value ->
   Value.value -> Value.value -> Value.value -> Value.value -> Value.value) ->
  unit
val replacen :
  string ->
  (Vm.virtual_machine -> Value.value array -> int -> Value.value) -> unit
val init_list : (string list -> unit) list ref
val init : string list -> unit
val load : string array -> Vm.prim_table
