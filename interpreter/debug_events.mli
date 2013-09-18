
val event_of_pc:
  int -> Instruct.debug_event option

val parse_debug_section: 
  int ->
  (int * Instruct.debug_event list) list -> Instruct.debug_event option array
