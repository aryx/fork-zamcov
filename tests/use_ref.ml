
let x = ref 42

let f () =
  incr x;
  failwith "HERE"

let _ =
  f ();
  print_int !x;
  ()
