let foo x = x + 1

let bar x = x + 2

let main () = 
  print_int (foo 3);
  print_int (bar 5);
  ()

let _ = main ()
