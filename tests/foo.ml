let x = 2

let this_is_a_long_function_name x = 
  x + 2

let _ =
  print_string "-------\n";
  print_int (this_is_a_long_function_name 2);
  print_string "-------\n";
  ()
