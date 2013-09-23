
class test =
object(o)
  val data = 0
  method get = data
  method not_executed = data
end

let test () =
  let x = new test in
  print_int x#get

let _ = test()

(* todo: if inherits, then needs a Array.concat that can
 * work on closures, see camlInternalOO.inherits and its
 * use of Array.concat on list of array of closures
 *)

class test2 =
  object(o)
  inherit test
  val data2 = 1
  method get2 = data2
  end

let not_executed () =
  1

let test2 () =
  let x = new test2 in
  print_int x#get2

let _ = test2()

