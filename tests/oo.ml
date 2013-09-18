
class test (x: int) =
object(o)
  val data = x
  method get = data
end

let test () =
  let x = new test 2 in
  print_int x#get

let _ = test()
