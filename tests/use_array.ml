
let test = [|
  (fun () -> 1);
  (fun () -> 2);
  |]

let test2 = Array.concat [test; test]

let elt = Array.get test 0

let _ = elt ()

