(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: instructions.ml                                               *)
(* authors: Adrien Jonquet, Philippe Wang, Alexis Darrasse             *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

exception Unknown_instruction of int

type nb_params =
  | No_params
  | One_int
  | Two_ints
  | Recursive_closure
  | Switch

(* Sum type of the different instructions in OCaml bytecode *)
type instruction =
  | ACC0 | ACC1 | ACC2 | ACC3 | ACC4 | ACC5
  | ACC6 | ACC7 | ACC of int | PUSH | PUSHACC0 | PUSHACC1
  | PUSHACC2 | PUSHACC3 | PUSHACC4 | PUSHACC5 | PUSHACC6 | PUSHACC7
  | PUSHACC of int | POP of int | ASSIGN of int | ENVACC1 | ENVACC2 | ENVACC3
  | ENVACC4 | ENVACC of int | PUSHENVACC1 | PUSHENVACC2 | PUSHENVACC3 | PUSHENVACC4
  | PUSHENVACC of int | PUSH_RETADDR of int | APPLY of int | APPLY1 | APPLY2 | APPLY3
  | APPTERM of int*int | APPTERM1 of int | APPTERM2 of int | APPTERM3 of int | RETURN of int | RESTART
  | GRAB of int | CLOSURE of int*int
  | CLOSUREREC of int*int*int*int array
  | OFFSETCLOSUREM2 | OFFSETCLOSURE0
  | OFFSETCLOSURE2 | OFFSETCLOSURE of int | PUSHOFFSETCLOSUREM2 | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE2 | PUSHOFFSETCLOSURE of int | GETGLOBAL of int | PUSHGETGLOBAL of int
  | GETGLOBALFIELD of int*int | PUSHGETGLOBALFIELD of int*int | SETGLOBAL of int | ATOM0 | ATOM of int | PUSHATOM0
  | PUSHATOM of int | MAKEBLOCK of int*int | MAKEBLOCK1 of int | MAKEBLOCK2 of int | MAKEBLOCK3 of int | MAKEFLOATBLOCK of int
  | GETFIELD0 | GETFIELD1 | GETFIELD2 | GETFIELD3 | GETFIELD of int | GETFLOATFIELD of int
  | SETFIELD0 | SETFIELD1 | SETFIELD2 | SETFIELD3 | SETFIELD of int | SETFLOATFIELD of int
  | VECTLENGTH | GETVECTITEM | SETVECTITEM | GETSTRINGCHAR | SETSTRINGCHAR
  | BRANCH of int | BRANCHIF of int | BRANCHIFNOT of int
  | SWITCH of int*int array
  | BOOLNOT | PUSHTRAP of int | POPTRAP
  | RAISE | CHECK_SIGNALS | C_CALL1 of int | C_CALL2 of int | C_CALL3 of int | C_CALL4 of int | C_CALL5 of int
  | C_CALLN of int*int | CONST0 | CONST1 | CONST2 | CONST3 | CONSTINT of int | PUSHCONST0
  | PUSHCONST1 | PUSHCONST2 | PUSHCONST3 | PUSHCONSTINT of int | NEGINT | ADDINT
  | SUBINT | MULINT | DIVINT | MODINT | ANDINT | ORINT | XORINT | LSLINT
  | LSRINT | ASRINT | EQ | NEQ | LTINT | LEINT | GTINT | GEINT | OFFSETINT of int
  | OFFSETREF of int | ISINT | GETMETHOD | BEQ of int*int | BNEQ of int*int
  | BLTINT of int*int | BLEINT of int*int | BGTINT of int*int 
  | BGEINT of int*int | ULTINT | UGEINT | BULTINT of int*int | BUGEINT of int*int
  | GETPUBMET of int*int | GETDYNMET
  | STOP | EVENT | BREAK | Param of int

let num_parameters = function
  | 8 -> One_int | 18 -> One_int | 19 -> One_int | 20 -> One_int | 25 -> One_int | 30 -> One_int
  | 31 -> One_int | 32 -> One_int | 36 -> Two_ints | 37 -> One_int | 38 -> One_int | 39 -> One_int
  | 40 -> One_int | 42 -> One_int | 43 -> Two_ints | 44 -> Recursive_closure | 48 -> One_int
  | 52 -> One_int | 53 -> One_int | 54 -> One_int | 55 -> Two_ints | 56 -> Two_ints | 57 -> One_int
  | 59 -> One_int | 61 -> One_int | 62 -> Two_ints | 63 -> One_int | 64 -> One_int | 65 -> One_int
  | 66 -> One_int | 71 -> One_int | 72 -> One_int | 77 -> One_int | 78 -> One_int | 84 -> One_int
  | 85 -> One_int | 86 -> One_int | 87 -> Switch | 89 -> One_int | 93 -> One_int | 94 -> One_int
  | 95 -> One_int | 96 -> One_int | 97 -> One_int | 98 -> Two_ints | 103 -> One_int | 108 -> One_int
  | 127 -> One_int | 128 -> One_int | 131 -> Two_ints | 132 -> Two_ints | 133 -> Two_ints
  | 134 -> Two_ints | 135 -> Two_ints | 136 -> Two_ints | 139 -> Two_ints | 140 -> Two_ints
  | 141 -> Two_ints | _ -> No_params

let instruction_of_number0 = function
  | 0 -> ACC0 | 1 -> ACC1 | 2 -> ACC2 | 3 -> ACC3 | 4 -> ACC4
  | 5 -> ACC5 | 6 -> ACC6 | 7 -> ACC7 | 9 -> PUSH
  | 10 -> PUSHACC0 | 11 -> PUSHACC1 | 12 -> PUSHACC2 | 13 -> PUSHACC3
  | 14 -> PUSHACC4 | 15 -> PUSHACC5 | 16 -> PUSHACC6 | 17 -> PUSHACC7
  | 21 -> ENVACC1 | 22 -> ENVACC2 | 23 -> ENVACC3 | 24 -> ENVACC4
  | 26 -> PUSHENVACC1 | 27 -> PUSHENVACC2 | 28 -> PUSHENVACC3 | 29 -> PUSHENVACC4 
  | 33 -> APPLY1 | 34 -> APPLY2 | 35 -> APPLY3 | 41 -> RESTART
  | 45 -> OFFSETCLOSUREM2 | 46 -> OFFSETCLOSURE0 | 47 -> OFFSETCLOSURE2
  | 49 -> PUSHOFFSETCLOSUREM2 | 50 -> PUSHOFFSETCLOSURE0 | 51 -> PUSHOFFSETCLOSURE2
  | 58 -> ATOM0 | 60 -> PUSHATOM0
  | 67 -> GETFIELD0 | 68 -> GETFIELD1 | 69 -> GETFIELD2 | 70 -> GETFIELD3
  | 73 -> SETFIELD0 | 74 -> SETFIELD1 | 75 -> SETFIELD2 | 76 -> SETFIELD3
  | 79 -> VECTLENGTH | 80 -> GETVECTITEM
  | 81 -> SETVECTITEM | 82 -> GETSTRINGCHAR | 83 -> SETSTRINGCHAR
  | 88 -> BOOLNOT | 90 -> POPTRAP | 91 -> RAISE | 92 -> CHECK_SIGNALS
  | 99 -> CONST0 | 100 -> CONST1 | 101 -> CONST2 | 102 -> CONST3
  | 104 -> PUSHCONST0 | 105 -> PUSHCONST1 | 106 -> PUSHCONST2
  | 107 -> PUSHCONST3 | 109 -> NEGINT | 110 -> ADDINT
  | 111 -> SUBINT | 112 -> MULINT | 113 -> DIVINT | 114 -> MODINT
  | 115 -> ANDINT | 116 -> ORINT | 117 -> XORINT | 118 -> LSLINT
  | 119 -> LSRINT | 120 -> ASRINT | 121 -> EQ | 122 -> NEQ | 123 -> LTINT
  | 124 -> LEINT | 125 -> GTINT | 126 -> GEINT | 129 -> ISINT | 130 -> GETMETHOD
  | 137 -> ULTINT | 138 -> UGEINT
  | 142 -> GETDYNMET | 143 -> STOP | 144 -> EVENT | 145 -> BREAK
  | s -> raise (Unknown_instruction s)

let instruction_of_number1 n1 = function
  | 8 -> ACC n1 | 18 -> PUSHACC n1 | 19 -> POP n1 | 20 -> ASSIGN n1
  | 25 -> ENVACC n1 | 30 -> PUSHENVACC n1 | 31 -> PUSH_RETADDR n1 | 32 -> APPLY n1
  | 37 -> APPTERM1 n1 | 38 -> APPTERM2 n1 | 39 -> APPTERM3 n1 | 40 -> RETURN n1
  | 42 -> GRAB n1 | 48 -> OFFSETCLOSURE n1 | 52 -> PUSHOFFSETCLOSURE n1
  | 53 -> GETGLOBAL n1 | 54 -> PUSHGETGLOBAL n1
  | 57 -> SETGLOBAL n1 | 58 -> ATOM0 | 59 -> ATOM n1
  | 60 -> PUSHATOM0 | 61 -> PUSHATOM n1 | 63 -> MAKEBLOCK1 n1
  | 64 -> MAKEBLOCK2 n1 | 65 -> MAKEBLOCK3 n1 | 66 -> MAKEFLOATBLOCK n1
  | 71 -> GETFIELD n1 | 72 -> GETFLOATFIELD n1 | 77 -> SETFIELD n1 | 78 -> SETFLOATFIELD n1
  | 84 -> BRANCH n1 | 85 -> BRANCHIF n1 | 86 -> BRANCHIFNOT n1 | 89 -> PUSHTRAP n1
  | 93 -> C_CALL1 n1 | 94 -> C_CALL2 n1 | 95 -> C_CALL3 n1 | 96 -> C_CALL4 n1 | 97 -> C_CALL5 n1
  | 103 -> CONSTINT n1 | 108 -> PUSHCONSTINT n1 | 127 -> OFFSETINT n1 | 128 -> OFFSETREF n1
  | s -> raise (Unknown_instruction s)

let instruction_of_number2 n1 n2 = function
  | 36 -> APPTERM (n1,n2) | 43 -> CLOSURE (n1,n2) | 55 -> GETGLOBALFIELD (n1,n2)
  | 56 -> PUSHGETGLOBALFIELD (n1,n2) | 62 -> MAKEBLOCK (n1,n2) | 98 -> C_CALLN (n1,n2)
  | 131 -> BEQ (n1,n2) | 132 -> BNEQ (n1,n2) | 133 -> BLTINT (n1,n2) | 134 -> BLEINT (n1,n2)
  | 135 -> BGTINT (n1,n2) | 136 -> BGEINT (n1,n2) | 139 -> BULTINT (n1,n2)
  | 140 -> BUGEINT (n1,n2) | 141 -> GETPUBMET (n1,n2)
  | s -> raise (Unknown_instruction s)

let string_of_instructions prims = function
    ACC0 -> "ACC0" | ACC1 -> "ACC1"  | ACC2 -> "ACC2"  | ACC3 -> "ACC3"  | ACC4 -> "ACC4"  | ACC5 -> "ACC5" 
  | ACC6 -> "ACC6" | ACC7 -> "ACC7" | ACC n -> "ACC "^string_of_int n | PUSH -> "PUSH" | PUSHACC0 -> "PUSHACC0" | PUSHACC1 -> "PUSHACC1"
  | PUSHACC2 -> "PUSHACC2" | PUSHACC3 -> "PUSHACC3" | PUSHACC4 -> "PUSHACC4" | PUSHACC5 -> "PUSHACC5" | PUSHACC6 -> "PUSHACC6" | PUSHACC7 -> "PUSHACC7"
  | PUSHACC n -> "PUSHACC "^string_of_int n | POP n  -> "POP "^string_of_int n | ASSIGN n -> "ASSIGN "^string_of_int n | ENVACC1 -> "ENVACC1" | ENVACC2 -> "ENVACC2" | ENVACC3 -> "ENVACC3"
  | ENVACC4 -> "ENVACC4" | ENVACC n -> "ENVACC "^string_of_int n | PUSHENVACC1 -> "PUSHENVACC1" | PUSHENVACC2 -> "PUSHENVACC2" | PUSHENVACC3 -> "PUSHENVACC3" | PUSHENVACC4 -> "PUSHENVACC4"
  | PUSHENVACC n -> "PUSHENVACC "^string_of_int n | PUSH_RETADDR n -> "PUSH_RETADDR "^string_of_int n | APPLY n -> "APPLY "^string_of_int n | APPLY1 -> "APPLY1" | APPLY2 -> "APPLY2" | APPLY3 -> "APPLY3"
  | APPTERM (n1,n2) -> "APPTERM "^string_of_int n1^" "^string_of_int n2
  | APPTERM1 n -> "APPTERM1 "^string_of_int n | APPTERM2 n -> "APPTERM2 "^string_of_int n
  | APPTERM3 n -> "APPTERM3 "^string_of_int n | RETURN n -> "RETURN "^string_of_int n | RESTART -> "RESTART"
  | GRAB n -> "GRAB "^string_of_int n | CLOSURE (n1,n2) -> "CLOSURE "^string_of_int n1^" "^string_of_int n2
  | CLOSUREREC (f,v,o,t) -> "CLOSUREREC "^string_of_int f^" "^string_of_int v^" "^string_of_int o
  | OFFSETCLOSUREM2 -> "OFFSETCLOSUREM2" | OFFSETCLOSURE0 -> "OFFSETCLOSURE0"
  | OFFSETCLOSURE2 -> "OFFSETCLOSURE2" | OFFSETCLOSURE n -> "OFFSETCLOSURE "^string_of_int n
  | PUSHOFFSETCLOSUREM2 -> "PUSHOFFSETCLOSUREM2" | PUSHOFFSETCLOSURE0 -> "PUSHOFFSETCLOSURE0"
  | PUSHOFFSETCLOSURE2 -> "PUSHOFFSETCLOSURE2" | PUSHOFFSETCLOSURE n -> "PUSHOFFSETCLOSURE "^string_of_int n
  | GETGLOBAL n -> "GETGLOBAL "^string_of_int n | PUSHGETGLOBAL n -> "PUSHGETGLOBAL "^string_of_int n
  | GETGLOBALFIELD (n1,n2) -> "GETGLOBALFIELD "^string_of_int n1^" "^string_of_int n2
  | PUSHGETGLOBALFIELD (n1,n2) -> "PUSHGETGLOBALFIELD "^string_of_int n1^" "^string_of_int n2
  | SETGLOBAL n -> "SETGLOBAL "^string_of_int n | ATOM0 -> "ATOM0" | ATOM n -> "ATOM "^string_of_int n | PUSHATOM0 -> "PUSHATOM0"
  | PUSHATOM n -> "PUSHATOM "^string_of_int n | MAKEBLOCK (n1,n2) -> "MAKEBLOCK "^string_of_int n1^" "^string_of_int n2
  | MAKEBLOCK1 n -> "MAKEBLOCK1 "^string_of_int n | MAKEBLOCK2 n -> "MAKEBLOCK2 "^string_of_int n
  | MAKEBLOCK3 n -> "MAKEBLOCK3 "^string_of_int n | MAKEFLOATBLOCK n -> "MAKEFLOATBLOCK "^string_of_int n
  | GETFIELD0 -> "GETFIELD0" | GETFIELD1 -> "GETFIELD1" | GETFIELD2 -> "GETFIELD2" | GETFIELD3 -> "GETFIELD3"
  | GETFIELD n -> "GETFIELD "^string_of_int n | GETFLOATFIELD n -> "GETFLOATFIELD "^string_of_int n
  | SETFIELD0 -> "SETFIELD0" | SETFIELD1 -> "SETFIELD1" | SETFIELD2 -> "SETFIELD2" | SETFIELD3 -> "SETFIELD3"
  | SETFIELD n -> "SETFIELD "^string_of_int n | SETFLOATFIELD n -> "SETFLOATFIELD "^string_of_int n
  | VECTLENGTH -> "VECTLENGTH" | GETVECTITEM -> "GETVECTITEM" | SETVECTITEM -> "SETVECTITEM" | GETSTRINGCHAR -> "GETSTRINGCHAR" | SETSTRINGCHAR -> "SETSTRINGCHAR"
  | BRANCH n -> "BRANCH "^string_of_int n | BRANCHIF n -> "BRANCHIF "^string_of_int n | BRANCHIFNOT n -> "BRANCHIFNOT "^string_of_int n
  | SWITCH (n,tab) -> "SWITCH "^string_of_int (n lsr 16)^" "^string_of_int (n land 0xFFFF)
  | BOOLNOT -> "BOOLNOT" | PUSHTRAP n -> "PUSHTRAP "^string_of_int n | POPTRAP -> "POPTRAP"
  | RAISE -> "RAISE" | CHECK_SIGNALS -> "CHECK_SIGNALS"
  | C_CALL1 n -> "C_CALL1 "^prims.(n) | C_CALL2 n -> "C_CALL2 "^prims.(n)
  | C_CALL3 n -> "C_CALL3 "^prims.(n) | C_CALL4 n -> "C_CALL4 "^prims.(n)
  | C_CALL5 n -> "C_CALL5 "^prims.(n)
  | C_CALLN (n1,n2) -> "C_CALLN "^string_of_int n1^" "^string_of_int n2
  | CONST0 -> "CONST0" | CONST1 -> "CONST1" | CONST2 -> "CONST2" | CONST3 -> "CONST3"
  | CONSTINT n -> "CONSTINT "^string_of_int n | PUSHCONST0 -> "PUSHCONST0"
  | PUSHCONST1 -> "PUSHCONST1" | PUSHCONST2 -> "PUSHCONST2"
  | PUSHCONST3 -> "PUSHCONST3" | PUSHCONSTINT n -> "PUSHCONSTINT "^string_of_int n
  | NEGINT -> "NEGINT" | ADDINT -> "ADDINT"
  | SUBINT -> "SUBINT" | MULINT -> "MULINT" | DIVINT -> "DIVINT" | MODINT -> "MODINT" | ANDINT -> "ANDINT" | ORINT -> "ORINT" | XORINT -> "XORINT" | LSLINT -> "LSLINT"
  | LSRINT -> "LSRINT" | ASRINT -> "ASRINT" | EQ -> "EQ" | NEQ -> "NEQ" | LTINT -> "LTINT" | LEINT -> "LEINT" | GTINT -> "GTINT" | GEINT -> "GEINT"
  | OFFSETINT n -> "OFFSETINT "^string_of_int n | OFFSETREF n -> "OFFSETREF "^string_of_int n
  | ISINT -> "ISINT" | GETMETHOD -> "GETMETHOD"
  | BEQ (n1,n2) -> "BEQ "^string_of_int n1^" "^string_of_int n2
  | BNEQ (n1,n2) -> "BNEQ "^string_of_int n1^" "^string_of_int n2
  | BLTINT (n1,n2) -> "BLTINT "^string_of_int n1^" "^string_of_int n2
  | BLEINT (n1,n2) -> "BLEINT "^string_of_int n1^" "^string_of_int n2
  | BGTINT (n1,n2) -> "BGTINT "^string_of_int n1^" "^string_of_int n2
  | BGEINT (n1,n2) -> "BGEINT "^string_of_int n1^" "^string_of_int n2
  | ULTINT -> "ULTINT" | UGEINT -> "UGEINT" | BULTINT (n1,n2) -> "BULTINT "^string_of_int n1^" "^string_of_int n2
  | BUGEINT (n1,n2) -> "BUGEINT "^string_of_int n1^" "^string_of_int n2
  | GETPUBMET (n1,n2) -> "GETPUBMET "^string_of_int n1^" "^string_of_int n2
  | GETDYNMET -> "GETDYNMET" | STOP -> "STOP" | EVENT -> "EVENT" | BREAK -> "BREAK"
  | Param i -> "Parameter "^(string_of_int i)

(* convert a string into a int32 (OCaml type) *)
let input_int32_from_string s i =
  let n =
    Int32.logor
      (Int32.of_int
         (int_of_char s.[i+0] lsl 0
          lor (int_of_char s.[i+1] lsl 8)
          lor (int_of_char s.[i+2] lsl 16)))
      (Int32.shift_left (Int32.of_int (int_of_char s.[i+3])) 24) in
  assert (Int32.of_int (Int32.to_int n) = n);
  Int32.to_int n

let parse_code_section s =
  let len = String.length s / 4 in
  let res = Array.make len (STOP) in
  let i = ref 0 in
  while !i < len do
    let inst = int_of_char s.[!i*4] in
    (match num_parameters inst with
      | No_params ->
          res.(!i) <- instruction_of_number0 inst
      | One_int ->
          let n = input_int32_from_string s ((!i+1)*4) in
          res.(!i) <- instruction_of_number1 n inst;
          incr i;
          res.(!i) <- Param n
      | Two_ints ->
          let n1 = input_int32_from_string s ((!i+1)*4) in
          let n2 = input_int32_from_string s ((!i+2)*4) in
          res.(!i) <- instruction_of_number2 n1 n2 inst;
          incr i;
          res.(!i) <- Param n1;
          incr i;
          res.(!i) <- Param n2
      | Switch ->
          let n = input_int32_from_string s ((!i+1)*4) in
          let size = (n land 0xFFFF) + (n lsr 16) in
          let tab = Array.make size 0 in
          for j = 0 to size - 1 do
            tab.(j) <- input_int32_from_string s ((!i+2+j)*4)
          done;
          res.(!i) <- SWITCH (n,tab);
          incr i;
          res.(!i) <- Param n;
          for j = 0 to size - 1 do
            incr i;
            res.(!i) <- Param tab.(j)
          done
      | Recursive_closure ->
          let f = input_int32_from_string s ((!i+1)*4) in
          let v = input_int32_from_string s ((!i+2)*4) in
          let o = input_int32_from_string s ((!i+3)*4) in
          let t = Array.make (f - 1) 0 in
          for j = 0 to f - 2 do
            t.(j) <- input_int32_from_string s ((!i+4+j)*4)
          done;
          res.(!i) <- CLOSUREREC (f,v,o,t);
          incr i;
          res.(!i) <- Param f;
          incr i;
          res.(!i) <- Param v;
          incr i;
          res.(!i) <- Param o;
          for j = 0 to f - 2 do
            incr i;
            res.(!i) <- Param t.(j)
          done);
    incr i
  done;
  res

(* end of instructions.ml *)
