exception Unknown_instruction of int

type nb_params = No_params | One_int | Two_ints | Recursive_closure | Switch

type instruction =
    ACC0
  | ACC1
  | ACC2
  | ACC3
  | ACC4
  | ACC5
  | ACC6
  | ACC7
  | ACC of int

  | PUSH
  | PUSHACC0
  | PUSHACC1
  | PUSHACC2
  | PUSHACC3
  | PUSHACC4
  | PUSHACC5
  | PUSHACC6
  | PUSHACC7
  | PUSHACC of int

  | POP of int
  | ASSIGN of int

  | ENVACC1
  | ENVACC2
  | ENVACC3
  | ENVACC4
  | ENVACC of int

  | PUSHENVACC1
  | PUSHENVACC2
  | PUSHENVACC3
  | PUSHENVACC4
  | PUSHENVACC of int

  | PUSH_RETADDR of int

  | APPLY of int
  | APPLY1
  | APPLY2
  | APPLY3

  | APPTERM of int * int
  | APPTERM1 of int
  | APPTERM2 of int
  | APPTERM3 of int

  | RETURN of int

  | RESTART
  | GRAB of int
  | CLOSURE of int * int
  | CLOSUREREC of int * int * int * int array
  | OFFSETCLOSUREM2
  | OFFSETCLOSURE0
  | OFFSETCLOSURE2
  | OFFSETCLOSURE of int
  | PUSHOFFSETCLOSUREM2
  | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE2
  | PUSHOFFSETCLOSURE of int
  | GETGLOBAL of int
  | PUSHGETGLOBAL of int
  | GETGLOBALFIELD of int * int
  | PUSHGETGLOBALFIELD of int * int
  | SETGLOBAL of int
  | ATOM0
  | ATOM of int
  | PUSHATOM0
  | PUSHATOM of int
  | MAKEBLOCK of int * int
  | MAKEBLOCK1 of int
  | MAKEBLOCK2 of int
  | MAKEBLOCK3 of int
  | MAKEFLOATBLOCK of int
  | GETFIELD0
  | GETFIELD1
  | GETFIELD2
  | GETFIELD3
  | GETFIELD of int
  | GETFLOATFIELD of int
  | SETFIELD0
  | SETFIELD1
  | SETFIELD2
  | SETFIELD3
  | SETFIELD of int
  | SETFLOATFIELD of int
  | VECTLENGTH
  | GETVECTITEM
  | SETVECTITEM
  | GETSTRINGCHAR
  | SETSTRINGCHAR
  | BRANCH of int
  | BRANCHIF of int
  | BRANCHIFNOT of int
  | SWITCH of int * int array
  | BOOLNOT
  | PUSHTRAP of int
  | POPTRAP
  | RAISE
  | CHECK_SIGNALS

  | C_CALL1 of int
  | C_CALL2 of int
  | C_CALL3 of int
  | C_CALL4 of int
  | C_CALL5 of int
  | C_CALLN of int * int

  | CONST0
  | CONST1
  | CONST2
  | CONST3
  | CONSTINT of int
  | PUSHCONST0
  | PUSHCONST1
  | PUSHCONST2
  | PUSHCONST3
  | PUSHCONSTINT of int
  | NEGINT
  | ADDINT
  | SUBINT
  | MULINT
  | DIVINT
  | MODINT
  | ANDINT
  | ORINT
  | XORINT
  | LSLINT
  | LSRINT
  | ASRINT
  | EQ
  | NEQ
  | LTINT
  | LEINT
  | GTINT
  | GEINT
  | OFFSETINT of int
  | OFFSETREF of int
  | ISINT
  | GETMETHOD
  | BEQ of int * int
  | BNEQ of int * int
  | BLTINT of int * int
  | BLEINT of int * int
  | BGTINT of int * int
  | BGEINT of int * int
  | ULTINT
  | UGEINT
  | BULTINT of int * int
  | BUGEINT of int * int
  | GETPUBMET of int * int
  | GETDYNMET
  | STOP
  | EVENT
  | BREAK
  | Param of int

val num_parameters : int -> nb_params
val instruction_of_number0 : int -> instruction
val instruction_of_number1 : int -> int -> instruction
val instruction_of_number2 : int -> int -> int -> instruction
val string_of_instructions : string array -> instruction -> string
val input_int32_from_string : string -> int -> int

val parse_code_section : string -> instruction array
