(***********************************************************************)
(*                              Project Couverture                     *)
(*                                                                     *)
(* file: run_parsing.ml                                                *)
(* authors: Alexis Darrasse                                            *)
(* licence: CeCIL-B                                                    *)
(***********************************************************************)

open Ffi

let caml_parser_trace = ref false
let print_token tables state tok = output_string stderr "TODO"
let env_s_stack          = 0
and env_v_stack          = 1
and env_symb_start_stack = 2
and env_symb_end_stack   = 3
and env_stacksize        = 4
and env_stackbase        = 5
and env_curr_char        = 6
and env_lval             = 7
and env_symb_start       = 8
and env_symb_end         = 9
and env_asp              = 10
and env_rule_len         = 11
and env_rule_number      = 12
and env_sp               = 13
and env_state            = 14
and env_errflag          = 15
(*  and tables_actions        = 0 *)
and tables_transl_const   = 1
and tables_transl_block   = 2
and tables_lhs            = 3
and tables_len            = 4
and tables_defred         = 5
and tables_dgoto          = 6
and tables_sindex         = 7
and tables_rindex         = 8
and tables_gindex         = 9
and tables_tablesize      = 10
and tables_table          = 11
and tables_check          = 12
(*  and tables_error_function = 13
and tables_names_const    = 14
and tables_names_block    = 15 *)
and parser_read_token              = 0
and parser_raise_parse_error       = 1
and parser_grow_stacks_1           = 2
and parser_grow_stacks_2           = 3
and parser_compute_semantic_action = 4
and parser_call_error_function     = 5
let short_max = 1 lsl 15
let read_short s i =
  let res = int_of_char s.[2*i] + (int_of_char s.[2*i+1] lsl 8) in
  if res < short_max then
    res
  else
    (res mod short_max) - short_max
let caml_parse_engine vm tables env cmd arg =
  let state   = ref 0
  and sp      = ref 0
  and asp     = ref 0
  and errflag = ref 0
  and n       = ref 0
  and n1      = ref 0
  and n2      = ref 0
  and m       = ref 0
  and state1  = ref 0 in
  let save () =
    Utils.set_field env env_sp (Value.Int !sp);
    Utils.set_field env env_state (Value.Int !state);
    Utils.set_field env env_errflag (Value.Int !errflag) in
  let restore () =
    sp := Utils.int_of_value (Utils.get_field env env_sp);
    state := Utils.int_of_value (Utils.get_field env env_state);
    errflag := Utils.int_of_value (Utils.get_field env env_errflag) in
  let rec loop () =
    n := read_short (Utils.unbox_string (Utils.get_field tables tables_defred)) !state;
    if !n <> 0 then
      reduce ()
    else if Utils.int_of_value (Utils.get_field env env_curr_char) >= 0 then
      testshift ()
    else
      (save ();
       Value.Int parser_read_token)
  and testshift () =
    n1 := read_short (Utils.unbox_string (Utils.get_field tables tables_sindex)) !state;
    n2 := !n1 + Utils.int_of_value (Utils.get_field env env_curr_char);
    if !n1 <> 0 && !n2 >= 0 && !n2 <= Utils.int_of_value (Utils.get_field tables tables_tablesize) &&
       read_short (Utils.unbox_string (Utils.get_field tables tables_check)) !n2
         = Utils.int_of_value (Utils.get_field env env_curr_char) then
       shift ()
    else begin
      n1 := read_short (Utils.unbox_string (Utils.get_field tables tables_rindex)) !state;
      n2 := !n1 + Utils.int_of_value (Utils.get_field env env_curr_char);
      if !n1 <> 0 && !n2 >= 0 && !n2 <= Utils.int_of_value (Utils.get_field tables tables_tablesize) &&
         read_short (Utils.unbox_string (Utils.get_field tables tables_check)) !n2
           = Utils.int_of_value (Utils.get_field env env_curr_char) then
        (n := read_short (Utils.unbox_string (Utils.get_field tables tables_table)) !n2;
         reduce ())
      else
        if !errflag > 0 then
          recover ()
        else
          (save (); Value.Int parser_call_error_function)
    end
  and recover_loop () =
    state1 := Utils.int_of_value (Utils.get_field (Utils.get_field env env_s_stack) !sp);
    n1 := read_short (Utils.unbox_string (Utils.get_field tables tables_sindex)) !state1;
    n2 := !n1 + 256;
    if !n1 <> 0 && !n2 >= 0 && !n2 <= Utils.int_of_value (Utils.get_field tables tables_tablesize) &&
       read_short (Utils.unbox_string (Utils.get_field tables tables_check)) !n2
         = 256 then begin
      if !caml_parser_trace then
        output_string stderr ("Recovering in state "^string_of_int !state1^"\n");
      shift_recover ()
    end else begin
      if !caml_parser_trace then
        output_string stderr ("Discarding state "^string_of_int !state1^"\n");
      if !sp <= Utils.int_of_value (Utils.get_field env env_stackbase) then begin
        if !caml_parser_trace then
           output_string stderr ("No more states to discard\n");
        Value.Int parser_raise_parse_error
      end else
        (sp := !sp - 1; recover_loop ())
    end
  and recover () =
    if !errflag < 3 then
      (errflag := 3; recover_loop ())
    else
      if Utils.int_of_value (Utils.get_field env env_curr_char) = 0 then
        Value.Int parser_raise_parse_error
      else begin
        if !caml_parser_trace then
          output_string stderr ("Discarding last token\n");
        Utils.set_field env env_curr_char (Value.Int (-1));
        loop ()
      end
  and shift () =
    Utils.set_field env env_curr_char (Value.Int (-1));
    if !errflag > 0 then
      errflag := !errflag - 1;
    shift_recover ()
  and shift_recover () =
    if !caml_parser_trace then
      output_string stderr
        ("State "^string_of_int !state1^": shift to state "^
         string_of_int (read_short (Utils.unbox_string (Utils.get_field tables tables_table)) !n2)^"\n");
    state := read_short (Utils.unbox_string (Utils.get_field tables tables_table)) !n2;
    sp := !sp + 1;
    if !sp < Utils.int_of_value (Utils.get_field env env_stacksize) then
      push ()
    else
      (save (); Value.Int parser_grow_stacks_1)
  and push () =
    Utils.set_field (Utils.get_field env env_s_stack) !sp (Value.Int !state);
    Utils.set_field (Utils.get_field env env_v_stack) !sp (Utils.get_field env env_lval);
    Utils.set_field (Utils.get_field env env_symb_start_stack) !sp (Utils.get_field env env_symb_start);
    Utils.set_field (Utils.get_field env env_symb_end_stack) !sp (Utils.get_field env env_symb_end);
    loop ()
  and reduce () =
    if !caml_parser_trace then
      output_string stderr
        ("State "^string_of_int !state^": reduce by rule "^string_of_int !n^"\n");
    m := read_short (Utils.unbox_string (Utils.get_field tables tables_len)) !n;
    Utils.set_field env env_asp (Value.Int !sp);
    Utils.set_field env env_rule_number (Value.Int !n);
    Utils.set_field env env_rule_len (Value.Int !m);
    sp := !sp - !m + 1;
    m := read_short (Utils.unbox_string (Utils.get_field tables tables_lhs)) !n;
    state1 := Utils.int_of_value (Utils.get_field (Utils.get_field env env_s_stack) (!sp - 1));
    n1 := read_short (Utils.unbox_string (Utils.get_field tables tables_gindex)) !m;
    n2 := !n1 + !state1;
    if !n1 <> 0 && !n2 >= 0 && !n2 <= Utils.int_of_value (Utils.get_field tables tables_tablesize) &&
       read_short (Utils.unbox_string (Utils.get_field tables tables_check)) !n2
         = !state1 then
      state := read_short (Utils.unbox_string (Utils.get_field tables tables_table)) !n2
    else
      state := read_short (Utils.unbox_string (Utils.get_field tables tables_dgoto)) !m;
    if !sp < Utils.int_of_value (Utils.get_field env env_stacksize) then
      semantic_action ()
    else
      (save (); Value.Int parser_grow_stacks_2)
  and semantic_action () =
    save ();
    Value.Int parser_compute_semantic_action
    in
  match cmd with
  | Value.Int 0 ->
      state := 0;
      sp := Utils.int_of_value (Utils.get_field env env_sp);
      errflag := 0;
      loop ()
  | Value.Int 1 ->
      restore ();
      if Utils.is_int arg then
        (Utils.set_field env env_curr_char
           (Utils.get_field (Utils.get_field tables tables_transl_const) (Utils.int_of_value arg));
         Utils.set_field env env_lval (Value.Int 0))
      else
        (Utils.set_field env env_curr_char
           (Utils.get_field (Utils.get_field tables tables_transl_block)
                            (Utils.get_block_tag_int arg));
         Utils.set_field env env_lval (Utils.get_field arg 0));
      if !caml_parser_trace then
        print_token tables state arg;
      testshift ()
  | Value.Int 5 ->
      restore ();
      recover ()
  | Value.Int 2 ->
      restore ();
      push ()
  | Value.Int 3 ->
      restore ();
      semantic_action ()
  | Value.Int 4 ->
      restore ();
      Utils.set_field (Utils.get_field env env_s_stack) !sp (Value.Int !state);
      Utils.set_field (Utils.get_field env env_v_stack) !sp arg;
      asp := Utils.int_of_value (Utils.get_field env env_asp);
      Utils.set_field (Utils.get_field env env_symb_end_stack) !sp
        (Utils.get_field (Utils.get_field env env_symb_end_stack) !asp);
      if !sp > !asp then
        Utils.set_field (Utils.get_field env env_symb_start_stack) !sp
          (Utils.get_field (Utils.get_field env env_symb_start_stack) !asp);
      loop ()
  | _ -> ccall_failwith "error caml_parse_engine"

let caml_set_parser_trace vm flag =
  let oldflag = !caml_parser_trace in
  (match flag with
    | Value.Int 0 -> caml_parser_trace := false
    | Value.Int _ -> caml_parser_trace := true
    | _ -> ccall_failwith "error caml_set_parser");
  if oldflag then
    Value.Int 1
  else
    Value.Int 0

let prims () =
    add4 "caml_parse_engine" caml_parse_engine;
    add1 "caml_set_parser_trace" caml_set_parser_trace

let initialize dlls_section =
  prims ()
;;
init_list := initialize::!init_list
