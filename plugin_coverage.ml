(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module I = Instructions
module Conv = Conv_obj_value

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let coverage = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Plugin *)
(*****************************************************************************)

let step vm instruction =
  (match instruction with
  | I.C_CALL1 _
  | I.C_CALL2 _
  | I.C_CALL3 _
  | I.C_CALL4 _
  | I.C_CALL5 _
  | I.C_CALLN _

  | I.APPLY _
  | I.APPLY1
  | I.APPLY2
  | I.APPLY3

  | I.APPTERM _
  | I.APPTERM1 _
  | I.APPTERM2 _
  | I.APPTERM3 _

  | I.RETURN _ 
    ->
    ()
  | _ -> ()
  )
  
let init data =
  ()

let finalise () = 
  ()

let _ =
  Plugin.register_plugin init step finalise
    ("-coverage", Arg.Unit (fun s -> coverage := true),
     "  print coverage information")
