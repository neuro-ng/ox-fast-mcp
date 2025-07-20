open Core

exception Not_found_error of string
exception Tool_error of string * exn option

let not_found_error msg = raise (Not_found_error msg)
let tool_error ?exn msg = raise (Tool_error (msg, exn))

(* Printer registration removed - avoid Printexc.register_printer issues *)
