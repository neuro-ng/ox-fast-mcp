open Core

exception Not_found_error of string
exception Tool_error of string * exn option

let not_found_error msg = raise (Not_found_error msg)
let tool_error ?exn msg = raise (Tool_error (msg, exn))

let () =
  Caml.Printexc.register_printer (function
    | Not_found_error msg -> Some (sprintf "NotFoundError: %s" msg)
    | Tool_error (msg, Some exn) -> Some (sprintf "ToolError: %s: %s" msg (Exn.to_string exn))
    | Tool_error (msg, None) -> Some (sprintf "ToolError: %s" msg)
    | _ -> None) 