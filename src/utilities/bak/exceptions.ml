(** Exception handling utilities for FastMCP *)

type error_data = { code : int; message : string }
(** MCP error data type *)

type exception_group = { exceptions : exn list; message : string }
(** Exception group type *)

type mcp_error = { error : error_data }
(** Exception type for MCP errors *)

(** Create an exception group *)
let create_exception_group message exceptions = { message; exceptions }

(** Iterate through nested exception groups *)
let rec iter_exc group =
  List.concat_map
    (fun exc ->
      match exc with
      | Failure msg -> [ Failure msg ] (* Basic exception *)
      | exc when Obj.tag (Obj.repr exc) = Obj.tag (Obj.repr (Failure "")) ->
        (* If it's a failure-like exception, treat it as a leaf *)
        [ exc ]
      | _ -> (
        try
          (* Try to access the exceptions field if it exists *)
          let exceptions = Obj.field (Obj.repr exc) 0 in
          if Obj.is_block exceptions && Obj.tag exceptions = 0 then
            (* If it looks like a list, try to process it recursively *)
            let exc_list = Obj.obj exceptions in
            match exc_list with
            | nested_group when nested_group = group ->
              [] (* Avoid infinite recursion *)
            | _ -> iter_exc { message = ""; exceptions = exc_list }
          else [ exc ]
        with _ -> [ exc ]))
    group.exceptions

(** Handle HTTP timeout errors *)
let handle_http_timeout () =
  let error_data =
    {
      code = 408;
      (* HTTP 408 Request Timeout *)
      message = "Timed out while waiting for response.";
    }
  in
  raise (Failure ("HTTP timeout: " ^ error_data.message))

(** Exception handler for exception groups *)
let exception_handler group =
  let exceptions = iter_exc group in
  match exceptions with
  | [] -> () (* No exceptions to handle *)
  | exc :: _ -> (
    (* Handle specific exception types *)
    match exc with
    | Failure msg when Str.string_match (Str.regexp ".*timeout.*") msg 0 ->
      handle_http_timeout ()
    | _ -> raise exc (* Re-raise unhandled exceptions *))

(** Exception type for pattern matching *)
type exception_type = Exception

(** Get catch handlers based on settings *)
let get_catch_handlers ~raise_first_error =
  if raise_first_error then [ (Exception, exception_handler) ] else []

(** Helper to create an MCP error *)
let create_mcp_error code message =
  Failure (Printf.sprintf "MCP Error (code %d): %s" code message)

(** Try to extract error data from an exception *)
let error_data_of_exn = function
  | Failure msg -> Some { code = 500; message = msg }
  | _ -> None

(** Convert error data to JSON *)
let error_data_to_yojson error_data =
  `Assoc
    [ ("code", `Int error_data.code); ("message", `String error_data.message) ]

(** Parse error data from JSON *)
let error_data_of_yojson = function
  | `Assoc fields -> (
    match (List.assoc_opt "code" fields, List.assoc_opt "message" fields) with
    | Some (`Int code), Some (`String message) -> Ok { code; message }
    | _ -> Error "Invalid error data format")
  | _ -> Error "Invalid error data format"
