open! Core
open Types

type t = [
  | `Parse_error of string
  | `Invalid_request of string
  | `Method_not_found of string
  | `Invalid_params of string
  | `Internal_error of string
  | `Connection_closed of string
  | `Request_timeout of string
  | `Protocol_error of string * int
] [@@deriving sexp, yojson]

let to_error_data = function
  | `Parse_error msg -> { code = parse_error; message = msg; data = None }
  | `Invalid_request msg -> { code = invalid_request; message = msg; data = None }
  | `Method_not_found msg -> { code = method_not_found; message = msg; data = None }
  | `Invalid_params msg -> { code = invalid_params; message = msg; data = None }
  | `Internal_error msg -> { code = internal_error; message = msg; data = None }
  | `Connection_closed msg -> { code = connection_closed; message = msg; data = None }
  | `Request_timeout msg -> { code = -32001; message = msg; data = None }
  | `Protocol_error (msg, code) -> { code; message = msg; data = None }

let to_string = function
  | `Parse_error msg -> sprintf "Parse error: %s" msg
  | `Invalid_request msg -> sprintf "Invalid request: %s" msg
  | `Method_not_found msg -> sprintf "Method not found: %s" msg
  | `Invalid_params msg -> sprintf "Invalid parameters: %s" msg
  | `Internal_error msg -> sprintf "Internal error: %s" msg
  | `Connection_closed msg -> sprintf "Connection closed: %s" msg
  | `Request_timeout msg -> sprintf "Request timeout: %s" msg
  | `Protocol_error (msg, code) -> sprintf "Protocol error (%d): %s" code msg

let of_error_data ({ code; message; _ } : error_data) =
  match code with
  | c when c = parse_error -> `Parse_error message
  | c when c = invalid_request -> `Invalid_request message
  | c when c = method_not_found -> `Method_not_found message
  | c when c = invalid_params -> `Invalid_params message
  | c when c = internal_error -> `Internal_error message
  | c when c = connection_closed -> `Connection_closed message
  | -32001 -> `Request_timeout message
  | code -> `Protocol_error (message, code)

let raise err = raise_s (sexp_of_t err) 