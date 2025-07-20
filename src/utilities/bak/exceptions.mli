(** Exception handling utilities for FastMCP *)

type error_data = { code : int; message : string }
(** MCP error data type *)

type exception_group = { exceptions : exn list; message : string }
(** Exception group type *)

type mcp_error = { error : error_data }
(** Exception type for MCP errors *)

(** Exception type for pattern matching *)
type exception_type = Exception

val create_exception_group : string -> exn list -> exception_group
(** Create an exception group *)

val iter_exc : exception_group -> exn list
(** Iterate through nested exception groups *)

val handle_http_timeout : unit -> 'a
(** Handle HTTP timeout errors *)

val exception_handler : exception_group -> unit
(** Exception handler for exception groups *)

val get_catch_handlers :
  raise_first_error:bool -> (exception_type * (exception_group -> unit)) list
(** Get catch handlers based on settings *)

val create_mcp_error : int -> string -> exn
(** Helper to create an MCP error *)

val error_data_of_exn : exn -> error_data option
(** Try to extract error data from an exception *)

val error_data_to_yojson : error_data -> Yojson.Safe.t
(** Convert error data to JSON *)

val error_data_of_yojson : Yojson.Safe.t -> (error_data, string) result
(** Parse error data from JSON *)
