(** Exception handling utilities for FastMCP *)

(** MCP error data type *)
type error_data = {
  code : int;
  message : string;
}

(** Exception group type *)
type exception_group = {
  exceptions : exn list;
  message : string;
}

(** Exception type for MCP errors *)
type mcp_error = {
  error : error_data;
}

(** Exception type for pattern matching *)
type exception_type = Exception

(** Create an exception group *)
val create_exception_group : string -> exn list -> exception_group

(** Iterate through nested exception groups *)
val iter_exc : exception_group -> exn list

(** Handle HTTP timeout errors *)
val handle_http_timeout : unit -> 'a

(** Exception handler for exception groups *)
val exception_handler : exception_group -> unit

(** Get catch handlers based on settings *)
val get_catch_handlers : raise_first_error:bool -> (exception_type * (exception_group -> unit)) list

(** Helper to create an MCP error *)
val create_mcp_error : int -> string -> exn

(** Try to extract error data from an exception *)
val error_data_of_exn : exn -> error_data option

(** Convert error data to JSON *)
val error_data_to_yojson : error_data -> Yojson.Safe.t

(** Parse error data from JSON *)
val error_data_of_yojson : Yojson.Safe.t -> (error_data, string) result 