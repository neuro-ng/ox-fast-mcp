(** Custom exceptions for OxFastMCP *)

open! Core
open! Async
open Mcp.Types

type error_data = {
  message : string;
  code : int option; [@yojson.option]
  data : json option; [@yojson.option]
}
[@@deriving sexp, yojson]

exception OxFastMCP_error of error_data
(** Base error for OxFastMCP - equivalent to Python's OxFastMCPError *)

exception Validation_error of error_data
(** Error in validating parameters or return values - equivalent to Python's
    ValidationError *)

exception Resource_error of error_data
(** Error in resource operations - equivalent to Python's ResourceError *)

exception Tool_error of error_data
(** Error in tool operations - equivalent to Python's ToolError *)

exception Prompt_error of error_data
(** Error in prompt operations - equivalent to Python's PromptError *)

exception Invalid_signature of error_data
(** Invalid signature for use with OxFastMCP - equivalent to Python's
    InvalidSignature *)

exception Client_error of error_data
(** Error in client operations - equivalent to Python's ClientError *)

exception Not_found_error of error_data
(** Object not found - equivalent to Python's NotFoundError *)

exception Disabled_error of error_data
(** Object is disabled - equivalent to Python's DisabledError *)

val create_oxfastmcp_error :
  ?code:int option -> ?data:json option -> string -> exn
(** Create an OxFastMCP error with optional code and data *)

val create_validation_error :
  ?code:int option -> ?data:json option -> string -> exn
(** Create a validation error with optional code and data *)

val create_resource_error :
  ?code:int option -> ?data:json option -> string -> exn
(** Create a resource error with optional code and data *)

val create_tool_error : ?code:int option -> ?data:json option -> string -> exn
(** Create a tool error with optional code and data *)

val create_prompt_error : ?code:int option -> ?data:json option -> string -> exn
(** Create a prompt error with optional code and data *)

val create_invalid_signature :
  ?code:int option -> ?data:json option -> string -> exn
(** Create an invalid signature error with optional code and data *)

val create_client_error : ?code:int option -> ?data:json option -> string -> exn
(** Create a client error with optional code and data *)

val create_not_found_error :
  ?code:int option -> ?data:json option -> string -> exn
(** Create a not found error with optional code and data *)

val create_disabled_error :
  ?code:int option -> ?data:json option -> string -> exn
(** Create a disabled error with optional code and data *)

val to_string : exn -> string
(** Convert exception to string representation *)

val get_error_data : exn -> error_data
(** Get error data from an OxFastMCP exception
    @raise Failure if the exception is not an OxFastMCP error *)

val is_oxfastmcp_error : exn -> bool
(** Check if exception is an OxFastMCP exception *)
