(**
   Custom exceptions for FastMCP.
   This module provides exception types used throughout the FastMCP framework.
*)

(** Base error for FastMCP *)
exception Fast_mcp_error of string

(** Error in validating parameters or return values *)
exception Validation_error of string

(** Error in resource operations *)
exception Resource_error of string

(** Error in tool operations *)
exception Tool_error of string

(** Error in prompt operations *)
exception Prompt_error of string

(** Invalid signature for use with FastMCP *)
exception Invalid_signature of string

(** Error in client operations *)
exception Client_error of string

(** Object not found *)
exception Not_found_error of string

(** Object is disabled *)
exception Disabled_error of string

(** Create a FastMCP error *)
val fast_mcp_error : string -> exn

(** Create a validation error *)
val validation_error : string -> exn

(** Create a resource error *)
val resource_error : string -> exn

(** Create a tool error *)
val tool_error : string -> exn

(** Create a prompt error *)
val prompt_error : string -> exn

(** Create an invalid signature error *)
val invalid_signature : string -> exn

(** Create a client error *)
val client_error : string -> exn

(** Create a not found error *)
val not_found_error : string -> exn

(** Create a disabled error *)
val disabled_error : string -> exn

(** Convert an exception to a human-readable string *)
val error_to_string : exn -> string 