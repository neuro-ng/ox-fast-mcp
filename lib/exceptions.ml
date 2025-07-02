(**
   Custom exceptions for FastMCP.
   This module provides exception types used throughout the FastMCP framework.
*)

open Core

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

(** Helper functions to create exceptions *)
let fast_mcp_error msg = Fast_mcp_error msg
let validation_error msg = Validation_error msg
let resource_error msg = Resource_error msg
let tool_error msg = Tool_error msg
let prompt_error msg = Prompt_error msg
let invalid_signature msg = Invalid_signature msg
let client_error msg = Client_error msg
let not_found_error msg = Not_found_error msg
let disabled_error msg = Disabled_error msg

(** Helper functions to convert exceptions to strings *)
let error_to_string = function
  | Fast_mcp_error msg -> sprintf "FastMCP error: %s" msg
  | Validation_error msg -> sprintf "Validation error: %s" msg
  | Resource_error msg -> sprintf "Resource error: %s" msg
  | Tool_error msg -> sprintf "Tool error: %s" msg
  | Prompt_error msg -> sprintf "Prompt error: %s" msg
  | Invalid_signature msg -> sprintf "Invalid signature: %s" msg
  | Client_error msg -> sprintf "Client error: %s" msg
  | Not_found_error msg -> sprintf "Not found: %s" msg
  | Disabled_error msg -> sprintf "Disabled: %s" msg
  | exn -> sprintf "Unknown error: %s" (Exn.to_string exn) 