(** Custom exceptions for OxFastMCP *)

open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Import MCP types and exceptions - equivalent to 'from mcp import McpError' *)
open Mcp.Types

(* MCP error exception available as Mcp_error *)

type error_data = {
  message : string;
  code : int option; [@yojson.option]
  data : json option; [@yojson.option]
}
[@@deriving sexp, yojson]

(* Base error for OxFastMCP - equivalent to 'class OxFastMCPError(Exception)' *)
exception OxFastMCP_error of error_data

(* Error in validating parameters or return values - equivalent to 'class
   ValidationError(OxFastMCPError)' *)
exception Validation_error of error_data

(* Error in resource operations - equivalent to 'class
   ResourceError(OxFastMCPError)' *)
exception Resource_error of error_data

(* Error in tool operations - equivalent to 'class ToolError(OxFastMCPError)' *)
exception Tool_error of error_data

(* Error in prompt operations - equivalent to 'class
   PromptError(OxFastMCPError)' *)
exception Prompt_error of error_data

(* Invalid signature for use with OxFastMCP - equivalent to 'class
   InvalidSignature(Exception)' *)
exception Invalid_signature of error_data

(* Error in client operations - equivalent to 'class ClientError(Exception)' *)
exception Client_error of error_data

(* Object not found - equivalent to 'class NotFoundError(Exception)' *)
exception Not_found_error of error_data

(* Object is disabled - equivalent to 'class DisabledError(Exception)' *)
exception Disabled_error of error_data

(* Helper functions to create exceptions with just a message *)
let create_oxfastmcp_error ?(code = None) ?(data = None) message =
  OxFastMCP_error { message; code; data }

let create_validation_error ?(code = None) ?(data = None) message =
  Validation_error { message; code; data }

let create_resource_error ?(code = None) ?(data = None) message =
  Resource_error { message; code; data }

let create_tool_error ?(code = None) ?(data = None) message =
  Tool_error { message; code; data }

let create_prompt_error ?(code = None) ?(data = None) message =
  Prompt_error { message; code; data }

let create_invalid_signature ?(code = None) ?(data = None) message =
  Invalid_signature { message; code; data }

let create_client_error ?(code = None) ?(data = None) message =
  Client_error { message; code; data }

let create_not_found_error ?(code = None) ?(data = None) message =
  Not_found_error { message; code; data }

let create_disabled_error ?(code = None) ?(data = None) message =
  Disabled_error { message; code; data }

(* Convert to string representation *)
let to_string = function
  | OxFastMCP_error { message; _ } -> sprintf "OxFastMCP_error: %s" message
  | Validation_error { message; _ } -> sprintf "Validation_error: %s" message
  | Resource_error { message; _ } -> sprintf "Resource_error: %s" message
  | Tool_error { message; _ } -> sprintf "Tool_error: %s" message
  | Prompt_error { message; _ } -> sprintf "Prompt_error: %s" message
  | Invalid_signature { message; _ } -> sprintf "Invalid_signature: %s" message
  | Client_error { message; _ } -> sprintf "Client_error: %s" message
  | Not_found_error { message; _ } -> sprintf "Not_found_error: %s" message
  | Disabled_error { message; _ } -> sprintf "Disabled_error: %s" message
  | exn -> Exn.to_string exn

(* Get error data from exception *)
let get_error_data = function
  | OxFastMCP_error data -> data
  | Validation_error data -> data
  | Resource_error data -> data
  | Tool_error data -> data
  | Prompt_error data -> data
  | Invalid_signature data -> data
  | Client_error data -> data
  | Not_found_error data -> data
  | Disabled_error data -> data
  | _ -> failwith "Not an OxFastMCP error"

(* Check if exception is an OxFastMCP exception *)
let is_oxfastmcp_error = function
  | OxFastMCP_error _
  | Validation_error _
  | Resource_error _
  | Tool_error _
  | Prompt_error _
  | Invalid_signature _
  | Client_error _
  | Not_found_error _
  | Disabled_error _ -> true
  | _ -> false
