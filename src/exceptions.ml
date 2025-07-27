(** Custom exceptions for FastMCP *)

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

(* Base error for FastMCP - equivalent to 'class FastMCPError(Exception)' *)
exception FastMCP_error of error_data

(* Error in validating parameters or return values - equivalent to 'class
   ValidationError(FastMCPError)' *)
exception Validation_error of error_data

(* Error in resource operations - equivalent to 'class
   ResourceError(FastMCPError)' *)
exception Resource_error of error_data

(* Error in tool operations - equivalent to 'class ToolError(FastMCPError)' *)
exception Tool_error of error_data

(* Error in prompt operations - equivalent to 'class
   PromptError(FastMCPError)' *)
exception Prompt_error of error_data

(* Invalid signature for use with FastMCP - equivalent to 'class
   InvalidSignature(Exception)' *)
exception Invalid_signature of error_data

(* Error in client operations - equivalent to 'class ClientError(Exception)' *)
exception Client_error of error_data

(* Object not found - equivalent to 'class NotFoundError(Exception)' *)
exception Not_found_error of error_data

(* Object is disabled - equivalent to 'class DisabledError(Exception)' *)
exception Disabled_error of error_data

(* Helper functions to create exceptions with just a message *)
let create_fastmcp_error ?(code = None) ?(data = None) message =
  FastMCP_error { message; code; data }

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
  | FastMCP_error { message; _ } -> sprintf "FastMCP_error: %s" message
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
  | FastMCP_error data -> data
  | Validation_error data -> data
  | Resource_error data -> data
  | Tool_error data -> data
  | Prompt_error data -> data
  | Invalid_signature data -> data
  | Client_error data -> data
  | Not_found_error data -> data
  | Disabled_error data -> data
  | _ -> failwith "Not a FastMCP error"

(* Check if exception is a FastMCP exception *)
let is_fastmcp_error = function
  | FastMCP_error _
  | Validation_error _
  | Resource_error _
  | Tool_error _
  | Prompt_error _
  | Invalid_signature _
  | Client_error _
  | Not_found_error _
  | Disabled_error _ -> true
  | _ -> false
