(**
   Exceptions Module

   This module provides exception types used throughout the MCP framework.
*)

open Mcp.Types

(** Exception type raised when an error arrives over an MCP connection *)
exception Mcp_error of error_data

(** Create a new MCP error exception *)
let create_error error = Mcp_error error

(** Get the error data from an MCP error exception *)
let get_error = function
  | Mcp_error error -> error
  | _ -> failwith "Not an MCP error"

(** Get the error message from an MCP error exception *)
let get_error_message = function
  | Mcp_error error -> error.message
  | _ -> failwith "Not an MCP error" 