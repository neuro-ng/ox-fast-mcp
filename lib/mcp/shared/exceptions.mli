(**
   Exceptions Module

   This module provides exception types used throughout the MCP framework.
*)

open Mcp.Types

(** Exception type raised when an error arrives over an MCP connection *)
exception Mcp_error of error_data

(** Create a new MCP error exception *)
val create_error : error_data -> exn

(** Get the error data from an MCP error exception.
    @raise Failure if the exception is not an MCP error *)
val get_error : exn -> error_data

(** Get the error message from an MCP error exception.
    @raise Failure if the exception is not an MCP error *)
val get_error_message : exn -> string 