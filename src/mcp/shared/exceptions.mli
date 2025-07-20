(** Exceptions Module

    This module provides exception types used throughout the MCP framework. *)

open Mcp.Types

exception Mcp_error of error_data
(** Exception type raised when an error arrives over an MCP connection *)

val create_error : error_data -> exn
(** Create a new MCP error exception *)

val get_error : exn -> error_data
(** Get the error data from an MCP error exception.
    @raise Failure if the exception is not an MCP error *)

val get_error_message : exn -> string
(** Get the error message from an MCP error exception.
    @raise Failure if the exception is not an MCP error *)
