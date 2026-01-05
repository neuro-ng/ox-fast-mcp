(** Exceptions Module

    This module provides exception types used throughout the MCP framework. *)

exception Mcp_error of Mcp.Types.error_data
(** Exception type raised when an error arrives over an MCP connection *)

exception
  Url_elicitation_required of {
    elicitations : Yojson.Safe.t list;
        (* Using Yojson until elicit_request_url_params type is defined *)
    message : string;
  }
(** Exception for when a tool requires URL mode elicitation(s) before proceeding *)

val create_error : Mcp.Types.error_data -> exn
(** Create a new MCP error exception *)

val get_error : exn -> Mcp.Types.error_data
(** Get the error data from an MCP error exception *)

val get_error_message : exn -> string
(** Get the error message from an MCP error exception *)

val url_elicitation_required :
  Yojson.Safe.t list -> ?message:string -> unit -> exn
(** Create a URL elicitation required error *)

val url_elicitation_from_error : Mcp.Types.error_data -> exn
(** Reconstruct URL elicitation error from ErrorData received over the wire *)
