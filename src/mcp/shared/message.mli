(** Message wrapper with metadata support. This module defines a wrapper type
    that combines JSONRPCMessage with metadata to support transport-specific
    features like resumability. *)

open! Core
open! Lwt.Syntax

type resumption_token = string
(** Type alias for resumption token *)

type resumption_token_update_callback = resumption_token -> unit Lwt.t
(** Callback type for resumption token updates *)

type client_message_metadata = {
  resumption_token : resumption_token option;
  on_resumption_token_update : resumption_token_update_callback option;
}
[@@deriving yojson]
(** Metadata specific to client messages *)

type server_message_metadata = {
  related_request_id : Mcp.Types.request_id option;
  request_context : Mcp.Types.json option; (* Using JSON for arbitrary context *)
}
[@@deriving yojson]
(** Metadata specific to server messages *)

(** Combined message metadata type *)
type message_metadata =
  | Client of client_message_metadata
  | Server of server_message_metadata
[@@deriving yojson]

type session_message = {
  message : Mcp.Types.jsonrpc_message;
  metadata : message_metadata option;
}
[@@deriving yojson]
(** A message with specific metadata for transport-specific features *)
