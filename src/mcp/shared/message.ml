open Core
open Lwt.Syntax

type resumption_token = { value : string } [@@deriving yojson] 
type resumption_token_update_callback = resumption_token -> unit Lwt.t

type client_message_metadata = {
  resumption_token : resumption_token option;
  on_resumption_token_update : resumption_token_update_callback option;
}
[@@deriving yojson]

type server_message_metadata = {
  related_request_id : Mcp.Types.request_id option;
  request_context : Yojson.Safe.t option; (* Using JSON for arbitrary context *)
}
[@@deriving sexp, yojson]

type message_metadata =
  | Client of client_message_metadata
  | Server of server_message_metadata
[@@deriving sexp, yojson]

type session_message = {
  message : Mcp.Types.jsonrpc_message;
  metadata : message_metadata option;
}
[@@deriving sexp, yojson]
