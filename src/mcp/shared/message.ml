open Core
open Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type resumption_token = string [@@deriving yojson]
type resumption_token_update_callback = resumption_token -> unit Deferred.t

let resumption_token_update_callback_of_yojson (_ : Yojson.Safe.t) :
    resumption_token_update_callback =
 fun _ -> return ()

let resumption_token_update_callback_to_yojson
    (_ : resumption_token_update_callback) : Yojson.Safe.t =
  `Null

let yojson_of_resumption_token_update_callback =
  resumption_token_update_callback_to_yojson

type client_message_metadata = {
  resumption_token : resumption_token option;
  on_resumption_token_update : resumption_token_update_callback option;
}
[@@deriving yojson]

type server_message_metadata = {
  related_request_id : Mcp.Types.request_id option;
  request_context : Mcp.Types.json option; (* Using JSON for arbitrary context *)
}
[@@deriving yojson]

type message_metadata =
  | Client of client_message_metadata
  | Server of server_message_metadata
[@@deriving yojson]

type session_message = {
  message : Mcp.Types.jsonrpc_message;
  metadata : message_metadata option;
}
[@@deriving yojson]
