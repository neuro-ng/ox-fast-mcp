(** Sampling handler types for OxFastMCP server.

    This module defines the type alias for server-side sampling handlers that
    process sampling requests from MCP clients. *)

type sampling_params = Mcp.Types.create_message_request_params
(** Sampling parameters for create message requests *)

type sampling_message = Mcp.Types.sampling_message
(** Sampling message type *)

type create_message_result = Mcp.Types.create_message_result
(** Create message result type *)

(** Result type for sampling handlers *)
type sampling_result = Text of string | Full of create_message_result

type 'ctx server_sampling_handler =
  sampling_message list ->
  sampling_params ->
  'ctx ->
  sampling_result Async.Deferred.t
(** Server-side sampling handler type.

    A sampling handler receives:
    - A list of sampling messages (the conversation so far)
    - Sampling parameters (model preferences, system prompt, etc.)
    - Request context with session information

    And returns either a simple string or a full CreateMessageResult. *)

type 'ctx server_sampling_handler_sync =
  sampling_message list -> sampling_params -> 'ctx -> sampling_result
(** Synchronous sampling handler variant *)

val async_of_sync :
  'ctx server_sampling_handler_sync -> 'ctx server_sampling_handler
(** Convert a sync handler to async *)

val text_response : string -> sampling_result
(** Create a simple text response *)

val full_response : create_message_result -> sampling_result
(** Create a full response with role and content *)
