(** Sampling handler types for OxFastMCP server.

    This module defines the type alias for server-side sampling handlers
    that process sampling requests from MCP clients. *)

open! Core

(** Sampling parameters for create message requests.
    Maps to mcp.types.CreateMessageRequestParams in Python. *)
type sampling_params = Mcp.Types.create_message_request_params

(** Sampling message type.
    Maps to mcp.types.SamplingMessage in Python. *)
type sampling_message = Mcp.Types.sampling_message

(** Create message result type.
    Maps to mcp.CreateMessageResult in Python. *)
type create_message_result = Mcp.Types.create_message_result

(** Result type for sampling handlers - can return string or full result *)
type sampling_result =
  | Text of string
  | Full of create_message_result

(** Server-side sampling handler type.

    A sampling handler receives:
    - A list of sampling messages (the conversation so far)
    - Sampling parameters (model preferences, system prompt, etc.)
    - Request context with session information

    And returns either:
    - A simple string response
    - A full CreateMessageResult with role and content

    In Python this is defined as:
    {[
      ServerSamplingHandler: TypeAlias = Callable[
          [
              list[SamplingMessage],
              SamplingParams,
              RequestContext[ServerSession, LifespanContextT],
          ],
          str | CreateMessageResult | Awaitable[str | CreateMessageResult],
      ]
    ]}

    In OCaml we use a function type with Deferred for async. *)
type 'ctx server_sampling_handler =
  sampling_message list ->
  sampling_params ->
  'ctx ->
  sampling_result Async.Deferred.t

(** Synchronous sampling handler variant *)
type 'ctx server_sampling_handler_sync =
  sampling_message list ->
  sampling_params ->
  'ctx ->
  sampling_result

(** Convert a sync handler to async *)
let async_of_sync (handler : 'ctx server_sampling_handler_sync) : 'ctx server_sampling_handler =
  fun messages params ctx ->
    Async.return (handler messages params ctx)

(** Create a simple text response *)
let text_response (text : string) : sampling_result = Text text

(** Create a full response with role and content *)
let full_response (result : create_message_result) : sampling_result = Full result
