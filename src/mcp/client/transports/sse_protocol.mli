open Core
open Async
module Types = Mcp.Types

(** SSE Protocol Parser for cohttp-async
    
    Implements RFC 8388 Server-Sent Events protocol parsing.
    Designed to work with cohttp-async streaming HTTP responses. *)

(** {1 SSE Event Type} *)

module Event : sig
  type t = {
    event_type : string; (* Event type, default "message" *)
    data : string; (* Event data payload, may be multi-line *)
    id : string option; (* Event ID for resumption tokens *)
    retry : int option; (* Reconnection time in milliseconds *)
  }
  [@@deriving sexp_of]

  (** Create a default event *)
  val create :
    ?event_type:string ->
    ?id:string option ->
    ?retry:int option ->
    string ->
    t

  (** Parse event data as JSON-RPC message *)
  val parse_jsonrpc : t -> Types.jsonrpc_message Or_error.t
end

(** {1 SSE Parser} *)

module Parser : sig
  type t

  (** Create a new SSE parser *)
  val create : unit -> t

  (** Feed a single line to the parser, returns completed events *)
  val feed_line : t -> string -> Event.t list

  (** Parse a streaming HTTP body into SSE events *)
  val parse_stream : Cohttp_async.Body.t -> Event.t Pipe.Reader.t
end
