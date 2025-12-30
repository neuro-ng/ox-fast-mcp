(** Execution Context for OxFastMCP

    Centralizes context management for tool, resource, and prompt execution.
    Provides logging, progress reporting, and state management. *)

open! Core
open! Async

(** {1 Log Levels} *)

(** MCP logging levels *)
module Log_level : sig
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Critical
    | Alert
    | Emergency
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string : string -> t
  val to_logs_level : t -> Logs.level
end

(** Log data for passing to client-side handlers *)
module Log_data : sig
  type t = { msg : string; extra : (string * Yojson.Safe.t) list option }

  val create : msg:string -> ?extra:(string * Yojson.Safe.t) list -> unit -> t
end

(** {1 Model Preferences} *)

(** Model hint for sampling *)
module Model_hint : sig
  type t = { name : string } [@@deriving sexp, yojson]

  val create : name:string -> t
end

(** Model preferences for sampling requests *)
module Model_preferences : sig
  type t = { hints : Model_hint.t list } [@@deriving sexp, yojson]

  val create : hints:Model_hint.t list -> t
  val empty : t
end

val parse_model_preferences :
  [ `None
  | `String of string
  | `List of string list
  | `Preferences of Model_preferences.t ] ->
  Model_preferences.t option
(** Parse model preferences from various input types *)

(** {1 Context Type} *)

type t = {
  request_id : string option;  (** Optional request identifier *)
  client_id : string option;  (** Optional client identifier *)
  session_id : string option;  (** Optional session identifier *)
  session_data : (string, Yojson.Safe.t) Hashtbl.t;  (** Session storage *)
  mutable state : (string, Yojson.Safe.t) Hashtbl.t;  (** Context state *)
  mutable tools_changed : bool;  (** Flag: tools list modified *)
  mutable resources_changed : bool;  (** Flag: resources list modified *)
  mutable prompts_changed : bool;  (** Flag: prompts list modified *)
  notification_queue : (string, unit) Hashtbl.t;  (** Pending notifications *)
  logger : Logs.src;  (** Logger source *)
  (* Protocol handler fields *)
  method_name : string option;  (** MCP method name *)
  params : Yojson.Safe.t option;  (** MCP method parameters *)
  session : Session_bridge.Async_session.t option;  (** Optional MCP session *)
}
(** Execution context passed to handlers *)

(** {1 Context Creation} *)

val create :
  ?request_id:string ->
  ?client_id:string ->
  ?session_id:string ->
  ?method_name:string ->
  ?params:Yojson.Safe.t ->
  ?logger:Logs.src ->
  ?session:Session_bridge.Async_session.t ->
  unit ->
  t
(** Create a new execution context *)

val create_with_session :
  ?request_id:string ->
  ?client_id:string ->
  ?session_id:string ->
  ?method_name:string ->
  ?params:Yojson.Safe.t ->
  session_data:(string, Yojson.Safe.t) Hashtbl.t ->
  ?logger:Logs.src ->
  ?session:Session_bridge.Async_session.t ->
  unit ->
  t
(** Create context with existing session data *)

(** {1 State Management} *)

val get_state : t -> string -> Yojson.Safe.t option
(** Get a value from context state *)

val set_state : t -> string -> Yojson.Safe.t -> unit
(** Set a value in context state *)

val copy_state : t -> (string, Yojson.Safe.t) Hashtbl.t
(** Create a copy of the context state *)

val with_inherited_state : t -> t -> t
(** Create context with state inherited from parent *)

(** {1 Change Notifications} *)

val queue_tool_list_changed : t -> unit
(** Mark that the tool list has changed *)

val queue_resource_list_changed : t -> unit
(** Mark that the resource list has changed *)

val queue_prompt_list_changed : t -> unit
(** Mark that the prompt list has changed *)

val has_changes : t -> bool
(** Check if any list has changed *)

val get_changed_lists : t -> string list
(** Get names of changed lists *)

val reset_changes : t -> unit
(** Reset all change flags and clear notification queue *)

val get_pending_notifications : t -> string list
(** Get list of pending notification types *)

val send_resources_list_changed : t -> unit Deferred.t
(** Send notification that resources list has changed *)

val send_tools_list_changed : t -> unit Deferred.t
(** Send notification that tools list has changed *)

val send_prompts_list_changed : t -> unit Deferred.t
(** Send notification that prompts list has changed *)

(** {1 Sampling and Elicitation} *)

val sample :
  t ->
  messages:Mcp.Types.sampling_message list ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  ?include_context:Mcp.Types.include_context ->
  ?temperature:float ->
  ?stop_sequences:string list ->
  ?metadata:Yojson.Safe.t ->
  ?model_preferences:Mcp.Types.model_preferences ->
  unit ->
  Mcp.Types.client_request Deferred.t
(** Send a sampling request to the client to generate LLM completions.
    Raises Failure if no active session exists. *)

val elicit :
  t ->
  message:string ->
  requested_schema:Mcp.Types.elicit_requested_schema ->
  unit ->
  Mcp.Types.client_request Deferred.t
(** Send an elicitation request to the client to prompt user input.
    Raises Failure if no active session exists. *)

(** {1 Session Data Access} *)

val get_session_value : t -> string -> Yojson.Safe.t option
(** Get a value from session data *)

val set_session_value : t -> string -> Yojson.Safe.t -> unit
(** Set a value in session data *)

val remove_session_value : t -> string -> unit
(** Remove a value from session data *)

val clear_session : t -> unit
(** Clear all session data *)

(** {1 Context Information} *)

val get_request_id : t -> string option
(** Get the request ID *)

val get_client_id : t -> string option
(** Get the client ID *)

val get_session_id : t -> string option
(** Get the session ID *)

val with_request_id : t -> string -> t
(** Create new context with different request ID *)

val with_client_id : t -> string -> t
(** Create new context with different client ID *)

val with_session_id : t -> string -> t
(** Create new context with different session ID *)

(** {1 Logging} *)

val log :
  t ->
  level:Log_level.t ->
  message:string ->
  ?logger_name:string ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  unit
(** Log a message at a specific level *)

val debug :
  t ->
  message:string ->
  ?logger_name:string ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  unit
(** Log a DEBUG-level message *)

val info :
  t ->
  message:string ->
  ?logger_name:string ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  unit
(** Log an INFO-level message *)

val warning :
  t ->
  message:string ->
  ?logger_name:string ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  unit
(** Log a WARNING-level message *)

val error :
  t ->
  message:string ->
  ?logger_name:string ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  unit
(** Log an ERROR-level message *)

(** {1 Progress Reporting} *)

(** Progress information *)
module Progress : sig
  type t = {
    progress : float;
    total : float option;
    message : string option;
    request_id : string option;
  }
  [@@deriving sexp]

  val create :
    progress:float ->
    ?total:float ->
    ?message:string ->
    ?request_id:string ->
    unit ->
    t
end

val report_progress :
  t -> progress:float -> ?total:float -> ?message:string -> unit -> Progress.t
(** Report progress for the current operation *)

(** {1 Type Aliases} *)

type 'a with_context = t -> 'a
(** Function that takes context as first parameter *)
