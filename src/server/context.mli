(** Execution Context for OxFastMCP
    
    Centralizes context management for tool, resource, and prompt execution.
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 8 (lines 665-687)
    See: Task 8.1 - Centralized Context Module
*)

open! Core
open! Async

(** {1 Context Type} *)

(** Execution context passed to handlers
    
    Contains request metadata and mutation flags for tracking changes.
*)
type t = {
  request_id : string option;  (** Optional request identifier *)
  client_id : string option;  (** Optional client identifier *)
  session_data : (string, Yojson.Safe.t) Hashtbl.t;  (** Session storage *)
  mutable tools_changed : bool;  (** Flag: tools list modified *)
  mutable resources_changed : bool;  (** Flag: resources list modified *)
  mutable prompts_changed : bool;  (** Flag: prompts list modified *)
}

(** {1 Context Creation} *)

val create :
  ?request_id:string ->
  ?client_id:string ->
  unit ->
  t
(** Create a new execution context *)

val create_with_session :
  ?request_id:string ->
  ?client_id:string ->
  session_data:(string, Yojson.Safe.t) Hashtbl.t ->
  unit ->
  t
(** Create context with existing session data *)

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
(** Reset all change flags *)

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

val with_request_id : t -> string -> t
(** Create new context with different request ID *)

val with_client_id : t -> string -> t
(** Create new context with different client ID *)

(** {1 Type Aliases} *)

(** Function that takes context as first parameter *)
type 'a with_context = t -> 'a

