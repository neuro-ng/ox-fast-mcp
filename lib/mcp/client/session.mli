open Ox_fast_mcp.Mcp
open Mcp_types
open Mcp_shared
open Types

(** Error type for session operations *)
type error = [
  | `Timeout of string
  | `InvalidMessage of string
  | `RequestFailed of string
  | `ValidationError of string
  | `ProtocolError of string
]

(** Convert error to string *)
val error_to_string : error -> string

(** Default client info *)
val default_client_info : implementation

(** Client session type *)
type t

(** Progress callback type *)
type progress_fn = float -> float option -> string option -> unit Lwt.t

(** Sampling callback type *)
type sampling_fn = request_context -> create_message_request_params -> (create_message_result, error_data) result Lwt.t

(** Elicitation callback type *)
type elicitation_fn = request_context -> elicit_request_params -> (elicit_result, error_data) result Lwt.t

(** List roots callback type *)
type list_roots_fn = request_context -> (list_roots_result, error_data) result Lwt.t

(** Logging callback type *)
type logging_fn = logging_message_notification_params -> unit Lwt.t

(** Message handler type *)
type message_handler = [
  | `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn
] -> unit Lwt.t

(** Create a new client session *)
val create :
  ?read_timeout:float ->
  ?sampling_callback:sampling_fn ->
  ?elicitation_callback:elicitation_fn ->
  ?list_roots_callback:list_roots_fn ->
  ?logging_callback:logging_fn ->
  ?message_handler:message_handler ->
  ?client_info:implementation ->
  unit -> t Lwt.t

(** Initialize the session *)
val initialize : t -> initialize_result Lwt.t

(** Send a ping request *)
val send_ping : t -> empty_result Lwt.t

(** Send a progress notification *)
val send_progress_notification :
  t ->
  string ->
  float ->
  ?total:float ->
  ?message:string ->
  unit -> unit Lwt.t

(** Set logging level *)
val set_logging_level : t -> logging_level -> empty_result Lwt.t

(** List resources *)
val list_resources : t -> ?cursor:string -> unit -> list_resources_result Lwt.t

(** List resource templates *)
val list_resource_templates : t -> ?cursor:string -> unit -> list_resource_templates_result Lwt.t

(** Read resource *)
val read_resource : t -> Uri.t -> read_resource_result Lwt.t

(** Subscribe to resource *)
val subscribe_resource : t -> Uri.t -> empty_result Lwt.t

(** Unsubscribe from resource *)
val unsubscribe_resource : t -> Uri.t -> empty_result Lwt.t

(** Call tool *)
val call_tool :
  t ->
  string ->
  ?arguments:Yojson.Safe.t ->
  ?read_timeout:float ->
  ?progress_callback:progress_fn ->
  unit -> call_tool_result Lwt.t

(** List prompts *)
val list_prompts : t -> ?cursor:string -> unit -> list_prompts_result Lwt.t

(** Get prompt *)
val get_prompt :
  t ->
  string ->
  ?arguments:(string * string) list ->
  unit -> get_prompt_result Lwt.t

(** Complete request *)
val complete :
  t ->
  [< `Resource_template of resource_template_reference
   | `Prompt of prompt_reference ] ->
  (string * string) list ->
  ?context_arguments:(string * string) list ->
  unit -> complete_result Lwt.t

(** List tools *)
val list_tools : t -> ?cursor:string -> unit -> list_tools_result Lwt.t

(** Send roots list changed notification *)
val send_roots_list_changed : t -> unit Lwt.t

(** Start the session message handling loop. Must be called after initialization. *)
val start : t -> unit Lwt.t 