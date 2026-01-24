(** Execution Context for OxFastMCP

    Centralizes context management for tool, resource, and prompt execution.
    Provides logging, progress reporting, and state management. *)

open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 Log Levels} *)

(** MCP logging levels *)
module Log_level = struct
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

  let to_string = function
    | Debug -> "debug"
    | Info -> "info"
    | Notice -> "notice"
    | Warning -> "warning"
    | Error -> "error"
    | Critical -> "critical"
    | Alert -> "alert"
    | Emergency -> "emergency"

  let of_string = function
    | "debug" -> Debug
    | "info" -> Info
    | "notice" -> Notice
    | "warning" -> Warning
    | "error" -> Error
    | "critical" -> Critical
    | "alert" -> Alert
    | "emergency" -> Emergency
    | s -> raise_s [%message "Unknown log level" (s : string)]

  (** Convert MCP level to OCaml Logs level *)
  let to_logs_level = function
    | Debug -> Logs.Debug
    | Info -> Logs.Info
    | Notice -> Logs.Info
    | Warning -> Logs.Warning
    | Error -> Logs.Error
    | Critical -> Logs.Error
    | Alert -> Logs.Error
    | Emergency -> Logs.Error
end

(** Log data for passing to client-side handlers *)
module Log_data = struct
  type t = { msg : string; extra : (string * Yojson.Safe.t) list option }

  let create ~msg ?extra () = { msg; extra }
end

(** {1 Model Preferences} *)

(** Model hint for sampling *)
module Model_hint = struct
  type t = { name : string } [@@deriving sexp, yojson]

  let create ~name = { name }
end

(** Model preferences for sampling requests *)
module Model_preferences = struct
  type t = { hints : Model_hint.t list } [@@deriving sexp, yojson]

  let create ~hints = { hints }
  let empty = { hints = [] }
end

(** Parse model preferences from various input types *)
let parse_model_preferences
    (input :
      [ `None
      | `String of string
      | `List of string list
      | `Preferences of Model_preferences.t ]) : Model_preferences.t option =
  match input with
  | `None -> None
  | `String name ->
    Some (Model_preferences.create ~hints:[ Model_hint.create ~name ])
  | `List names ->
    Some
      (Model_preferences.create
         ~hints:(List.map names ~f:(fun name -> Model_hint.create ~name)))
  | `Preferences prefs -> Some prefs

(** {1 Context Type} *)

type t = {
  request_id : string option;
  client_id : string option;
  session_id : string option;
  session_data : (string, Yojson.Safe.t) Hashtbl.t;
  mutable state : (string, Yojson.Safe.t) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
  notification_queue : (string, unit) Hashtbl.t;
  logger : Logs.src;
  (* Protocol handler fields *)
  method_name : string option;
  params : Yojson.Safe.t option;
  session : Session_bridge.Async_session.t option;
  (* Server delegation callbacks *)
  list_resources_fn : (unit -> Yojson.Safe.t list Deferred.t) option;
  list_prompts_fn : (unit -> Yojson.Safe.t list Deferred.t) option;
  read_resource_fn : (uri:string -> string Deferred.t) option;
  get_prompt_fn :
    (name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t) option;
}

(** {1 Context Creation} *)

let default_logger = Logs.Src.create "oxfastmcp.context"

let create ?(request_id : string option) ?(client_id : string option)
    ?(session_id : string option) ?(method_name : string option)
    ?(params : Yojson.Safe.t option) ?(logger = default_logger)
    ?(session : Session_bridge.Async_session.t option)
    ?(list_resources_fn = None) ?(list_prompts_fn = None)
    ?(read_resource_fn = None) ?(get_prompt_fn = None) () : t =
  {
    request_id;
    client_id;
    session_id;
    session_data = Hashtbl.create (module String);
    state = Hashtbl.create (module String);
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
    notification_queue = Hashtbl.create (module String);
    logger;
    method_name;
    params;
    session;
    list_resources_fn;
    list_prompts_fn;
    read_resource_fn;
    get_prompt_fn;
  }

let create_with_session ?(request_id : string option)
    ?(client_id : string option) ?(session_id : string option)
    ?(method_name : string option) ?(params : Yojson.Safe.t option)
    ~(session_data : (string, Yojson.Safe.t) Hashtbl.t)
    ?(logger = default_logger)
    ?(session : Session_bridge.Async_session.t option)
    ?(list_resources_fn = None) ?(list_prompts_fn = None)
    ?(read_resource_fn = None) ?(get_prompt_fn = None) () : t =
  {
    request_id;
    client_id;
    session_id;
    session_data;
    state = Hashtbl.create (module String);
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
    notification_queue = Hashtbl.create (module String);
    logger;
    method_name;
    params;
    session;
    list_resources_fn;
    list_prompts_fn;
    read_resource_fn;
    get_prompt_fn;
  }

(** {1 State Management} *)

let get_state (ctx : t) (key : string) : Yojson.Safe.t option =
  Hashtbl.find ctx.state key

let set_state (ctx : t) (key : string) (value : Yojson.Safe.t) : unit =
  Hashtbl.set ctx.state ~key ~data:value

let copy_state (ctx : t) : (string, Yojson.Safe.t) Hashtbl.t =
  Hashtbl.copy ctx.state

let with_inherited_state (parent : t) (child : t) : t =
  { child with state = Hashtbl.copy parent.state }

(** {1 Change Notifications} *)

let queue_tool_list_changed (ctx : t) : unit =
  ctx.tools_changed <- true;
  Hashtbl.set ctx.notification_queue ~key:"notifications/tools/list_changed"
    ~data:()

let queue_resource_list_changed (ctx : t) : unit =
  ctx.resources_changed <- true;
  Hashtbl.set ctx.notification_queue ~key:"notifications/resources/list_changed"
    ~data:()

let queue_prompt_list_changed (ctx : t) : unit =
  ctx.prompts_changed <- true;
  Hashtbl.set ctx.notification_queue ~key:"notifications/prompts/list_changed"
    ~data:()

let has_changes (ctx : t) : bool =
  ctx.tools_changed || ctx.resources_changed || ctx.prompts_changed

let get_changed_lists (ctx : t) : string list =
  let changed = [] in
  let changed = if ctx.tools_changed then "tools" :: changed else changed in
  let changed =
    if ctx.resources_changed then "resources" :: changed else changed
  in
  let changed = if ctx.prompts_changed then "prompts" :: changed else changed in
  List.rev changed

let reset_changes (ctx : t) : unit =
  ctx.tools_changed <- false;
  ctx.resources_changed <- false;
  ctx.prompts_changed <- false;
  Hashtbl.clear ctx.notification_queue

let get_pending_notifications (ctx : t) : string list =
  Hashtbl.keys ctx.notification_queue

(** Send notification that resources list has changed *)
let send_resources_list_changed (ctx : t) : unit Deferred.t =
  match ctx.session with
  | None -> return ()
  | Some session ->
    Session_bridge.Async_session.send_resource_list_changed session

(** Send notification that tools list has changed *)
let send_tools_list_changed (ctx : t) : unit Deferred.t =
  match ctx.session with
  | None -> return ()
  | Some session -> Session_bridge.Async_session.send_tool_list_changed session

(** Send notification that prompts list has changed *)
let send_prompts_list_changed (ctx : t) : unit Deferred.t =
  match ctx.session with
  | None -> return ()
  | Some session ->
    Session_bridge.Async_session.send_prompt_list_changed session

(** {1 Sampling and Elicitation} *)

(** Send a sampling request to the client to generate LLM completions *)
let sample (ctx : t) ~(messages : Mcp.Types.sampling_message list)
    ?(max_tokens = 1000) ?system_prompt ?include_context ?temperature
    ?stop_sequences ?metadata ?model_preferences () :
    Mcp.Types.client_request Deferred.t =
  match ctx.session with
  | None ->
    failwith
      "Cannot sample: no active session. Sampling requires an active MCP \
       session."
  | Some session ->
    (* Check if client supports sampling *)
    let has_sampling =
      Session_bridge.Async_session.check_client_capability session
        {
          sampling = Some (`Assoc []);
          elicitation = None;
          roots = None;
          experimental = None;
        }
    in
    if not has_sampling then
      failwith
        "Cannot sample: client does not support sampling capability. The \
         connected client must advertise sampling support during \
         initialization."
    else
      Session_bridge.Async_session.create_message session ~messages ~max_tokens
        ?system_prompt ?include_context ?temperature ?stop_sequences ?metadata
        ?model_preferences ()

(** Send an elicitation request to the client to prompt user input *)
let elicit (ctx : t) ~(message : string)
    ~(requested_schema : Mcp.Types.elicit_requested_schema) () :
    Mcp.Types.client_request Deferred.t =
  match ctx.session with
  | None ->
    failwith
      "Cannot elicit: no active session. Elicitation requires an active MCP \
       session."
  | Some session ->
    (* Check if client supports elicitation *)
    let has_elicitation =
      Session_bridge.Async_session.check_client_capability session
        {
          sampling = None;
          elicitation = Some (`Assoc []);
          roots = None;
          experimental = None;
        }
    in
    if not has_elicitation then
      failwith
        "Cannot elicit: client does not support elicitation capability. The \
         connected client must advertise elicitation support during \
         initialization."
    else
      Session_bridge.Async_session.elicit session ~message ~requested_schema ()

(** {1 Session Data Access} *)

let get_session_value (ctx : t) (key : string) : Yojson.Safe.t option =
  Hashtbl.find ctx.session_data key

let set_session_value (ctx : t) (key : string) (value : Yojson.Safe.t) : unit =
  Hashtbl.set ctx.session_data ~key ~data:value

let remove_session_value (ctx : t) (key : string) : unit =
  Hashtbl.remove ctx.session_data key

let clear_session (ctx : t) : unit = Hashtbl.clear ctx.session_data

(** {1 Context Information} *)

let get_request_id (ctx : t) : string option = ctx.request_id
let get_client_id (ctx : t) : string option = ctx.client_id
let get_session_id (ctx : t) : string option = ctx.session_id

let with_request_id (ctx : t) (request_id : string) : t =
  { ctx with request_id = Some request_id }

let with_client_id (ctx : t) (client_id : string) : t =
  { ctx with client_id = Some client_id }

let with_session_id (ctx : t) (session_id : string) : t =
  { ctx with session_id = Some session_id }

(** {1 Logging} *)

(** Log a message at a specific level *)
let log (ctx : t) ~(level : Log_level.t) ~(message : string) ?logger_name ?extra
    () : unit =
  let logs_level = Log_level.to_logs_level level in
  let prefix =
    match logger_name with
    | Some name -> Printf.sprintf "[%s] " name
    | None -> ""
  in
  let _ = extra in
  (* Extra data not yet used in OCaml logs *)
  Logs.msg ~src:ctx.logger logs_level (fun m -> m "%s%s" prefix message)

let debug (ctx : t) ~(message : string) ?logger_name ?extra () : unit =
  log ctx ~level:Debug ~message ?logger_name ?extra ()

let info (ctx : t) ~(message : string) ?logger_name ?extra () : unit =
  log ctx ~level:Info ~message ?logger_name ?extra ()

let warning (ctx : t) ~(message : string) ?logger_name ?extra () : unit =
  log ctx ~level:Warning ~message ?logger_name ?extra ()

let error (ctx : t) ~(message : string) ?logger_name ?extra () : unit =
  log ctx ~level:Error ~message ?logger_name ?extra ()

(** {1 Progress Reporting} *)

(** Progress information *)
module Progress = struct
  type t = {
    progress : float;
    total : float option;
    message : string option;
    request_id : string option;
  }
  [@@deriving sexp]

  let create ~progress ?total ?message ?request_id () =
    { progress; total; message; request_id }
end

(** Report progress for the current operation. Note: Full implementation
    requires MCP session integration. *)
let report_progress (ctx : t) ~(progress : float) ?total ?message () :
    Progress.t =
  Progress.create ~progress ?total ?message ?request_id:ctx.request_id ()

(** {1 Resource and Prompt Delegation} *)

(** List all resources from the server *)
let list_resources (ctx : t) : Yojson.Safe.t list Deferred.t =
  match ctx.list_resources_fn with
  | None ->
    failwith
      "Cannot list resources: no server reference. Context must be created \
       with server delegation."
  | Some fn -> fn ()

(** List all prompts from the server *)
let list_prompts (ctx : t) : Yojson.Safe.t list Deferred.t =
  match ctx.list_prompts_fn with
  | None ->
    failwith
      "Cannot list prompts: no server reference. Context must be created with \
       server delegation."
  | Some fn -> fn ()

(** Read a resource from the server *)
let read_resource (ctx : t) ~(uri : string) : string Deferred.t =
  match ctx.read_resource_fn with
  | None ->
    failwith
      "Cannot read resource: no server reference. Context must be created with \
       server delegation."
  | Some fn -> fn ~uri

(** Get a prompt from the server *)
let get_prompt (ctx : t) ~(name : string) ~(arguments : Yojson.Safe.t) :
    Yojson.Safe.t Deferred.t =
  match ctx.get_prompt_fn with
  | None ->
    failwith
      "Cannot get prompt: no server reference. Context must be created with \
       server delegation."
  | Some fn -> fn ~name ~arguments

(** {1 HTTP Request Access} *)

(** Get the current HTTP request if running in HTTP transport mode. Returns None
    when running via STDIO or if no HTTP context is active. *)
let get_http_request (_ctx : t) : Http.Request.t option =
  Http.get_current_request ()

(** {1 Type Aliases} *)

type 'a with_context = t -> 'a
