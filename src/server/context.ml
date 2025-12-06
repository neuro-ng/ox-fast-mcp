(** Execution Context for OxFastMCP

    Centralizes context management for tool, resource, and prompt execution.
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 8 (lines 665-687) See: Task 8.1 -
    Centralized Context Module *)

open! Core
open! Async

(** {1 Context Type} *)

type t = {
  request_id : string option;
  client_id : string option;
  session_data : (string, Yojson.Safe.t) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
}

(** {1 Context Creation} *)

let create ?(request_id : string option) ?(client_id : string option) () : t =
  {
    request_id;
    client_id;
    session_data = Hashtbl.create (module String);
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
  }

let create_with_session ?(request_id : string option)
    ?(client_id : string option)
    ~(session_data : (string, Yojson.Safe.t) Hashtbl.t) () : t =
  {
    request_id;
    client_id;
    session_data;
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
  }

(** {1 Change Notifications} *)

let queue_tool_list_changed (ctx : t) : unit = ctx.tools_changed <- true
let queue_resource_list_changed (ctx : t) : unit = ctx.resources_changed <- true
let queue_prompt_list_changed (ctx : t) : unit = ctx.prompts_changed <- true

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
  ctx.prompts_changed <- false

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

let with_request_id (ctx : t) (request_id : string) : t =
  { ctx with request_id = Some request_id }

let with_client_id (ctx : t) (client_id : string) : t =
  { ctx with client_id = Some client_id }

(** {1 Type Aliases} *)

type 'a with_context = t -> 'a
