(** MCP Server Module

    This module provides a framework for creating an MCP (Model Context
    Protocol) server. It allows you to easily define and handle various types of
    requests and notifications in an asynchronous manner.

    Example usage:
    {[
      let server = Server.create ~name:"your_server_name" () in

      (* Define request handlers *)
      let handle_list_prompts _ctx =
        (* Implementation *)
        Lwt.return prompts
      in
      Server.register_list_prompts server handle_list_prompts;

      let handle_get_prompt ctx ~name ?arguments =
        (* Implementation *)
        Lwt.return prompt_result
      in
      Server.register_get_prompt server handle_get_prompt;

      (* Run the server *)
      let main () =
        Stdio_server.with_server (fun ~read_stream ~write_stream ->
            let init_options =
              Server.create_initialization_options server ~notification_options
                ~experimental_capabilities
            in
            Server.run server ~read_stream ~write_stream ~init_options ())
      in
      Lwt_main.run (main ())
    ]} *)

open Core
open Lwt.Syntax
open Mcp.Types
module Log = (val Logs.src_log (Logs.Src.create "mcp.server"))

(* Types for tool call results *)
type unstructured_content = content_block list
type structured_content = (string * Yojson.Safe.t) list
type combination_content = unstructured_content * structured_content

type tool_result =
  [ `Unstructured of unstructured_content
  | `Structured of structured_content
  | `Combined of combination_content ]

type notification_options = {
  prompts_changed : bool;
  resources_changed : bool;
  tools_changed : bool;
}

let default_notification_options =
  { prompts_changed = false; resources_changed = false; tools_changed = false }

type 'lifespan_context t = {
  name : string;
  version : string option;
  instructions : string option;
  request_handlers :
    (module Mcp.Shared.Session.Request_responder.S) String.Map.t;
  notification_handlers :
    (Mcp.Types.client_notification -> unit Lwt.t) String.Map.t;
  notification_options : notification_options;
  tool_cache : Mcp.Types.tool String.Map.t ref;
  lifespan :
    (unit -> 'lifespan_context Lwt.t) * ('lifespan_context -> unit Lwt.t);
  mutable current_context :
    (Mcp.Types.request_id * 'lifespan_context * Mcp.Server.Session.t) option;
}

let create ?(version = None) ?(instructions = None)
    ?(lifespan_start = fun () -> Lwt.return_unit)
    ?(lifespan_end = fun _ -> Lwt.return_unit) ~name () =
  let module Ping = struct
    let handle _req = Lwt.return (Mcp.Types.server_result_of_empty ())
  end in
  {
    name;
    version;
    instructions;
    request_handlers =
      String.Map.singleton "ping"
        (module Ping : Mcp.Shared.Session.Request_responder.S);
    notification_handlers = String.Map.empty;
    notification_options = default_notification_options;
    tool_cache = ref String.Map.empty;
    lifespan = (lifespan_start, lifespan_end);
    current_context = None;
  }

let get_request_context t =
  match t.current_context with
  | Some (request_id, lifespan_ctx, session) ->
    Some
      (Mcp.Shared.Context.create ~request_id ~session
         ~lifespan_context:lifespan_ctx ())
  | None -> None

let create_initialization_options t
    ?(notification_options = default_notification_options)
    ?(experimental_capabilities = String.Map.empty) () =
  let version =
    match t.version with
    | Some v -> v
    | None -> "unknown" (* TODO: Get from opam version *)
  in
  Mcp.Server.Models.create_initialization_options ~server_name:t.name
    ~server_version:version
    ~capabilities:
      (get_capabilities t notification_options experimental_capabilities)
    ?instructions:t.instructions ()

let get_capabilities t notification_options experimental_capabilities =
  let prompts_capability =
    if Map.mem t.request_handlers "list_prompts" then
      Some { list_changed = notification_options.prompts_changed }
    else None
  in
  let resources_capability =
    if Map.mem t.request_handlers "list_resources" then
      Some
        {
          subscribe = false;
          list_changed = notification_options.resources_changed;
        }
    else None
  in
  let tools_capability =
    if Map.mem t.request_handlers "list_tools" then
      Some { list_changed = notification_options.tools_changed }
    else None
  in
  let logging_capability =
    if Map.mem t.request_handlers "set_level" then Some { set_level = true }
    else None
  in
  {
    prompts = prompts_capability;
    resources = resources_capability;
    tools = tools_capability;
    logging = logging_capability;
    experimental = experimental_capabilities;
  }

(* Handler registration functions *)

let register_list_prompts t handler =
  Log.debug (fun m -> m "Registering handler for PromptListRequest");
  let module H = struct
    let handle _req =
      let* prompts = handler () in
      Lwt.return (Mcp.Types.server_result_of_list_prompts ~prompts ())
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"list_prompts"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_get_prompt t handler =
  Log.debug (fun m -> m "Registering handler for GetPromptRequest");
  let module H = struct
    let handle req =
      match req with
      | Get_prompt params ->
        let* result =
          handler ~name:params.name ?arguments:params.arguments ()
        in
        Lwt.return (Mcp.Types.server_result_of_get_prompt result)
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"get_prompt"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

(* More handler registration functions following the same pattern... *)

let make_error_result error_message =
  Mcp.Types.server_result_of_call_tool
    ~content:[ Mcp.Types.text_content_of_text error_message ]
    ~is_error:true ()

let get_cached_tool_definition t tool_name =
  match Map.find !(t.tool_cache) tool_name with
  | Some tool -> Lwt.return_some tool
  | None -> (
    match Map.find t.request_handlers "list_tools" with
    | Some (module H : Mcp.Shared.Session.Request_responder.S) -> (
      Log.debug (fun m ->
          m "Tool cache miss for %s, refreshing cache" tool_name);
      let* result =
        H.handle
          (List_tools_request { list_tools_request_params = { filter = None } })
      in
      match result with
      | List_tools_result { tools } ->
        t.tool_cache :=
          List.fold tools ~init:String.Map.empty ~f:(fun acc tool ->
              Map.set acc ~key:tool.name ~data:tool);
        Lwt.return (Map.find !(t.tool_cache) tool_name)
      | _ -> Lwt.return_none)
    | None ->
      Log.warning (fun m ->
          m "Tool '%s' not listed, no validation will be performed" tool_name);
      Lwt.return_none)

let process_tool_result = function
  | `Unstructured content ->
    Mcp.Types.server_result_of_call_tool ~content ~is_error:false ()
  | `Structured content ->
    let json_str = Yojson.Safe.to_string (`Assoc content) in
    Mcp.Types.server_result_of_call_tool
      ~content:[ Mcp.Types.text_content_of_text json_str ]
      ~structured_content:(`Assoc content) ~is_error:false ()
  | `Combined (content, structured) ->
    Mcp.Types.server_result_of_call_tool ~content
      ~structured_content:(`Assoc structured) ~is_error:false ()

let register_call_tool ?(validate_input = true) t handler =
  Log.debug (fun m -> m "Registering handler for CallToolRequest");
  let module H = struct
    let handle req =
      match req with
      | Call_tool params ->
        let tool_name = params.name in
        let arguments =
          Option.value params.arguments ~default:String.Map.empty
        in

        let* tool = get_cached_tool_definition t tool_name in

        (* Input validation *)
        let* () =
          if validate_input then
            match tool with
            | Some tool -> (
              try
                Jsonschema.validate tool.input_schema arguments;
                Lwt.return_unit
              with Jsonschema.Validation_error msg ->
                Lwt.return
                  (make_error_result ("Input validation error: " ^ msg)))
            | None -> Lwt.return_unit
          else Lwt.return_unit
        in

        (* Tool call *)
        let* result =
          try%lwt handler ~name:tool_name ~arguments ()
          with e -> Lwt.return (make_error_result (Exn.to_string e))
        in

        (* Output validation *)
        let* () =
          match tool with
          | Some tool when Option.is_some tool.output_schema -> (
            match result with
            | `Structured content | `Combined (_, content) -> (
              try
                Jsonschema.validate
                  (Option.value_exn tool.output_schema)
                  (`Assoc content);
                Lwt.return_unit
              with Jsonschema.Validation_error msg ->
                Lwt.return
                  (make_error_result ("Output validation error: " ^ msg)))
            | `Unstructured _ ->
              Lwt.return
                (make_error_result
                   "Output validation error: outputSchema defined but no \
                    structured output returned"))
          | _ -> Lwt.return_unit
        in

        Lwt.return (process_tool_result result)
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"call_tool"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_progress_notification t handler =
  Log.debug (fun m -> m "Registering handler for ProgressNotification");
  let handle notify =
    match notify with
    | Progress params ->
      handler ~progress_token:params.progress_token ~progress:params.progress
        ?total:params.total ?message:params.message ()
    | _ -> Lwt.fail_with "Invalid notification type"
  in
  t.notification_handlers <-
    Map.set t.notification_handlers ~key:"progress" ~data:handle

let run t ~read_stream ~write_stream ~init_options ?(raise_exceptions = false)
    ?(stateless = false) () =
  let lifespan_start, lifespan_end = t.lifespan in

  let* lifespan_ctx = lifespan_start () in

  let rec handle_message session msg =
    match msg with
    | `Request responder ->
      let request_id =
        responder.Mcp.Shared.Session.Request_responder.request_id
      in
      t.current_context <- Some (request_id, lifespan_ctx, session);

      let* result =
        try%lwt
          let req = responder.Mcp.Shared.Session.Request_responder.request in
          match Map.find t.request_handlers (string_of_request_type req) with
          | Some (module H : Mcp.Shared.Session.Request_responder.S) ->
            H.handle req
          | None -> Lwt.return (Mcp.Types.error_data_of_method_not_found ())
        with e ->
          if raise_exceptions then Lwt.fail e
          else Lwt.return (Mcp.Types.error_data_of_error (Exn.to_string e))
      in

      let* () = Mcp.Shared.Session.Request_responder.respond responder result in
      t.current_context <- None;
      Lwt.return_unit
    | `Notification notify -> (
      try%lwt
        match
          Map.find t.notification_handlers (string_of_notification_type notify)
        with
        | Some handler -> handler notify
        | None -> Lwt.return_unit
      with e ->
        Log.err (fun m ->
            m "Uncaught exception in notification handler: %s" (Exn.to_string e));
        Lwt.return_unit)
    | `Error e ->
      Log.err (fun m -> m "Error in message handler: %s" (Exn.to_string e));
      Lwt.return_unit
  in

  let* session =
    Mcp.Server.Session.create ~read_stream ~write_stream ~init_options
      ~stateless ()
  in

  let rec message_loop () =
    match%lwt Lwt_stream.get session.incoming_messages with
    | Some msg ->
      let* () = handle_message session msg in
      message_loop ()
    | None ->
      (* Clean up when message loop ends *)
      lifespan_end lifespan_ctx
  in

  message_loop ()

(* Helper functions for request/notification type conversion *)
let string_of_request_type = function
  | Initialize _ -> "initialize"
  | List_prompts _ -> "list_prompts"
  | Get_prompt _ -> "get_prompt"
  | List_resources _ -> "list_resources"
  | List_resource_templates _ -> "list_resource_templates"
  | Read_resource _ -> "read_resource"
  | Set_level _ -> "set_level"
  | Subscribe _ -> "subscribe"
  | Unsubscribe _ -> "unsubscribe"
  | List_tools _ -> "list_tools"
  | Call_tool _ -> "call_tool"
  | Complete _ -> "complete"
  | Ping _ -> "ping"

let string_of_notification_type = function
  | Initialized -> "initialized"
  | Progress _ -> "progress"
  | Resource_updated _ -> "resource_updated"
  | Resource_list_changed -> "resource_list_changed"
  | Tool_list_changed -> "tool_list_changed"
  | Prompt_list_changed -> "prompt_list_changed"
  | Message _ -> "message"

(* Additional handler registration functions *)

let register_list_resources t handler =
  Log.debug (fun m -> m "Registering handler for ListResourcesRequest");
  let module H = struct
    let handle _req =
      let* resources = handler () in
      Lwt.return (Mcp.Types.server_result_of_list_resources ~resources ())
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"list_resources"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_list_resource_templates t handler =
  Log.debug (fun m -> m "Registering handler for ListResourceTemplatesRequest");
  let module H = struct
    let handle _req =
      let* templates = handler () in
      Lwt.return
        (Mcp.Types.server_result_of_list_resource_templates
           ~resource_templates:templates ())
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"list_resource_templates"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_read_resource t handler =
  Log.debug (fun m -> m "Registering handler for ReadResourceRequest");
  let module H = struct
    let handle req =
      match req with
      | Read_resource params ->
        let* result = handler params.uri in
        let create_content data mime_type =
          match data with
          | `Text text ->
            Mcp.Types.text_resource_contents_of_text ~uri:params.uri ~text
              ~mime_type:(Option.value mime_type ~default:"text/plain")
              ()
          | `Blob blob ->
            Mcp.Types.blob_resource_contents_of_blob ~uri:params.uri
              ~blob:(Base64.encode_string blob)
              ~mime_type:
                (Option.value mime_type ~default:"application/octet-stream")
              ()
        in
        let contents =
          match result with
          | `Single (data, mime_type) -> [ create_content data mime_type ]
          | `Multiple contents ->
            List.map contents ~f:(fun content ->
                create_content content.Helper_types.content
                  content.Helper_types.mime_type)
        in
        Lwt.return (Mcp.Types.server_result_of_read_resource ~contents ())
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"read_resource"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_set_logging_level t handler =
  Log.debug (fun m -> m "Registering handler for SetLevelRequest");
  let module H = struct
    let handle req =
      match req with
      | Set_level params ->
        let* () = handler params.level in
        Lwt.return (Mcp.Types.server_result_of_empty ())
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"set_level"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_subscribe_resource t handler =
  Log.debug (fun m -> m "Registering handler for SubscribeRequest");
  let module H = struct
    let handle req =
      match req with
      | Subscribe params ->
        let* () = handler params.uri in
        Lwt.return (Mcp.Types.server_result_of_empty ())
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"subscribe"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_unsubscribe_resource t handler =
  Log.debug (fun m -> m "Registering handler for UnsubscribeRequest");
  let module H = struct
    let handle req =
      match req with
      | Unsubscribe params ->
        let* () = handler params.uri in
        Lwt.return (Mcp.Types.server_result_of_empty ())
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"unsubscribe"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)

let register_completion t handler =
  Log.debug (fun m -> m "Registering handler for CompleteRequest");
  let module H = struct
    let handle req =
      match req with
      | Complete params ->
        let* completion = handler params.ref params.argument params.context in
        let completion =
          Option.value completion
            ~default:{ values = []; total = None; has_more = None }
        in
        Lwt.return (Mcp.Types.server_result_of_complete ~completion ())
      | _ -> Lwt.fail_with "Invalid request type"
  end in
  t.request_handlers <-
    Map.set t.request_handlers ~key:"complete"
      ~data:(module H : Mcp.Shared.Session.Request_responder.S)
