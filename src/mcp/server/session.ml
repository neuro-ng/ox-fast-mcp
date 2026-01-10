(** ServerSession Module

    This module provides the ServerSession functionality for managing
    communication between the server and client in the MCP (Model Context
    Protocol) framework. It is most commonly used in MCP servers to interact
    with the client.

    Common usage pattern:
    {[
      let server = Server.create ~name in

      let handle_tool_call ctx arguments =
        (* Check client capabilities before proceeding *)
        if check_client_capability ctx.session
            { experimental = Some (String.Map.singleton "advanced_tools" `Assoc []) } then
          (* Perform advanced tool operations *)
          perform_advanced_tool_operation arguments
        else
          (* Fall back to basic tool operations *)
          perform_basic_tool_operation arguments

      let handle_list_prompts ctx =
        (* Access session for any necessary checks or operations *)
        match ctx.session.client_params with
        | Some params ->
          (* Customize prompts based on client initialization parameters *)
          generate_custom_prompts params
        | None -> default_prompts
    ]}

    The ServerSession module is typically used internally by the Server module
    and should not be instantiated directly by users of the MCP framework. *)

open Core
open Async
open Mcp.Types

type initialization_state = Not_initialized | Initializing | Initialized

type t = {
  mutable initialization_state : initialization_state;
  mutable client_params : initialize_request_params option;
  init_options : Models.initialization_options;
  base_session :
    ( server_request,
      server_notification,
      server_result,
      client_request,
      client_notification )
    Mcp_shared.Session.Base_session.t;
  incoming_message_stream :
    [ `Request of
      (client_request, server_result) Mcp_shared.Session.Request_responder.t
    | `Notification of client_notification
    | `Error of exn ]
    Pipe.Reader.t;
  mutable message_handler :
    t ->
    [ `Request of
      (client_request, server_result) Mcp_shared.Session.Request_responder.t
    | `Notification of client_notification
    | `Error of exn ] ->
    unit Deferred.t;
}

let create ~read_stream ~write_stream ~init_options ?(stateless = false) () =
  let stream_reader, _stream_writer = Pipe.create () in
  (* Create dummy values of proper types for type witnesses *)
  let dummy_request : client_request =
    `Ping { method_ = "ping"; params = None }
  in
  let dummy_notification : client_notification =
    `Initialized { method_ = "notifications/initialized"; params = None }
  in
  let base_session =
    Mcp_shared.Session.Base_session.create ~read_stream ~write_stream
      ~receive_request_type:dummy_request
      ~receive_notification_type:dummy_notification ()
  in
  {
    initialization_state = (if stateless then Initialized else Not_initialized);
    client_params = None;
    init_options;
    base_session;
    incoming_message_stream = stream_reader;
    message_handler = (fun _ _ -> return ());
  }

let check_client_capability t (capability : client_capabilities) =
  match t.client_params with
  | None -> false
  | Some params -> (
    let client_caps = params.capabilities in
    (* Check roots capability *)
    (match capability.roots with
    | None -> true
    | Some cap_roots -> (
      match client_caps.roots with
      | None -> false
      | Some client_roots ->
        (* Check if client supports list_changed if required *)
        let cap_requires_list_changed =
          Option.value cap_roots.list_changed ~default:false
        in
        let client_has_list_changed =
          Option.value client_roots.list_changed ~default:false
        in
        not (cap_requires_list_changed && not client_has_list_changed)))
    (* Check sampling capability *)
    && (match capability.sampling with
       | None -> true
       | Some _ -> Option.is_some client_caps.sampling)
    (* Check elicitation capability *)
    && (match capability.elicitation with
       | None -> true
       | Some _ -> Option.is_some client_caps.elicitation)
    &&
    (* Check experimental capabilities *)
    match capability.experimental with
    | None -> true
    | Some cap_exp -> (
      match client_caps.experimental with
      | None -> false
      | Some client_exp ->
        List.for_all cap_exp ~f:(fun (key, data) ->
            match List.Assoc.find client_exp ~equal:String.equal key with
            | None -> false
            | Some client_data -> Yojson.Safe.equal client_data data)))

let send_log_message t ~level ~data ?logger ?related_request_id () =
  let params = { level; logger; data; notification_params = { meta = None } } in
  let notification =
    `LoggingMessage { method_ = "notifications/message"; params }
  in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ?related_request_id ()

let send_resource_updated t ~uri =
  let uri_str = Uri.to_string uri in
  let params = { uri = uri_str; notification_params = { meta = None } } in
  let notification =
    `ResourceUpdated { method_ = "notifications/resources/updated"; params }
  in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ()

let create_message t ~messages ~max_tokens ?system_prompt ?include_context
    ?temperature ?stop_sequences ?metadata ?model_preferences
    ?related_request_id () =
  let params =
    {
      messages;
      max_tokens;
      system_prompt;
      include_context;
      temperature;
      stop_sequences;
      metadata;
      model_preferences;
      request_params = { meta = None };
    }
  in
  let request : server_request =
    `CreateMessage { method_ = "sampling/createMessage"; params }
  in
  let metadata =
    Option.map related_request_id ~f:(fun id ->
        Mcp_shared.Message.Server
          { related_request_id = Some id; request_context = None })
  in
  Mcp_shared.Session.Base_session.send_request t.base_session request ?metadata
    ()

let list_roots t =
  let request : server_request =
    `ListRoots { method_ = "roots/list"; params = None }
  in
  Mcp_shared.Session.Base_session.send_request t.base_session request ()

let elicit t ~message ~requested_schema ?related_request_id () =
  let params =
    { message; requested_schema; request_params = { meta = None } }
  in
  let request : server_request =
    `Elicit { method_ = "prompts/elicit"; params }
  in
  let metadata =
    Option.map related_request_id ~f:(fun id ->
        Mcp_shared.Message.Server
          { related_request_id = Some id; request_context = None })
  in
  Mcp_shared.Session.Base_session.send_request t.base_session request ?metadata
    ()

let send_ping t =
  let request : server_request = `Ping { method_ = "ping"; params = None } in
  Mcp_shared.Session.Base_session.send_request t.base_session request ()

let send_progress_notification t ~progress_token ~progress ?total ?message
    ?related_request_id () =
  let params =
    {
      progress_token;
      progress;
      total;
      message;
      notification_params = { meta = None };
    }
  in
  let notification = `Progress { method_ = "notifications/progress"; params } in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ?related_request_id ()

let send_resource_list_changed t =
  let notification =
    `ResourceListChanged
      { method_ = "notifications/resources/list_changed"; params = None }
  in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ()

let send_tool_list_changed t =
  let notification =
    `ToolListChanged
      { method_ = "notifications/tools/list_changed"; params = None }
  in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ()

let send_prompt_list_changed t =
  let notification =
    `PromptListChanged
      { method_ = "notifications/prompts/list_changed"; params = None }
  in
  Mcp_shared.Session.Base_session.send_notification t.base_session notification
    ()

let incoming_messages t = t.incoming_message_stream

(** Handle an incoming request *)
let handle_request t responder =
  match responder.Mcp_shared.Session.Request_responder.request with
  | `Initialize ({ params = init_params; _ } : initialize_request) ->
    t.initialization_state <- Initializing;
    t.client_params <- Some init_params;
    let requested_version = init_params.protocol_version in
    let response =
      `Initialize
        {
          protocol_version =
            (if
               List.mem Mcp_shared.Version.supported_protocol_versions
                 requested_version ~equal:String.equal
             then requested_version
             else latest_protocol_version);
          capabilities = t.init_options.capabilities;
          server_info =
            {
              version = t.init_options.server_version;
              website_url = None;
              icons = None;
              base_metadata =
                { name = t.init_options.server_name; title = None };
            };
          instructions = t.init_options.instructions;
          result = { meta = None };
        }
    in
    Mcp_shared.Session.Request_responder.respond responder response
  | _ -> (
    match t.initialization_state with
    | Initialized -> t.message_handler t (`Request responder)
    | _ -> raise (Failure "Received request before initialization was complete")
    )

(** Handle an incoming notification *)
let handle_notification t notification =
  match notification with
  | `Initialized _ ->
    t.initialization_state <- Initialized;
    t.message_handler t (`Notification notification)
  | _ -> (
    match t.initialization_state with
    | Initialized -> t.message_handler t (`Notification notification)
    | _ ->
      raise (Failure "Received notification before initialization was complete")
    )

(** Handle an incoming message *)
let handle_message t msg =
  match msg with
  | `Request responder -> handle_request t responder
  | `Notification notif -> handle_notification t notif
  | `Error exn ->
    Logs.err (fun m -> m "Error in message handler: %s" (Exn.to_string exn));
    t.message_handler t (`Error exn)

(** Start the message handling loop *)
let start_message_loop t =
  let rec loop () =
    let%bind msg = Pipe.read t.incoming_message_stream in
    match msg with
    | `Eof -> return ()
    | `Ok msg ->
      let%bind () = handle_message t msg in
      loop ()
  in
  loop ()

(** Set the message handler *)
let[@warning "-32"] set_message_handler t handler = t.message_handler <- handler

(** Initialize the session *)
let[@warning "-32"] initialize t =
  match t.initialization_state with
  | Not_initialized ->
    let%bind () = start_message_loop t in
    return ()
  | _ -> return ()
