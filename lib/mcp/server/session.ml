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
open Lwt.Syntax
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
    Mcp.Shared.Session.Base_session.t;
  incoming_message_stream :
    ( [ `Request of
        (client_request, server_result) Mcp.Shared.Session.Request_responder.t
      | `Notification of client_notification
      | `Error of exn ],
      [ `Closed ] )
    Lwt_stream.t;
  mutable message_handler :
    t ->
    [ `Request of
      (client_request, server_result) Mcp.Shared.Session.Request_responder.t
    | `Notification of client_notification
    | `Error of exn ] ->
    unit Lwt.t;
}
[@@deriving sexp]

let create ~read_stream ~write_stream ~init_options ?(stateless = false) () =
  let stream, push = Lwt_stream.create () in
  let base_session =
    Mcp.Shared.Session.Base_session.create ~read_stream ~write_stream
      ~receive_request_type:client_request_type
      ~receive_notification_type:client_notification_type ()
  in
  {
    initialization_state = (if stateless then Initialized else Not_initialized);
    client_params = None;
    init_options;
    base_session;
    incoming_message_stream = stream;
    message_handler = (fun _ _ -> Lwt.return_unit);
  }

let check_client_capability t capability =
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
        not (cap_roots.list_changed && not client_roots.list_changed)))
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
        Map.for_all cap_exp ~f:(fun ~key ~data ->
            match Map.find client_exp key with
            | None -> false
            | Some client_data -> client_data = data)))

let send_log_message t ~level ~data ?logger ?related_request_id () =
  let notification =
    server_notification_of_logging_message ~level ~data ?logger ()
  in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ?related_request_id ()

let send_resource_updated t ~uri =
  let notification = server_notification_of_resource_updated ~uri () in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ()

let create_message t ~messages ~max_tokens ?system_prompt ?include_context
    ?temperature ?stop_sequences ?metadata ?model_preferences
    ?related_request_id () =
  let request =
    server_request_of_create_message ~messages ~max_tokens ?system_prompt
      ?include_context ?temperature ?stop_sequences ?metadata ?model_preferences
      ()
  in
  let metadata =
    Option.map related_request_id ~f:(fun id ->
        Mcp.Shared.Message.Server
          { related_request_id = Some id; request_context = None })
  in
  Mcp.Shared.Session.Base_session.send_request t.base_session request ?metadata
    ()

let list_roots t =
  let request = server_request_of_list_roots () in
  Mcp.Shared.Session.Base_session.send_request t.base_session request ()

let elicit t ~message ~requested_schema ?related_request_id () =
  let request = server_request_of_elicit ~message ~requested_schema () in
  let metadata =
    Option.map related_request_id ~f:(fun id ->
        Mcp.Shared.Message.Server
          { related_request_id = Some id; request_context = None })
  in
  Mcp.Shared.Session.Base_session.send_request t.base_session request ?metadata
    ()

let send_ping t =
  let request = server_request_of_ping () in
  Mcp.Shared.Session.Base_session.send_request t.base_session request ()

let send_progress_notification t ~progress_token ~progress ?total ?message
    ?related_request_id () =
  let notification =
    server_notification_of_progress ~progress_token ~progress ?total ?message ()
  in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ?related_request_id ()

let send_resource_list_changed t =
  let notification = server_notification_of_resource_list_changed () in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ()

let send_tool_list_changed t =
  let notification = server_notification_of_tool_list_changed () in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ()

let send_prompt_list_changed t =
  let notification = server_notification_of_prompt_list_changed () in
  Mcp.Shared.Session.Base_session.send_notification t.base_session notification
    ()

let incoming_messages t = t.incoming_message_stream

(** Handle an incoming request *)
let handle_request t responder =
  match responder.Mcp.Shared.Session.Request_responder.request with
  | Initialize params ->
    t.initialization_state <- Initializing;
    t.client_params <- Some params;
    let requested_version = params.protocol_version in
    let response =
      server_result_of_initialize
        ~protocol_version:
          (if
             List.mem Mcp.Shared.Version.supported_protocol_versions
               requested_version
           then requested_version
           else latest_protocol_version)
        ~capabilities:t.init_options.capabilities
        ~server_info:
          {
            name = t.init_options.server_name;
            version = t.init_options.server_version;
          }
        ?instructions:t.init_options.instructions ()
    in
    Mcp.Shared.Session.Request_responder.respond responder response
  | _ ->
    if t.initialization_state <> Initialized then
      Lwt.fail_with "Received request before initialization was complete"
    else t.message_handler t (`Request responder)

(** Handle an incoming notification *)
let handle_notification t notification =
  match notification with
  | Initialized ->
    t.initialization_state <- Initialized;
    t.message_handler t (`Notification notification)
  | _ ->
    if t.initialization_state <> Initialized then
      Lwt.fail_with "Received notification before initialization was complete"
    else t.message_handler t (`Notification notification)

(** Handle an incoming message *)
let handle_message t msg =
  match msg with
  | `Request responder -> handle_request t responder
  | `Notification notif -> handle_notification t notif
  | `Error exn ->
    let* () =
      Logs_lwt.err (fun m ->
          m "Error in message handler: %s" (Exn.to_string exn))
    in
    t.message_handler t (`Error exn)

(** Start the message handling loop *)
let start_message_loop t =
  let rec loop () =
    match%lwt Lwt_stream.get t.incoming_message_stream with
    | None -> Lwt.return_unit
    | Some msg ->
      let* () = handle_message t msg in
      loop ()
  in
  loop ()

(** Set the message handler *)
let set_message_handler t handler = t.message_handler <- handler

(** Initialize the session *)
let initialize t =
  if t.initialization_state = Not_initialized then
    let* () = start_message_loop t in
    Lwt.return_unit
  else Lwt.return_unit
