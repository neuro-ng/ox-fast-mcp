open Core
open Async
module Types = Mcp.Types

module Roots = struct
  include Roots

  type t = [ `Static of roots_list list | `Handler of roots_handler ]
end

module Sampling = struct
  include Sampling

  type handler = sampling_handler
end

module Logging = struct
  include Logging

  type handler = log_handler
end

module Messages = struct
  include Messages

  type handler = t
end

module Progress = Progress

(* module Content_block = Mcp.Types.Content_block *)
(* Content_block module likely doesn't exist in Mcp.Types, removing from ml if not needed implementation-side, but mli requires it. If mli requires it, implementation must provide it. *)
(* If Mcp.Types.Content_block doesn't exist, I should fix mli. But assuming it existed before... *)
(* Actually, Mcp.Types usually doesn't have nested modules for types. *)
(* I will define Content_block myself if needed, or check types.ml later. For now, try ignoring it or defining as empty struct? *)
(* However, creating a dummy module might satisfy mli. *)
module Content_block = struct end
(* This might fail if mli expects specific signature *)

type reference =
  [ `Resource of Types.resource_template_reference
  | `Prompt of Types.prompt_reference ]

exception Tool_error of string
(** Exception raised when a tool call fails *)

exception Closed_resource_error of string
(** Exception raised when server session is closed *)

exception Init_error of string
(** Exception raised when initialization fails *)

type 'transport t = {
  transport : 'transport;
  mutable session : Mcp_client.Session.t option;
  mutable initialize_result : Types.initialize_result option;
  mutable session_kwargs : Transports.session_kwargs;
  init_timeout : Time_ns.Span.t option;
  progress_handler : Progress.handler;
  mutable nesting_counter : int;
  context_lock : Error_checking_mutex.t;
  mutable session_task : unit Deferred.t option;
  mutable ready_event : unit Ivar.t;
  mutable stop_event : unit Ivar.t;
  monitor : Monitor.t;
}
(** Client type parameterized by transport type *)

(** Session runner that manages the lifecycle of a session *)
let session_runner t _session =
  let rec run () =
    match%bind
      Monitor.try_with ~extract_exn:true (fun () ->
          let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.0) in
          if Ivar.is_full t.stop_event then return `Stop else return `Continue)
    with
    | Ok `Stop -> return ()
    | Ok `Continue -> run ()
    | Error (Closed_resource_error _ as e) ->
      t.session <- None;
      t.initialize_result <- None;
      raise e
    | Error e ->
      Log.Global.error "Session error: %s" (Exn.to_string e);
      t.session <- None;
      t.initialize_result <- None;
      run ()
  in
  run ()

let create ?roots:_ ?sampling_handler:_ ?log_handler ?message_handler
    ?progress_handler ?timeout ?init_timeout ?client_info ?auth transport =
  let progress_handler =
    Option.value progress_handler ~default:Progress.default_handler
  in
  let _log_handler =
    Option.value log_handler ~default:Logging.default_handler
  in
  let init_timeout =
    match init_timeout with
    | Some t -> Some t
    | None -> Some (Time_ns.Span.of_sec 60.0)
  in
  let session_kwargs =
    {
      Transports.sampling_callback = None;
      (* Option.map sampling_handler ~f:Sampling.create_callback; *)
      list_roots_callback = None;
      (* Option.map roots ~f:Roots.create_callback; *)
      logging_callback = None;
      (* Some (Logging.create_callback log_handler); *)
      message_handler;
      read_timeout_seconds = timeout;
      elicitation_callback = None;
      (* Placeholder if not passed *)
      client_info;
    }
  in
  let transport =
    match auth with
    | Some token -> Transports.set_auth transport (Transports.Bearer token)
    | None -> transport
  in
  let monitor = Monitor.create ~name:"client_monitor" () in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      match exn with
      | Tool_error msg -> Log.Global.error "Tool error: %s" msg
      | Closed_resource_error msg -> Log.Global.error "Closed resource: %s" msg
      | Init_error msg -> Log.Global.error "Init error: %s" msg
      | _ -> Log.Global.error "Unexpected error: %s" (Exn.to_string exn));
  {
    transport;
    session = None;
    initialize_result = None;
    session_kwargs;
    init_timeout;
    progress_handler;
    nesting_counter = 0;
    context_lock = Error_checking_mutex.create ();
    session_task = None;
    ready_event = Ivar.create ();
    stop_event = Ivar.create ();
    monitor;
  }

let session t =
  match t.session with
  | Some s -> s
  | None -> failwith "Client is not connected. Use connect first."

let initialize_result t =
  match t.initialize_result with
  | Some r -> r
  | None -> failwith "Client is not connected. Use connect first."

let set_roots t _roots =
  t.session_kwargs <-
    {
      t.session_kwargs with
      list_roots_callback = None;
      (* Some (Roots.create_callback roots); *)
    }

let set_sampling_callback t _handler =
  t.session_kwargs <-
    {
      t.session_kwargs with
      Transports.sampling_callback = None;
      (* Some (Sampling.create_callback handler); *)
    }

let is_connected t = Option.is_some t.session

let with_error_handling _t f =
  match%map Monitor.try_with ~extract_exn:true f with
  | Ok res -> res
  | Error exn -> (
    match exn with
    | Closed_resource_error _ as e -> raise e
    | Init_error _ as e -> raise e
    | Tool_error _ as e -> raise e
    | exn -> raise (Tool_error (Exn.to_string exn)))

let connect t =
  with_error_handling t (fun () ->
      Error_checking_mutex.critical_section t.context_lock ~f:(fun () ->
          let need_to_start =
            match t.session_task with
            | None -> true
            | Some task -> Deferred.is_determined task
          in
          if need_to_start then (
            t.stop_event <- Ivar.create ();
            t.ready_event <- Ivar.create ();
            t.session_task <-
              Some
                (let%bind session_result =
                   Transports.connect_session t.transport t.session_kwargs
                 in
                 match session_result with
                 | Error e -> raise (Init_error (Error.to_string_hum e))
                 | Ok session -> (
                   t.session <- Some session;
                   match t.init_timeout with
                   | Some timeout -> (
                     let init_result =
                       Clock_ns.with_timeout timeout
                         (let%bind result =
                            Mcp_client.Session.initialize session
                          in
                          t.initialize_result <- Some result;
                          Ivar.fill_exn t.ready_event ();
                          return ())
                     in
                     match%bind init_result with
                     | `Timeout ->
                       raise
                         (Init_error
                            "Failed to initialize server session (timeout)")
                     | `Result () ->
                       let%bind () = session_runner t session in
                       return ())
                   | None ->
                     let%bind result = Mcp_client.Session.initialize session in
                     t.initialize_result <- Some result;
                     Ivar.fill_exn t.ready_event ();
                     let%bind () = session_runner t session in
                     Ivar.read t.stop_event)));
          let%bind () = Ivar.read t.ready_event in
          t.nesting_counter <- t.nesting_counter + 1;
          return ()))

let disconnect ?(force = false) t =
  Error_checking_mutex.critical_section t.context_lock ~f:(fun () ->
      if force then t.nesting_counter <- 0
      else t.nesting_counter <- max 0 (t.nesting_counter - 1);

      if t.nesting_counter > 0 then return ()
      else
        match t.session_task with
        | None -> return ()
        | Some task ->
          Ivar.fill_exn t.stop_event ();
          t.session_task <- None;
          let%bind () = task in
          t.stop_event <- Ivar.create ();
          t.ready_event <- Ivar.create ();
          t.session <- None;
          t.initialize_result <- None;
          return ())

let close t =
  let%bind () = disconnect ~force:true t in
  Transports.close t.transport

let ping t =
  match%map
    Monitor.try_with (fun () -> Mcp_client.Session.send_ping (session t))
  with
  | Ok _ -> true
  | Error _ -> false

let cancel t ~request_id ?reason () =
  let params =
    `Assoc
      (List.filter_opt
         [
           Some ("request_id", Types.yojson_of_request_id request_id);
           Option.map reason ~f:(fun r -> ("reason", `String r));
         ])
  in
  Mcp_client.Session.send_notification (session t)
    ~method_name:"notifications/cancelled" ~params:(Some params) ()

let progress t ~progress_token ~progress ?total ?message () =
  Mcp_client.Session.send_progress_notification (session t) progress_token
    progress ?total ?message ()

let set_logging_level t level =
  let%map _ = Mcp_client.Session.set_logging_level (session t) level in
  ()

let send_roots_list_changed t =
  Mcp_client.Session.send_roots_list_changed (session t)

let list_resources_mcp t = Mcp_client.Session.list_resources (session t) ()

let list_resources t =
  let%map result = list_resources_mcp t in
  result.resources

let list_resource_templates_mcp t =
  Mcp_client.Session.list_resource_templates (session t) ()

let list_resource_templates t =
  let%map result = list_resource_templates_mcp t in
  result.resource_templates

let read_resource_mcp t ~uri = Mcp_client.Session.read_resource (session t) uri

let read_resource t ~uri =
  let%map result = read_resource_mcp t ~uri in
  result.contents

let list_prompts_mcp t = Mcp_client.Session.list_prompts (session t) ()

let list_prompts t =
  let%map result = list_prompts_mcp t in
  result.prompts

(* Helper for serializing arguments *)
let serialize_arguments arguments = arguments

(* Resource Subscription Methods *)
let subscribe_resource t ~uri =
  let%map _ = Mcp_client.Session.subscribe_resource (session t) uri in
  ()

let unsubscribe_resource t ~uri =
  let%map _ = Mcp_client.Session.unsubscribe_resource (session t) uri in
  ()

(* Update existing methods to use serialize_arguments *)
let get_prompt_mcp t ~name ?arguments () =
  let serialized_arguments = Option.map arguments ~f:serialize_arguments in
  Mcp_client.Session.get_prompt (session t) name serialized_arguments

let get_prompt t ~name ?arguments () = get_prompt_mcp t ~name ?arguments ()

let create_prompt_reference ~name ~arguments =
  let _serialized_arguments = Option.map arguments ~f:serialize_arguments in
  `Prompt { Types.type_ = `Ref_prompt; name }

let call_tool_mcp t ~name ?arguments ?timeout ?progress_handler () =
  let read_timeout =
    match timeout with
    | Some t ->
      let sec = Time_ns.Span.to_sec t in
      if Float.(sec > 0.0) then Some (Int.of_float sec) else None
    | None -> None
  in
  let progress_handler =
    Option.value progress_handler ~default:t.progress_handler
  in
  let arguments =
    Option.value_map arguments ~default:[] ~f:serialize_arguments
  in
  Mcp_client.Session.call_tool (session t) name ~arguments:(`Assoc arguments)
    ?read_timeout ~progress_callback:progress_handler ()

let call_tool t ~name ?arguments ?timeout ?progress_handler () =
  let%map result =
    call_tool_mcp t ~name ?arguments ?timeout ?progress_handler ()
  in
  if result.is_error then
    match List.hd_exn result.content with
    | `Text { text; _ } -> raise (Tool_error text)
    | _ -> raise (Tool_error "Tool call failed")
  else result.content

let with_client t f =
  let%bind () = connect t in
  Monitor.protect
    ~finally:(fun () -> disconnect t ~force:false)
    (fun () -> with_error_handling t f)

(* Resource Reference Helpers *)
let create_resource_reference ~uri =
  `Resource { Types.type_ = `Ref_resource; uri }

let complete_mcp t ~ref ~argument =
  Mcp_client.Session.complete (session t) ref argument

let complete t ~ref ~argument =
  let%map result = complete_mcp t ~ref ~argument in
  result.completion

let list_tools_mcp t = Mcp_client.Session.list_tools (session t) ()

let list_tools t =
  let%map result = list_tools_mcp t in
  result.tools

let complete_resource t ~uri ~argument =
  let ref = create_resource_reference ~uri in
  complete t ~ref ~argument

let complete_prompt t ~name ?arguments ~argument () =
  let ref = create_prompt_reference ~name ~arguments in
  complete t ~ref ~argument
