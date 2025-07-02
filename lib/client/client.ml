open Core
open Async

(** Exception raised when a tool call fails *)
exception Tool_error of string

(** Exception raised when server session is closed *)
exception Closed_resource_error of string

(** Exception raised when initialization fails *)
exception Init_error of string

(** Client type parameterized by transport type *)
type 'transport t = {
  transport : 'transport;
  mutable session : Types.client_session option;
  mutable initialize_result : Types.initialize_result option;
  session_kwargs : Types.session_kwargs;
  init_timeout : Time.Span.t option;
  progress_handler : Progress.handler;
  mutable nesting_counter : int;
  context_lock : Mutex.t;
  mutable session_task : unit Deferred.t option;
  ready_event : unit Ivar.t;
  stop_event : unit Ivar.t;
  monitor : Monitor.t;
}

(** Session runner that manages the lifecycle of a session *)
let session_runner t session =
  let rec run () =
    match%bind 
      Monitor.try_with ~extract_exn:true
        (fun () -> 
          let%bind () = Clock.after (Time.Span.of_sec 1.0) in
          if Ivar.is_full t.stop_event then
            return `Stop
          else
            return `Continue)
    with
    | Ok `Stop -> return ()
    | Ok `Continue -> run ()
    | Error (Closed_resource_error _ as e) ->
        t.session <- None;
        t.initialize_result <- None;
        raise e
    | Error e ->
        Log.error "Session error: %s" (Exn.to_string e);
        t.session <- None;
        t.initialize_result <- None;
        run ()
  in
  run ()

let create
    ?roots
    ?sampling_handler
    ?log_handler
    ?message_handler
    ?progress_handler
    ?timeout
    ?init_timeout
    ?client_info
    ?auth
    transport =
  let progress_handler = Option.value progress_handler ~default:Progress.default_handler in
  let log_handler = Option.value log_handler ~default:Logging.default_handler in
  let init_timeout = match init_timeout with
    | Some t -> Some t
    | None -> Some (Time.Span.of_sec Settings.client_init_timeout)
  in
  let session_kwargs = {
    Types.sampling_callback = Option.map sampling_handler ~f:Sampling.create_callback;
    list_roots_callback = Option.map roots ~f:Roots.create_callback;
    logging_callback = Logging.create_callback log_handler;
    message_handler;
    read_timeout = timeout;
    client_info;
  } in
  Option.iter auth ~f:(fun a -> Transports.set_auth transport a);
  let monitor = Monitor.create ~name:"client_monitor" () in
  Monitor.handle_errors monitor
    ~rest:(fun exn ->
      match exn with
      | Tool_error msg -> Log.error "Tool error: %s" msg
      | Closed_resource_error msg -> Log.error "Closed resource: %s" msg
      | Init_error msg -> Log.error "Init error: %s" msg
      | _ -> Log.error "Unexpected error: %s" (Exn.to_string exn));
  {
    transport;
    session = None;
    initialize_result = None;
    session_kwargs;
    init_timeout;
    progress_handler;
    nesting_counter = 0;
    context_lock = Mutex.create ();
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

let set_roots t roots =
  t.session_kwargs <- { t.session_kwargs with
    list_roots_callback = Some (Roots.create_callback roots)
  }

let set_sampling_callback t handler =
  t.session_kwargs <- { t.session_kwargs with
    sampling_callback = Some (Sampling.create_callback handler)
  }

let is_connected t = Option.is_some t.session

let with_error_handling t f =
  Monitor.try_with ~monitor:t.monitor
    ~extract_exn:true
    ~rest:(function
      | Closed_resource_error _ as e -> raise e
      | Init_error _ as e -> raise e
      | Tool_error _ as e -> raise e
      | exn -> raise (Tool_error (Exn.to_string exn)))
    f

let connect t =
  with_error_handling t (fun () ->
    Mutex.critical_section t.context_lock ~f:(fun () ->
      let need_to_start = match t.session_task with
        | None -> true
        | Some task -> Deferred.is_determined task
      in
      if need_to_start then begin
        t.stop_event <- Ivar.create ();
        t.ready_event <- Ivar.create ();
        t.session_task <- Some (
          let%bind session = Transports.connect_session t.transport t.session_kwargs in
          t.session <- Some session;
          match t.init_timeout with
          | Some timeout ->
            let%bind () = Clock.with_timeout timeout
              (let%bind result = Types.initialize session in
               t.initialize_result <- Some result;
               Ivar.fill t.ready_event ();
               return ())
            in
            begin match%map Ivar.read t.stop_event with
            | `Timeout -> raise (Init_error "Failed to initialize server session")
            | `Result () -> 
                let%bind () = session_runner t session in
                return ()
            end
          | None ->
            let%bind result = Types.initialize session in
            t.initialize_result <- Some result;
            Ivar.fill t.ready_event ();
            let%bind () = session_runner t session in
            Ivar.read t.stop_event
        )
      end;
      let%bind () = Ivar.read t.ready_event in
      t.nesting_counter <- t.nesting_counter + 1;
      return ()
    ))

let disconnect ?(force=false) t =
  Mutex.critical_section t.context_lock ~f:(fun () ->
    if force then
      t.nesting_counter <- 0
    else
      t.nesting_counter <- max 0 (t.nesting_counter - 1);
    
    if t.nesting_counter > 0 then
      return ()
    else begin
      match t.session_task with
      | None -> return ()
      | Some task ->
        Ivar.fill t.stop_event ();
        t.session_task <- None;
        let%bind () = task in
        t.stop_event <- Ivar.create ();
        t.ready_event <- Ivar.create ();
        t.session <- None;
        t.initialize_result <- None;
        return ()
    end
  )

let close t =
  let%bind () = disconnect ~force:true t in
  Transports.close t.transport

let ping t =
  let%map result = Types.send_ping (session t) in
  match result with
  | Types.Empty_result -> true
  | _ -> false

let cancel t ~request_id ?reason () =
  let notification = Types.Client_notification (
    Types.Cancelled_notification {
      method_ = "notifications/cancelled";
      params = {
        request_id;
        reason;
      }
    }
  ) in
  Types.send_notification (session t) notification

let progress t ~progress_token ~progress ?total ?message () =
  Types.send_progress_notification (session t) progress_token progress total message

let set_logging_level t level =
  Types.set_logging_level (session t) level

let send_roots_list_changed t =
  Types.send_roots_list_changed (session t)

let list_resources_mcp t =
  Types.list_resources (session t)

let list_resources t =
  let%map result = list_resources_mcp t in
  result.resources

let list_resource_templates_mcp t =
  Types.list_resource_templates (session t)

let list_resource_templates t =
  let%map result = list_resource_templates_mcp t in
  result.resource_templates

let read_resource_mcp t ~uri =
  let uri = Uri.of_string uri in
  Types.read_resource (session t) uri

let read_resource t ~uri =
  let%map result = read_resource_mcp t ~uri in
  result.contents

let list_prompts_mcp t =
  Types.list_prompts (session t)

let list_prompts t =
  let%map result = list_prompts_mcp t in
  result.prompts

(* Helper for serializing arguments *)
let serialize_arguments arguments =
  List.map arguments ~f:(fun (key, value) ->
    if String.is_valid_utf8 (Yojson.Safe.to_string value) then
      (key, value)
    else
      (key, `String (Yojson.Safe.to_string value))
  )

(* Resource Subscription Methods *)
let subscribe_resource t ~uri =
  let uri = Uri.of_string uri in
  Types.subscribe_resource (session t) uri

let unsubscribe_resource t ~uri =
  let uri = Uri.of_string uri in
  Types.unsubscribe_resource (session t) uri

(* Update existing methods to use serialize_arguments *)
let get_prompt_mcp t ~name ?arguments () =
  let serialized_arguments = Option.map arguments ~f:serialize_arguments in
  Types.get_prompt (session t) name serialized_arguments

let get_prompt t ~name ?arguments () =
  get_prompt_mcp t ~name ?arguments ()

let create_prompt_reference ~name ~arguments =
  let serialized_arguments = Option.map arguments ~f:serialize_arguments in
  Types.Prompt_reference { name; arguments = serialized_arguments }

let call_tool_mcp t ~name ?arguments ?timeout ?progress_handler () =
  let timeout = Option.map timeout ~f:(fun t ->
    if Time.Span.to_sec t > 0.0 then Some t else None
  ) in
  let progress_handler = Option.value progress_handler ~default:t.progress_handler in
  let arguments = Option.value_map arguments ~default:[] ~f:serialize_arguments in
  Types.call_tool (session t)
    ~name
    ~arguments
    ~timeout
    ~progress_callback:progress_handler

let call_tool t ~name ?arguments ?timeout ?progress_handler () =
  let%map result = call_tool_mcp t ~name ?arguments ?timeout ?progress_handler () in
  if result.is_error then
    match List.hd_exn result.content with
    | Types.Text_content { text; _ } -> raise (Tool_error text)
    | _ -> raise (Tool_error "Tool call failed")
  else
    result.content

let with_client t f =
  let%bind () = connect t in
  Monitor.protect ~finally:(fun () -> disconnect t ~force:false)
    (fun () -> with_error_handling t f)

let call_tool t ~name ?arguments ?timeout ?progress_handler () =
  let%map result = call_tool t ~name ?arguments ?timeout ?progress_handler () in
  if result.is_error then
    match List.hd_exn result.content with
    | Types.Text_content { text; _ } -> raise (Tool_error text)
    | _ -> raise (Tool_error "Tool call failed")
  else
    result.content

(* Resource Reference Helpers *)
let create_resource_reference ~uri =
  Types.Resource_reference { uri = Uri.of_string uri }

let complete_mcp t ~ref ~argument =
  Types.complete (session t) ref argument

let complete t ~ref ~argument =
  let%map result = complete_mcp t ~ref ~argument in
  result.completion

let list_tools_mcp t =
  Types.list_tools (session t)

let list_tools t =
  let%map result = list_tools_mcp t in
  result.tools

let complete_resource t ~uri ~argument =
  let ref = create_resource_reference ~uri in
  complete t ~ref ~argument

let complete_prompt t ~name ?arguments ~argument () =
  let ref = create_prompt_reference ~name ~arguments in
  complete t ~ref ~argument 