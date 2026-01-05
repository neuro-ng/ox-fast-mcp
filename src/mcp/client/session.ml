open Core
open Async
open Mcp.Types
open Mcp_shared.Message

let default_client_info = { 
  version = "0.1.0";
  website_url = None;
  icons = None;
  base_metadata = { name = "mcp"; title = None };
}

type t = {
  read_stream : session_message Pipe.Reader.t;
  write_stream : session_message Pipe.Writer.t;
  read_timeout : Time_ns.Span.t option;
  sampling_callback : sampling_fn option;
  elicitation_callback : elicitation_fn option;
  list_roots_callback : list_roots_fn option;
  logging_callback : logging_fn;
  message_handler : message_handler;
  client_info : implementation;
  tool_output_schemas : (string, Yojson.Safe.t option) Hashtbl.t;
  mutable initialized : bool;
}

and request_context = {
  req_id : string;
  req_meta : Mcp_shared.Message.server_message_metadata option;
  session : t;
  lifespan_context : Yojson.Safe.t option;
}

and ('req, 'res) request_responder = {
  request_id : string;
  request_meta : Mcp_shared.Message.server_message_metadata option;
  request : 'req;
  respond : 'res -> unit Deferred.t;
}

and progress_fn = float -> float option -> string option -> unit Deferred.t

and sampling_fn =
  request_context ->
  create_message_request_params ->
  (create_message_result, error_data) Result.t Deferred.t

and elicitation_fn =
  request_context ->
  elicit_request_params ->
  (elicit_result, error_data) Result.t Deferred.t

and list_roots_fn =
  request_context -> (list_roots_result, error_data) Result.t Deferred.t

and logging_fn = logging_message_notification_params -> unit Deferred.t

and message_handler =
  [ `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn ] ->
  unit Deferred.t

let default_message_handler _ = return ()

let invalid_request_error = -32600

let default_sampling_callback _ctx _params =
  return
    (Error { code = invalid_request_error; message = "Sampling not supported"; data = None })

let default_elicitation_callback _ctx _params =
  return
    (Error { code = invalid_request_error; message = "Elicitation not supported"; data = None })

let default_list_roots_callback _ctx =
  return
    (Error { code = invalid_request_error; message = "List roots not supported"; data = None })

let default_logging_callback _params = return ()
let logger = Logs.Src.create "client" ~doc:"Client session logger"

module Log = (val Logs.src_log logger)

let create ?read_timeout ?sampling_callback ?elicitation_callback
    ?list_roots_callback ?logging_callback ?message_handler ?client_info () =
  Log.debug (fun m -> m "Creating new client session");
  let read_stream, write_stream = Pipe.create () in
  return
    {
      read_stream;
      write_stream;
      read_timeout;
      sampling_callback;
      elicitation_callback;
      list_roots_callback;
      logging_callback =
        Option.value ~default:default_logging_callback logging_callback;
      message_handler =
        Option.value ~default:default_message_handler message_handler;
      client_info = Option.value ~default:default_client_info client_info;
      tool_output_schemas = Hashtbl.create (module String);
      initialized = false;
    }

type error =
  [ `Timeout of string
  | `InvalidMessage of string
  | `RequestFailed of string
  | `ValidationError of string
  | `ProtocolError of string ]

let error_to_string = function
  | `Timeout msg -> Printf.sprintf "Timeout: %s" msg
  | `InvalidMessage msg -> Printf.sprintf "Invalid message: %s" msg
  | `RequestFailed msg -> Printf.sprintf "Request failed: %s" msg
  | `ValidationError msg -> Printf.sprintf "Validation error: %s" msg
  | `ProtocolError msg -> Printf.sprintf "Protocol error: %s" msg

let with_timeout timeout promise =
  match timeout with
  | None -> promise >>| Result.return
  | Some span ->
    Clock_ns.with_timeout span promise >>| function
    | `Result r -> Result.return r
    | `Timeout -> Error (`Timeout "Operation timed out")

let send_request t request expected_type =
  let request_id_str = Printf.sprintf "%f-%d" (Unix.gettimeofday ()) (Random.int 1000000) in
  let request_id = `String request_id_str in
  Log.debug (fun m -> m "Sending request %s" request_id_str);
  
  (* Serialize the request to JSON *)
  let params_json = request in (* request is already the params, needs to be serialized *)
  
  let jsonrpc_req = {
    Mcp.Types.jsonrpc = "2.0";
    id = request_id;
    method_ = ""; (* This will need to come from the request type *)
    params = Some params_json;
  } in
  
  let msg : Mcp_shared.Message.session_message = {
    message = `Request jsonrpc_req;
    metadata = None;
  } in
  
  let%bind () = Pipe.write t.write_stream msg in
  let read_promise =
    let%bind response = Pipe.read t.read_stream in
    match response with
    | `Ok msg -> (
      match msg.message with
      | `Response resp when Poly.equal resp.id request_id ->
        Log.debug (fun m ->
            m "Received successful response for request %s" request_id_str);
        return (Ok resp.result)
      | `Error err when Poly.equal err.id request_id ->
        Log.warn (fun m -> m "Request %s failed: %s" request_id_str err.error.message);
        return (Error (`RequestFailed err.error.message))
      | _ ->
        Log.warn (fun m ->
            m "Received response with mismatched ID for request %s" request_id_str);
        return (Error (`InvalidMessage "Mismatched response ID")))
    | `Eof ->
      Log.err (fun m ->
          m "Connection closed for request %s" request_id_str);
      return (Error (`ProtocolError "Connection closed"))
  in
  let%bind result = with_timeout t.read_timeout read_promise in
  match result with
  | Ok (Ok result) -> return result
  | Ok (Error err) ->
    Log.err (fun m ->
        m "Request %s failed: %s" request_id_str (error_to_string err));
    raise (Failure (error_to_string err))
  | Error err ->
    Log.err (fun m ->
        m "Request %s timed out: %s" request_id_str (error_to_string err));
    raise (Failure (error_to_string err))

let send_notification t notification =
  let jsonrpc_notif = {
    Mcp.Types.jsonrpc = "2.0";
    method_ = ""; (* Will need to extract from notification *)
    params = Some notification;
  } in
  
  let msg : Mcp_shared.Message.session_message = {
    message = `Notification jsonrpc_notif;
    metadata = None;
  } in
  Pipe.write_without_pushback t.write_stream msg;
  return ()

let initialize t =
  let sampling =
    match t.sampling_callback with
    | Some _ -> Some (`Assoc [("enabled", `Bool true)])
    | None -> None
  in
  let elicitation =
    match t.elicitation_callback with
    | Some _ -> Some (`Assoc [("enabled", `Bool true)])
    | None -> None
  in
  let roots_cap : roots_capability option =
    match t.list_roots_callback with
    | Some _ -> Some { list_changed = Some true }
    | None -> None
  in
  let request =
    {
      method_ = "initialize";
      params =
        {
          protocol_version = latest_protocol_version;
          capabilities = { 
            experimental = None;
            sampling = sampling;
            elicitation = elicitation;
            roots = roots_cap;
          };
          client_info = t.client_info;
          request_params = { Request_params.meta = None };
        };
    }
  in
  let%bind result = send_request t request initialize_result_of_yojson in
  if not (List.mem result.protocol_version supported_protocol_versions ~equal:(=)) then
    raise (Failure
      ("Unsupported protocol version from server: "
      ^ Int.to_string result.protocol_version))
  else
    let%bind () =
      send_notification t
        { method_ = "notifications/initialized"; params = None }
    in
    t.initialized <- true;
    return result

let send_ping t =
  let request = { method_ = "ping"; params = None } in
  send_request t request empty_result_of_yojson

let send_progress_notification t token progress ?total ?message () =
  let notification =
    {
      method_ = "notifications/progress";
      params = Some { progress_token = token; progress; total; message };
    }
  in
  send_notification t notification

let set_logging_level t level =
  let request = { method_ = "logging/setLevel"; params = { level } } in
  send_request t request empty_result_of_yojson

let list_resources t ?cursor () =
  let request =
    {
      method_ = "resources/list";
      params = Option.map (fun c -> { cursor = Some c }) cursor;
    }
  in
  send_request t request list_resources_result_of_yojson

let list_resource_templates t ?cursor () =
  let request =
    {
      method_ = "resources/templates/list";
      params = Option.map (fun c -> { cursor = Some c }) cursor;
    }
  in
  send_request t request list_resource_templates_result_of_yojson

let read_resource t uri =
  let request = { method_ = "resources/read"; params = { uri } } in
  send_request t request read_resource_result_of_yojson

let subscribe_resource t uri =
  let request = { method_ = "resources/subscribe"; params = { uri } } in
  send_request t request empty_result_of_yojson

let unsubscribe_resource t uri =
  let request = { method_ = "resources/unsubscribe"; params = { uri } } in
  send_request t request empty_result_of_yojson

let validate_tool_result t name result =
  let rec validate_with_refresh first_try =
    match Hashtbl.find t.tool_output_schemas name with
    | None when first_try ->
      Log.debug (fun m ->
          m "No schema found for tool %s, refreshing schema cache" name);
      let%bind _ = list_tools t () in
      validate_with_refresh false
    | None ->
      Log.warn (fun m ->
          m
            "Tool %s not listed by server, cannot validate any structured \
             content"
            name);
      return ()
    | Some None ->
      Log.debug (fun m ->
          m "Tool %s has no schema defined, skipping validation" name);
      return ()
    | Some (Some schema) -> (
      match result.structured_content with
      | None ->
        Log.err (fun m ->
            m "Tool %s has output schema but returned no structured content"
              name);
        raise (Failure
          (Printf.sprintf
             "Tool %s has output schema but returned no structured content" name))
      | Some content -> (
        try
          Log.debug (fun m ->
              m "Validating structured content for tool %s" name);
          let validate_type expected actual =
            match (expected, actual) with
            | "string", `String _ -> true
            | "number", `Float _ | "number", `Int _ -> true
            | "integer", `Int _ -> true
            | "boolean", `Bool _ -> true
            | "array", `List _ -> true
            | "object", `Assoc _ -> true
            | "null", `Null -> true
            | _ -> false
          in
          let rec validate_schema schema value =
            match schema with
            | `Assoc props -> (
              (match List.Assoc.find ~equal:String.equal props "type" with
              | Some (`String typ) ->
                if not (validate_type typ value) then (
                  let msg = Printf.sprintf "Expected type %s" typ in
                  Log.err (fun m ->
                      m "Validation error for tool %s: %s" name msg);
                  raise (Invalid_argument msg))
              | _ -> ());
              (match (List.Assoc.find ~equal:String.equal props "properties", value) with
              | Some (`Assoc properties), `Assoc obj ->
                List.iter
                  (fun (key, prop_schema) ->
                    match List.Assoc.find ~equal:String.equal obj key with
                    | Some v -> validate_schema prop_schema v
                    | None -> (
                      match List.Assoc.find ~equal:String.equal props "required" with
                      | Some (`List required) ->
                        if
                          List.exists
                            (function
                              | `String k -> String.equal k key
                              | _ -> false)
                            required
                        then (
                          let msg =
                            Printf.sprintf "Missing required property %s" key
                          in
                          Log.err (fun m ->
                              m "Validation error for tool %s: %s" name msg);
                          raise (Invalid_argument msg))
                      | _ -> ()))
                  properties
              | _ -> ());
              match (List.Assoc.find ~equal:String.equal props "items", value) with
              | Some item_schema, `List items ->
                List.iter (fun item -> validate_schema item_schema item) items
              | _ -> ())
            | _ -> ()
          in
          validate_schema schema content;
          Log.debug (fun m -> m "Validation successful for tool %s" name);
          return ()
        with
        | Invalid_argument msg ->
          Log.err (fun m -> m "Validation error for tool %s: %s" name msg);
          raise (Failure
            (Printf.sprintf "Invalid structured content returned by tool %s: %s"
               name msg))
        | e ->
          Log.err (fun m ->
              m "Schema validation error for tool %s: %s" name
                (Printexc.to_string e));
          raise (Failure
            (Printf.sprintf "Invalid schema for tool %s: %s" name
               (Printexc.to_string e)))))
  in
  validate_with_refresh true

let call_tool t name ?arguments ?read_timeout ?progress_callback () =
  let request = { method_ = "tools/call"; params = { name; arguments } } in
  let handle_progress notification =
    match progress_callback with
    | Some callback -> (
      match notification with
      | { method_ = "notifications/progress"; params = Some params } ->
        callback params.progress params.total params.message
      | _ -> return ())
    | None -> return ()
  in
  let rec wait_for_response () =
    let read_promise =
      let%bind msg = Pipe.read t.read_stream in
      match msg with
      | `Ok
          { id = None; params = Some notification; result = None; error = None }
        ->
        let%bind () = handle_progress notification in
        wait_for_response ()
      | `Ok { id = Some _; result = Some result; error = None } ->
        return (Ok result)
      | `Ok { error = Some error } ->
        return (Error (`RequestFailed error.message))
      | `Eof -> return (Error (`ProtocolError "Connection closed"))
      | _ -> return (Error (`InvalidMessage "Invalid message format"))
    in
    let timeout_span = Option.map read_timeout ~f:(fun ms -> Time.Span.of_ms (Float.of_int ms)) in
    let%bind result =
      with_timeout timeout_span read_promise
    in
    match result with
    | Ok (Ok result) -> return result
    | Ok (Error err) -> raise (Failure (error_to_string err))
    | Error err -> raise (Failure (error_to_string err))
  in
  let%bind result = wait_for_response () in
  let result = result_to_call_tool_result result in
  if not result.is_error then
    let%bind () = validate_tool_result t name result in
    return result
  else return result

let list_prompts t ?cursor () =
  let request =
    {
      method_ = "prompts/list";
      params = Option.map (fun c -> { cursor = Some c }) cursor;
    }
  in
  send_request t request list_prompts_result_of_yojson

let get_prompt t name ?arguments () =
  let request =
    {
      method_ = "prompts/get";
      params =
        {
          name;
          arguments =
            Option.map
              (fun args -> List.to_seq args |> Hashtbl.of_seq)
              arguments;
        };
    }
  in
  send_request t request get_prompt_result_of_yojson

let complete t ref arguments ?context_arguments () =
  let context =
    Option.map
      (fun args ->
        { CompletionContext.arguments = List.to_seq args |> Hashtbl.of_seq })
      context_arguments
  in
  let request =
    {
      method_ = "completion/complete";
      params =
        { ref; argument = List.to_seq arguments |> Hashtbl.of_seq; context };
    }
  in
  send_request t request complete_result_of_yojson

let list_tools t ?cursor () =
  let request =
    {
      method_ = "tools/list";
      params = Option.map (fun c -> { cursor = Some c }) cursor;
    }
  in
  let%bind result = send_request t request list_tools_result_of_yojson in
  List.iter
    (fun tool ->
      Hashtbl.set t.tool_output_schemas ~key:tool.name ~data:tool.output_schema)
    result.tools;
  return result

let send_roots_list_changed t =
  let notification =
    { method_ = "notifications/roots/list_changed"; params = None }
  in
  send_notification t notification



let _received_request t responder =
  let ctx =
    {
      req_id = responder.request_id;
      req_meta = responder.request_meta;
      session = t;
      lifespan_context = None;
    }
  in
  match responder.request.root with
  | CreateMessageRequest { params } -> (
    match t.sampling_callback with
    | Some callback ->
      let%bind response = callback ctx params in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "Sampling not supported"; data = None }))
  | ElicitRequest { params } -> (
    match t.elicitation_callback with
    | Some callback ->
      let%bind response = callback ctx params in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "Elicitation not supported"; data = None }))
  | ListRootsRequest -> (
    match t.list_roots_callback with
    | Some callback ->
      let%bind response = callback ctx in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "List roots not supported"; data = None }))
  | PingRequest -> responder.respond (ClientResult { root = EmptyResult })

let _received_notification t notification =
  Log.debug (fun m -> m "Received notification: %s" notification.method_);
  match notification.root with
  | LoggingMessageNotification { params } ->
    let%bind () = t.logging_callback params in
    (match notification.meta with
    | Some meta ->
      Log.debug (fun m ->
          m "Notification meta: source=%s target=%s timestamp=%f"
            (Option.value ~default:"unknown" meta.source)
            (Option.value ~default:"unknown" meta.target)
            meta.timestamp)
    | None -> Log.debug (fun m -> m "No meta information in notification"));
    return ()
  | _ ->
    Log.debug (fun m -> m "Ignoring unknown notification type");
    return ()

let rec _handle_incoming t =
  let%bind msg = Pipe.read t.read_stream in
  match msg with
  | `Ok
      { id = Some id; params = Some request; result = None; error = None; meta }
    ->
    let responder =
      {
        request_id = id;
        request_meta = meta;
        request;
        respond =
          (fun response ->
            Pipe.write_without_pushback t.write_stream
              {
                id = Some id;
                params = None;
                result = Some response;
                error = None;
                meta = None;
              };
            return ());
      }
    in
    let%bind () = t.message_handler (`Request responder) in
    let%bind () = _received_request t responder in
    _handle_incoming t
  | `Ok
      {
        id = None;
        params = Some notification;
        result = None;
        error = None;
        meta;
      } ->
    let notification = { notification with meta } in
    let%bind () = t.message_handler (`Notification notification) in
    let%bind () = _received_notification t notification in
    _handle_incoming t
  | `Eof ->
    let%bind () = t.message_handler (`Exception (Failure "Connection closed")) in
    raise (Failure "Connection closed")
  | _ ->
    let%bind () =
      t.message_handler (`Exception (Failure "Invalid message format"))
    in
    _handle_incoming t

let start t =
  if not t.initialized then raise (Failure "Session not initialized")
  else _handle_incoming t
