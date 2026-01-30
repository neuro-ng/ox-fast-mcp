open Core
open Async
open Mcp.Types
open Mcp_shared.Message

let default_client_info =
  {
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
    (Error
       {
         code = invalid_request_error;
         message = "Sampling not supported";
         data = None;
       })
[@@warning "-32"]

let default_elicitation_callback _ctx _params =
  return
    (Error
       {
         code = invalid_request_error;
         message = "Elicitation not supported";
         data = None;
       })
[@@warning "-32"]

let default_list_roots_callback _ctx =
  return
    (Error
       {
         code = invalid_request_error;
         message = "List roots not supported";
         data = None;
       })
[@@warning "-32"]

let default_logging_callback _params = return ()
let logger = Logs.Src.create "client" ~doc:"Client session logger"

module Log = (val Logs.src_log logger)

[@@@warning "-32"]

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

let create_from_pipes ~read_stream ~write_stream ?read_timeout
    ?sampling_callback ?elicitation_callback ?list_roots_callback
    ?logging_callback ?message_handler ?client_info () =
  Log.debug (fun m -> m "Creating client session from existing pipes");
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
  | Some span -> (
    Clock_ns.with_timeout span promise >>| function
    | `Result r -> Result.return r
    | `Timeout -> Error (`Timeout "Operation timed out"))

let send_request t ~method_name ~params ~result_decoder () =
  let request_id_str =
    Printf.sprintf "%f-%d" (Unix.gettimeofday ()) (Random.int 1000000)
  in
  let request_id = `String request_id_str in
  Log.debug (fun m -> m "Sending request %s: %s" request_id_str method_name);

  (* Create JSON-RPC envelope *)
  let jsonrpc_req =
    {
      Mcp.Types.jsonrpc = "2.0";
      id = request_id;
      method_ = method_name;
      params;
    }
  in

  let msg : Mcp_shared.Message.session_message =
    { message = `Request jsonrpc_req; metadata = None }
  in

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
        Log.warn (fun m ->
            m "Request %s failed: %s" request_id_str err.error.message);
        return (Error (`RequestFailed err.error.message))
      | _ ->
        Log.warn (fun m ->
            m "Received response with mismatched ID for request %s"
              request_id_str);
        return (Error (`InvalidMessage "Mismatched response ID")))
    | `Eof ->
      Log.err (fun m -> m "Connection closed for request %s" request_id_str);
      return (Error (`ProtocolError "Connection closed"))
  in
  let%bind result = with_timeout t.read_timeout read_promise in
  match result with
  | Ok (Ok json_result) -> (
    match result_decoder json_result with
    | Ok decoded -> return decoded
    | Error err ->
      Log.err (fun m ->
          m "Failed to decode result for request %s: %s" request_id_str err);
      raise (Failure (Printf.sprintf "Result decoding failed: %s" err)))
  | Ok (Error err) ->
    Log.err (fun m ->
        m "Request %s failed: %s" request_id_str (error_to_string err));
    raise (Failure (error_to_string err))
  | Error err ->
    Log.err (fun m ->
        m "Request %s timed out: %s" request_id_str (error_to_string err));
    raise (Failure (error_to_string err))

let send_notification t ~method_name ~params () =
  let jsonrpc_notif =
    { Mcp.Types.jsonrpc = "2.0"; method_ = method_name; params }
  in

  let msg : Mcp_shared.Message.session_message =
    { message = `Notification jsonrpc_notif; metadata = None }
  in
  Pipe.write_without_pushback t.write_stream msg;
  return ()

let initialize t =
  let sampling =
    match t.sampling_callback with
    | Some _ -> Some (`Assoc [ ("enabled", `Bool true) ])
    | None -> None
  in
  let elicitation =
    match t.elicitation_callback with
    | Some _ -> Some (`Assoc [ ("enabled", `Bool true) ])
    | None -> None
  in
  let roots_cap : roots_capability option =
    match t.list_roots_callback with
    | Some _ -> Some { list_changed = Some true }
    | None -> None
  in
  let params =
    `Assoc
      [
        ("protocolVersion", `String latest_protocol_version);
        ( "capabilities",
          yojson_of_client_capabilities
            { experimental = None; sampling; elicitation; roots = roots_cap } );
        ("clientInfo", yojson_of_implementation t.client_info);
        ("_meta", `Assoc []);
      ]
  in
  let%bind result =
    send_request t ~method_name:"initialize" ~params:(Some params)
      ~result_decoder:(fun json ->
        try Ok (initialize_result_of_yojson json)
        with exn -> Error (Exn.to_string exn))
      ()
  in
  if
    not
      (List.mem Mcp_shared.Version.supported_protocol_versions
         result.protocol_version ~equal:String.equal)
  then
    raise
      (Failure
         ("Unsupported protocol version from server: " ^ result.protocol_version))
  else
    let%bind () =
      send_notification t ~method_name:"notifications/initialized" ~params:None
        ()
    in
    t.initialized <- true;
    return result

let send_ping t =
  send_request t ~method_name:"ping" ~params:None
    ~result_decoder:(fun json ->
      try Ok (empty_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

[@@@warning "-32"]

let send_progress_notification t progress_token progress ?total ?message () =
  let params =
    {
      progress_token;
      progress;
      total;
      message;
      notification_params = { meta = None };
    }
  in
  send_notification t ~method_name:"notifications/progress"
    ~params:(Some (progress_notification_params_to_yojson params))
    ()

let set_logging_level t level =
  let params = `Assoc [ ("level", yojson_of_logging_level level) ] in
  send_request t ~method_name:"logging/setLevel" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (empty_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let list_resources t ?cursor () =
  let params =
    match cursor with
    | Some c -> Some (`Assoc [ ("cursor", `String c) ])
    | None -> None
  in
  send_request t ~method_name:"resources/list" ~params
    ~result_decoder:(fun json ->
      try Ok (list_resources_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let list_prompts t ?cursor () =
  let params =
    match cursor with
    | Some c -> Some (`Assoc [ ("cursor", `String c) ])
    | None -> None
  in
  send_request t ~method_name:"prompts/list" ~params
    ~result_decoder:(fun json ->
      try Ok (list_prompts_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let list_resource_templates t ?cursor () =
  let params =
    match cursor with
    | Some c -> Some (`Assoc [ ("cursor", `String c) ])
    | None -> None
  in
  send_request t ~method_name:"resources/templates/list" ~params
    ~result_decoder:(fun json ->
      try Ok (list_resource_templates_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let read_resource t uri =
  let params = `Assoc [ ("uri", `String uri) ] in
  send_request t ~method_name:"resources/read" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (read_resource_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let subscribe_resource t uri =
  let params = `Assoc [ ("uri", `String uri) ] in
  send_request t ~method_name:"resources/subscribe" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (empty_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let unsubscribe_resource t uri =
  let params = `Assoc [ ("uri", `String uri) ] in
  send_request t ~method_name:"resources/unsubscribe" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (empty_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let get_prompt t name arguments =
  let params =
    `Assoc
      ([ ("name", `String name) ]
      @
      match arguments with
      | Some args -> [ ("arguments", `Assoc args) ]
      | None -> [])
  in
  send_request t ~method_name:"prompts/get" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (get_prompt_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let complete t ref argument =
  let params =
    Mcp.Types.complete_request_params_to_yojson
      {
        reference = ref;
        argument =
          {
            name = fst (List.hd_exn argument);
            value = snd (List.hd_exn argument);
          };
        context = None;
        request_params = { meta = None };
      }
  in
  send_request t ~method_name:"completion/complete" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (complete_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

let rec list_tools t ?cursor () =
  let params =
    match cursor with
    | Some c -> Some (`Assoc [ ("cursor", `String c) ])
    | None -> None
  in
  let%bind result =
    send_request t ~method_name:"tools/list" ~params
      ~result_decoder:(fun json ->
        try Ok (list_tools_result_of_yojson json)
        with exn -> Error (Exn.to_string exn))
      ()
  in
  List.iter result.tools ~f:(fun tool ->
      Hashtbl.set t.tool_output_schemas ~key:tool.base_metadata.name
        ~data:tool.output_schema);
  return result

and validate_tool_result t name result =
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
        raise
          (Failure
             (Printf.sprintf
                "Tool %s has output schema but returned no structured content"
                name))
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
              (match
                 (List.Assoc.find ~equal:String.equal props "properties", value)
               with
              | Some (`Assoc properties), `Assoc obj ->
                List.iter properties ~f:(fun (key, prop_schema) ->
                    match List.Assoc.find ~equal:String.equal obj key with
                    | Some v -> validate_schema prop_schema v
                    | None -> (
                      match
                        List.Assoc.find ~equal:String.equal props "required"
                      with
                      | Some (`List required) ->
                        if
                          List.exists required ~f:(function
                            | `String k -> String.equal k key
                            | _ -> false)
                        then (
                          let msg =
                            Printf.sprintf "Missing required property %s" key
                          in
                          Log.err (fun m ->
                              m "Validation error for tool %s: %s" name msg);
                          raise (Invalid_argument msg))
                      | _ -> ()))
              | _ -> ());
              match
                (List.Assoc.find ~equal:String.equal props "items", value)
              with
              | Some item_schema, `List items ->
                List.iter items ~f:(fun item ->
                    validate_schema item_schema item)
              | _ -> ())
            | _ -> ()
          in
          validate_schema schema content;
          Log.debug (fun m -> m "Validation successful for tool %s" name);
          return ()
        with
        | Invalid_argument msg ->
          Log.err (fun m -> m "Validation error for tool %s: %s" name msg);
          raise
            (Failure
               (Printf.sprintf
                  "Invalid structured content returned by tool %s: %s" name msg))
        | e ->
          Log.err (fun m ->
              m "Schema validation error for tool %s: %s" name (Exn.to_string e));
          raise
            (Failure
               (Printf.sprintf "Invalid schema for tool %s: %s" name
                  (Exn.to_string e)))))
  in
  validate_with_refresh true

let call_tool t name ?arguments ?read_timeout ?progress_callback () =
  (* Serialize params to JSON *)
  let params =
    `Assoc
      ([ ("name", `String name) ]
      @
      match arguments with
      | Some args -> [ ("arguments", args) ]
      | None -> [])
  in

  (* Create JSON-RPC request manually (can't use send_request due to progress
     handling) *)
  let request_id_str =
    Printf.sprintf "%f-%d" (Unix.gettimeofday ()) (Random.int 1000000)
  in
  let request_id = `String request_id_str in

  let jsonrpc_req =
    {
      Mcp.Types.jsonrpc = "2.0";
      id = request_id;
      method_ = "tools/call";
      params = Some params;
    }
  in

  let msg : Mcp_shared.Message.session_message =
    { message = `Request jsonrpc_req; metadata = None }
  in

  (* Send the request *)
  let%bind () = Pipe.write t.write_stream msg in

  (* Handle progress notifications during response waiting *)
  let handle_progress json_params =
    match progress_callback with
    | Some callback -> (
      (* Parse progress notification params *)
      try
        let progress_notif =
          progress_notification_params_of_yojson json_params
        in
        let progress = progress_notif.progress in
        let total = progress_notif.total in
        let message = progress_notif.message in
        callback progress total message
      with _ -> return ())
    | None -> return ()
  in

  let rec wait_for_response () =
    let read_promise =
      let%bind response = Pipe.read t.read_stream in
      match response with
      | `Ok msg -> (
        match msg.message with
        | `Notification notif
          when String.equal notif.method_ "notifications/progress" -> (
          (* Handle progress notification *)
          match notif.params with
          | Some params_json ->
            let%bind () = handle_progress params_json in
            wait_for_response ()
          | None -> wait_for_response ())
        | `Response resp when Poly.equal resp.id request_id ->
          return resp.result
        | `Error err when Poly.equal err.id request_id ->
          raise (Failure err.error.message)
        | _ -> wait_for_response ())
      | `Eof -> raise (Failure "Connection closed")
    in
    match read_timeout with
    | None -> read_promise
    | Some ms -> (
      let timeout_span = Time_ns.Span.of_ms (Float.of_int ms) in
      Clock_ns.with_timeout timeout_span read_promise >>| function
      | `Result r -> r
      | `Timeout -> raise (Failure "Tool call timed out"))
  in

  (* Wait for result and deserialize *)
  let%bind json_result = wait_for_response () in
  let result =
    try call_tool_result_of_yojson json_result
    with exn ->
      raise
        (Failure
           (Printf.sprintf "Failed to parse call_tool result: %s"
              (Exn.to_string exn)))
  in

  (* Validate if not an error *)
  if not result.is_error then
    let%bind () = validate_tool_result t name result in
    return result
  else return result

let send_roots_list_changed t =
  send_notification t ~method_name:"notifications/roots/list_changed"
    ~params:None ()

(* (* TODO: These functions need to be rewritten to work with JSON-based message
   handling *) let _received_request t responder = let ctx = { req_id =
   responder.request_id; req_meta = responder.request_meta; session = t;
   lifespan_context = None; } in match responder.request.root with |
   CreateMessageRequest { params } -> ( match t.sampling_callback with | Some
   callback -> let%bind response = callback ctx params in let client_response =
   match response with | Ok result -> ClientResult { root = result } | Error err
   -> ErrorData err in responder.respond client_response | None ->
   responder.respond (ErrorData { code = invalid_request; message = "Sampling
   not supported"; data = None })) | ElicitRequest { params } -> ( match
   t.elicitation_callback with | Some callback -> let%bind response = callback
   ctx params in let client_response = match response with | Ok result ->
   ClientResult { root = result } | Error err -> ErrorData err in
   responder.respond client_response | None -> responder.respond (ErrorData {
   code = invalid_request; message = "Elicitation not supported"; data = None
   })) | ListRootsRequest -> ( match t.list_roots_callback with | Some callback
   -> let%bind response = callback ctx in let client_response = match response
   with | Ok result -> ClientResult { root = result } | Error err -> ErrorData
   err in responder.respond client_response | None -> responder.respond
   (ErrorData { code = invalid_request; message = "List roots not supported";
   data = None })) | PingRequest -> responder.respond (ClientResult { root =
   EmptyResult })

   let _received_notification t notification = Log.debug (fun m -> m "Received
   notification: %s" notification.method_); match notification.root with |
   LoggingMessageNotification { params } -> let%bind () = t.logging_callback
   params in (match notification.meta with | Some meta -> Log.debug (fun m -> m
   "Notification meta: source=%s target=%s timestamp=%f" (Option.value
   ~default:"unknown" meta.source) (Option.value ~default:"unknown" meta.target)
   meta.timestamp) | None -> Log.debug (fun m -> m "No meta information in
   notification")); return () | _ -> Log.debug (fun m -> m "Ignoring unknown
   notification type"); return ()

   let rec _handle_incoming t = let%bind msg = Pipe.read t.read_stream in match
   msg with | `Ok { id = Some id; params = Some request; result = None; error =
   None; meta } -> let responder = { request_id = id; request_meta = meta;
   request; respond = (fun response -> Pipe.write_without_pushback
   t.write_stream { id = Some id; params = None; result = Some response; error =
   None; meta = None; }; return ()); } in let%bind () = t.message_handler
   (`Request responder) in let%bind () = _received_request t responder in
   _handle_incoming t | `Ok { id = None; params = Some notification; result =
   None; error = None; meta; } -> let notification = { notification with meta }
   in let%bind () = t.message_handler (`Notification notification) in let%bind
   () = _received_notification t notification in _handle_incoming t | `Eof ->
   let%bind () = t.message_handler (`Exception (Failure "Connection closed")) in
   raise (Failure "Connection closed") | _ -> let%bind () = t.message_handler
   (`Exception (Failure "Invalid message format")) in _handle_incoming t

   let start t = if not t.initialized then raise (Failure "Session not
   initialized") else _handle_incoming t *)
