open Lwt.Syntax
open Mcp_types
open Mcp_shared
open Types
open Ox_fast_mcp.Mcp

let default_client_info = { name = "mcp"; version = "0.1.0" }

type t = {
  read_stream : (session_message, exn) result Lwt_stream.t;
  write_stream : session_message Lwt_stream.t;
  read_timeout : float option;
  sampling_callback : sampling_fn option;
  elicitation_callback : elicitation_fn option;
  list_roots_callback : list_roots_fn option;
  logging_callback : logging_fn;
  message_handler : message_handler;
  client_info : implementation;
  tool_output_schemas : (string, Yojson.Safe.t option) Hashtbl.t;
  mutable initialized : bool;
}

and progress_fn = float -> float option -> string option -> unit Lwt.t

and sampling_fn =
  request_context ->
  create_message_request_params ->
  (create_message_result, error_data) result Lwt.t

and elicitation_fn =
  request_context ->
  elicit_request_params ->
  (elicit_result, error_data) result Lwt.t

and list_roots_fn =
  request_context -> (list_roots_result, error_data) result Lwt.t

and logging_fn = logging_message_notification_params -> unit Lwt.t

and message_handler =
  [ `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn ] ->
  unit Lwt.t

let default_message_handler _ = Lwt.return_unit

let default_sampling_callback _ctx _params =
  Lwt.return
    (Error { code = invalid_request; message = "Sampling not supported" })

let default_elicitation_callback _ctx _params =
  Lwt.return
    (Error { code = invalid_request; message = "Elicitation not supported" })

let default_list_roots_callback _ctx =
  Lwt.return
    (Error { code = invalid_request; message = "List roots not supported" })

let default_logging_callback _params = Lwt.return_unit
let logger = Logs.Src.create "client" ~doc:"Client session logger"

module Log = (val Logs.src_log logger)

let create ?read_timeout ?sampling_callback ?elicitation_callback
    ?list_roots_callback ?logging_callback ?message_handler ?client_info () =
  Log.debug (fun m -> m "Creating new client session");
  let read_stream, write_stream = Lwt_stream.create () in
  Lwt.return
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
      tool_output_schemas = Hashtbl.create 32;
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
  | None -> promise
  | Some t ->
    let timeout_promise =
      let* () = Lwt_unix.sleep t in
      Lwt.return_error (`Timeout "Operation timed out")
    in
    Lwt.pick
      [
        (let* result = promise in
         Lwt.return_ok result);
        timeout_promise;
      ]

let send_request t request expected_type =
  let request_id = Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_string in
  Log.debug (fun m -> m "Sending request %s" request_id);
  let request_meta =
    {
      timestamp = Unix.gettimeofday ();
      source = Some "client";
      target = Some "server";
    }
  in
  let msg =
    {
      id = Some request_id;
      params = Some request;
      result = None;
      error = None;
      meta = Some request_meta;
    }
  in
  Lwt_stream.add msg t.write_stream;
  let read_promise =
    let* response = Lwt_stream.next t.read_stream in
    match response with
    | Ok msg when msg.id = Some request_id -> (
      match (msg.result, msg.error) with
      | Some result, None ->
        Log.debug (fun m ->
            m "Received successful response for request %s" request_id);
        Lwt.return_ok result
      | None, Some error ->
        Log.warn (fun m -> m "Request %s failed: %s" request_id error.message);
        Lwt.return_error (`RequestFailed error.message)
      | None, None ->
        Log.warn (fun m ->
            m "Invalid response for request %s: no result or error" request_id);
        Lwt.return_error (`InvalidMessage "No result or error in response")
      | Some _, Some _ ->
        Log.warn (fun m ->
            m "Invalid response for request %s: both result and error present"
              request_id);
        Lwt.return_error
          (`InvalidMessage "Both result and error present in response"))
    | Ok _ ->
      Log.warn (fun m ->
          m "Received response with mismatched ID for request %s" request_id);
      Lwt.return_error (`InvalidMessage "Mismatched response ID")
    | Error e ->
      Log.err (fun m ->
          m "Protocol error for request %s: %s" request_id
            (Printexc.to_string e));
      Lwt.return_error (`ProtocolError (Printexc.to_string e))
  in
  let* result = with_timeout t.read_timeout read_promise in
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    Log.err (fun m ->
        m "Request %s failed: %s" request_id (error_to_string err));
    Lwt.fail_with (error_to_string err)

let send_notification t notification =
  let request_meta =
    {
      timestamp = Unix.gettimeofday ();
      source = Some "client";
      target = Some "server";
    }
  in
  let msg =
    {
      id = None;
      params = Some notification;
      result = None;
      error = None;
      meta = Some request_meta;
    }
  in
  Lwt_stream.add msg t.write_stream;
  Lwt.return_unit

let initialize t =
  let sampling =
    match t.sampling_callback with
    | Some _ -> Some { SamplingCapability.empty with enabled = true }
    | None -> None
  in
  let elicitation =
    match t.elicitation_callback with
    | Some _ -> Some { ElicitationCapability.empty with enabled = true }
    | None -> None
  in
  let roots =
    match t.list_roots_callback with
    | Some _ -> Some { RootsCapability.empty with list_changed = true }
    | None -> None
  in
  let request =
    {
      method_ = "initialize";
      params =
        {
          protocol_version = latest_protocol_version;
          capabilities = { sampling; elicitation; experimental = None; roots };
          client_info = t.client_info;
        };
    }
  in
  let* result = send_request t request initialize_result_of_yojson in
  if not (List.mem result.protocol_version supported_protocol_versions) then
    Lwt.fail_with
      ("Unsupported protocol version from server: "
      ^ string_of_int result.protocol_version)
  else
    let* () =
      send_notification t
        { method_ = "notifications/initialized"; params = None }
    in
    t.initialized <- true;
    Lwt.return result

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
    match Hashtbl.find_opt t.tool_output_schemas name with
    | None when first_try ->
      Log.debug (fun m ->
          m "No schema found for tool %s, refreshing schema cache" name);
      let* _ = list_tools t () in
      validate_with_refresh false
    | None ->
      Log.warn (fun m ->
          m
            "Tool %s not listed by server, cannot validate any structured \
             content"
            name);
      Lwt.return_unit
    | Some None ->
      Log.debug (fun m ->
          m "Tool %s has no schema defined, skipping validation" name);
      Lwt.return_unit
    | Some (Some schema) -> (
      match result.structured_content with
      | None ->
        Log.err (fun m ->
            m "Tool %s has output schema but returned no structured content"
              name);
        Lwt.fail_with
          (Printf.sprintf
             "Tool %s has output schema but returned no structured content" name)
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
              (match List.assoc_opt "type" props with
              | Some (`String typ) ->
                if not (validate_type typ value) then (
                  let msg = Printf.sprintf "Expected type %s" typ in
                  Log.err (fun m ->
                      m "Validation error for tool %s: %s" name msg);
                  raise (Invalid_argument msg))
              | _ -> ());
              (match (List.assoc_opt "properties" props, value) with
              | Some (`Assoc properties), `Assoc obj ->
                List.iter
                  (fun (key, prop_schema) ->
                    match List.assoc_opt key obj with
                    | Some v -> validate_schema prop_schema v
                    | None -> (
                      match List.assoc_opt "required" props with
                      | Some (`List required) ->
                        if
                          List.exists
                            (function
                              | `String k -> k = key
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
              match (List.assoc_opt "items" props, value) with
              | Some item_schema, `List items ->
                List.iter (fun item -> validate_schema item_schema item) items
              | _ -> ())
            | _ -> ()
          in
          validate_schema schema content;
          Log.debug (fun m -> m "Validation successful for tool %s" name);
          Lwt.return_unit
        with
        | Invalid_argument msg ->
          Log.err (fun m -> m "Validation error for tool %s: %s" name msg);
          Lwt.fail_with
            (Printf.sprintf "Invalid structured content returned by tool %s: %s"
               name msg)
        | e ->
          Log.err (fun m ->
              m "Schema validation error for tool %s: %s" name
                (Printexc.to_string e));
          Lwt.fail_with
            (Printf.sprintf "Invalid schema for tool %s: %s" name
               (Printexc.to_string e))))
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
      | _ -> Lwt.return_unit)
    | None -> Lwt.return_unit
  in
  let rec wait_for_response () =
    let read_promise =
      let* msg = Lwt_stream.next t.read_stream in
      match msg with
      | Ok
          { id = None; params = Some notification; result = None; error = None }
        ->
        let* () = handle_progress notification in
        wait_for_response ()
      | Ok { id = Some _; result = Some result; error = None } ->
        Lwt.return_ok result
      | Ok { error = Some error } ->
        Lwt.return_error (`RequestFailed error.message)
      | Error e -> Lwt.return_error (`ProtocolError (Printexc.to_string e))
      | _ -> Lwt.return_error (`InvalidMessage "Invalid message format")
    in
    let* result =
      with_timeout (Option.map float_of_int read_timeout) read_promise
    in
    match result with
    | Ok result -> Lwt.return result
    | Error err -> Lwt.fail_with (error_to_string err)
  in
  let* result = wait_for_response () in
  let result = result_to_call_tool_result result in
  if not result.is_error then
    let* () = validate_tool_result t name result in
    Lwt.return result
  else Lwt.return result

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
  let* result = send_request t request list_tools_result_of_yojson in
  List.iter
    (fun tool ->
      Hashtbl.replace t.tool_output_schemas tool.name tool.output_schema)
    result.tools;
  Lwt.return result

let send_roots_list_changed t =
  let notification =
    { method_ = "notifications/roots/list_changed"; params = None }
  in
  send_notification t notification

let _received_request t responder =
  let ctx =
    {
      request_id = responder.request_id;
      meta = responder.request_meta;
      session = t;
      lifespan_context = None;
    }
  in
  match responder.request.root with
  | CreateMessageRequest { params } -> (
    match t.sampling_callback with
    | Some callback ->
      let* response = callback ctx params in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "Sampling not supported" }))
  | ElicitRequest { params } -> (
    match t.elicitation_callback with
    | Some callback ->
      let* response = callback ctx params in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "Elicitation not supported" }))
  | ListRootsRequest -> (
    match t.list_roots_callback with
    | Some callback ->
      let* response = callback ctx in
      let client_response =
        match response with
        | Ok result -> ClientResult { root = result }
        | Error err -> ErrorData err
      in
      responder.respond client_response
    | None ->
      responder.respond
        (ErrorData
           { code = invalid_request; message = "List roots not supported" }))
  | PingRequest -> responder.respond (ClientResult { root = EmptyResult })

let _received_notification t notification =
  Log.debug (fun m -> m "Received notification: %s" notification.method_);
  match notification.root with
  | LoggingMessageNotification { params } ->
    let* () = t.logging_callback params in
    (match notification.meta with
    | Some meta ->
      Log.debug (fun m ->
          m "Notification meta: source=%s target=%s timestamp=%f"
            (Option.value ~default:"unknown" meta.source)
            (Option.value ~default:"unknown" meta.target)
            meta.timestamp)
    | None -> Log.debug (fun m -> m "No meta information in notification"));
    Lwt.return_unit
  | _ ->
    Log.debug (fun m -> m "Ignoring unknown notification type");
    Lwt.return_unit

let rec _handle_incoming t =
  let* msg = Lwt_stream.next t.read_stream in
  match msg with
  | Ok
      { id = Some id; params = Some request; result = None; error = None; meta }
    ->
    let responder =
      {
        request_id = id;
        request_meta = meta;
        request;
        respond =
          (fun response ->
            let response_meta =
              {
                timestamp = Unix.gettimeofday ();
                source = Some "client";
                target = Some "server";
              }
            in
            Lwt_stream.add
              {
                id = Some id;
                params = None;
                result = Some response;
                error = None;
                meta = Some response_meta;
              }
              t.write_stream;
            Lwt.return_unit);
      }
    in
    let* () = t.message_handler (`Request responder) in
    let* () = _received_request t responder in
    _handle_incoming t
  | Ok
      {
        id = None;
        params = Some notification;
        result = None;
        error = None;
        meta;
      } ->
    let notification = { notification with meta } in
    let* () = t.message_handler (`Notification notification) in
    let* () = _received_notification t notification in
    _handle_incoming t
  | Error exn ->
    let* () = t.message_handler (`Exception exn) in
    Lwt.fail exn
  | _ ->
    let* () =
      t.message_handler (`Exception (Failure "Invalid message format"))
    in
    _handle_incoming t

let start t =
  if not t.initialized then Lwt.fail_with "Session not initialized"
  else _handle_incoming t
