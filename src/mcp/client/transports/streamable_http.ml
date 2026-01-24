open Core
open Async
module Types = Mcp.Types

(** Streamable HTTP client transport for OxFastMCP.

    Provides client-side StreamableHTTP transport supporting HTTP POST requests
    with optional SSE streaming responses and session management. *)

(** HTTP header constants *)
let mcp_session_id = "mcp-session-id"

let mcp_protocol_version = "mcp-protocol-version"
let last_event_id = "last-event-id"
let content_type_header = "content-type"
let accept_header = "accept"

(** Content type constants *)
let json_content_type = "application/json"

let sse_content_type = "text/event-stream"

(** StreamableHTTP transport errors *)
module Error = struct
  type t = Streamable_http_error of string | Resumption_error of string
  [@@deriving sexp_of]

  let to_string = function
    | Streamable_http_error msg -> sprintf "StreamableHTTP error: %s" msg
    | Resumption_error msg -> sprintf "Resumption error: %s" msg
end

type request_context = {
  headers : (string * string) list;
  session_id : string option;
  session_message : Types.jsonrpc_message;
  sse_read_timeout : Time_float.Span.t;
}
[@@deriving sexp_of]
(** Request context for processing *)

type config = {
  url : string;
  headers : (string * string) list;
  timeout : Time_float.Span.t;
  sse_read_timeout : Time_float.Span.t;
}
[@@deriving sexp_of]
(** StreamableHTTP transport configuration *)

(** Create StreamableHTTP configuration *)
let create_config ~url ?(headers = []) ?timeout ?sse_read_timeout () =
  {
    url;
    headers;
    timeout = Option.value timeout ~default:(Time_float.Span.of_sec 30.);
    sse_read_timeout =
      Option.value sse_read_timeout ~default:(Time_float.Span.of_sec 300.);
  }

type t = {
  config : config;
  read_stream :
    [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t;
  write_stream : Types.jsonrpc_message Pipe.Writer.t;
  mutable session_id : string option;
  mutable protocol_version : string option;
  request_headers : (string * string) list;
}
(** StreamableHTTP transport state *)

(** Prepare request headers with session ID and protocol version *)
let prepare_request_headers t base_headers =
  let headers = base_headers in
  let headers =
    match t.session_id with
    | Some sid -> (mcp_session_id, sid) :: headers
    | None -> headers
  in
  match t.protocol_version with
  | Some pv -> (mcp_protocol_version, pv) :: headers
  | None -> headers

(** Check if message is an initialization request *)
let is_initialization_request message =
  match message with
  | `Request (req : Types.jsonrpc_request) ->
    String.equal req.method_ "initialize"
  | _ -> false

(** Check if message is an initialized notification *)
let is_initialized_notification message =
  match message with
  | `Notification (notif : Types.jsonrpc_notification) ->
    String.equal notif.method_ "notifications/initialized"
  | _ -> false

(** Extract session ID from response headers *)
let maybe_extract_session_id_from_response t response =
  (* Extract mcp-session-id header from HTTP response *)
  let headers = Cohttp.Response.headers response in
  match Cohttp.Header.get headers mcp_session_id with
  | Some sid ->
    t.session_id <- Some sid;
    Logs.info (fun m -> m "Session ID extracted: %s" sid)
  | None -> Logs.debug (fun m -> m "No session ID in response headers")

(** Extract protocol version from initialization response *)
let maybe_extract_protocol_version_from_message t message =
  match message with
  | `Response (resp : Types.jsonrpc_response) -> (
    (* The result field in jsonrpc_response is a Yojson.Safe.t *)
    match Yojson.Safe.Util.member "protocolVersion" resp.result with
    | `String version ->
      t.protocol_version <- Some version;
      Logs.info (fun m -> m "Negotiated protocol version: %s" version)
    | _ ->
      Logs.debug (fun m ->
          m "No protocolVersion found in initialization response"))
  | _ -> ()

(** Send session terminated error to read stream *)
let send_session_terminated_error ~read_writer ~request_id =
  let error_data =
    Types.{ code = 32600; message = "Session terminated"; data = None }
  in
  let jsonrpc_error =
    `Error Types.{ jsonrpc = "2.0"; id = request_id; error = error_data }
  in
  Pipe.write_without_pushback read_writer (`Message jsonrpc_error)

(** Handle SSE event - parse event and dispatch to read stream *)
let handle_sse_event t sse_event read_writer ~original_request_id:_
    ~is_initialization =
  match sse_event.Sse_protocol.Event.event_type with
  | "message" -> (
    match Sse_protocol.Event.parse_jsonrpc sse_event with
    | Ok jsonrpc_msg -> (
      (* Extract protocol version if this is an initialization response *)
      if is_initialization then
        maybe_extract_protocol_version_from_message t jsonrpc_msg;

      (* Send to read stream *)
      let%bind () = Pipe.write read_writer (`Message jsonrpc_msg) in

      (* Return true if this completes the request (response or error) *)
      match jsonrpc_msg with
      | `Response _ | `Error _ -> return true
      | _ -> return false)
    | Error err ->
      Logs.err (fun m ->
          m "Failed to parse SSE event: %s" (Core.Error.to_string_hum err));
      return false)
  | _ ->
    (* Ignore other event types *)
    Logs.debug (fun m -> m "Ignoring SSE event type: %s" sse_event.event_type);
    return false

(** Handle GET stream for server-initiated messages *)
let handle_get_stream t read_writer =
  match t.session_id with
  | None ->
    Logs.debug (fun m -> m "No session ID, skipping GET stream");
    return ()
  | Some sid -> (
    Logs.info (fun m ->
        m "Starting GET stream for server messages (session: %s)" sid);

    (* Prepare headers with session ID and Accept header for SSE *)
    let uri = Uri.of_string t.config.url in
    let cohttp_headers =
      let base_headers =
        Cohttp.Header.init () |> fun h ->
        Cohttp.Header.add h accept_header sse_content_type |> fun h ->
        Cohttp.Header.add h mcp_session_id sid
      in
      List.fold t.request_headers ~init:base_headers ~f:(fun h (k, v) ->
          Cohttp.Header.add h k v)
    in

    (* Establish GET SSE connection *)
    try_with (fun () ->
        Cohttp_async.Client.get ~headers:cohttp_headers uri
        >>= fun (resp, body) ->
        let status_code =
          Cohttp.Response.status resp |> Cohttp.Code.code_of_status
        in

        if status_code >= 200 && status_code < 300 then
          (* Parse SSE events from response body *)
          let event_stream = Sse_protocol.Parser.parse_stream body in

          (* Process events until stream closes *)
          Pipe.iter event_stream ~f:(fun event ->
              let%bind _is_complete =
                handle_sse_event t event read_writer ~original_request_id:None
                  ~is_initialization:false
              in
              return ())
        else (
          Logs.warn (fun m -> m "GET stream returned status %d" status_code);
          return ()))
    >>= function
    | Ok () ->
      Logs.info (fun m -> m "GET stream completed");
      return ()
    | Error exn ->
      Logs.err (fun m -> m "GET stream error: %s" (Exn.to_string exn));
      return ())

(** Handle resumption request (skeleton - future feature) *)
let[@warning "-32"] handle_resumption_request _t _ctx =
  let () =
    Logs.debug (fun m -> m "Handling resumption request (placeholder)")
  in
  (* Note: Full implementation would: 1. Validate resumption token exists 2. Add
     Last-Event-ID header 3. Establish GET SSE connection 4. Handle events until
     complete response *)
  return ()

(** Handle JSON response *)
let handle_json_response t response read_writer ~is_initialization =
  (* Read JSON body from response *)
  let _resp, body = response in
  let%bind body_str = Cohttp_async.Body.to_string body in

  Logs.debug (fun m ->
      m "Received JSON response: %s" (String.prefix body_str 100));

  (* Parse JSON-RPC message *)
  try_with (fun () ->
      let json = Yojson.Safe.from_string body_str in
      let message = Types.jsonrpc_message_of_yojson json in

      (* Extract protocol version if initialization response *)
      if is_initialization then
        maybe_extract_protocol_version_from_message t message;

      (* Send to read stream *)
      Pipe.write read_writer (`Message message))
  >>= function
  | Ok () -> return ()
  | Error exn ->
    Logs.err (fun m ->
        m "Failed to parse JSON response: %s" (Exn.to_string exn));
    Pipe.write read_writer
      (`Error (Error.Streamable_http_error "JSON parse error"))

(** Handle SSE response *)
let handle_sse_response t response ctx read_writer ~is_initialization =
  (* Parse SSE events from response body stream *)
  let _resp, body = response in
  let event_stream = Sse_protocol.Parser.parse_stream body in

  Logs.debug (fun m -> m "Handling SSE response stream");

  (* Process SSE events until completion *)
  (* Note: original_request_id reserved for future resumption token support *)
  let _original_request_id =
    match ctx.session_message with
    | `Request req -> Some req.id
    | `Notification _ -> None
    | `Response resp -> Some resp.id
    | `Error err -> Some err.id
  in

  (* Read SSE events until we get a complete response *)
  let rec process_events () =
    match%bind Pipe.read event_stream with
    | `Eof ->
      Logs.debug (fun m -> m "SSE stream completed");
      return ()
    | `Ok event -> (
      (* Handle the SSE event *)
      match event.Sse_protocol.Event.event_type with
      | "message" -> (
        match Sse_protocol.Event.parse_jsonrpc event with
        | Ok jsonrpc_msg -> (
          (* Extract protocol version if initialization *)
          if is_initialization then
            maybe_extract_protocol_version_from_message t jsonrpc_msg;

          (* Send to read stream *)
          let%bind () = Pipe.write read_writer (`Message jsonrpc_msg) in

          (* Check if this completes the request (response or error) *)
          match jsonrpc_msg with
          | `Response _ | `Error _ ->
            Logs.debug (fun m -> m "Received complete response via SSE");
            return ()
          | _ ->
            (* Continue reading for more events *)
            process_events ())
        | Error err ->
          Logs.err (fun m ->
              m "Failed to parse SSE event: %s" (Core.Error.to_string_hum err));
          process_events ())
      | _ ->
        (* Ignore other event types *)
        Logs.debug (fun m -> m "Ignoring SSE event type: %s" event.event_type);
        process_events ())
  in

  process_events ()

(** Handle POST request *)
let handle_post_request t ctx read_writer =
  let { headers; session_id = _; session_message; sse_read_timeout = _ } =
    ctx
  in
  let is_initialization = is_initialization_request session_message in

  (* Serialize message to JSON *)
  let json = Types.jsonrpc_message_to_yojson session_message in
  let body_str = Yojson.Safe.to_string json in

  Logs.debug (fun m ->
      m "Sending POST request: %s" (String.prefix body_str 100));

  (* Prepare headers *)
  let uri = Uri.of_string t.config.url in
  let cohttp_headers =
    List.fold headers ~init:(Cohttp.Header.init ()) ~f:(fun h (k, v) ->
        Cohttp.Header.add h k v)
  in

  (* Send POST request with timeout *)
  let post_deferred =
    try_with (fun () ->
        Cohttp_async.Client.post ~body:(`String body_str)
          ~headers:cohttp_headers uri)
  in

  match%bind Clock.with_timeout t.config.timeout post_deferred with
  | `Timeout ->
    Logs.err (fun m -> m "POST request timeout");
    Pipe.write read_writer
      (`Error (Error.Streamable_http_error "Request timeout"))
  | `Result (Error exn) ->
    Logs.err (fun m -> m "POST request error: %s" (Exn.to_string exn));
    Pipe.write read_writer
      (`Error (Error.Streamable_http_error (Exn.to_string exn)))
  | `Result (Ok (resp, body)) -> (
    let status = Cohttp.Response.status resp in
    let status_code = Cohttp.Code.code_of_status status in

    (* Extract session ID from response headers *)
    maybe_extract_session_id_from_response t resp;

    match status_code with
    | 404 ->
      Logs.warn (fun m -> m "Session terminated (404)");
      (* Send error for the request *)
      (match session_message with
      | `Request req ->
        send_session_terminated_error ~read_writer ~request_id:req.id
      | _ -> ());
      return ()
    | 202 ->
      (* Accepted - check content type for response *)
      Logs.debug (fun m -> m "Request accepted (202)");
      let content_type =
        Cohttp.Response.headers resp |> fun h ->
        Cohttp.Header.get h "content-type" |> Option.value ~default:""
      in

      if String.is_substring content_type ~substring:sse_content_type then
        (* SSE response *)
        handle_sse_response t (resp, body) ctx read_writer ~is_initialization
      else
        (* JSON response *)
        handle_json_response t (resp, body) read_writer ~is_initialization
    | code when code >= 200 && code < 300 ->
      (* Success - handle based on content type *)
      let content_type =
        Cohttp.Response.headers resp |> fun h ->
        Cohttp.Header.get h "content-type" |> Option.value ~default:""
      in

      if String.is_substring content_type ~substring:sse_content_type then
        handle_sse_response t (resp, body) ctx read_writer ~is_initialization
      else handle_json_response t (resp, body) read_writer ~is_initialization
    | code ->
      Logs.err (fun m -> m "Unexpected status code: %d" code);
      Pipe.write read_writer
        (`Error (Error.Streamable_http_error (sprintf "HTTP %d" code))))

(** POST writer task - writes messages to server *)
let post_writer t write_reader read_writer () =
  Logs.debug (fun m -> m "Starting POST writer");

  (* Track whether we've started the GET stream for server-initiated messages *)
  let get_stream_started = ref false in

  let rec loop () =
    match%bind Pipe.read write_reader with
    | `Eof ->
      Logs.debug (fun m -> m "Write stream closed");
      return ()
    | `Ok message ->
      (* Check if this is an initialized notification - start GET stream *)
      if is_initialized_notification message && not !get_stream_started then (
        get_stream_started := true;
        (* Start GET stream in background for server-initiated messages *)
        don't_wait_for
          (Monitor.try_with (fun () -> handle_get_stream t read_writer)
           >>| function
           | Ok () -> ()
           | Error exn ->
             Logs.err (fun m -> m "GET stream error: %s" (Exn.to_string exn))));

      (* Build request context *)
      let ctx =
        {
          headers = prepare_request_headers t t.request_headers;
          session_id = t.session_id;
          session_message = message;
          sse_read_timeout = t.config.sse_read_timeout;
        }
      in

      (* Handle the POST request *)
      let%bind () = handle_post_request t ctx read_writer in

      (* Continue processing messages *)
      loop ()
  in

  Monitor.protect
    ~finally:(fun () ->
      Pipe.close_read write_reader;
      return ())
    (fun () -> loop ())

(** Terminate session *)
let terminate_session t =
  match t.session_id with
  | None ->
    Logs.debug (fun m -> m "No session to terminate");
    return ()
  | Some sid -> (
    Logs.info (fun m -> m "Terminating session: %s" sid);

    (* Send DELETE request to terminate session *)
    let uri = Uri.of_string t.config.url in
    let headers =
      Cohttp.Header.init () |> fun h -> Cohttp.Header.add h mcp_session_id sid
    in

    try_with (fun () ->
        Cohttp_async.Client.delete ~headers uri >>= fun (resp, _body) ->
        let status_code =
          Cohttp.Response.status resp |> Cohttp.Code.code_of_status
        in

        if status_code = 405 then
          Logs.debug (fun m ->
              m "DELETE not supported (405) - session termination optional")
        else if status_code >= 200 && status_code < 300 then
          Logs.info (fun m -> m "Session terminated successfully")
        else
          Logs.warn (fun m ->
              m "Session termination returned status %d" status_code);

        t.session_id <- None;
        return ())
    >>= function
    | Ok () -> return ()
    | Error exn ->
      Logs.err (fun m -> m "Error terminating session: %s" (Exn.to_string exn));
      t.session_id <- None;
      return ())

(** Get current session ID *)
let get_session_id t = t.session_id

(** Create StreamableHTTP transport connection *)
let connect config =
  let read_reader, read_writer = Pipe.create () in
  let write_reader, write_stream = Pipe.create () in
  let request_headers =
    [
      (accept_header, sprintf "%s, %s" json_content_type sse_content_type);
      (content_type_header, json_content_type);
    ]
    @ config.headers
  in
  let t =
    {
      config;
      read_stream = read_reader;
      write_stream;
      session_id = None;
      protocol_version = None;
      request_headers;
    }
  in

  (* Start the POST writer task in background *)
  don't_wait_for
    (Monitor.try_with (fun () -> post_writer t write_reader read_writer ())
     >>| function
     | Ok () -> ()
     | Error exn ->
       Logs.err (fun m -> m "POST writer error: %s" (Exn.to_string exn));
       Pipe.close read_writer);

  t

(** Close StreamableHTTP connection *)
let close t ~terminate_on_close =
  let%bind () = if terminate_on_close then terminate_session t else return () in
  Pipe.close_read t.read_stream;
  Pipe.close t.write_stream;
  return ()

(** Access the read stream *)
let read_stream t = t.read_stream

(** Access the write stream *)
let write_stream t = t.write_stream
