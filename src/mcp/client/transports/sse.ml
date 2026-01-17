open Core
open Async
module Types = Mcp.Types

(** SSE client transport for OxFastMCP.

    Provides client-side Server-Sent Events (SSE) transport for connecting to
    MCP servers over HTTP. *)

(** Remove query parameters from URL, keeping only the path *)
let remove_request_params url =
  match Uri.of_string url with
  | uri -> Uri.to_string (Uri.with_query uri [])

(** Validate that endpoint URL matches the connection origin *)
let validate_endpoint_origin ~connection_url ~endpoint_url =
  let conn_uri = Uri.of_string connection_url in
  let endpoint_uri = Uri.of_string endpoint_url in
  let conn_scheme = Uri.scheme conn_uri in
  let endpoint_scheme = Uri.scheme endpoint_uri in
  let conn_host = Uri.host conn_uri in
  let endpoint_host = Uri.host endpoint_uri in
  match (conn_scheme, endpoint_scheme, conn_host, endpoint_host) with
  | Some cs, Some es, Some ch, Some eh
    when String.equal cs es && String.equal ch eh -> Or_error.return ()
  | _ ->
    Or_error.error_string
      (sprintf "Endpoint origin does not match connection origin: %s vs %s"
         endpoint_url connection_url)

type sse_event = { event : string; data : string } [@@deriving sexp_of]
(** SSE event types *)

type config = {
  url : string;
  headers : (string * string) list option; [@default None]
  timeout : Time_float.Span.t; [@default Time_float.Span.of_sec 5.]
  sse_read_timeout : Time_float.Span.t; [@default Time_float.Span.of_sec 300.]
}
[@@deriving sexp_of, fields ~getters]
(** SSE client configuration *)

(** Create default SSE client configuration *)
let create_config ~url ?headers ?timeout ?sse_read_timeout () =
  {
    url;
    headers;
    timeout = Option.value timeout ~default:(Time_float.Span.of_sec 5.);
    sse_read_timeout =
      Option.value sse_read_timeout ~default:(Time_float.Span.of_sec 300.);
  }

type t = {
  config : config;
  read_stream :
    [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t;
  write_stream : Types.jsonrpc_message Pipe.Writer.t;
  mutable endpoint_url : string option;
  mutable is_connected : bool;
}
(** SSE connection state *)

(** Parse SSE event from string *)
let parse_sse_event line =
  match String.split line ~on:':' with
  | [] -> None
  | [ event ] -> Some { event = String.strip event; data = "" }
  | event :: data_parts ->
    Some
      {
        event = String.strip event;
        data = String.strip (String.concat ~sep:":" data_parts);
      }
[@@warning "-32"]
(* Unused in skeleton implementation *)

(** SSE reader task - reads SSE events and processes them *)
let sse_reader ~connection_url ~read_writer ~endpoint_ivar ~headers () =
  Logs.info (fun m ->
      m "Connecting to SSE endpoint: %s" (remove_request_params connection_url));

  (* Prepare headers for SSE connection *)
  let uri = Uri.of_string connection_url in
  let cohttp_headers =
    let base_headers =
      Cohttp.Header.init () |> fun h ->
      Cohttp.Header.add h "Accept" "text/event-stream"
    in
    List.fold headers ~init:base_headers ~f:(fun h (k, v) ->
        Cohttp.Header.add h k v)
  in

  (* Establish HTTP GET connection for SSE stream *)
  Monitor.try_with (fun () ->
      let%bind resp, body =
        Cohttp_async.Client.get ~headers:cohttp_headers uri
      in
      let status = Cohttp.Response.status resp in

      (* Check response status *)
      if Cohttp.Code.code_of_status status <> 200 then (
        let error_msg =
          sprintf "SSE connection failed with status %d"
            (Cohttp.Code.code_of_status status)
        in
        Logs.err (fun m -> m "%s" error_msg);
        raise_s [%message "SSE connection error" error_msg])
      else (
        Logs.debug (fun m -> m "SSE connection established");

        (* Parse SSE events from HTTP body stream *)
        let event_stream = Sse_protocol.Parser.parse_stream body in

        (* Process SSE events *)
        Pipe.iter event_stream ~f:(fun event ->
            Logs.debug (fun m ->
                m "Received SSE event: type=%s"
                  event.Sse_protocol.Event.event_type);

            (* Handle different event types *)
            match event.event_type with
            | "endpoint" -> (
              (* Extract and validate endpoint URL *)
              let endpoint = event.data in
              match
                validate_endpoint_origin ~connection_url ~endpoint_url:endpoint
              with
              | Ok () ->
                Logs.info (fun m -> m "Endpoint URL received: %s" endpoint);
                if Ivar.is_empty endpoint_ivar then
                  Ivar.fill_exn endpoint_ivar (Ok endpoint);
                return ()
              | Error err ->
                Logs.err (fun m ->
                    m "Invalid endpoint URL: %s" (Error.to_string_hum err));
                if Ivar.is_empty endpoint_ivar then
                  Ivar.fill_exn endpoint_ivar (Error err);
                return ())
            | "message" -> (
              (* Parse JSON-RPC message from event data *)
              match Sse_protocol.Event.parse_jsonrpc event with
              | Ok jsonrpc_msg ->
                Logs.debug (fun m -> m "Parsed JSON-RPC message from SSE");
                Pipe.write read_writer (`Message jsonrpc_msg)
              | Error err ->
                Logs.err (fun m ->
                    m "Failed to parse JSON-RPC: %s" (Error.to_string_hum err));
                Pipe.write read_writer (`Error err))
            | _ ->
              (* Ignore unknown event types *)
              Logs.debug (fun m ->
                  m "Ignoring unknown SSE event type: %s" event.event_type);
              return ())))
  >>= function
  | Ok () ->
    Logs.info (fun m -> m "SSE reader completed");
    return ()
  | Error exn ->
    let error_msg = sprintf "SSE reader error: %s" (Exn.to_string exn) in
    Logs.err (fun m -> m "%s" error_msg);
    if Ivar.is_empty endpoint_ivar then
      Ivar.fill_exn endpoint_ivar (Or_error.error_string error_msg);
    Pipe.write read_writer (`Error (Error.of_exn exn))

(** POST writer task - writes messages to endpoint *)
let post_writer ~endpoint_url ~write_reader ~headers ~timeout () =
  Logs.debug (fun m ->
      m "Starting post writer with endpoint URL: %s" endpoint_url);

  let rec loop () =
    match%bind Pipe.read write_reader with
    | `Eof ->
      Logs.debug (fun m -> m "Write stream closed");
      return ()
    | `Ok message -> (
      Logs.debug (fun m ->
          m "Sending client message: %s"
            (Yojson.Safe.to_string (Types.jsonrpc_message_to_yojson message)));

      (* Serialize message to JSON *)
      let json = Types.jsonrpc_message_to_yojson message in
      let body_str = Yojson.Safe.to_string json in

      (* Prepare headers *)
      let uri = Uri.of_string endpoint_url in
      let cohttp_headers =
        let base_headers =
          Cohttp.Header.init () |> fun h ->
          Cohttp.Header.add h "Content-Type" "application/json"
        in
        List.fold headers ~init:base_headers ~f:(fun h (k, v) ->
            Cohttp.Header.add h k v)
      in

      (* Send POST request with timeout *)
      let post_deferred =
        Monitor.try_with (fun () ->
            Cohttp_async.Client.post ~body:(`String body_str)
              ~headers:cohttp_headers uri)
      in

      match%bind Clock.with_timeout timeout post_deferred with
      | `Timeout ->
        Logs.err (fun m ->
            m "POST request timeout after %s"
              (Time_float.Span.to_string timeout));
        loop ()
      | `Result (Ok (resp, _body)) ->
        let status_code =
          Cohttp.Response.status resp |> Cohttp.Code.code_of_status
        in
        if status_code >= 200 && status_code < 300 then (
          Logs.debug (fun m ->
              m "Client message sent successfully (status %d)" status_code);
          loop ())
        else (
          Logs.err (fun m -> m "POST request failed with status %d" status_code);
          loop ())
      | `Result (Error exn) ->
        Logs.err (fun m -> m "POST request error: %s" (Exn.to_string exn));
        loop ())
  in
  Monitor.protect
    ~finally:(fun () ->
      Pipe.close_read write_reader;
      return ())
    (fun () -> loop ())

(** Create SSE client connection

    Returns (read_stream, write_stream) where:
    - read_stream provides messages from server (or errors)
    - write_stream accepts messages to send to server *)
let connect config =
  let read_reader, read_writer = Pipe.create () in
  let write_reader, write_stream = Pipe.create () in
  let endpoint_ivar = Ivar.create () in
  let t =
    {
      config;
      read_stream = read_reader;
      write_stream;
      endpoint_url = None;
      is_connected = false;
    }
  in
  (* Extract headers from config *)
  let headers = Option.value config.headers ~default:[] in

  (* Start SSE reader task *)
  don't_wait_for
    (Monitor.try_with (fun () ->
         sse_reader ~connection_url:config.url ~read_writer ~endpoint_ivar
           ~headers ())
     >>| function
     | Ok () -> ()
     | Error exn ->
       Logs.err (fun m -> m "SSE reader error: %s" (Exn.to_string exn));
       Pipe.close read_writer);
  (* Wait for endpoint URL and start writer *)
  don't_wait_for
    (let%bind endpoint_result = Ivar.read endpoint_ivar in
     match endpoint_result with
     | Ok endpoint_url -> (
       t.endpoint_url <- Some endpoint_url;
       t.is_connected <- true;
       Monitor.try_with (fun () ->
           post_writer ~endpoint_url ~write_reader ~headers
             ~timeout:config.timeout ())
       >>| function
       | Ok () -> ()
       | Error exn ->
         Logs.err (fun m -> m "POST writer error: %s" (Exn.to_string exn));
         Pipe.close_read write_reader)
     | Error err ->
       Logs.err (fun m ->
           m "Endpoint validation failed: %s" (Error.to_string_hum err));
       Pipe.close read_writer;
       Pipe.close_read write_reader;
       return ());
  t

(** Close SSE connection *)
let close t =
  t.is_connected <- false;
  Pipe.close_read t.read_stream;
  Pipe.close t.write_stream;
  return ()

(** Check if SSE client is connected *)
let is_connected t = t.is_connected

(** Get endpoint URL *)
let endpoint_url t = t.endpoint_url

(** Access the read stream *)
let read_stream t = t.read_stream

(** Access the write stream *)
let write_stream t = t.write_stream
