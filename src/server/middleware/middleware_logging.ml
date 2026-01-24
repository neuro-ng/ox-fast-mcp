(** Comprehensive logging middleware for OxFastMCP servers *)

open Core
open Async
open Middleware
open Logging

(** Get duration in milliseconds since start time *)
let get_duration_ms start_time =
  let now = Time_ns.now () in
  let diff = Time_ns.diff now start_time in
  Float.round_decimal ~decimal_digits:2 (Time_ns.Span.to_ms diff)

(** Default payload serializer *)
let default_serializer (payload : Yojson.Safe.t) : string =
  Yojson.Safe.to_string payload

(** Basic logging middleware *)
module Logging_middleware = struct
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    include_payload_length : bool;
    estimate_payload_tokens : bool;
    max_payload_length : int;
    methods : string list option;
    payload_serializer : (Yojson.Safe.t -> string) option;
  }

  let create
      ?(middleware_logger = Logger.get_logger "OxFastMCP.Middleware.Logging")
      ?(log_level = Level.Info) ?(include_payloads = false)
      ?(include_payload_length = false) ?(estimate_payload_tokens = false)
      ?(max_payload_length = 1000) ?(methods = None)
      ?(payload_serializer = None) () =
    {
      middleware_logger;
      log_level;
      include_payloads;
      include_payload_length;
      estimate_payload_tokens;
      max_payload_length;
      methods;
      payload_serializer;
    }

  let serialize_payload t context =
    match t.payload_serializer with
    | Some serializer -> (
      try serializer context.message
      with exn ->
        Logger.warning t.middleware_logger
          (sprintf "Failed to serialize payload due to %s: %s %s"
             (Exn.to_string exn)
             (match context.type_ with
             | `Request -> "request"
             | `Notification -> "notification")
             (Option.value context.method_ ~default:"unknown"));
        default_serializer context.message)
    | None -> default_serializer context.message

  let create_before_message t context =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_start" in
    let parts =
      [
        sprintf "event=%s" event;
        sprintf "method=%s" (Option.value context.method_ ~default:"unknown");
        sprintf "source=%s" source_str;
      ]
    in
    let parts =
      if
        t.include_payloads || t.include_payload_length
        || t.estimate_payload_tokens
      then
        let payload = serialize_payload t context in
        let payload_length = String.length payload in
        let payload_tokens = payload_length / 4 in
        let parts =
          if t.estimate_payload_tokens then
            parts @ [ sprintf "payload_tokens=%d" payload_tokens ]
          else parts
        in
        let parts =
          if t.include_payload_length then
            parts @ [ sprintf "payload_length=%d" payload_length ]
          else parts
        in
        let parts =
          if t.include_payloads then
            let truncated_payload =
              if String.length payload > t.max_payload_length then
                String.prefix payload t.max_payload_length ^ "..."
              else payload
            in
            parts
            @ [
                sprintf "payload=%s" truncated_payload;
                sprintf "payload_type=%s" "message";
              ]
          else parts
        in
        parts
      else parts
    in
    String.concat ~sep:" " parts

  let create_after_message context start_time =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_success" in
    let duration_ms = get_duration_ms start_time in
    String.concat ~sep:" "
      [
        sprintf "event=%s" event;
        sprintf "method=%s" (Option.value context.method_ ~default:"unknown");
        sprintf "source=%s" source_str;
        sprintf "duration_ms=%.2f" duration_ms;
      ]

  let create_error_message context start_time error =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_error" in
    let duration_ms = get_duration_ms start_time in
    String.concat ~sep:" "
      [
        sprintf "event=%s" event;
        sprintf "method=%s" (Option.value context.method_ ~default:"unknown");
        sprintf "source=%s" source_str;
        sprintf "duration_ms=%.2f" duration_ms;
        sprintf "error=%s" (Exn.to_string error);
      ]

  let log_at_level t message =
    match t.log_level with
    | Level.Debug -> Logger.debug t.middleware_logger message
    | Level.Info -> Logger.info t.middleware_logger message
    | Level.Warning -> Logger.warning t.middleware_logger message
    | Level.Error -> Logger.error t.middleware_logger message
    | Level.Critical -> Logger.critical t.middleware_logger message

  let on_message t context call_next =
    let should_log =
      match (t.methods, context.method_) with
      | Some methods, Some method_name ->
        List.mem methods method_name ~equal:String.equal
      | Some _, None -> false
      | None, _ -> true
    in
    if not should_log then call_next context
    else
      let start_time = Time_ns.now () in
      log_at_level t (create_before_message t context);
      Monitor.try_with (fun () -> call_next context) >>= function
      | Ok result ->
        log_at_level t (create_after_message context start_time);
        return result
      | Error exn ->
        Logger.error t.middleware_logger
          (create_error_message context start_time exn);
        raise exn
end

(** Structured JSON logging middleware *)
module Structured_logging = struct
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    include_payload_length : bool;
    estimate_payload_tokens : bool;
    methods : string list option;
    payload_serializer : (Yojson.Safe.t -> string) option;
  }

  let create
      ?(middleware_logger = Logger.get_logger "OxFastMCP.Middleware.Structured")
      ?(log_level = Level.Info) ?(include_payloads = false)
      ?(include_payload_length = false) ?(estimate_payload_tokens = false)
      ?(methods = None) ?(payload_serializer = None) () =
    {
      middleware_logger;
      log_level;
      include_payloads;
      include_payload_length;
      estimate_payload_tokens;
      methods;
      payload_serializer;
    }

  let serialize_payload t context =
    match t.payload_serializer with
    | Some serializer -> (
      try serializer context.message
      with exn ->
        Logger.warning t.middleware_logger
          (sprintf "Failed to serialize payload due to %s" (Exn.to_string exn));
        default_serializer context.message)
    | None -> default_serializer context.message

  let create_before_message t context =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_start" in
    let base_fields =
      [
        ("event", `String event);
        ("method", `String (Option.value context.method_ ~default:"unknown"));
        ("source", `String source_str);
      ]
    in
    let fields =
      if
        t.include_payloads || t.include_payload_length
        || t.estimate_payload_tokens
      then
        let payload = serialize_payload t context in
        let payload_length = String.length payload in
        let payload_tokens = payload_length / 4 in
        let fields =
          if t.estimate_payload_tokens then
            base_fields @ [ ("payload_tokens", `Int payload_tokens) ]
          else base_fields
        in
        let fields =
          if t.include_payload_length then
            fields @ [ ("payload_length", `Int payload_length) ]
          else fields
        in
        let fields =
          if t.include_payloads then
            fields
            @ [
                ("payload", `String payload); ("payload_type", `String "message");
              ]
          else fields
        in
        fields
      else base_fields
    in
    `Assoc fields

  let create_after_message context start_time =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_success" in
    let duration_ms = get_duration_ms start_time in
    `Assoc
      [
        ("event", `String event);
        ("method", `String (Option.value context.method_ ~default:"unknown"));
        ("source", `String source_str);
        ("duration_ms", `Float duration_ms);
      ]

  let create_error_message context start_time error =
    let source_str =
      match context.source with
      | `Client -> "client"
      | `Server -> "server"
    in
    let type_str =
      match context.type_ with
      | `Request -> "request"
      | `Notification -> "notification"
    in
    let event = type_str ^ "_error" in
    let duration_ms = get_duration_ms start_time in
    `Assoc
      [
        ("event", `String event);
        ("method", `String (Option.value context.method_ ~default:"unknown"));
        ("source", `String source_str);
        ("duration_ms", `Float duration_ms);
        ("error", `String (Exn.to_string error));
      ]

  let log_at_level t message =
    let msg = Yojson.Safe.to_string message in
    match t.log_level with
    | Level.Debug -> Logger.debug t.middleware_logger msg
    | Level.Info -> Logger.info t.middleware_logger msg
    | Level.Warning -> Logger.warning t.middleware_logger msg
    | Level.Error -> Logger.error t.middleware_logger msg
    | Level.Critical -> Logger.critical t.middleware_logger msg

  let on_message t context call_next =
    let should_log =
      match (t.methods, context.method_) with
      | Some methods, Some method_name ->
        List.mem methods method_name ~equal:String.equal
      | Some _, None -> false
      | None, _ -> true
    in
    if not should_log then call_next context
    else
      let start_time = Time_ns.now () in
      log_at_level t (create_before_message t context);
      Monitor.try_with (fun () -> call_next context) >>= function
      | Ok result ->
        log_at_level t (create_after_message context start_time);
        return result
      | Error exn ->
        Logger.error t.middleware_logger
          (Yojson.Safe.to_string (create_error_message context start_time exn));
        raise exn
end
