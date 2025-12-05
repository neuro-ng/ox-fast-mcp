(** Comprehensive logging middleware for OxFastMCP servers *)

open Core
open Async
open Middleware
open Logging

(** Basic logging middleware *)
module Logging_middleware = struct
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    max_payload_length : int;
    methods : string list option;
  }

  let create ?(middleware_logger = Logger.get_logger "OxFastMCP.Requests")
      ?(log_level = Level.Info) ?(include_payloads = false)
      ?(max_payload_length = 1000) ?(methods = None) () =
    { middleware_logger; log_level; include_payloads; max_payload_length; methods }

  let format_message t context =
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
    let parts =
      [
        sprintf "source=%s" source_str;
        sprintf "type=%s" type_str;
        sprintf "method=%s" (Option.value context.method_ ~default:"unknown");
      ]
    in
    let parts =
      if t.include_payloads then
        match Yojson.Safe.pretty_to_string context.message with
        | payload when String.length payload > t.max_payload_length ->
          parts
          @ [
              sprintf "payload=%s..."
                (String.prefix payload t.max_payload_length);
            ]
        | payload -> parts @ [ sprintf "payload=%s" payload ]
      else parts
    in
    String.concat ~sep:" " parts

  let log_at_level t message =
    match t.log_level with
    | Level.Debug -> Logger.debug t.middleware_logger message
    | Level.Info -> Logger.info t.middleware_logger message
    | Level.Warning -> Logger.warning t.middleware_logger message
    | Level.Error -> Logger.error t.middleware_logger message
    | Level.Critical -> Logger.critical t.middleware_logger message

  let on_message t context call_next =
    let%bind () =
      match (t.methods, context.method_) with
      | Some methods, Some method_name
        when not (List.mem methods method_name ~equal:String.equal) -> return ()
      | _, _ ->
        let message_info = format_message t context in
        log_at_level t (sprintf "Processing message: %s" message_info);
        return ()
    in
    Monitor.try_with (fun () -> call_next context) >>= function
    | Ok result ->
      log_at_level t (sprintf "Completed message: %s" 
        (Option.value context.method_ ~default:"unknown"));
      return result
    | Error exn ->
      Logger.error t.middleware_logger 
        (sprintf "Failed message: %s - %s" 
           (Option.value context.method_ ~default:"unknown")
           (Exn.to_string exn));
      raise exn
end

(** Structured JSON logging middleware *)
module Structured_logging = struct
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    methods : string list option;
  }

  let create ?(middleware_logger = Logger.get_logger "OxFastMCP.Structured")
      ?(log_level = Level.Info) ?(include_payloads = false) ?(methods = None) () =
    { middleware_logger; log_level; include_payloads; methods }

  let create_log_entry t context event extra_fields =
    let base_fields =
      [
        ("event", `String event);
        ("timestamp", `String (Time_ns.to_string context.timestamp));
        ( "source",
          `String
            (match context.source with
            | `Client -> "client"
            | `Server -> "server") );
        ( "type",
          `String
            (match context.type_ with
            | `Request -> "request"
            | `Notification -> "notification") );
        ("method", `String (Option.value context.method_ ~default:"unknown"));
      ]
    in
    let fields =
      if t.include_payloads then base_fields @ [ ("payload", context.message) ]
      else base_fields
    in
    `Assoc (fields @ extra_fields)

  let log_at_level t message =
    match t.log_level with
    | Level.Debug -> Logger.debug t.middleware_logger message
    | Level.Info -> Logger.info t.middleware_logger message
    | Level.Warning -> Logger.warning t.middleware_logger message
    | Level.Error -> Logger.error t.middleware_logger message
    | Level.Critical -> Logger.critical t.middleware_logger message

  let on_message t context call_next =
    let%bind () =
      match (t.methods, context.method_) with
      | Some methods, Some method_name
        when not (List.mem methods method_name ~equal:String.equal) -> return ()
      | _ ->
        let start_entry = create_log_entry t context "request_start" [] in
        log_at_level t (Yojson.Safe.to_string start_entry);
        return ()
    in
    Monitor.try_with (fun () -> call_next context) >>= function
    | Ok result ->
      let success_entry =
        create_log_entry t context "request_success"
          [ ("result_type", `String "success") ]
      in
      log_at_level t (Yojson.Safe.to_string success_entry);
      return result
    | Error exn ->
      let error_entry =
        create_log_entry t context "request_error"
          [
            ("error_type", `String (Exn.to_string exn));
            ("error_message", `String (Exn.to_string exn));
          ]
      in
      Logger.error t.middleware_logger (Yojson.Safe.to_string error_entry);
      raise exn
end
