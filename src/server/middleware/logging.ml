open Core
open Async
open Middleware

(* Simple logger type for middleware *)
type logger_t = {
  name : string;
  level : [ `Debug | `Info | `Warning | `Error | `Critical ];
}

module Logging = struct
  type t = {
    logger : logger_t;
    log_level : [ `Debug | `Info | `Warning | `Error | `Critical ];
    include_payloads : bool;
    max_payload_length : int;
    methods : string list option;
  }

  let create ?(logger = { name = "fastmcp.requests"; level = `Info })
      ?(log_level = `Info) ?(include_payloads = false)
      ?(max_payload_length = 1000) ?(methods = None) () =
    { logger; log_level; include_payloads; max_payload_length; methods }

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

  let on_message t context call_next =
    let%bind () =
      match (t.methods, context.method_) with
      | Some methods, Some method_name
        when not (List.mem methods method_name ~equal:String.equal) -> return ()
      | _, _ ->
        let message_info = format_message t context in
        (* Logger.log t.logger t.log_level (fun () -> sprintf "Processing
           message: %s" message_info); *)
        ignore message_info;
        return ()
    in
    Monitor.try_with (fun () -> call_next context) >>= function
    | Ok result ->
      (* Logger.log t.logger t.log_level (fun () -> sprintf "Completed message:
         %s" (Option.value context.method_ ~default:"unknown")); *)
      return result
    | Error exn ->
      (* Logger.log t.logger Logger.Level.Error (fun () -> sprintf "Failed
         message: %s - %s" (Option.value context.method_ ~default:"unknown")
         (Exn.to_string exn)); *)
      raise exn
end

module Structured_logging = struct
  type t = {
    logger : logger_t;
    log_level : [ `Debug | `Info | `Warning | `Error | `Critical ];
    include_payloads : bool;
    methods : string list option;
  }

  let create ?(logger = { name = "fastmcp.structured"; level = `Info })
      ?(log_level = `Info) ?(include_payloads = false) ?(methods = None) () =
    { logger; log_level; include_payloads; methods }

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

  let on_message t context call_next =
    let%bind () =
      match (t.methods, context.method_) with
      | Some methods, Some method_name
        when not (List.mem methods method_name ~equal:String.equal) -> return ()
      | _ ->
        let start_entry = create_log_entry t context "request_start" [] in
        (* Logger.log t.logger t.log_level (fun () -> Yojson.Safe.to_string
           start_entry); *)
        ignore start_entry;
        return ()
    in
    Monitor.try_with (fun () -> call_next context) >>= function
    | Ok result ->
      let success_entry =
        create_log_entry t context "request_success"
          [ ("result_type", `String "success") ]
      in
      (* Logger.log t.logger t.log_level (fun () -> Yojson.Safe.to_string
         success_entry); *)
      ignore success_entry;
      return result
    | Error exn ->
      let error_entry =
        create_log_entry t context "request_error"
          [
            ("error_type", `String (Exn.to_string exn));
            ("error_message", `String (Exn.to_string exn));
          ]
      in
      (* Logger.log t.logger Logger.Level.Error (fun () -> Yojson.Safe.to_string
         error_entry); *)
      ignore error_entry;
      raise exn
end
