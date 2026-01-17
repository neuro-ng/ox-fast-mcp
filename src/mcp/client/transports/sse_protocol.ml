open Core
open Async
module Types = Mcp.Types

(** SSE Protocol Parser for cohttp-async

    Implements RFC 8388 Server-Sent Events protocol parsing. Designed to work
    with cohttp-async streaming HTTP responses. *)

(** {1 SSE Event Type} *)

module Event = struct
  type t = {
    event_type : string; (* Event type, default "message" *)
    data : string; (* Event data payload, may be multi-line *)
    id : string option; (* Event ID for resumption tokens *)
    retry : int option; (* Reconnection time in milliseconds *)
  }
  [@@deriving sexp_of]

  (** Create a default event *)
  let create ?(event_type = "message") ?(id = None) ?(retry = None) data =
    { event_type; data; id; retry }

  (** Parse event data as JSON-RPC message

      JSON-RPC 2.0 messages are distinguished by their fields:
      - Response: has 'result' field
      - Error: has 'error' field
      - Request: has 'id' and 'method' fields
      - Notification: has 'method' but no 'id' field

      We handle manual parsing of request_id since ppx_yojson_conv doesn't
      correctly handle polymorphic variants for raw JSON integers/strings. *)
  let parse_jsonrpc t =
    try
      let json = Yojson.Safe.from_string t.data in
      let open Yojson.Safe.Util in
      (* Helper to parse request_id manually *)
      let parse_request_id_field name =
        match member name json with
        | `Int i -> `Int i
        | `String s -> `String s
        | _ -> failwith (sprintf "Invalid request_id in field '%s'" name)
      in

      (* Helper to check if field exists *)
      let has_field name =
        match member name json with
        | `Null -> false
        | _ -> true
      in

      (* Determine message type based on fields present *)
      if has_field "result" then
        (* This is a response *)
        let jsonrpc = member "jsonrpc" json |> to_string in
        let id = parse_request_id_field "id" in
        let result = member "result" json in
        Ok (`Response { Types.jsonrpc; id; result })
      else if has_field "error" then
        (* This is an error response *)
        let jsonrpc = member "jsonrpc" json |> to_string in
        let id = parse_request_id_field "id" in
        let error = Types.error_data_of_yojson (member "error" json) in
        Ok (`Error { Types.jsonrpc; id; error })
      else if has_field "method" then
        (* Has method field - either request or notification *)
        let method_ = member "method" json |> to_string in
        let jsonrpc = member "jsonrpc" json |> to_string in
        let params =
          match member "params" json with
          | `Null -> None
          | p -> Some p
        in

        if has_field "id" then
          (* Request: has both id and method *)
          let id = parse_request_id_field "id" in
          Ok (`Request { Types.jsonrpc; id; method_; params })
        else
          (* Notification: has method but no id *)
          Ok (`Notification { Types.jsonrpc; method_; params })
      else
        Or_error.error_string
          "Invalid JSON-RPC message: missing required fields"
    with
    | Yojson.Json_error msg ->
      Or_error.error_string (sprintf "JSON parse error: %s" msg)
    | exn ->
      Or_error.error_string
        (sprintf "Failed to parse JSON-RPC: %s" (Exn.to_string exn))
end

(** {1 SSE Parser} *)

module Parser = struct
  type state = {
    mutable event_type : string;
    mutable data_lines : string list;
    mutable id : string option;
    mutable retry : int option;
  }
  (** Parser state accumulating fields until a complete event *)

  type t = { mutable state : state }

  (** Create a new SSE parser with initial state *)
  let create () =
    {
      state =
        { event_type = "message"; data_lines = []; id = None; retry = None };
    }

  (** Reset parser state for next event *)
  let reset_state t =
    t.state <-
      { event_type = "message"; data_lines = []; id = None; retry = None }

  (** Build event from accumulated state *)
  let build_event state =
    let data = String.concat ~sep:"\n" (List.rev state.data_lines) in
    Event.create ~event_type:state.event_type ~id:state.id ~retry:state.retry
      data

  (** Process a single SSE field *)
  let process_field state field_name field_value =
    match field_name with
    | "event" -> state.event_type <- String.strip field_value
    | "data" ->
      (* Accumulate data lines, SSE allows multiple data: fields *)
      state.data_lines <- field_value :: state.data_lines
    | "id" ->
      let stripped = String.strip field_value in
      if not (String.is_empty stripped) then state.id <- Some stripped
    | "retry" -> (
      try state.retry <- Some (Int.of_string (String.strip field_value))
      with _ -> Logs.warn (fun m -> m "Invalid retry value: %s" field_value))
    | _ ->
      (* Ignore unknown fields per SSE spec *)
      Logs.debug (fun m -> m "Ignoring unknown SSE field: %s" field_name)

  (** Feed a single line to the parser, returns completed events

      Returns a list because a blank line completes an event, so we might have 0
      or 1 events per line. *)
  let feed_line t line =
    (* Blank line signals end of event *)
    if String.is_empty (String.strip line) then (
      if List.is_empty t.state.data_lines then
        (* Blank line with no accumulated data - ignore *)
        []
      else
        let event = build_event t.state in
        reset_state t;
        [ event ])
    else if String.is_prefix line ~prefix:":" then
      (* Comment line - ignore per SSE spec *)
      []
    else
      match String.lsplit2 line ~on:':' with
      | Some (field_name, field_value) ->
        (* Field has format "name: value" or "name:value" *)
        let field_value =
          (* Remove leading space if present (SSE spec allows "name: value") *)
          if String.is_prefix field_value ~prefix:" " then
            String.drop_prefix field_value 1
          else field_value
        in
        process_field t.state field_name field_value;
        []
      | None ->
        (* Field has format "name" with no colon - treat as "name:" with empty
           value *)
        process_field t.state line "";
        []

  (** Parse a streaming HTTP body into SSE events

      Takes a cohttp-async Body.t and returns a Pipe of parsed events. This is
      the main entry point for SSE parsing. *)
  let parse_stream (body : Cohttp_async.Body.t) : Event.t Pipe.Reader.t =
    let event_reader, event_writer = Pipe.create () in
    let parser = create () in

    (* Convert body to line stream *)
    let body_pipe = Cohttp_async.Body.to_pipe body in

    (* Process body chunks line by line *)
    don't_wait_for
      (Monitor.protect
         ~finally:(fun () ->
           Pipe.close event_writer;
           return ())
         (fun () ->
           let line_buffer = Buffer.create 256 in
           Pipe.iter body_pipe ~f:(fun chunk ->
               (* Add chunk to buffer and process complete lines *)
               Buffer.add_string line_buffer chunk;
               let buffered = Buffer.contents line_buffer in
               let lines = String.split_lines buffered in

               (* Check if last line is complete (ends with newline) *)
               let has_trailing_newline = String.is_suffix chunk ~suffix:"\n" in

               let complete_lines, incomplete =
                 if has_trailing_newline then (lines, "")
                 else
                   match List.rev lines with
                   | [] -> ([], "")
                   | last :: rest -> (List.rev rest, last)
               in

               (* Clear buffer and add back incomplete line *)
               Buffer.clear line_buffer;
               if not (String.is_empty incomplete) then
                 Buffer.add_string line_buffer incomplete;

               (* Process complete lines *)
               Deferred.List.iter complete_lines ~how:`Sequential
                 ~f:(fun line ->
                   let events = feed_line parser line in
                   Deferred.List.iter events ~how:`Sequential ~f:(fun event ->
                       Pipe.write event_writer event)))));

    event_reader
end
