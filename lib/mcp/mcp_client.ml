open Core
open Async
open Types

module Transport = struct
  type t = {
    send : jsonrpc_message -> unit Deferred.t;
    receive : unit -> jsonrpc_message Deferred.t;
    close : unit -> unit Deferred.t;
  }

  let create ~send ~receive ~close = { send; receive; close }
end

type t = {
  transport : Transport.t;
  mutable initialized : bool;
  mutable server_info : implementation option;
  mutable server_capabilities : server_capabilities option;
  mutable protocol_version : string option;
}

let create transport = {
  transport;
  initialized = false;
  server_info = None;
  server_capabilities = None;
  protocol_version = None;
}

let close t = t.transport.close ()

let send_request t ~id ~method_ ~params =
  let request = {
    jsonrpc = "2.0";
    id;
    method_;
    params;
  } in
  t.transport.send (`Request request)

let send_notification t ~method_ ~params =
  let notification = {
    jsonrpc = "2.0";
    method_;
    params;
  } in
  t.transport.send (`Notification notification)

let receive t = t.transport.receive ()

let initialize t ~client_info ~capabilities =
  if t.initialized then
    return (Error (`Invalid_request "Client already initialized"))
  else
    let params = {
      protocol_version = latest_protocol_version;
      capabilities;
      client_info;
    } in
    let%bind result = send_request t ~id:(`Int 0) ~method_:"initialize" ~params:(Some (initialize_request_params_to_yojson params)) in
    match result with
    | `Response { result; _ } -> (
      match initialize_result_of_yojson result with
      | Ok init_result ->
        t.initialized <- true;
        t.server_info <- Some init_result.server_info;
        t.server_capabilities <- Some init_result.capabilities;
        t.protocol_version <- Some init_result.protocol_version;
        return (Ok init_result)
      | Error msg -> return (Error (`Parse_error msg))
    )
    | `Error err -> return (Error (Mcp_error.of_error_data err.error))
    | _ -> return (Error (`Invalid_request "Unexpected response type"))

let initialized t =
  if not t.initialized then
    return (Error (`Invalid_request "Client not initialized"))
  else
    send_notification t ~method_:"initialized" ~params:None

let ping t =
  send_request t ~id:(`Int 1) ~method_:"ping" ~params:None

let set_level t ~level =
  let params = { level } in
  send_request t ~id:(`Int 2) ~method_:"logging/setLevel" ~params:(Some (set_level_request_params_to_yojson params))

let list_resources t ?cursor () =
  let params = match cursor with
    | None -> None
    | Some cursor -> Some { meta = None; cursor = Some cursor }
  in
  send_request t ~id:(`Int 3) ~method_:"resources/list" ~params:(Option.map params ~f:paginated_request_params_to_yojson)

let read_resource t ~uri =
  let params = { uri } in
  send_request t ~id:(`Int 4) ~method_:"resources/read" ~params:(Some (read_resource_result_to_yojson params))

let list_tools t ?cursor () =
  let params = match cursor with
    | None -> None
    | Some cursor -> Some { meta = None; cursor = Some cursor }
  in
  send_request t ~id:(`Int 5) ~method_:"tools/list" ~params:(Option.map params ~f:paginated_request_params_to_yojson)

let call_tool t ~name ~arguments =
  let params = {
    name;
    arguments;
  } in
  send_request t ~id:(`Int 6) ~method_:"tools/call" ~params:(Some (call_tool_request_params_to_yojson params))

let list_prompts t ?cursor () =
  let params = match cursor with
    | None -> None
    | Some cursor -> Some { meta = None; cursor = Some cursor }
  in
  send_request t ~id:(`Int 7) ~method_:"prompts/list" ~params:(Option.map params ~f:paginated_request_params_to_yojson)

let get_prompt t ~name ?arguments () =
  let params = {
    name;
    arguments;
  } in
  send_request t ~id:(`Int 8) ~method_:"prompts/get" ~params:(Some (get_prompt_request_params_to_yojson params))

let list_roots t =
  send_request t ~id:(`Int 9) ~method_:"roots/list" ~params:None

let create_message t ~messages ~model_preferences ~system_prompt ~include_context ~temperature ~max_tokens ~stop_sequences ~metadata =
  let params = {
    messages;
    model_preferences;
    system_prompt;
    include_context;
    temperature;
    max_tokens;
    stop_sequences;
    metadata;
  } in
  send_request t ~id:(`Int 10) ~method_:"sampling/createMessage" ~params:(Some (create_message_request_params_to_yojson params))

let elicit t ~message ~requested_schema =
  let params = {
    message;
    requested_schema;
  } in
  send_request t ~id:(`Int 11) ~method_:"elicitation/create" ~params:(Some (elicit_request_params_to_yojson params))

let subscribe t ~uri =
  let params = { uri } in
  send_request t ~id:(`Int 9) ~method_:"resources/subscribe" ~params:(Some (subscribe_request_params_to_yojson params))

let unsubscribe t ~uri =
  let params = { uri } in
  send_request t ~id:(`Int 10) ~method_:"resources/unsubscribe" ~params:(Some (unsubscribe_request_params_to_yojson params))
