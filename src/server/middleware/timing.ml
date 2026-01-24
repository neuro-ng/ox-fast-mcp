(** Timing middleware for monitoring MCP operation performance *)

open Core
open Async
open Middleware
open Logging

type timing_config = {
  logger : Logger.t;
  log_level : Logs.level;
  detailed : bool;  (** Enable detailed operation logging *)
}

let create ?(logger = Logger.get_logger "OxFastMCP.Timing")
    ?(log_level = Logs.Info) ?(detailed = false) () =
  { logger; log_level; detailed }

(** Extract operation name from context for detailed logging *)
let get_operation_name context =
  match context.method_ with
  | Some "tools/call" -> (
    (* Try to extract tool name from params *)
    match context.params with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "name" with
      | Some (`String name) -> sprintf "Tool '%s'" name
      | _ -> "Tool call")
    | _ -> "Tool call")
  | Some "resources/read" -> (
    (* Try to extract resource URI from params *)
    match context.params with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "uri" with
      | Some (`String uri) -> sprintf "Resource '%s'" uri
      | _ -> "Resource read")
    | _ -> "Resource read")
  | Some "prompts/get" -> (
    (* Try to extract prompt name from params *)
    match context.params with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "name" with
      | Some (`String name) -> sprintf "Prompt '%s'" name
      | _ -> "Prompt get")
    | _ -> "Prompt get")
  | Some "tools/list" -> "List tools"
  | Some "resources/list" -> "List resources"
  | Some "resources/templates/list" -> "List resource templates"
  | Some "prompts/list" -> "List prompts"
  | Some method_name -> sprintf "Request %s" method_name
  | None -> "Unknown request"

(** Time an operation and log the duration *)
let time_operation config context call_next =
  let start_time = Time_ns.now () in
  let operation_name =
    if config.detailed then get_operation_name context
    else Option.value context.method_ ~default:"unknown"
  in

  Monitor.try_with (fun () -> call_next context) >>= function
  | Ok result ->
    let duration_ns = Time_ns.diff (Time_ns.now ()) start_time in
    let duration_ms = Time_ns.Span.to_ms duration_ns in
    Logger.info config.logger
      (sprintf "%s completed in %.2fms" operation_name duration_ms);
    return result
  | Error exn ->
    let duration_ns = Time_ns.diff (Time_ns.now ()) start_time in
    let duration_ms = Time_ns.Span.to_ms duration_ns in
    Logger.warning config.logger
      (sprintf "%s failed after %.2fms: %s" operation_name duration_ms
         (Exn.to_string exn));
    raise exn

let on_message config context call_next =
  time_operation config context call_next

(** Middleware module implementing the Middleware.S interface *)
module Timing : Middleware.S = struct
  type t = timing_config

  let create () =
    {
      logger = Logger.get_logger "OxFastMCP.Timing";
      log_level = Logs.Info;
      detailed = false;
    }

  let on_message = on_message
  let on_request config context call_next = on_message config context call_next

  let on_notification config context call_next =
    on_message config context call_next

  let on_call_tool config context call_next =
    on_message config context call_next

  let on_read_resource config context call_next =
    on_message config context call_next

  let on_get_prompt config context call_next =
    on_message config context call_next

  let on_list_tools config context call_next =
    on_message config context call_next

  let on_list_resources config context call_next =
    on_message config context call_next

  let on_list_resource_templates config context call_next =
    on_message config context call_next

  let on_list_prompts config context call_next =
    on_message config context call_next

  let dispatch_handler _config _context call_next =
    (* Just return the call_next as is, timing happens in on_message *)
    return call_next

  let call config context call_next =
    let%bind handler = dispatch_handler config context call_next in
    on_message config context handler
end
