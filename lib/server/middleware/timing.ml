open Core
open Lwt.Syntax
open Cohttp_lwt_unix

(* Timing middleware implementation *)
module TimingMiddleware = struct
  type t = {
    logger : Logs.src;
    log_level : Logs.level;
  }

  let create ?(logger = Logs.Src.create "fastmcp.timing") ?(log_level = Logs.Info) () = {
    logger;
    log_level;
  }

  let middleware t handler req body =
    let method_name = req |> Request.meth |> Code.string_of_method in
    let start_time = Unix.gettimeofday () in
    
    Lwt.catch
      (fun () ->
        let* (response, body') = handler req body in
        let duration_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
        Logs.msg ~src:t.logger t.log_level
          (fun m -> m "Request %s completed in %.2fms" method_name duration_ms);
        Lwt.return (response, body'))
      (fun exn ->
        let duration_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
        Logs.msg ~src:t.logger t.log_level
          (fun m -> m "Request %s failed after %.2fms: %s" 
            method_name duration_ms (Exn.to_string exn));
        Lwt.fail exn)
end

(* Detailed timing middleware implementation *)
module DetailedTimingMiddleware = struct
  type t = {
    logger : Logs.src;
    log_level : Logs.level;
  }

  let create ?(logger = Logs.Src.create "fastmcp.timing.detailed") ?(log_level = Logs.Info) () = {
    logger;
    log_level;
  }

  let time_operation t operation_name handler req body =
    let start_time = Unix.gettimeofday () in
    
    Lwt.catch
      (fun () ->
        let* (response, body') = handler req body in
        let duration_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
        Logs.msg ~src:t.logger t.log_level
          (fun m -> m "%s completed in %.2fms" operation_name duration_ms);
        Lwt.return (response, body'))
      (fun exn ->
        let duration_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
        Logs.msg ~src:t.logger t.log_level
          (fun m -> m "%s failed after %.2fms: %s" 
            operation_name duration_ms (Exn.to_string exn));
        Lwt.fail exn)

  let middleware t handler req body =
    let operation_name = match Request.meth req with
    | `GET -> "GET"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | _ -> "Unknown"
    in
    time_operation t operation_name handler req body

  let on_call_tool t tool_name handler req body =
    time_operation t (Printf.sprintf "Tool '%s'" tool_name) handler req body

  let on_read_resource t resource_uri handler req body =
    time_operation t (Printf.sprintf "Resource '%s'" resource_uri) handler req body

  let on_get_prompt t prompt_name handler req body =
    time_operation t (Printf.sprintf "Prompt '%s'" prompt_name) handler req body

  let on_list_tools t handler req body =
    time_operation t "List tools" handler req body

  let on_list_resources t handler req body =
    time_operation t "List resources" handler req body

  let on_list_resource_templates t handler req body =
    time_operation t "List resource templates" handler req body

  let on_list_prompts t handler req body =
    time_operation t "List prompts" handler req body
end 