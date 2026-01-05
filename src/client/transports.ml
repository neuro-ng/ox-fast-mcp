(** Client Transport Implementations

    Provides various transport mechanisms for connecting MCP clients to servers.

    Translation from Python fastmcp/client/transports.py *)

open Core
open Async

(** {1 Session Configuration} *)

type sampling_fn = unit -> unit Deferred.t
(** Callback types for MCP client session *)

type list_roots_fn = unit -> unit Deferred.t
type logging_fn = string -> unit Deferred.t
type elicitation_fn = unit -> unit Deferred.t
type message_handler_fn = unit -> unit Deferred.t

type implementation = { name : string; version : string } [@@deriving fields]
(** Implementation info for MCP client *)

type session_kwargs = {
  read_timeout_seconds : Time_ns.Span.t option;
  sampling_callback : sampling_fn option;
  list_roots_callback : list_roots_fn option;
  logging_callback : logging_fn option;
  elicitation_callback : elicitation_fn option;
  message_handler : message_handler_fn option;
  client_info : implementation option;
}
[@@deriving fields]
(** Session configuration parameters *)

let default_session_kwargs =
  {
    read_timeout_seconds = None;
    sampling_callback = None;
    list_roots_callback = None;
    logging_callback = None;
    elicitation_callback = None;
    message_handler = None;
    client_info = None;
  }

(** {1 Transport Interface} *)

(** Abstract client session type - placeholder until MCP SDK is available *)
module type Client_session_intf = sig
  type t

  val create :
    read_stream:unit -> write_stream:unit -> session_kwargs -> t Deferred.t

  val close : t -> unit Deferred.t
end

(** Placeholder for client session *)
module Client_session : Client_session_intf = struct
  type t = { id : string }

  let create ~read_stream:_ ~write_stream:_ _kwargs =
    return { id = "placeholder" }

  let close _t = return ()
end

(** {1 HTTP Transport Types} *)

(** Authentication configuration *)
type auth_config =
  | No_auth
  | OAuth
  | Bearer of string
  | Custom of (unit -> unit Deferred.t)
(* Placeholder for httpx.Auth *)

type sse_config = {
  url : string;
  headers : (string * string) list;
  auth : auth_config;
  sse_read_timeout : Time_ns.Span.t option;
}
[@@deriving fields]
(** SSE transport configuration *)

type streamable_http_config = {
  url : string;
  headers : (string * string) list;
  auth : auth_config;
  sse_read_timeout : Time_ns.Span.t option;
}
[@@deriving fields]
(** Streamable HTTP transport configuration *)

type ws_config = { url : string } [@@deriving fields]
(** WebSocket transport configuration *)

(** {1 Stdio Transport Types} *)

type stdio_config = {
  command : string;
  args : string list;
  env : (string * string) list option;
  cwd : string option;
  keep_alive : bool;
  log_file : string option; (* Path to log file *)
}
[@@deriving fields]
(** Stdio transport configuration *)

type python_stdio_config = {
  script_path : string;
  args : string list option;
  env : (string * string) list option;
  cwd : string option;
  python_cmd : string;
  keep_alive : bool;
  log_file : string option;
}
[@@deriving fields]
(** Python stdio transport configuration *)

(** {1 Transport Variant Type} *)

(** All transport types *)
type t =
  | Ws_transport of ws_config
  | Sse_transport of sse_config
  | Streamable_http_transport of streamable_http_config
  | Stdio_transport of stdio_config
  | Python_stdio_transport of python_stdio_config
  | Node_stdio_transport of python_stdio_config (* Same config structure *)
  | Fastmcp_stdio_transport of python_stdio_config
  | Uv_stdio_transport of stdio_config
  | Uvx_stdio_transport of stdio_config
  | Npx_stdio_transport of stdio_config
  | Fastmcp_transport of string (* Server name *)
  | Mcp_config_transport of string (* Config path *)

(** {1 Helper Functions} *)

let validate_url url protocol =
  if not (String.is_prefix url ~prefix:protocol) then
    Error (Error.of_string (sprintf "Invalid %s URL: %s" protocol url))
  else Ok ()

let validate_file_exists path =
  match%map Sys.file_exists path with
  | `Yes -> Ok ()
  | `No | `Unknown ->
    Error (Error.of_string (sprintf "File not found: %s" path))

let validate_python_script path =
  let%bind res = validate_file_exists path in
  match res with
  | Error e -> return (Error e)
  | Ok () ->
    if not (String.is_suffix path ~suffix:".py") then
      return (Error (Error.of_string (sprintf "Not a Python script: %s" path)))
    else return (Ok ())

let _validate_javascript_script path =
  let%bind res = validate_file_exists path in
  match res with
  | Error e -> return (Error e)
  | Ok () ->
    if not (String.is_suffix path ~suffix:".js") then
      return
        (Error (Error.of_string (sprintf "Not a JavaScript script: %s" path)))
    else return (Ok ())

(** {1 Transport Constructors} *)

(** Create WebSocket transport (deprecated) *)
let create_ws_transport url =
  match validate_url url "ws" with
  | Error e -> Error e
  | Ok () ->
    Logs.warn (fun m ->
        m "WSTransport is deprecated, use StreamableHttpTransport");
    Ok (Ws_transport { url })

(** Create SSE transport *)
let create_sse_transport ?(headers = []) ?(auth = No_auth)
    ?(sse_read_timeout = None) url =
  match validate_url url "http" with
  | Error e -> Error e
  | Ok () -> Ok (Sse_transport { url; headers; auth; sse_read_timeout })

(** Create Streamable HTTP transport *)
let create_streamable_http_transport ?(headers = []) ?(auth = No_auth)
    ?(sse_read_timeout = None) url =
  match validate_url url "http" with
  | Error e -> Error e
  | Ok () ->
    Ok (Streamable_http_transport { url; headers; auth; sse_read_timeout })

(** Create stdio transport *)
let create_stdio_transport ?(env = None) ?(cwd = None) ?(keep_alive = true)
    ?(log_file = None) command args =
  Ok (Stdio_transport { command; args; env; cwd; keep_alive; log_file })

(** Create Python stdio transport *)
let create_python_stdio_transport ?(args = None) ?(env = None) ?(cwd = None)
    ?(python_cmd = "python3") ?(keep_alive = true) ?(log_file = None)
    script_path =
  let script_path = Filename_unix.realpath script_path in
  match
    Async.Thread_safe.block_on_async_exn (fun () ->
        validate_python_script script_path)
  with
  | Error e -> Error e
  | Ok () ->
    Ok
      (Python_stdio_transport
         { script_path; args; env; cwd; python_cmd; keep_alive; log_file })

(** String representation *)
let to_string = function
  | Ws_transport { url } -> sprintf "<WSTransport(url='%s')>" url
  | Sse_transport { url; _ } -> sprintf "<SSETransport(url='%s')>" url
  | Streamable_http_transport { url; _ } ->
    sprintf "<StreamableHttpTransport(url='%s')>" url
  | Stdio_transport { command; args; _ } ->
    sprintf "<StdioTransport(command='%s', args=%s)>" command
      (Sexp.to_string (sexp_of_list sexp_of_string args))
  | Python_stdio_transport { script_path; _ } ->
    sprintf "<PythonStdioTransport(script='%s')>" script_path
  | Node_stdio_transport { script_path; _ } ->
    sprintf "<NodeStdioTransport(script='%s')>" script_path
  | Fastmcp_stdio_transport { script_path; _ } ->
    sprintf "<FastMCPStdioTransport(script='%s')>" script_path
  | Uv_stdio_transport { command; _ } ->
    sprintf "<UvStdioTransport(command='%s')>" command
  | Uvx_stdio_transport { command; _ } ->
    sprintf "<UvxStdioTransport(command='%s')>" command
  | Npx_stdio_transport { command; _ } ->
    sprintf "<NpxStdioTransport(command='%s')>" command
  | Fastmcp_transport name -> sprintf "<FastMCPTransport(server='%s')>" name
  | Mcp_config_transport path ->
    sprintf "<MCPConfigTransport(config='%s')>" path

(** {1 Transport Operations} *)

(** Connect to transport and create session

    NOTE: Full implementation requires MCP SDK client modules. This is a stub
    that will be completed when dependencies are available. *)
let connect_session _transport session_kwargs =
  Logs.warn (fun m ->
      m "Transport.connect_session is stubbed - MCP SDK client modules required");
  return
    (Ok
       (Async.Thread_safe.block_on_async_exn (fun () ->
            Client_session.create ~read_stream:() ~write_stream:()
              session_kwargs)))

(** Close transport *)
let close _transport =
  Logs.debug (fun m -> m "Closing transport");
  return ()

(** {1 Type Inference} *)

(** Infer transport type from URL or path *)
let infer_transport_from_string s =
  (* Check if it's a file path *)
  let%bind file_exists = Sys.file_exists s in
  match file_exists with
  | `Yes ->
    if String.is_suffix s ~suffix:".py" then
      return (create_python_stdio_transport s)
    else if String.is_suffix s ~suffix:".js" then
      return
        (Ok
           (Node_stdio_transport
              {
                script_path = s;
                args = None;
                env = None;
                cwd = None;
                python_cmd = "node";
                keep_alive = true;
                log_file = None;
              }))
    else
      return (Error (Error.of_string (sprintf "Unsupported script type: %s" s)))
  | `No | `Unknown ->
    (* Not a file, try URLs *)
    if String.is_prefix s ~prefix:"http" then
      (* Default to StreamableHttp, unless URL ends with /sse *)
      if String.is_suffix s ~suffix:"/sse" then return (create_sse_transport s)
      else return (create_streamable_http_transport s)
        (* Check if it's a WebSocket URL *)
    else if String.is_prefix s ~prefix:"ws" then return (create_ws_transport s)
    else
      return
        (Error
           (Error.of_string (sprintf "Could not infer transport from: %s" s)))
