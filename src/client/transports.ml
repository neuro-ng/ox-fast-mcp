(** Client Transport Implementations

    Provides various transport mechanisms for connecting MCP clients to servers.

    Translation from Python fastmcp/client/transports.py *)

open Core
open Async
module Mcp_client = Mcp_client
module Mcp_client_transports = Mcp_client_transports
module Session = Mcp_client.Session
module Sse = Mcp_client_transports.Sse
module Streamable_http = Mcp_client_transports.Streamable_http
module Stdio = Mcp_client_transports.Stdio
module Message = Mcp_shared.Message

(** {1 Session Configuration} *)

(* Re-export callback types from Session for consistency *)
type sampling_fn = Session.sampling_fn
type elicitation_fn = Session.elicitation_fn
type list_roots_fn = Session.list_roots_fn
type logging_fn = Session.logging_fn
type message_handler = Session.message_handler

type implementation = Mcp.Types.implementation
(** Implementation info for MCP client *)

type session_kwargs = {
  read_timeout_seconds : Time_ns.Span.t option;
  sampling_callback : sampling_fn option;
  list_roots_callback : list_roots_fn option;
  logging_callback : logging_fn option;
  elicitation_callback : elicitation_fn option;
  message_handler : message_handler option;
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

module Client_session = Session
(** Client session module - delegates to Mcp_client.Session for actual session
    management *)

(** {1 HTTP Transport Types} *)

(** Authentication configuration *)
type auth_config =
  | No_auth
  | OAuth
  | Bearer of string
  | Custom of (unit -> unit Deferred.t)

(** Convert auth_config to HTTP headers for transport *)
let auth_config_to_headers = function
  | No_auth -> []
  | Bearer token -> [ ("Authorization", "Bearer " ^ token) ]
  | OAuth -> [] (* OAuth uses separate flow with token refresh *)
  | Custom _ -> [] (* Custom auth handled by callback, not headers *)

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

(** Create WebSocket transport *)
let create_ws_transport url =
  match validate_url url "ws" with
  | Error e -> Error e
  | Ok () -> Ok (Ws_transport { url })

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

(** Set authentication configuration on transport *)
let set_auth t auth =
  match t with
  | Sse_transport config -> Sse_transport { config with auth }
  | Streamable_http_transport config ->
    Streamable_http_transport { config with auth }
  | _ -> t
(* Other transports don't support dynamic auth or use different mechanism *)

(** {1 Transport Operations} *)

(** Connect to transport and return an initialized client session. Supports SSE,
    StreamableHTTP, and Stdio transports. Other transports return stub sessions.
    @param transport The transport configuration
    @param session_kwargs Session callbacks and configuration (partially wired) *)
let connect_session transport session_kwargs =
  let pipe_adapter_read raw_reader =
    let r, w = Pipe.create () in
    don't_wait_for
      (Pipe.transfer raw_reader w ~f:(function
        | `Message msg -> { Message.message = msg; metadata = None }
        | `Error e ->
          Logs.err (fun m -> m "Transport error: %s" (Error.to_string_hum e));
          (* Create an error notification or close? For now we might just drop
             or throw? Actually we can't easily synthesize a session_message
             from Error.t here unless we wrap it in a pseudo notification or
             just close. *)
          {
            Message.message =
              `Notification
                {
                  Mcp.Types.jsonrpc = "2.0";
                  method_ = "transport_error";
                  params =
                    Some (`Assoc [ ("error", `String (Error.to_string_hum e)) ]);
                };
            metadata = None;
          }));
    r
  in
  let pipe_adapter_write () =
    let r, w = Pipe.create () in
    let raw_r, raw_w = Pipe.create () in
    don't_wait_for
      (Pipe.transfer r raw_w ~f:(fun session_msg -> session_msg.Message.message));
    (raw_r, w)
  in
  match transport with
  | Sse_transport config ->
    let auth_headers = auth_config_to_headers config.auth in
    let all_headers = config.headers @ auth_headers in
    let sse_config =
      Sse.create_config ~url:config.url ~headers:all_headers
        ?sse_read_timeout:
          (Option.map config.sse_read_timeout
             ~f:Core.Time_ns.Span.to_span_float_round_nearest)
        ()
    in
    let t = Sse.connect sse_config in
    let read_stream = pipe_adapter_read (Sse.read_stream t) in
    let raw_write_read, write_stream = pipe_adapter_write () in
    don't_wait_for (Pipe.transfer raw_write_read (Sse.write_stream t) ~f:Fn.id);
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream ~write_stream
            ?read_timeout:session_kwargs.read_timeout_seconds
            ?sampling_callback:session_kwargs.sampling_callback
            ?logging_callback:session_kwargs.logging_callback
            ?elicitation_callback:session_kwargs.elicitation_callback
            ?list_roots_callback:session_kwargs.list_roots_callback
            ?message_handler:session_kwargs.message_handler
            ?client_info:session_kwargs.client_info ()))
  | Streamable_http_transport config ->
    let auth_headers = auth_config_to_headers config.auth in
    let all_headers = config.headers @ auth_headers in
    let http_config =
      Streamable_http.create_config ~url:config.url ~headers:all_headers
        ?sse_read_timeout:
          (Option.map config.sse_read_timeout
             ~f:Core.Time_ns.Span.to_span_float_round_nearest)
        ()
    in
    let t = Streamable_http.connect http_config in
    let read_stream =
      let stream = Streamable_http.read_stream t in
      pipe_adapter_read
        (Pipe.map stream ~f:(function
          | `Message m -> `Message m
          | `Error e ->
            `Error (Error.of_string (Streamable_http.Error.to_string e))))
    in
    let raw_write_read, write_stream = pipe_adapter_write () in
    don't_wait_for
      (Pipe.transfer raw_write_read (Streamable_http.write_stream t) ~f:Fn.id);
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream ~write_stream
            ?read_timeout:session_kwargs.read_timeout_seconds
            ?sampling_callback:session_kwargs.sampling_callback
            ?logging_callback:session_kwargs.logging_callback
            ?elicitation_callback:session_kwargs.elicitation_callback
            ?list_roots_callback:session_kwargs.list_roots_callback
            ?message_handler:session_kwargs.message_handler
            ?client_info:session_kwargs.client_info ()))
  | Stdio_transport config ->
    let params =
      {
        Stdio.command = config.command;
        args = config.args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Python_stdio_transport config ->
    let args = config.script_path :: Option.value config.args ~default:[] in
    let params =
      {
        Stdio.command = config.python_cmd;
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Node_stdio_transport config ->
    (* Node transport uses same config structure as Python, but python_cmd is
       "node" *)
    let args = config.script_path :: Option.value config.args ~default:[] in
    let params =
      {
        Stdio.command = config.python_cmd;
        (* "node" for Node transport *)
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Fastmcp_stdio_transport config ->
    (* FastMCP uses python -m fastmcp run <script> pattern *)
    let args =
      [ "-m"; "fastmcp"; "run"; config.script_path ]
      @ Option.value config.args ~default:[]
    in
    let params =
      {
        Stdio.command = config.python_cmd;
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Uv_stdio_transport config ->
    (* UV transport uses: uv run <command> [args...] *)
    let args = "run" :: config.command :: config.args in
    let params =
      {
        Stdio.command = "uv";
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Uvx_stdio_transport config ->
    (* UVX transport uses: uvx <command> [args...] *)
    let args = config.command :: config.args in
    let params =
      {
        Stdio.command = "uvx";
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Npx_stdio_transport config ->
    (* NPX transport uses: npx <package> [args...] *)
    let args = config.command :: config.args in
    let params =
      {
        Stdio.command = "npx";
        args;
        env = config.env;
        cwd = config.cwd;
        encoding = "utf-8";
        encoding_error_handler = `Replace;
      }
    in
    let%bind read_pipe, write_pipe =
      Stdio.stdio_client params ~stderr:(Writer.create (Fd.stderr ()))
    in
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream:read_pipe
            ~write_stream:write_pipe ()))
  | Fastmcp_transport _server_name ->
    (* FastMCP in-memory transport - requires server.run_on_pipes integration *)
    Logs.warn (fun m ->
        m
          "Fastmcp_transport (in-memory) not yet implemented - requires \
           Server.run_on_pipes integration");
    let r, _ = Pipe.create () in
    let _, w = Pipe.create () in
    return
      (Ok (Client_session.create_from_pipes ~read_stream:r ~write_stream:w ()))
  | Mcp_config_transport _config_path ->
    (* MCP Config transport - requires parsing config file *)
    Logs.warn (fun m ->
        m
          "Mcp_config_transport not yet implemented - requires config file \
           parsing");
    let r, _ = Pipe.create () in
    let _, w = Pipe.create () in
    return
      (Ok (Client_session.create_from_pipes ~read_stream:r ~write_stream:w ()))
  | Ws_transport config ->
    (* WebSocket transport using websocket-async *)
    Logs.info (fun m -> m "Connecting via WebSocket transport");
    let ws_config =
      Mcp_client_transports.Ws_client.create_config ~url:config.url ()
    in
    let ws_t = Mcp_client_transports.Ws_client.connect ws_config in
    let read_stream =
      pipe_adapter_read (Mcp_client_transports.Ws_client.read_stream ws_t)
    in
    let raw_write_read, write_stream = pipe_adapter_write () in
    don't_wait_for
      (Pipe.transfer raw_write_read
         (Mcp_client_transports.Ws_client.write_stream ws_t)
         ~f:Fn.id);
    return
      (Ok
         (Client_session.create_from_pipes ~read_stream ~write_stream
            ?read_timeout:session_kwargs.read_timeout_seconds
            ?sampling_callback:session_kwargs.sampling_callback
            ?logging_callback:session_kwargs.logging_callback
            ?elicitation_callback:session_kwargs.elicitation_callback
            ?list_roots_callback:session_kwargs.list_roots_callback
            ?message_handler:session_kwargs.message_handler
            ?client_info:session_kwargs.client_info ()))

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
