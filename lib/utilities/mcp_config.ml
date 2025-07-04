open Core
open! Base

let extract_path url =
  let uri = Uri.of_string url in
  Uri.path uri

(* Transport types *)
type transport_type = Http | SSE | StreamableHttp | Stdio
[@@deriving sexp, compare, equal]

(* Server configuration *)
type server_config = {
  url : string;
  transport : transport_type;
  options : (string * string) list;
} [@@deriving sexp, compare]

(* MCP configuration *)
type t = {
  mcp_servers : (string * server_config) list;
} [@@deriving sexp, compare]

let json_of_transport = function
  | Http -> `String "http"
  | SSE -> `String "sse"
  | StreamableHttp -> `String "streamable_http"
  | Stdio -> `String "stdio"

let json_of_server server =
  `Assoc [
    ("url", `String server.url);
    ("transport", json_of_transport server.transport);
    ("options", `Assoc (List.map server.options ~f:(fun (k, v) -> (k, `String v))))
  ]

let json_of_config config =
  `Assoc [
    ("mcp_servers",
     `Assoc (List.map config.mcp_servers ~f:(fun (name, server) ->
       (name, json_of_server server))))
  ]

let transport_of_json = function
  | `String "http" -> Ok Http
  | `String "sse" -> Ok SSE
  | `String "streamable_http" -> Ok StreamableHttp
  | `String "stdio" -> Ok Stdio
  | _ -> Error "Invalid transport type"

let server_of_json = function
  | `Assoc fields ->
    let open Result.Let_syntax in
    let%bind url = match List.Assoc.find fields "url" ~equal:String.equal with
      | Some (`String u) -> Ok u
      | _ -> Error "Missing or invalid url field"
    in
    let%bind transport = match List.Assoc.find fields "transport" ~equal:String.equal with
      | Some t -> transport_of_json t
      | None -> Ok Http (* Default to HTTP *)
    in
    let options = match List.Assoc.find fields "options" ~equal:String.equal with
      | Some (`Assoc opts) ->
        List.filter_map opts ~f:(fun (k, v) ->
          match v with
          | `String v -> Some (k, v)
          | _ -> None)
      | _ -> []
    in
    Ok { url; transport; options }
  | _ -> Error "Invalid server configuration"

let config_of_json = function
  | `Assoc fields ->
    let open Result.Let_syntax in
    let%bind servers = match List.Assoc.find fields "mcp_servers" ~equal:String.equal with
      | Some (`Assoc servers) ->
        List.fold servers ~init:(Ok []) ~f:(fun acc (name, server_json) ->
          let%bind acc = acc in
          let%bind server = server_of_json server_json in
          Ok ((name, server) :: acc))
      | _ -> Error "Missing or invalid mcp_servers field"
    in
    Ok { mcp_servers = List.rev servers }
  | _ -> Error "Invalid configuration"

let load_config path =
  let json = Yojson.Safe.from_file path in
  match config_of_json json with
  | Ok config -> config
  | Error msg -> failwith msg

let save_config config path =
  let json = json_of_config config in
  Yojson.Safe.to_file path json

let get_server config name =
  List.Assoc.find config.mcp_servers name ~equal:String.equal

let add_server config name server =
  { mcp_servers = (name, server) :: List.filter config.mcp_servers ~f:(fun (n, _) -> n <> name) }

let remove_server config name =
  { mcp_servers = List.filter config.mcp_servers ~f:(fun (n, _) -> n <> name) }

let update_server config name f =
  match get_server config name with
  | Some server -> add_server config name (f server)
  | None -> config

let server_names config =
  List.map config.mcp_servers ~f:fst

let server_exists config name =
  List.Assoc.mem config.mcp_servers name ~equal:String.equal

let empty = { mcp_servers = [] }

let merge config1 config2 =
  { mcp_servers = config1.mcp_servers @ config2.mcp_servers }

let filter config f =
  { mcp_servers = List.filter config.mcp_servers ~f }

let map config f =
  { mcp_servers = List.map config.mcp_servers ~f }

let fold config ~init ~f =
  List.fold config.mcp_servers ~init ~f

let iter config ~f =
  List.iter config.mcp_servers ~f

let to_list config =
  config.mcp_servers

let of_list servers =
  { mcp_servers = servers }

let validate_server server =
  let open Result.Let_syntax in
  let%bind () = match server.transport with
    | Http | SSE | StreamableHttp ->
      if not (String.is_prefix server.url ~prefix:"http") then
        Error "URL must start with 'http' for HTTP/SSE/StreamableHttp transport"
      else
        Ok ()
    | Stdio -> Ok ()
  in
  Ok ()

let validate config =
  List.fold config.mcp_servers ~init:(Ok ()) ~f:(fun acc (_, server) ->
    let open Result.Let_syntax in
    let%bind () = acc in
    validate_server server)

(* Server configurations *)
type stdio_mcp_server = {
  command: string;
  args: string list;
  env: (string * string) list;
  cwd: string option;
  transport: [`Stdio];
}

type remote_mcp_server = {
  url: string;
  headers: (string * string) list;
  transport: [`Http | `StreamableHttp | `SSE] option;
  auth: [`Bearer of string | `OAuth | `Custom of string] option;
}

type mcp_server = 
  | StdioServer of stdio_mcp_server
  | RemoteServer of remote_mcp_server

type mcp_config = {
  mcp_servers: (string * mcp_server) list;
}

(* Helper functions *)
let infer_transport_type_from_url url =
  if not (String.is_prefix url ~prefix:"http") then
    invalid_arg ("Invalid URL: " ^ url)
  else
    let path = Uri.path (Uri.of_string url) in
    if Re.execp (Re.compile (Re.seq [Re.str "/sse"; Re.alt [Re.char '/'; Re.char '?'; Re.char '&'; Re.eos]])) path then
      SSE
    else
      Http

(* Transport conversion functions *)
let stdio_to_transport (server: stdio_mcp_server) =
  `Stdio {
    command = server.command;
    args = server.args;
    env = server.env;
    cwd = server.cwd;
    transport = server.transport;
  }

let remote_to_transport (server: remote_mcp_server) =
  let transport = match server.transport with
    | Some `Http -> `Http
    | Some `StreamableHttp -> `StreamableHttp
    | Some `SSE -> `SSE
    | None -> 
        match infer_transport_type_from_url server.url with
        | SSE -> `SSE
        | Http -> `Http
        | _ -> `StreamableHttp
  in
  match transport with
  | `SSE -> `SSE {
      url = server.url;
      headers = server.headers;
      auth = server.auth;
      transport = Some `SSE;
    }
  | _ -> `StreamableHttp {
      url = server.url;
      headers = server.headers;
      auth = server.auth;
      transport = Some `StreamableHttp;
    }

(* JSON conversion functions *)
let server_of_json json =
  let open Yojson.Safe.Util in
  match json with
  | `Assoc fields ->
      if List.Assoc.mem fields "command" ~equal:String.equal then
        StdioServer {
          command = member "command" json |> to_string;
          args = member "args" json |> to_list |> List.map to_string;
          env = (try member "env" json |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))
                with _ -> []);
          cwd = member "cwd" json |> to_string_option;
          transport = `Stdio;
        }
      else if List.Assoc.mem fields "url" ~equal:String.equal then
        RemoteServer {
          url = member "url" json |> to_string;
          headers = (try member "headers" json |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))
                    with _ -> []);
          transport = (match member "transport" json |> to_string_option with
                      | Some "http" -> Some `Http
                      | Some "streamable-http" -> Some `StreamableHttp
                      | Some "sse" -> Some `SSE
                      | _ -> None);
          auth = (match member "auth" json |> to_string_option with
                 | Some "oauth" -> Some `OAuth
                 | Some token -> Some (`Bearer token)
                 | None -> None);
        }
      else
        invalid_arg "Invalid server configuration"
  | _ -> invalid_arg "Invalid server configuration"

let config_of_json json =
  let open Yojson.Safe.Util in
  let servers = match json with
    | `Assoc [("mcpServers", servers)] -> servers
    | servers -> servers
  in
  let mcp_servers = servers
    |> to_assoc
    |> List.map (fun (name, config) -> (name, server_of_json config))
  in
  { mcp_servers }

let json_of_server = function
  | StdioServer server ->
      let fields = [
        ("command", `String server.command);
        ("args", `List (List.map (fun s -> `String s) server.args));
      ] in
      let fields = if server.env <> [] then
        ("env", `Assoc (List.map (fun (k, v) -> (k, `String v)) server.env)) :: fields
      else fields in
      let fields = match server.cwd with
        | Some s -> ("cwd", `String s) :: fields
        | None -> fields
      in
      `Assoc fields
  | RemoteServer server ->
      let fields = [
        ("url", `String server.url);
      ] in
      let fields = if server.headers <> [] then
        ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) server.headers)) :: fields
      else fields in
      let fields = match server.transport with
        | Some `Http -> ("transport", `String "http") :: fields
        | Some `StreamableHttp -> ("transport", `String "streamable-http") :: fields
        | Some `SSE -> ("transport", `String "sse") :: fields
        | None -> fields
      in
      let fields = match server.auth with
        | Some `OAuth -> ("auth", `String "oauth") :: fields
        | Some (`Bearer token) -> ("auth", `String token) :: fields
        | Some (`Custom auth) -> ("auth", `String auth) :: fields
        | None -> fields
      in
      `Assoc fields

let json_of_config config =
  `Assoc [
    ("mcpServers", `Assoc (List.map (fun (name, server) -> (name, json_of_server server)) config.mcp_servers))
  ] 