(* Transport types *)
type transport_type = Http | SSE | StreamableHttp | Stdio

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
  if not (String.starts_with ~prefix:"http" url) then
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
      if List.mem_assoc "command" fields then
        StdioServer {
          command = member "command" json |> to_string;
          args = member "args" json |> to_list |> List.map to_string;
          env = (try member "env" json |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))
                with _ -> []);
          cwd = member "cwd" json |> to_string_option;
          transport = `Stdio;
        }
      else if List.mem_assoc "url" fields then
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