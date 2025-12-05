(** MCP Configuration types for server transport configuration *)

open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Transport types supported by MCP servers *)
module Transport_type = struct
  type t =
    | Stdio
    | Sse
    | Streamable_http
  [@@deriving sexp, compare, equal, yojson]

  let to_string = function
    | Stdio -> "stdio"
    | Sse -> "sse"
    | Streamable_http -> "streamable-http"

  let of_string = function
    | "stdio" -> Stdio
    | "sse" -> Sse
    | "http" | "streamable-http" -> Streamable_http
    | s -> failwithf "Unknown transport type: %s" s ()
end

(** Stdio MCP server configuration *)
type stdio_mcp_server = {
  command : string;
  args : string list; [@default []]
  env : (string * string) list; [@default []]
  cwd : string option; [@yojson.option]
  timeout : int option; [@yojson.option]
  description : string option; [@yojson.option]
  icon : string option; [@yojson.option]
}
[@@deriving sexp, compare, yojson]

let create_stdio_server ~command ?(args = []) ?(env = []) ?cwd ?timeout
    ?description ?icon () =
  { command; args; env; cwd; timeout; description; icon }

(** Remote MCP server configuration for HTTP/SSE transport *)
type remote_mcp_server = {
  url : string;
  transport : Transport_type.t option; [@yojson.option]
  headers : (string * string) list; [@default []]
  sse_read_timeout : float option; [@yojson.option]
  timeout : int option; [@yojson.option]
  description : string option; [@yojson.option]
  icon : string option; [@yojson.option]
}
[@@deriving sexp, compare, yojson]

let create_remote_server ~url ?transport ?(headers = []) ?sse_read_timeout
    ?timeout ?description ?icon () =
  { url; transport; headers; sse_read_timeout; timeout; description; icon }

(** MCP server configuration - either stdio or remote *)
type mcp_server =
  | Stdio of stdio_mcp_server
  | Remote of remote_mcp_server
[@@deriving sexp, compare]

let mcp_server_of_yojson json =
  match json with
  | `Assoc fields ->
    (match List.Assoc.find fields ~equal:String.equal "command" with
    | Some _ -> Stdio (stdio_mcp_server_of_yojson json)
    | None ->
      (match List.Assoc.find fields ~equal:String.equal "url" with
      | Some _ -> Remote (remote_mcp_server_of_yojson json)
      | None -> failwith "MCP server must have either 'command' or 'url' field"))
  | _ -> failwith "MCP server configuration must be an object"

let yojson_of_mcp_server = function
  | Stdio server -> yojson_of_stdio_mcp_server server
  | Remote server -> yojson_of_remote_mcp_server server

(** MCP configuration containing multiple servers *)
type mcp_config = { mcp_servers : (string * mcp_server) list [@key "mcpServers"] }
[@@deriving sexp, compare, yojson]

let create_config ?(servers = []) () = { mcp_servers = servers }

let add_server config ~name ~server =
  let servers =
    List.filter config.mcp_servers ~f:(fun (n, _) ->
        not (String.equal n name))
    @ [ (name, server) ]
  in
  { mcp_servers = servers }

let get_server config ~name =
  List.Assoc.find config.mcp_servers ~equal:String.equal name

let remove_server config ~name =
  {
    mcp_servers =
      List.filter config.mcp_servers ~f:(fun (n, _) ->
          not (String.equal n name));
  }

(** Infer transport type from URL *)
let infer_transport_type_from_url url =
  let uri = Uri.of_string url in
  let path = Uri.path uri in
  if String.is_suffix path ~suffix:"/sse" || String.is_suffix path ~suffix:"/sse/"
  then Transport_type.Sse
  else Transport_type.Streamable_http

(** Get the effective transport type for a remote server *)
let get_transport_type server =
  match server.transport with
  | Some t -> t
  | None -> infer_transport_type_from_url server.url

(** Load MCP config from a JSON file *)
let load_from_file path =
  let%map contents = Reader.file_contents path in
  let json = Yojson.Safe.from_string contents in
  mcp_config_of_yojson json

(** Save MCP config to a JSON file *)
let save_to_file config path =
  let json = yojson_of_mcp_config config in
  let contents = Yojson.Safe.pretty_to_string json in
  Writer.save path ~contents

(** Update a server in a config file *)
let update_config_file ~path ~name ~server =
  let%bind config =
    match%map Monitor.try_with (fun () -> load_from_file path) with
    | Ok config -> config
    | Error _ -> create_config ()
  in
  let updated = add_server config ~name ~server in
  save_to_file updated path
