open! Core
open! Async

(* Types from other modules *)
module Transport_type = struct
  type t = Http | Sse | Stdio [@@deriving yojson, compare, sexp]
end

let infer_transport_type_from_url (url : string) : Transport_type.t =
  if not (String.is_prefix ~prefix:"http" url) then
    raise (Invalid_argument (sprintf "Invalid URL: %s" url));
  let uri = Uri.of_string url in
  let path = Uri.path uri in
  if
    String.is_substring ~substring:"/sse" path
    && (String.is_suffix ~suffix:"/" path
       || String.is_suffix ~suffix:"?" path
       || String.is_suffix ~suffix:"&" path
       || String.length path = String.length "/sse")
  then Transport_type.Sse
  else Transport_type.Http

module Stdio_mcp_server = struct
  type t = {
    command : string;
    args : string list; [@default []]
    env : (string * string) list; [@default []]
    cwd : string option; [@default None]
    transport : Transport_type.t; [@default Transport_type.Stdio]
  }
  [@@deriving yojson, compare, sexp]

  let to_transport t =
    let open Client.Transports in
    create_stdio_transport ~command:t.command ~args:t.args ~env:t.env ?cwd:t.cwd
      ()
end

module Remote_mcp_server = struct
  type auth = Bearer of string | OAuth | Custom of Client.Auth.t
  [@@deriving compare, sexp]

  let auth_of_yojson = function
    | `String "oauth" -> Ok OAuth
    | `String s -> Ok (Bearer s)
    | _ -> Error "Invalid auth format"

  let yojson_of_auth = function
    | OAuth -> `String "oauth"
    | Bearer token -> `String token
    | Custom _ -> `String "custom" (* Custom auth serialization not supported *)

  type t = {
    url : string;
    headers : (string * string) list; [@default []]
    transport : Transport_type.t option; [@default None]
    auth : auth option; [@default None]
  }
  [@@deriving yojson, compare, sexp]

  let to_transport t =
    let open Client.Transports in
    let transport_type =
      match t.transport with
      | Some tp -> tp
      | None -> infer_transport_type_from_url t.url
    in
    match transport_type with
    | Transport_type.Sse ->
      create_sse_transport ~url:t.url ~headers:t.headers ?auth:t.auth ()
    | _ ->
      create_streamable_http_transport ~url:t.url ~headers:t.headers
        ?auth:t.auth ()
end

module Mcp_config = struct
  type server = Stdio of Stdio_mcp_server.t | Remote of Remote_mcp_server.t
  [@@deriving yojson, compare, sexp]

  type t = { mcp_servers : (string * server) list }
  [@@deriving yojson, compare, sexp]

  let of_dict dict =
    let mcp_servers =
      match Yojson.Safe.Util.member "mcpServers" dict with
      | `Null -> Yojson.Safe.Util.to_assoc dict
      | servers -> Yojson.Safe.Util.to_assoc servers
    in
    { mcp_servers }
end
