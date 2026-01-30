open Async
open Core

(** Simplified test client for in-process server testing *)

type initialize_result = {
  server_info : Mcp.Types.implementation;
  capabilities : Mcp.Types.server_capabilities;
}

type t = {
  server : Ox_fast_mcp_server.Server.Ox_fast_mcp.t;
  initialize_result : initialize_result;
}

(** Create test client connected to server *)
let create (server : Ox_fast_mcp_server.Server.Ox_fast_mcp.t) : t Deferred.t =
  (* Simulate MCP initialization - simplified version *)
  let server_info : Mcp.Types.implementation =
    {
      version =
        Option.value
          (Ox_fast_mcp_server.Server.Ox_fast_mcp.version server)
          ~default:"1.0.0";
      website_url = None;
      (* Cannot access private field from interface *)
      icons = None;
      (* Cannot access private field from interface *)
      base_metadata =
        {
          name = Ox_fast_mcp_server.Server.Ox_fast_mcp.name server;
          title = None;
        };
    }
  in
  let capabilities : Mcp.Types.server_capabilities =
    {
      experimental = None;
      logging = None;
      prompts = Some { list_changed = Some false };
      resources = Some { subscribe = Some false; list_changed = Some false };
      tools = Some { list_changed = Some false };
    }
  in
  let initialize_result = { server_info; capabilities } in
  return { server; initialize_result }

(** Close client and cleanup *)
let close _t : unit Deferred.t = return ()

(** Get initialization result *)
let initialize_result t = t.initialize_result

(** List operations - return raw JSON from server *)

let list_tools_json (t : t) : Yojson.Safe.t list Deferred.t =
  return (Ox_fast_mcp_server.Server.Ox_fast_mcp.list_tools_mcp t.server)

let list_resources_json (t : t) : Yojson.Safe.t list Deferred.t =
  return (Ox_fast_mcp_server.Server.Ox_fast_mcp.list_resources_mcp t.server)

(** Tool operations *)
let call_tool (t : t) (name : string)
    (arguments : (string * Yojson.Safe.t) list) : Yojson.Safe.t Deferred.t =
  let args_json = `Assoc arguments in
  Ox_fast_mcp_server.Server.Ox_fast_mcp.call_tool t.server ~name
    ~arguments:args_json

(** Resource operations *)
let read_resource (t : t) (uri : string) : string Deferred.t =
  Ox_fast_mcp_server.Server.Ox_fast_mcp.read_resource t.server ~uri

(** Prompt operations *)
let get_prompt (t : t) (name : string)
    (arguments : (string * Yojson.Safe.t) list) : Yojson.Safe.t Deferred.t =
  let args_json = `Assoc arguments in
  Ox_fast_mcp_server.Server.Ox_fast_mcp.get_prompt t.server ~name
    ~arguments:args_json
