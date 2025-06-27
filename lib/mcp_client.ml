open Mcp_types

type t = { server : Fast_mcp_server.t option; mutable connected : bool }
(** Client state *)

(** Create a new in-memory client connected to a server *)
let create_in_memory server = { server = Some server; connected = true }

(** Create a client (placeholder for other transport types) *)
let create () = { server = None; connected = false }

(** Check if client is connected *)
let is_connected client = client.connected

(** Close the client connection *)
let close client = client.connected <- false

(** List available tools *)
let list_tools client =
  match client.server with
  | Some server when client.connected -> Ok (Fast_mcp_server.list_tools server)
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"

(** List available resources *)
let list_resources client =
  match client.server with
  | Some server when client.connected ->
    Ok (Fast_mcp_server.list_resources server)
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"

(** List available prompts *)
let list_prompts client =
  match client.server with
  | Some server when client.connected ->
    Ok (Fast_mcp_server.list_prompts server)
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"

(** Call a tool - simplified implementation *)
let call_tool client tool_name _params =
  match client.server with
  | Some _server when client.connected ->
    (* For now, return a simple test result *)
    Ok (Text ("Called tool: " ^ tool_name))
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"

(** Read a resource - simplified implementation *)
let read_resource client uri =
  match client.server with
  | Some _server when client.connected ->
    (* For now, return a simple test result *)
    Ok (Text ("Read resource: " ^ uri))
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"

(** Get a prompt - simplified implementation *)
let get_prompt client prompt_name _params =
  match client.server with
  | Some _server when client.connected ->
    (* For now, return a simple test result *)
    Ok ("Prompt: " ^ prompt_name)
  | Some _ -> Error "Client not connected"
  | None -> Error "No server attached"
