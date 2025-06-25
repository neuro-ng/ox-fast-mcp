(** Ox Fast MCP - OCaml implementation of FastMCP *)

(** Core MCP protocol types *)
module Mcp_types = Mcp_types

(** FastMCP server implementation *)
module Fast_mcp_server = Fast_mcp_server

(** MCP client implementation *)
module Mcp_client = Mcp_client

(** Re-export commonly used types for convenience *)
module Types = struct
  include Mcp_types
  
  (** Server type *)
  type server = Fast_mcp_server.t
  
  (** Client type *)
  type client = Mcp_client.t
end

(** High-level convenience functions *)
module Server = struct
  include Fast_mcp_server
  
  (** Create a new server with a name *)
  let make = create
  
  (** Decorator-style functions for a more functional approach *)
  
  let with_tool server ~name ~description func =
    register_tool server ~name ~description ~func;
    server
  
  let with_resource server ~uri ~name ~description func =
    register_resource server ~uri ~name ~description ~func;
    server
  
  let with_prompt server ~name ~description func =
    register_prompt server ~name ~description ~func;
    server
end

(** High-level client convenience functions *)
module Client = struct
  include Mcp_client
  
  (** Create a new in-memory client *)
  let make_in_memory = create_in_memory
end

(** Version information *)
let version = "0.1.0"
let protocol_version = "2024-11-05" 