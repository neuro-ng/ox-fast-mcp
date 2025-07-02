(** Ox Fast MCP - OCaml implementation of FastMCP *)

module Mcp_types = Mcp_types
(** Core MCP protocol types *)

module Fast_mcp_server = Fast_mcp_server
(** FastMCP server implementation *)

module Mcp_client = Mcp_client
(** MCP client implementation *)

module Settings = Settings
module Version = Version

let settings = Settings.default
let version = Version.version

(** Re-export commonly used types for convenience *)
module Types = struct
  include Mcp_types

  type server = Fast_mcp_server.t
  (** Server type *)

  type client = Mcp_client.t
  (** Client type *)
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

let protocol_version = "2024-11-05"
