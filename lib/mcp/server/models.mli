(** This module provides simpler types to use with the server for managing prompts
    and tools. *)

open Core

(** Initialization options for the server *)
type initialization_options = {
  server_name: string;
  server_version: string;
  capabilities: Mcp.Types.server_capabilities;
  instructions: string option;
} [@@deriving sexp, yojson] 