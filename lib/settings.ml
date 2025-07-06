open! Core
open! Async

(* Use the Log_level from utilities/logging *)
module Log_level = Logging.Log_level

module Duplicate_behavior = struct
  type t =
    | Warn
    | Error
    | Replace
    | Ignore
  [@@deriving compare, equal, sexp, yojson]

  let of_string = function
    | "warn" -> Ok Warn
    | "error" -> Ok Error
    | "replace" -> Ok Replace
    | "ignore" -> Ok Ignore
    | s -> Or_error.error_string ("Invalid duplicate behavior: " ^ s)

  let to_string = function
    | Warn -> "warn"
    | Error -> "error"
    | Replace -> "replace"
    | Ignore -> "ignore"
end

module Resource_prefix_format = struct
  type t =
    | Protocol
    | Path
  [@@deriving compare, equal, sexp, yojson]

  let of_string = function
    | "protocol" -> Ok Protocol
    | "path" -> Ok Path
    | s -> Or_error.error_string ("Invalid resource prefix format: " ^ s)

  let to_string = function
    | Protocol -> "protocol"
    | Path -> "path"
end

module Auth_provider = struct
  type t =
    | Bearer_env
  [@@deriving compare, equal, sexp, yojson]

  let of_string = function
    | "bearer_env" -> Ok Bearer_env
    | s -> Or_error.error_string ("Invalid auth provider: " ^ s)

  let to_string = function
    | Bearer_env -> "bearer_env"
end

module Settings_error = struct
  type t =
    | Invalid_log_level of string
    | Invalid_duplicate_behavior of string
    | Invalid_resource_prefix_format of string
    | Invalid_auth_provider of string
  [@@deriving sexp, compare]

  exception Settings_error of t

  let to_string = function
    | Invalid_log_level s -> sprintf "Invalid log level: %s" s
    | Invalid_duplicate_behavior s -> sprintf "Invalid duplicate behavior: %s" s
    | Invalid_resource_prefix_format s -> sprintf "Invalid resource prefix format: %s" s
    | Invalid_auth_provider s -> sprintf "Invalid auth provider: %s" s
end

module Settings = struct
  type t = {
    home : string;
    test_mode : bool;
    log_level : Log_level.t;
    enable_rich_tracebacks : bool;
    deprecation_warnings : bool;
    client_raise_first_exceptiongroup_error : bool;
    resource_prefix_format : Resource_prefix_format.t;
    client_init_timeout : float option;
    host : string;
    port : int;
    sse_path : string;
    message_path : string;
    streamable_http_path : string;
    debug : bool;
    mask_error_details : bool;
    server_dependencies : string list;
    json_response : bool;
    stateless_http : bool;
    default_auth_provider : Auth_provider.t option;
    include_tags : string list option;
    exclude_tags : string list option;
  }
  [@@deriving compare, equal, sexp, yojson]

  let create () =
    let home = Filename.concat (Sys.home_dir ()) ".oxfastmcp" in
    {
      home;
      test_mode = false;
      log_level = Log_level.Info;
      enable_rich_tracebacks = true;
      deprecation_warnings = true;
      client_raise_first_exceptiongroup_error = true;
      resource_prefix_format = Resource_prefix_format.Path;
      client_init_timeout = None;
      host = "127.0.0.1";
      port = 8000;
      sse_path = "/sse/";
      message_path = "/messages/";
      streamable_http_path = "/mcp/";
      debug = false;
      mask_error_details = false;
      server_dependencies = [];
      json_response = false;
      stateless_http = false;
      default_auth_provider = None;
      include_tags = None;
      exclude_tags = None;
    }

  let configure_logging t =
    let level = Log_level.to_level t.log_level in
    Logging.configure_logging
      ~level
      ~enable_rich_tracebacks:t.enable_rich_tracebacks
      ()
end

let settings = Settings.create () 