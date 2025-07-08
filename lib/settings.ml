open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Logging

(* Use Log_types.Level for log levels *)
module Log_level = Log_types.Level

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
    | Invalid_env_value of string * string  (* var_name * value *)
    | Missing_required_env of string        (* var_name *)
  [@@deriving sexp, compare]

  exception Settings_error of t

  let to_string = function
    | Invalid_log_level s -> sprintf "Invalid log level: %s" s
    | Invalid_duplicate_behavior s -> sprintf "Invalid duplicate behavior: %s" s
    | Invalid_resource_prefix_format s -> sprintf "Invalid resource prefix format: %s" s
    | Invalid_auth_provider s -> sprintf "Invalid auth provider: %s" s
    | Invalid_env_value (var, value) -> sprintf "Invalid value '%s' for environment variable %s" value var
    | Missing_required_env var -> sprintf "Missing required environment variable: %s" var
end

module Settings_source = struct
  type t =
    | Init
    | Environment
    | Dotenv
    | File_secrets
  [@@deriving compare, equal, sexp]

  let priority = function
    | Init -> 0
    | Environment -> 1
    | Dotenv -> 2
    | File_secrets -> 3
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

  let env_prefixes = ["OXFASTMCP_"; "OXFASTMCP_SERVER_"]
  let env_nested_delimiter = "__"
  let env_file = ".env"

  let create 
      ?(home =
        match Sys.getenv "HOME" with
        | Some home -> Filename.concat home ".oxfastmcp"
        | None -> Filename.concat "/tmp" ".oxfastmcp")
      ?(test_mode = false) 
      ?(log_level = Log_level.Info) 
      ?(enable_rich_tracebacks = true) 
      ?(deprecation_warnings = true) 
      ?(client_raise_first_exceptiongroup_error = true) 
      ?(resource_prefix_format = Resource_prefix_format.Path) 
      ?(client_init_timeout = None) 
      ?(host = "127.0.0.1") 
      ?(port = 8000) 
      ?(sse_path = "/sse/") 
      ?(message_path = "/messages/") 
      ?(streamable_http_path = "/mcp/") 
      ?(debug = false) 
      ?(mask_error_details = false) 
      ?(server_dependencies = []) 
      ?(json_response = false) 
      ?(stateless_http = false) 
      ?(default_auth_provider = None) 
      ?(include_tags = None) 
      ?(exclude_tags = None) 
      () =  
    {
      home; 
      test_mode;
      log_level;
      enable_rich_tracebacks;
      deprecation_warnings;
      client_raise_first_exceptiongroup_error;
      resource_prefix_format;
      client_init_timeout;
      host;
      port;
      sse_path;
      message_path;
      streamable_http_path;
      debug;
      mask_error_details;
      server_dependencies;
      json_response;
      stateless_http;
      default_auth_provider;
      include_tags;
      exclude_tags;
    }

  let configure_logging t =
    Logging.configure_logging
      ~level:t.log_level
      ~enable_rich_tracebacks:t.enable_rich_tracebacks
      ()

  let get_env_value var_name =
    match Sys.getenv var_name with
    | Some value -> Ok value
    | None -> Or_error.error_string (sprintf "Environment variable %s not found" var_name)

  let warn_if_deprecated prefix value =
    if String.equal prefix "OX_FASTMCP_SERVER_" then
      Logging.Global.warning "Using `OX_FASTMCP_SERVER_` environment variables is deprecated. Use `OX_FASTMCP_` instead."
    else
      ();
    value

  let load_from_env t =
    let open Or_error.Let_syntax in
    let try_prefixes var_name =
      List.find_map env_prefixes ~f:(fun prefix ->
        let full_name = prefix ^ var_name in
        match Sys.getenv full_name with
        | Some value -> Some (warn_if_deprecated prefix value)
        | None -> None)
    in
    let%bind log_level =
      try
        match try_prefixes "LOG_LEVEL" with
        | Some value -> Ok (Log_level.of_string value)
        | None -> Ok t.log_level
      with Failure msg -> Or_error.error_string msg
    in
    let%bind resource_prefix_format =
      match try_prefixes "RESOURCE_PREFIX_FORMAT" with
      | Some value -> Resource_prefix_format.of_string value
      | None -> Ok t.resource_prefix_format
    in
    let%bind default_auth_provider =
      match try_prefixes "DEFAULT_AUTH_PROVIDER" with
      | Some value -> 
        let%bind provider = Auth_provider.of_string value in
        Ok (Some provider)
      | None -> Ok t.default_auth_provider
    in
    Ok { t with log_level; resource_prefix_format; default_auth_provider }

  let load_from_dotenv t =
    let%bind exists = Sys.file_exists env_file in
    match exists with
    | `Yes -> 
      (* TODO: Implement .env file parsing *)
      return (Ok t)
    | _ -> return (Ok t)

  let load_from_file_secrets t =
    (* TODO: Implement file secrets loading *)
    Ok t

  let merge base override =
    { base with
      log_level = override.log_level;
      resource_prefix_format = override.resource_prefix_format;
      default_auth_provider = override.default_auth_provider;
      (* Add other fields as needed *)
    }
 
  let validate t =
    let open Deferred.Or_error.Let_syntax in 
    let%bind () =
      if String.equal t.home "" then
        Deferred.Or_error.error_string (sprintf "Home path cannot be empty")
      else
        Deferred.Or_error.return ()
    in

    let%bind () =
      if t.port < 0 || t.port > 65535 then
        Deferred.Or_error.error_string (sprintf "Invalid port number: %d" t.port)
      else
        Deferred.Or_error.return ()
    in
    
  Deferred.return (Ok ())

  let settings t =
    Logging.Global.warning "Using settings.settings is deprecated. Use settings directly instead.";
    t
end

let settings = Settings.create () 