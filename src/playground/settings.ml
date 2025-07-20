open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Logging

(* Use Log_types.Level for log levels *)
module Log_level = Log_types.Level

module Duplicate_behavior = struct
  type t = Warn | Error | Replace | Ignore
  [@@deriving compare, equal, sexp, yojson]

  let of_string = function
    | "warn" -> Ok Warn
    | "error" -> Ok Error
    | "replace" -> Ok Replace
    | "ignore" -> Ok Ignore
    | s -> failwith (sprintf "Invalid duplicate behavior: %s" s)

  let to_string = function
    | Warn -> "warn"
    | Error -> "error"
    | Replace -> "replace"
    | Ignore -> "ignore"
end

module Resource_prefix_format = struct
  type t = Protocol | Path [@@deriving compare, equal, sexp, yojson]

  let of_string = function
    | "protocol" -> Ok Protocol
    | "path" -> Ok Path
    | s -> failwith (sprintf "Invalid resource prefix format: %s" s)

  let to_string = function
    | Protocol -> "protocol"
    | Path -> "path"
end

module Auth_provider = struct
  type t = Bearer_env [@@deriving compare, equal, sexp, yojson]

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
    | Invalid_env_value of string * string (* var_name * value *)
    | Missing_required_env of string (* var_name *)
  [@@deriving sexp, compare]

  exception Settings_error of t

  let to_string = function
    | Invalid_log_level s -> sprintf "Invalid log level: %s" s
    | Invalid_duplicate_behavior s -> sprintf "Invalid duplicate behavior: %s" s
    | Invalid_resource_prefix_format s ->
      sprintf "Invalid resource prefix format: %s" s
    | Invalid_auth_provider s -> sprintf "Invalid auth provider: %s" s
    | Invalid_env_value (var, value) ->
      sprintf "Invalid value '%s' for environment variable %s" value var
    | Missing_required_env var ->
      sprintf "Missing required environment variable: %s" var
end

module Settings_source = struct
  type t = Init | Environment | Dotenv | File_secrets
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

  let env_prefixes = [ "OXFASTMCP_"; "OXFASTMCP_SERVER_" ]
  let env_nested_delimiter = "__"
  let env_file = ".env"

  let create
      ?(home =
        match Sys.getenv "HOME" with
        | Some home -> Filename.concat home ".oxfastmcp"
        | None -> Filename.concat "/tmp" ".oxfastmcp") ?(test_mode = false)
      ?(log_level = Log_level.Info) ?(enable_rich_tracebacks = true)
      ?(deprecation_warnings = true)
      ?(client_raise_first_exceptiongroup_error = true)
      ?(resource_prefix_format = Resource_prefix_format.Path)
      ?(client_init_timeout = None) ?(host = "127.0.0.1") ?(port = 8000)
      ?(sse_path = "/sse/") ?(message_path = "/messages/")
      ?(streamable_http_path = "/mcp/") ?(debug = false)
      ?(mask_error_details = false) ?(server_dependencies = [])
      ?(json_response = false) ?(stateless_http = false)
      ?(default_auth_provider = None) ?(include_tags = None)
      ?(exclude_tags = None) () =
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
    Logging.configure_logging ~level:t.log_level
      ~enable_rich_tracebacks:t.enable_rich_tracebacks ()

  let get_env_value var_name =
    match Sys.getenv var_name with
    | Some value -> Ok value
    | None ->
      Or_error.error_string
        (sprintf "Environment variable %s not found" var_name)

  let warn_if_deprecated prefix value =
    if String.equal prefix "OX_FASTMCP_SERVER_" then
      Logging.Global.warning
        "Using `OX_FASTMCP_SERVER_` environment variables is deprecated. Use \
         `OXFASTMCP_` instead."
    else ();
    value

  let parse_bool_or_error value var_name =
    match String.lowercase value with
    | "true" | "1" -> Ok true
    | "false" | "0" -> Ok false
    | _ ->
      Or_error.error_string
        (sprintf
           "Invalid boolean value for %s: '%s'. Expected 'true'/'false' or \
            '1'/'0'."
           var_name value)

  let parse_int_or_error value var_name =
    try Ok (Int.of_string value)
    with _ ->
      Or_error.error_string
        (sprintf "Invalid integer value for %s: '%s'." var_name value)

  let parse_float_or_error value var_name =
    try Ok (Float.of_string value)
    with _ ->
      Or_error.error_string
        (sprintf "Invalid float value for %s: '%s'." var_name value)

  let load_from_env t =
    let open Or_error.Let_syntax in
    let try_prefixes var_name =
      List.find_map env_prefixes ~f:(fun prefix ->
          let full_name = prefix ^ var_name in
          match Sys.getenv full_name with
          | Some value -> Some (warn_if_deprecated prefix value)
          | None -> None)
    in

    let%bind home =
      match try_prefixes "HOME" with
      | Some value -> Ok value
      | None -> Ok t.home
    in

    let%bind test_mode =
      match try_prefixes "TEST_MODE" with
      | Some value -> parse_bool_or_error value "TEST_MODE"
      | None -> Ok t.test_mode
    in

    let%bind log_level =
      try
        match try_prefixes "LOG_LEVEL" with
        | Some value -> Ok (Log_level.of_string value)
        | None -> Ok t.log_level
      with Failure msg -> Or_error.error_string msg
    in

    let%bind enable_rich_tracebacks =
      match try_prefixes "ENABLE_RICH_TRACEBACKS" with
      | Some value -> parse_bool_or_error value "ENABLE_RICH_TRACEBACKS"
      | None -> Ok t.enable_rich_tracebacks
    in

    let%bind deprecation_warnings =
      match try_prefixes "DEPRECATION_WARNINGS" with
      | Some value -> parse_bool_or_error value "DEPRECATION_WARNINGS"
      | None -> Ok t.deprecation_warnings
    in

    let%bind client_raise_first_exceptiongroup_error =
      match try_prefixes "CLIENT_RAISE_FIRST_EXCEPTIONGROUP_ERROR" with
      | Some value ->
        parse_bool_or_error value "CLIENT_RAISE_FIRST_EXCEPTIONGROUP_ERROR"
      | None -> Ok t.client_raise_first_exceptiongroup_error
    in

    let%bind resource_prefix_format =
      match try_prefixes "RESOURCE_PREFIX_FORMAT" with
      | Some value -> Resource_prefix_format.of_string value
      | None -> Ok t.resource_prefix_format
    in

    let%bind client_init_timeout =
      match try_prefixes "CLIENT_INIT_TIMEOUT" with
      | Some value ->
        let%bind f = parse_float_or_error value "CLIENT_INIT_TIMEOUT" in
        Ok (Some f)
      | None -> Ok t.client_init_timeout (* Keep existing option state *)
    in

    let%bind host =
      match try_prefixes "HOST" with
      | Some value -> Ok value
      | None -> Ok t.host
    in

    let%bind port =
      match try_prefixes "PORT" with
      | Some value -> parse_int_or_error value "PORT"
      | None -> Ok t.port
    in

    let%bind sse_path =
      match try_prefixes "SSE_PATH" with
      | Some value -> Ok value
      | None -> Ok t.sse_path
    in

    let%bind message_path =
      match try_prefixes "MESSAGE_PATH" with
      | Some value -> Ok value
      | None -> Ok t.message_path
    in

    let%bind streamable_http_path =
      match try_prefixes "STREAMABLE_HTTP_PATH" with
      | Some value -> Ok value
      | None -> Ok t.streamable_http_path
    in

    let%bind debug =
      match try_prefixes "DEBUG" with
      | Some value -> parse_bool_or_error value "DEBUG"
      | None -> Ok t.debug
    in

    let%bind mask_error_details =
      match try_prefixes "MASK_ERROR_DETAILS" with
      | Some value -> parse_bool_or_error value "MASK_ERROR_DETAILS"
      | None -> Ok t.mask_error_details
    in

    let%bind server_dependencies =
      match try_prefixes "SERVER_DEPENDENCIES" with
      | Some value -> Ok (String.split ~on:',' value |> List.map ~f:String.strip)
      | None -> Ok t.server_dependencies
    in

    let%bind json_response =
      match try_prefixes "JSON_RESPONSE" with
      | Some value -> parse_bool_or_error value "JSON_RESPONSE"
      | None -> Ok t.json_response
    in

    let%bind stateless_http =
      match try_prefixes "STATELESS_HTTP" with
      | Some value -> parse_bool_or_error value "STATELESS_HTTP"
      | None -> Ok t.stateless_http
    in

    let%bind default_auth_provider =
      match try_prefixes "DEFAULT_AUTH_PROVIDER" with
      | Some value ->
        let%bind provider = Auth_provider.of_string value in
        Ok (Some provider)
      | None -> Ok t.default_auth_provider
    in

    let%bind include_tags =
      match try_prefixes "INCLUDE_TAGS" with
      | Some value ->
        Ok (Some (String.split ~on:',' value |> List.map ~f:String.strip))
      | None -> Ok t.include_tags (* Keep existing option state *)
    in

    let%bind exclude_tags =
      match try_prefixes "EXCLUDE_TAGS" with
      | Some value ->
        Ok (Some (String.split ~on:',' value |> List.map ~f:String.strip))
      | None -> Ok t.exclude_tags (* Keep existing option state *)
    in

    (* Construct the new record with all updated fields *)
    Ok
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
    {
      base with
      log_level = override.log_level;
      resource_prefix_format = override.resource_prefix_format;
      default_auth_provider = override.default_auth_provider;
      (* Add other fields as needed *)
    }

  let validate t =
    (* Validate home path *)
    if String.equal t.home "" then failwith "Home path cannot be empty"
    else if t.port < 0 || t.port > 65535 then
      failwith (sprintf "Invalid port number: %d" t.port)
    else Ok ()

  let settings t =
    Logging.Global.warning
      "Using settings.settings is deprecated. Use settings directly instead.";
    t
end

let settings = Settings.create ()
