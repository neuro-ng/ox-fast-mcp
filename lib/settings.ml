open Core
open Async

module Log_level = struct
  type t =
    | Debug
    | Info
    | Warning
    | Error
    | Critical
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  let of_string = function
    | "DEBUG" -> Ok Debug
    | "INFO" -> Ok Info
    | "WARNING" -> Ok Warning
    | "ERROR" -> Ok Error
    | "CRITICAL" -> Ok Critical
    | s -> Error (sprintf "Invalid log level: %s" s)
end

module Duplicate_behavior = struct
  type t =
    | Warn
    | Error
    | Replace
    | Ignore
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  let of_string = function
    | "warn" -> Ok Warn
    | "error" -> Ok Error
    | "replace" -> Ok Replace
    | "ignore" -> Ok Ignore
    | s -> Error (sprintf "Invalid duplicate behavior: %s" s)
end

module Resource_prefix_format = struct
  type t =
    | Protocol
    | Path
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  let of_string = function
    | "protocol" -> Ok Protocol
    | "path" -> Ok Path
    | s -> Error (sprintf "Invalid resource prefix format: %s" s)
end

module Auth_provider = struct
  type t =
    | Bearer_env
    | None
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  let of_string = function
    | "bearer_env" -> Ok Bearer_env
    | "none" -> Ok None
    | s -> Error (sprintf "Invalid auth provider: %s" s)
end

module Settings_error = struct
  type t =
    | Invalid_env_var of string * string
    | Missing_required_field of string
    | Invalid_field_value of string * string
    | File_error of string
  [@@deriving sexp]

  exception Settings_error of t

  let to_string = function
    | Invalid_env_var (name, value) ->
      sprintf "Invalid environment variable %s: %s" name value
    | Missing_required_field name -> sprintf "Missing required field: %s" name
    | Invalid_field_value (field, value) ->
      sprintf "Invalid value for field %s: %s" field value
    | File_error msg -> sprintf "File error: %s" msg
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
    default_auth_provider : Auth_provider.t;
    include_tags : string list option;
    exclude_tags : string list option;
  } [@@deriving yojson_of, of_yojson, sexp, fields]

  let env_prefixes = ["FASTMCP_"; "FASTMCP_SERVER_"]

  let bool_of_string s =
    match String.lowercase s with
    | "true" | "1" | "yes" | "on" -> Ok true
    | "false" | "0" | "no" | "off" -> Ok false
    | _ -> Error "Invalid boolean value"

  let list_of_string s =
    String.split ~on:',' s |> List.map ~f:String.strip

  let get_env_var ?(prefixes = env_prefixes) name =
    List.find_map prefixes ~f:(fun prefix ->
        let env_name = prefix ^ name in
        match Sys.getenv env_name with
        | Some value -> Some (env_name, value)
        | None -> None)

  let validate_field field_name value_str conv_fn =
    match conv_fn value_str with
    | Ok v -> v
    | Error msg ->
      raise
        (Settings_error.Settings_error
           (Invalid_field_value (field_name, msg)))

  let load_from_env () =
    let get_env_field name conv_fn =
      match get_env_var name with
      | Some (env_name, value) -> Some (validate_field env_name value conv_fn)
      | None -> None
    in
    let get_env_bool = get_env_field ~conv_fn:bool_of_string in
    let get_env_int name =
      get_env_field name ~conv_fn:(fun s ->
          try Ok (Int.of_string s) with
          | _ -> Error "Invalid integer value")
    in
    let get_env_float name =
      get_env_field name ~conv_fn:(fun s ->
          try Ok (Float.of_string s) with
          | _ -> Error "Invalid float value")
    in
    let get_env_string_list name =
      Option.map (get_env_var name) ~f:(fun (_, value) -> list_of_string value)
    in
    {
      home =
        (match get_env_var "HOME" with
        | Some (_, value) -> value
        | None -> Filename.concat (Sys.home_directory ()) ".fastmcp");
      test_mode = Option.value (get_env_bool "TEST_MODE") ~default:false;
      log_level =
        Option.value
          (get_env_field "LOG_LEVEL" Log_level.of_string)
          ~default:Log_level.Info;
      enable_rich_tracebacks =
        Option.value (get_env_bool "ENABLE_RICH_TRACEBACKS") ~default:true;
      deprecation_warnings =
        Option.value (get_env_bool "DEPRECATION_WARNINGS") ~default:true;
      client_raise_first_exceptiongroup_error =
        Option.value
          (get_env_bool "CLIENT_RAISE_FIRST_EXCEPTIONGROUP_ERROR")
          ~default:true;
      resource_prefix_format =
        Option.value
          (get_env_field "RESOURCE_PREFIX_FORMAT" Resource_prefix_format.of_string)
          ~default:Resource_prefix_format.Path;
      client_init_timeout =
        Option.map (get_env_float "CLIENT_INIT_TIMEOUT") ~f:Fn.id;
      host = Option.value (get_env_var "HOST" |> Option.map ~f:snd) ~default:"127.0.0.1";
      port = Option.value (get_env_int "PORT") ~default:8000;
      sse_path =
        Option.value (get_env_var "SSE_PATH" |> Option.map ~f:snd) ~default:"/sse/";
      message_path =
        Option.value
          (get_env_var "MESSAGE_PATH" |> Option.map ~f:snd)
          ~default:"/messages/";
      streamable_http_path =
        Option.value
          (get_env_var "STREAMABLE_HTTP_PATH" |> Option.map ~f:snd)
          ~default:"/mcp/";
      debug = Option.value (get_env_bool "DEBUG") ~default:false;
      mask_error_details =
        Option.value (get_env_bool "MASK_ERROR_DETAILS") ~default:false;
      server_dependencies =
        Option.value (get_env_string_list "SERVER_DEPENDENCIES") ~default:[];
      json_response = Option.value (get_env_bool "JSON_RESPONSE") ~default:false;
      stateless_http = Option.value (get_env_bool "STATELESS_HTTP") ~default:false;
      default_auth_provider =
        Option.value
          (get_env_field "DEFAULT_AUTH_PROVIDER" Auth_provider.of_string)
          ~default:Auth_provider.None;
      include_tags = get_env_string_list "INCLUDE_TAGS";
      exclude_tags = get_env_string_list "EXCLUDE_TAGS";
    }

  let create
      ?(home = Filename.concat (Sys.home_directory ()) ".fastmcp")
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
      ?(default_auth_provider = Auth_provider.None)
      ?(include_tags = None)
      ?(exclude_tags = None)
      () =
    { home;
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

  let with_error_handling f =
    match Exceptions.handle_errors f with
    | Ok result -> result
    | Error msg ->
      raise (Settings_error.Settings_error (Settings_error.File_error msg))

  let load_from_file path =
    with_error_handling (fun () ->
      let json = Yojson.Safe.from_file path in
      match t_of_yojson json with
      | Ok settings -> settings
      | Error msg ->
        raise
          (Settings_error.Settings_error
             (Settings_error.File_error
                (sprintf "Failed to parse settings file: %s" msg))))

  let save_to_file t path =
    with_error_handling (fun () ->
      let json = yojson_of_t t in
      Yojson.Safe.to_file path json)

  let load_dotenv ?(path = ".env") () =
    with_error_handling (fun () ->
      Utilities.Dotenv.load_into_env ~path ();
      load_from_env ())

  let configure_logging t =
    let open Logs in
    let fmt_reporter = Logs_fmt.reporter () in
    let level =
      match t.log_level with
      | Debug -> Some Debug
      | Info -> Some Info
      | Warning -> Some Warning
      | Error -> Some Error
      | Critical -> Some Error (* Logs doesn't have Critical *)
    in
    set_reporter fmt_reporter;
    set_level level;
    if t.enable_rich_tracebacks then
      Printexc.record_backtrace true
    else
      Printexc.record_backtrace false;
    if t.deprecation_warnings then
      Caml.Sys.enable_runtime_warnings true
    else
      Caml.Sys.enable_runtime_warnings false

  let with_logging t f =
    with_error_handling (fun () ->
      configure_logging t;
      f ())

  let default = create ()
end

let settings = Settings.default 