open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Logging

(* Use Logging.Level for log levels - include to inherit yojson derivers *)
module Log_level = struct
  include Logging.Level
end

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

(* Common parsing functions *)
module Parsers = struct
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

  let parse_csv_list value =
    String.split ~on:',' value |> List.map ~f:String.strip

  let parse_log_level value =
    try Ok (Log_level.of_string value)
    with Failure msg -> Or_error.error_string msg
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

  let env_prefixes = [ "FASTMCP_"; "FASTMCP_SERVER_" ]
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
         `FASTMCP_` instead."
    else ();
    value

  (* Field update functions that handle parsing and updating directly *)
  let update_string_field lookup_fn env_name update_fn settings =
    match lookup_fn env_name with
    | Some value -> Ok (update_fn settings value)
    | None -> Ok settings

  let update_bool_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Parsers.parse_bool_or_error value env_name in
      Ok (update_fn settings parsed)
    | None -> Ok settings

  let update_int_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Parsers.parse_int_or_error value env_name in
      Ok (update_fn settings parsed)
    | None -> Ok settings

  let update_log_level_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Parsers.parse_log_level value in
      Ok (update_fn settings parsed)
    | None -> Ok settings

  let update_resource_prefix_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Resource_prefix_format.of_string value in
      Ok (update_fn settings parsed)
    | None -> Ok settings

  let update_string_list_field lookup_fn env_name update_fn settings =
    match lookup_fn env_name with
    | Some value -> Ok (update_fn settings (Parsers.parse_csv_list value))
    | None -> Ok settings

  let update_float_option_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Parsers.parse_float_or_error value env_name in
      Ok (update_fn settings (Some parsed))
    | None -> Ok settings

  let update_auth_provider_option_field lookup_fn env_name update_fn settings =
    let open Or_error.Let_syntax in
    match lookup_fn env_name with
    | Some value ->
      let%bind parsed = Auth_provider.of_string value in
      Ok (update_fn settings (Some parsed))
    | None -> Ok settings

  let update_string_list_option_field lookup_fn env_name update_fn settings =
    match lookup_fn env_name with
    | Some value ->
      Ok (update_fn settings (Some (Parsers.parse_csv_list value)))
    | None -> Ok settings

  (* Apply all field updates *)
  let apply_all_field_updates lookup_fn settings =
    let open Or_error.Let_syntax in
    let%bind s1 =
      update_string_field lookup_fn "HOME"
        (fun t v -> { t with home = v })
        settings
    in
    let%bind s2 =
      update_string_field lookup_fn "HOST" (fun t v -> { t with host = v }) s1
    in
    let%bind s3 =
      update_string_field lookup_fn "SSE_PATH"
        (fun t v -> { t with sse_path = v })
        s2
    in
    let%bind s4 =
      update_string_field lookup_fn "MESSAGE_PATH"
        (fun t v -> { t with message_path = v })
        s3
    in
    let%bind s5 =
      update_string_field lookup_fn "STREAMABLE_HTTP_PATH"
        (fun t v -> { t with streamable_http_path = v })
        s4
    in
    let%bind s6 =
      update_bool_field lookup_fn "TEST_MODE"
        (fun t v -> { t with test_mode = v })
        s5
    in
    let%bind s7 =
      update_bool_field lookup_fn "ENABLE_RICH_TRACEBACKS"
        (fun t v -> { t with enable_rich_tracebacks = v })
        s6
    in
    let%bind s8 =
      update_bool_field lookup_fn "DEPRECATION_WARNINGS"
        (fun t v -> { t with deprecation_warnings = v })
        s7
    in
    let%bind s9 =
      update_bool_field lookup_fn "CLIENT_RAISE_FIRST_EXCEPTIONGROUP_ERROR"
        (fun t v -> { t with client_raise_first_exceptiongroup_error = v })
        s8
    in
    let%bind s10 =
      update_bool_field lookup_fn "DEBUG" (fun t v -> { t with debug = v }) s9
    in
    let%bind s11 =
      update_bool_field lookup_fn "MASK_ERROR_DETAILS"
        (fun t v -> { t with mask_error_details = v })
        s10
    in
    let%bind s12 =
      update_bool_field lookup_fn "JSON_RESPONSE"
        (fun t v -> { t with json_response = v })
        s11
    in
    let%bind s13 =
      update_bool_field lookup_fn "STATELESS_HTTP"
        (fun t v -> { t with stateless_http = v })
        s12
    in
    let%bind s14 =
      update_int_field lookup_fn "PORT" (fun t v -> { t with port = v }) s13
    in
    let%bind s15 =
      update_log_level_field lookup_fn "LOG_LEVEL"
        (fun t v -> { t with log_level = v })
        s14
    in
    let%bind s16 =
      update_resource_prefix_field lookup_fn "RESOURCE_PREFIX_FORMAT"
        (fun t v -> { t with resource_prefix_format = v })
        s15
    in
    let%bind s17 =
      update_string_list_field lookup_fn "SERVER_DEPENDENCIES"
        (fun t v -> { t with server_dependencies = v })
        s16
    in
    let%bind s18 =
      update_float_option_field lookup_fn "CLIENT_INIT_TIMEOUT"
        (fun t v -> { t with client_init_timeout = v })
        s17
    in
    let%bind s19 =
      update_auth_provider_option_field lookup_fn "DEFAULT_AUTH_PROVIDER"
        (fun t v -> { t with default_auth_provider = v })
        s18
    in
    let%bind s20 =
      update_string_list_option_field lookup_fn "INCLUDE_TAGS"
        (fun t v -> { t with include_tags = v })
        s19
    in
    update_string_list_option_field lookup_fn "EXCLUDE_TAGS"
      (fun t v -> { t with exclude_tags = v })
      s20

  let load_from_env t =
    let try_prefixes var_name =
      List.find_map env_prefixes ~f:(fun prefix ->
          let full_name = prefix ^ var_name in
          match Sys.getenv full_name with
          | Some value -> Some (warn_if_deprecated prefix value)
          | None -> None)
    in
    apply_all_field_updates try_prefixes t

  let load_from_dotenv t =
    let%bind exists = Sys.file_exists env_file in
    match exists with
    | `Yes -> (
      try
        let lines = In_channel.read_lines env_file in
        let env_vars = Hashtbl.create (module String) in

        (* Parse .env file *)
        List.iter lines ~f:(fun line ->
            let trimmed = String.strip line in
            if
              (not (String.is_empty trimmed))
              && not (String.is_prefix trimmed ~prefix:"#")
            then
              match String.split trimmed ~on:'=' with
              | key :: value_parts when List.length value_parts >= 1 ->
                let value = String.concat ~sep:"=" value_parts in
                let clean_key = String.strip key in
                let clean_value = String.strip value in
                let unquoted_value =
                  if
                    String.is_prefix clean_value ~prefix:"\""
                    && String.is_suffix clean_value ~suffix:"\""
                    || String.is_prefix clean_value ~prefix:"'"
                       && String.is_suffix clean_value ~suffix:"'"
                  then String.slice clean_value 1 (-1)
                  else clean_value
                in
                Hashtbl.set env_vars ~key:clean_key ~data:unquoted_value
              | _ ->
                Logging.Global.warning
                  (sprintf "Malformed line in .env file: %s" trimmed));

        let try_dotenv_lookup var_name =
          List.find_map env_prefixes ~f:(fun prefix ->
              let full_name = prefix ^ var_name in
              Hashtbl.find env_vars full_name
              |> Option.map ~f:(warn_if_deprecated prefix))
        in

        return (apply_all_field_updates try_dotenv_lookup t)
      with
      | Sys_error msg ->
        return
          (Or_error.error_string (sprintf "Error reading .env file: %s" msg))
      | exn ->
        return
          (Or_error.error_string
             (sprintf "Error parsing .env file: %s" (Exn.to_string exn))))
    | _ -> return (Ok t)

  let load_from_file_secrets t =
    let secrets_dir = Filename.concat t.home "secrets" in

    try
      match Sys_unix.file_exists secrets_dir with
      | `Yes ->
        let open Or_error.Let_syntax in
        let%bind files =
          try Ok (Sys_unix.readdir secrets_dir)
          with exn ->
            Or_error.error_string
              (sprintf "Error reading secrets directory: %s" (Exn.to_string exn))
        in

        let secret_values = Hashtbl.create (module String) in

        (* Read secret files *)
        Array.iter files ~f:(fun filename ->
            let filepath = Filename.concat secrets_dir filename in
            try
              match Sys_unix.is_file filepath with
              | `Yes ->
                let content = In_channel.read_all filepath |> String.strip in
                let env_var_name = String.uppercase filename in
                Hashtbl.set secret_values ~key:env_var_name ~data:content
              | _ -> ()
            with exn ->
              Logging.Global.warning
                (sprintf "Error reading secret file %s: %s" filename
                   (Exn.to_string exn)));

        let try_secret_lookup var_name =
          List.find_map env_prefixes ~f:(fun prefix ->
              let full_name = prefix ^ var_name in
              let secret_key = String.uppercase full_name in
              Hashtbl.find secret_values secret_key)
        in

        apply_all_field_updates try_secret_lookup t
      | _ -> Ok t
    with exn ->
      Or_error.error_string
        (sprintf "Error loading file secrets: %s" (Exn.to_string exn))

  let merge base override =
    {
      base with
      log_level = override.log_level;
      resource_prefix_format = override.resource_prefix_format;
      default_auth_provider = override.default_auth_provider;
    }

  let validate t =
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
