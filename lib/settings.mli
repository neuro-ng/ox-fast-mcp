open! Core
open! Async

(* Use Log_types.Level for log levels *)
module Log_level = Log_types.Level

module Duplicate_behavior : sig
  type t =
    | Warn
    | Error
    | Replace
    | Ignore
  [@@deriving compare, equal, sexp, yojson]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Resource_prefix_format : sig
  type t =
    | Protocol
    | Path
  [@@deriving compare, equal, sexp, yojson]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Auth_provider : sig
  type t =
    | Bearer_env
  [@@deriving compare, equal, sexp, yojson]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Settings_error : sig
  type t =
    | Invalid_log_level of string
    | Invalid_duplicate_behavior of string
    | Invalid_resource_prefix_format of string
    | Invalid_auth_provider of string
    | Invalid_env_value of string * string  (* var_name * value *)
    | Missing_required_env of string        (* var_name *)
  [@@deriving sexp, compare]

  exception Settings_error of t

  val to_string : t -> string
end

module Settings_source : sig
  type t =
    | Init
    | Environment
    | Dotenv
    | File_secrets
  [@@deriving compare, equal, sexp]

  val priority : t -> int
end

module Settings : sig
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

  val create : unit -> t
  val configure_logging : t -> Logging.Logger.t

  (** Environment variable handling *)
  val get_env_value : string -> (string, Base.Error.t) result
  val env_prefixes : string list
  val env_nested_delimiter : string
  val env_file : string

  (** Settings source customization *)
  val load_from_env : t -> t Or_error.t
  val load_from_dotenv : t -> (t, Core.Error.t) result Async.Deferred.t
  val load_from_file_secrets : t -> t Or_error.t
  val merge : t -> t -> t

  (** Validation *)
  val validate : t -> unit Async_kernel__Deferred_or_error.t

  (** Deprecated functionality *)
  val settings : t -> t
  [@@deprecated "[since 2.8.0] Use the settings value directly instead of accessing through settings property"]
end

val settings : Settings.t 