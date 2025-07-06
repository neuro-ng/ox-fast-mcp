open Core
open Async

(* Use the Log_level from utilities/logging *)
module Log_level = Logging.Log_level

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
  [@@deriving sexp, compare]

  exception Settings_error of t

  val to_string : t -> string
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
  val configure_logging : t -> Logger.t
end

val settings : Settings.t 