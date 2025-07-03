open Core
open Async

module Log_level : sig
  type t =
    | Debug
    | Info
    | Warning
    | Error
    | Critical
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  val of_string : string -> (t, string) Result.t
end

module Duplicate_behavior : sig
  type t =
    | Warn
    | Error
    | Replace
    | Ignore
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  val of_string : string -> (t, string) Result.t
end

module Resource_prefix_format : sig
  type t =
    | Protocol
    | Path
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  val of_string : string -> (t, string) Result.t
end

module Auth_provider : sig
  type t =
    | Bearer_env
    | None
  [@@deriving yojson_of, of_yojson, sexp, compare, equal]

  val of_string : string -> (t, string) Result.t
end

module Settings_error : sig
  type t =
    | Invalid_env_var of string * string
    | Missing_required_field of string
    | Invalid_field_value of string * string
    | File_error of string
  [@@deriving sexp]

  exception Settings_error of t

  val to_string : t -> string
end

module Settings : sig
  type t = private {
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

  (** Environment variable prefixes used for loading settings *)
  val env_prefixes : string list

  (** Load settings from environment variables *)
  val load_from_env : unit -> t

  (** Load settings from a JSON file *)
  val load_from_file : string -> t

  (** Save settings to a JSON file *)
  val save_to_file : t -> string -> unit

  (** Create a new settings value with optional parameters *)
  val create :
    ?home:string
    -> ?test_mode:bool
    -> ?log_level:Log_level.t
    -> ?enable_rich_tracebacks:bool
    -> ?deprecation_warnings:bool
    -> ?client_raise_first_exceptiongroup_error:bool
    -> ?resource_prefix_format:Resource_prefix_format.t
    -> ?client_init_timeout:float option
    -> ?host:string
    -> ?port:int
    -> ?sse_path:string
    -> ?message_path:string
    -> ?streamable_http_path:string
    -> ?debug:bool
    -> ?mask_error_details:bool
    -> ?server_dependencies:string list
    -> ?json_response:bool
    -> ?stateless_http:bool
    -> ?default_auth_provider:Auth_provider.t
    -> ?include_tags:string list option
    -> ?exclude_tags:string list option
    -> unit
    -> t

  (** Default settings instance *)
  val default : t

  (** Load settings from a .env file and environment variables *)
  val load_dotenv : ?path:string -> unit -> t

  (** Configure logging based on settings *)
  val configure_logging : t -> unit

  (** Run a function with configured logging *)
  val with_logging : t -> (unit -> 'a) -> 'a
end

(** Global settings instance *)
val settings : Settings.t 