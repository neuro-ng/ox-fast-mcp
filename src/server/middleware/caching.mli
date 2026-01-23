(** A middleware for response caching.

    The response caching middleware offers a simple way to cache responses to
    MCP methods. Supports TTL-based caching with configurable settings. *)

(** Constants *)

val one_hour_in_seconds : int
val five_minutes_in_seconds : int
val one_mb_in_bytes : int
val global_key : string

(** A wrapper for ReadResourceContents that can be cached. *)
module Cachable_read_resource_contents : sig
  type t = { content : string; mime_type : string option }
  [@@deriving sexp, yojson, compare]

  val get_size : t -> int
  val get_sizes : t list -> int
  val wrap : content:string -> ?mime_type:string -> unit -> t
  val unwrap : t -> string * string option
end

(** A wrapper for ToolResult that can be cached. *)
module Cachable_tool_result : sig
  type t = {
    content_json : string;
    structured_content_json : string option;
    meta_json : string option;
  }
  [@@deriving sexp, yojson, compare]

  val wrap :
    content_json:string ->
    ?structured_content_json:string ->
    ?meta_json:string ->
    unit ->
    t

  val unwrap : t -> string * string option * string option
end

(** Shared config for a cache method. *)
module Shared_method_settings : sig
  type t = { ttl : int option; enabled : bool option }
  [@@deriving sexp, yojson, compare]

  val create : ?ttl:int -> ?enabled:bool -> unit -> t
  val default : unit -> t
  val is_enabled : t -> bool
  val get_ttl : t -> default:int -> int
end

module List_tools_settings = Shared_method_settings
module List_resources_settings = Shared_method_settings
module List_prompts_settings = Shared_method_settings
module Read_resource_settings = Shared_method_settings
module Get_prompt_settings = Shared_method_settings

(** Configuration options for call_tool caching. *)
module Call_tool_settings : sig
  type t = {
    ttl : int option;
    enabled : bool option;
    included_tools : string list option;
    excluded_tools : string list option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?ttl:int ->
    ?enabled:bool ->
    ?included_tools:string list ->
    ?excluded_tools:string list ->
    unit ->
    t

  val default : unit -> t
  val is_enabled : t -> bool
  val get_ttl : t -> default:int -> int
  val matches_tool : string -> t -> bool
end

(** Cache entry with expiration time. *)
module Cache_entry : sig
  type 'a t [@@deriving sexp]

  val create : value:'a -> ttl_seconds:int -> 'a t
  val is_expired : 'a t -> bool
  val get_if_valid : 'a t -> 'a option
end

(** Statistics for get operations. *)
module Get_statistics : sig
  type t = { mutable count : int; mutable hit : int; mutable miss : int }
  [@@deriving sexp, yojson, compare]

  val create : unit -> t
  val record_hit : t -> unit
  val record_miss : t -> unit
end

(** Statistics for put operations. *)
module Put_statistics : sig
  type t = { mutable count : int } [@@deriving sexp, yojson, compare]

  val create : unit -> t
  val record : t -> unit
end

(** Collection statistics. *)
module Collection_statistics : sig
  type t = { get : Get_statistics.t; put : Put_statistics.t }
  [@@deriving sexp, yojson, compare]

  val create : unit -> t
end

(** Response caching statistics. *)
module Response_caching_statistics : sig
  type t = {
    list_tools : Collection_statistics.t option;
    list_resources : Collection_statistics.t option;
    list_prompts : Collection_statistics.t option;
    read_resource : Collection_statistics.t option;
    get_prompt : Collection_statistics.t option;
    call_tool : Collection_statistics.t option;
  }
  [@@deriving sexp, yojson, compare]

  val empty : unit -> t
end

(** Simple in-memory cache with TTL support. *)
module Memory_cache : sig
  type 'a t

  val create : ?max_item_size:int -> unit -> 'a t
  val get : 'a t -> string -> 'a option
  val put : 'a t -> key:string -> value:'a -> ttl_seconds:int -> unit
  val statistics : 'a t -> Collection_statistics.t
end

(** The response caching middleware configuration. *)
module Response_caching_config : sig
  type t [@@deriving sexp, compare]

  val create :
    ?list_tools_settings:List_tools_settings.t ->
    ?list_resources_settings:List_resources_settings.t ->
    ?list_prompts_settings:List_prompts_settings.t ->
    ?read_resource_settings:Read_resource_settings.t ->
    ?get_prompt_settings:Get_prompt_settings.t ->
    ?call_tool_settings:Call_tool_settings.t ->
    ?max_item_size:int ->
    unit ->
    t

  val matches_tool_cache_settings : t -> string -> bool
  val list_tools_ttl : t -> int
  val list_resources_ttl : t -> int
  val list_prompts_ttl : t -> int
  val call_tool_ttl : t -> int
  val read_resource_ttl : t -> int
  val get_prompt_ttl : t -> int
  val list_tools_enabled : t -> bool
  val list_resources_enabled : t -> bool
  val list_prompts_enabled : t -> bool
  val call_tool_enabled : t -> bool
  val read_resource_enabled : t -> bool
  val get_prompt_enabled : t -> bool
end

val get_arguments_str : Yojson.Safe.t option -> string
(** Get a string representation of the arguments for cache key. *)

(** Response caching middleware instance *)
type response_caching_middleware

val create_middleware :
  ?config:Response_caching_config.t -> unit -> response_caching_middleware

(** ResponseCachingMiddleware implementing Middleware.S *)
module ResponseCachingMiddleware : Middleware.S

val clear_all_caches : response_caching_middleware -> unit
(** Clear all caches in the middleware instance *)

val invalidate_cache_on_notification :
  response_caching_middleware -> string -> unit
(** Invalidate specific cache based on notification method name *)
