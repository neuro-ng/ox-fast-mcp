(** A middleware for response caching.

    The response caching middleware offers a simple way to cache responses to
    MCP methods. The Middleware supports TTL-based caching with configurable
    settings for each method type.

    Caches:
    - tools/list, resources/list, prompts/list (default: 5 minute TTL)
    - tools/call, resources/read, prompts/get (default: 1 hour TTL) *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let _logger = Logging.Logger.get_logger "ox-fast-mcp.server.middleware.caching"

(* Constants *)
let one_hour_in_seconds = 3600
let five_minutes_in_seconds = 300
let one_mb_in_bytes = 1024 * 1024
let global_key = "__global__"

(* =============================================================================
   Cachable Types
   ============================================================================= *)

(** A wrapper for ReadResourceContents that can be cached. *)
module Cachable_read_resource_contents = struct
  type t = { content : string; mime_type : string option }
  [@@deriving sexp, yojson, compare]

  let get_size t =
    let json_str = Yojson.Safe.to_string (yojson_of_t t) in
    String.length json_str

  let get_sizes values = List.sum (module Int) values ~f:get_size
  let wrap ~content ?mime_type () = { content; mime_type }
  let unwrap t = (t.content, t.mime_type)
end

(** A wrapper for ToolResult that can be cached. *)
module Cachable_tool_result = struct
  type t = {
    content_json : string; (* JSON-serialized content blocks *)
    structured_content_json : string option;
        (* JSON-serialized structured content *)
    meta_json : string option; (* JSON-serialized meta *)
  }
  [@@deriving sexp, yojson, compare]

  let wrap ~content_json ?structured_content_json ?meta_json () =
    { content_json; structured_content_json; meta_json }

  let unwrap t = (t.content_json, t.structured_content_json, t.meta_json)
end

(* =============================================================================
   Settings Types
   ============================================================================= *)

(** Shared config for a cache method. *)
module Shared_method_settings = struct
  type t = { ttl : int option; (* TTL in seconds *) enabled : bool option }
  [@@deriving sexp, yojson, compare]

  let create ?ttl ?enabled () = { ttl; enabled }
  let default () = { ttl = None; enabled = None }
  let is_enabled t = Option.value t.enabled ~default:true
  let get_ttl t ~default = Option.value t.ttl ~default
end

module List_tools_settings = Shared_method_settings
(** Configuration options for list method caching. *)

module List_resources_settings = Shared_method_settings
module List_prompts_settings = Shared_method_settings

(** Configuration options for call_tool caching. *)
module Call_tool_settings = struct
  type t = {
    ttl : int option;
    enabled : bool option;
    included_tools : string list option;
    excluded_tools : string list option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?ttl ?enabled ?included_tools ?excluded_tools () =
    { ttl; enabled; included_tools; excluded_tools }

  let default () =
    { ttl = None; enabled = None; included_tools = None; excluded_tools = None }

  let is_enabled t = Option.value t.enabled ~default:true
  let get_ttl t ~default = Option.value t.ttl ~default

  (** Check if the tool matches the cache settings for tool calls. *)
  let matches_tool tool_name t =
    let included_ok =
      match t.included_tools with
      | None -> true
      | Some [] -> true
      | Some tools -> List.mem tools tool_name ~equal:String.equal
    in
    let excluded_ok =
      match t.excluded_tools with
      | None -> true
      | Some excluded -> not (List.mem excluded tool_name ~equal:String.equal)
    in
    included_ok && excluded_ok
end

module Read_resource_settings = Shared_method_settings
module Get_prompt_settings = Shared_method_settings

(* =============================================================================
   Cache Entry
   ============================================================================= *)

(** Cache entry with expiration time. *)
module Cache_entry = struct
  type 'a t = { value : 'a; expires_at : float (* Unix timestamp *) }
  [@@deriving sexp]

  let create ~value ~ttl_seconds =
    let now = Core_unix.gettimeofday () in
    { value; expires_at = now +. Float.of_int ttl_seconds }

  let is_expired t =
    let now = Core_unix.gettimeofday () in
    Float.( >= ) now t.expires_at

  let get_if_valid t = if is_expired t then None else Some t.value
end

(* =============================================================================
   Cache Statistics
   ============================================================================= *)

(** Statistics for get operations. *)
module Get_statistics = struct
  type t = { mutable count : int; mutable hit : int; mutable miss : int }
  [@@deriving sexp, yojson, compare]

  let create () = { count = 0; hit = 0; miss = 0 }

  let record_hit t =
    t.count <- t.count + 1;
    t.hit <- t.hit + 1

  let record_miss t =
    t.count <- t.count + 1;
    t.miss <- t.miss + 1
end

(** Statistics for put operations. *)
module Put_statistics = struct
  type t = { mutable count : int } [@@deriving sexp, yojson, compare]

  let create () = { count = 0 }
  let record t = t.count <- t.count + 1
end

(** Collection statistics. *)
module Collection_statistics = struct
  type t = { get : Get_statistics.t; put : Put_statistics.t }
  [@@deriving sexp, yojson, compare]

  let create () =
    { get = Get_statistics.create (); put = Put_statistics.create () }
end

(** Response caching statistics. *)
module Response_caching_statistics = struct
  type t = {
    list_tools : Collection_statistics.t option;
    list_resources : Collection_statistics.t option;
    list_prompts : Collection_statistics.t option;
    read_resource : Collection_statistics.t option;
    get_prompt : Collection_statistics.t option;
    call_tool : Collection_statistics.t option;
  }
  [@@deriving sexp, yojson, compare]

  let empty () =
    {
      list_tools = None;
      list_resources = None;
      list_prompts = None;
      read_resource = None;
      get_prompt = None;
      call_tool = None;
    }
end

(* =============================================================================
   In-Memory Cache
   ============================================================================= *)

(** Simple in-memory cache with TTL support. *)
module Memory_cache = struct
  type 'a t = {
    storage : (string, 'a Cache_entry.t, String.comparator_witness) Map.t ref;
    max_item_size : int;
    stats : Collection_statistics.t;
  }

  let create ?(max_item_size = one_mb_in_bytes) () =
    {
      storage = ref (Map.empty (module String));
      max_item_size;
      stats = Collection_statistics.create ();
    }

  let get t key =
    Get_statistics.record_miss t.stats.get;
    match Map.find !(t.storage) key with
    | None -> None
    | Some entry -> (
      match Cache_entry.get_if_valid entry with
      | None ->
        (* Expired, remove from cache *)
        t.storage := Map.remove !(t.storage) key;
        None
      | Some value ->
        (* Update stats for hit *)
        t.stats.get.miss <- t.stats.get.miss - 1;
        Get_statistics.record_hit t.stats.get;
        t.stats.get.count <- t.stats.get.count - 1;
        Some value)

  let put t ~key ~value ~ttl_seconds =
    let entry = Cache_entry.create ~value ~ttl_seconds in
    t.storage := Map.set !(t.storage) ~key ~data:entry;
    Put_statistics.record t.stats.put

  let statistics t = t.stats
end

(* =============================================================================
   Response Caching Middleware
   ============================================================================= *)

(** The response caching middleware configuration. *)
module Response_caching_config = struct
  type t = {
    list_tools_settings : List_tools_settings.t;
    list_resources_settings : List_resources_settings.t;
    list_prompts_settings : List_prompts_settings.t;
    read_resource_settings : Read_resource_settings.t;
    get_prompt_settings : Get_prompt_settings.t;
    call_tool_settings : Call_tool_settings.t;
    max_item_size : int;
  }
  [@@deriving sexp, compare]

  let create ?list_tools_settings ?list_resources_settings
      ?list_prompts_settings ?read_resource_settings ?get_prompt_settings
      ?call_tool_settings ?(max_item_size = one_mb_in_bytes) () =
    {
      list_tools_settings =
        Option.value list_tools_settings
          ~default:(List_tools_settings.default ());
      list_resources_settings =
        Option.value list_resources_settings
          ~default:(List_resources_settings.default ());
      list_prompts_settings =
        Option.value list_prompts_settings
          ~default:(List_prompts_settings.default ());
      read_resource_settings =
        Option.value read_resource_settings
          ~default:(Read_resource_settings.default ());
      get_prompt_settings =
        Option.value get_prompt_settings
          ~default:(Get_prompt_settings.default ());
      call_tool_settings =
        Option.value call_tool_settings ~default:(Call_tool_settings.default ());
      max_item_size;
    }

  (** Check if a tool matches the call_tool cache settings. *)
  let matches_tool_cache_settings t tool_name =
    Call_tool_settings.matches_tool tool_name t.call_tool_settings

  (** Get TTL for list_tools *)
  let list_tools_ttl t =
    Shared_method_settings.get_ttl t.list_tools_settings
      ~default:five_minutes_in_seconds

  (** Get TTL for list_resources *)
  let list_resources_ttl t =
    Shared_method_settings.get_ttl t.list_resources_settings
      ~default:five_minutes_in_seconds

  (** Get TTL for list_prompts *)
  let list_prompts_ttl t =
    Shared_method_settings.get_ttl t.list_prompts_settings
      ~default:five_minutes_in_seconds

  (** Get TTL for call_tool *)
  let call_tool_ttl t =
    Call_tool_settings.get_ttl t.call_tool_settings ~default:one_hour_in_seconds

  (** Get TTL for read_resource *)
  let read_resource_ttl t =
    Shared_method_settings.get_ttl t.read_resource_settings
      ~default:one_hour_in_seconds

  (** Get TTL for get_prompt *)
  let get_prompt_ttl t =
    Shared_method_settings.get_ttl t.get_prompt_settings
      ~default:one_hour_in_seconds

  (** Check if list_tools caching is enabled *)
  let list_tools_enabled t =
    Shared_method_settings.is_enabled t.list_tools_settings

  (** Check if list_resources caching is enabled *)
  let list_resources_enabled t =
    Shared_method_settings.is_enabled t.list_resources_settings

  (** Check if list_prompts caching is enabled *)
  let list_prompts_enabled t =
    Shared_method_settings.is_enabled t.list_prompts_settings

  (** Check if call_tool caching is enabled *)
  let call_tool_enabled t = Call_tool_settings.is_enabled t.call_tool_settings

  (** Check if read_resource caching is enabled *)
  let read_resource_enabled t =
    Shared_method_settings.is_enabled t.read_resource_settings

  (** Check if get_prompt caching is enabled *)
  let get_prompt_enabled t =
    Shared_method_settings.is_enabled t.get_prompt_settings
end

(** Get a string representation of the arguments for cache key. *)
let get_arguments_str arguments =
  match arguments with
  | None -> "null"
  | Some json -> Yojson.Safe.to_string json
