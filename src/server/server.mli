(** OxFastMCP Server Module

    The main server module providing the OxFastMCP server implementation. This
    is a more ergonomic interface for MCP servers with tool, resource, and
    prompt management, middleware support, and transport handling. *)

open! Core
open! Async

(** {1 Types} *)

(** Transport protocols for server communication *)
module Transport : sig
  type t = Stdio | Http | Sse | Streamable_http
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** Behavior when encountering duplicate components *)
module Duplicate_behavior : sig
  type t = Warn | Error | Replace | Ignore
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** Resource prefix format for mounted servers *)
module Resource_prefix_format : sig
  type t = Protocol | Path [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** {1 Component Types} *)

(** Tool representation *)
module Tool : sig
  type t = {
    name : string;
    key : string;
    description : string option;
    parameters : Yojson.Safe.t;
    annotations : Yojson.Safe.t option;
    output_schema : Yojson.Safe.t option;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    handler : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  val create :
    name:string ->
    ?description:string ->
    ?parameters:Yojson.Safe.t ->
    ?annotations:Yojson.Safe.t ->
    ?output_schema:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    handler:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    unit ->
    t

  val to_mcp_tool : ?include_fastmcp_meta:bool -> t -> Yojson.Safe.t
end

(** Resource representation *)
module Resource : sig
  type t = {
    uri : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    reader : unit -> string Deferred.t;
  }

  val create :
    uri:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    reader:(unit -> string Deferred.t) ->
    unit ->
    t

  val to_mcp_resource : ?include_fastmcp_meta:bool -> t -> Yojson.Safe.t
end

(** Resource template representation *)
module Resource_template : sig
  type t = {
    uri_template : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    parameters : Yojson.Safe.t;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    create_resource : params:(string * string) list -> Resource.t Deferred.t;
  }

  val create :
    uri_template:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?parameters:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    create_resource:(params:(string * string) list -> Resource.t Deferred.t) ->
    unit ->
    t
end

(** Prompt representation *)
module Prompt : sig
  type argument = {
    name : string;
    description : string option;
    required : bool;
  }

  type t = {
    name : string;
    key : string;
    description : string option;
    arguments : argument list;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    render : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  val create :
    name:string ->
    ?description:string ->
    ?arguments:argument list ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    render:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    unit ->
    t
end

(** {1 Middleware} *)

module Middleware : sig
  type context = {
    message : Yojson.Safe.t;
    source : string;
    type_ : string;
    method_ : string;
  }

  type next = context -> Yojson.Safe.t Deferred.t
  type t = context -> next:next -> Yojson.Safe.t Deferred.t

  val identity : t
  val compose : t list -> t
end

(** {1 Main Server} *)

module Ox_fast_mcp : sig
  type t

  val generate_name : unit -> string

  val create :
    ?name:string ->
    ?version:string ->
    ?instructions:string ->
    ?website_url:string ->
    ?icons:Yojson.Safe.t list ->
    ?resource_prefix_format:Resource_prefix_format.t ->
    ?include_tags:String.Set.t ->
    ?exclude_tags:String.Set.t ->
    ?strict_input_validation:bool ->
    ?include_fastmcp_meta:bool ->
    ?middleware:Middleware.t list ->
    ?on_duplicate_tools:Duplicate_behavior.t ->
    ?on_duplicate_resources:Duplicate_behavior.t ->
    ?on_duplicate_prompts:Duplicate_behavior.t ->
    unit ->
    t

  val name : t -> string
  (** Accessors *)

  val version : t -> string option
  val instructions : t -> string option

  val add_tool : t -> Tool.t -> unit
  (** Tool management *)

  val remove_tool : t -> name:string -> unit
  val get_tools : t -> (string, Tool.t) Hashtbl.t
  val list_tools_mcp : t -> Yojson.Safe.t list

  val add_resource : t -> Resource.t -> unit
  (** Resource management *)

  val get_resources : t -> (string, Resource.t) Hashtbl.t
  val list_resources_mcp : t -> Yojson.Safe.t list

  val add_template : t -> Resource_template.t -> unit
  (** Template management *)

  val get_templates : t -> (string, Resource_template.t) Hashtbl.t

  val add_prompt : t -> Prompt.t -> unit
  (** Prompt management *)

  val get_prompts : t -> (string, Prompt.t) Hashtbl.t

  val call_tool :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t
  (** Operations *)

  val read_resource : t -> uri:string -> string Deferred.t

  val get_prompt :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val add_middleware : t -> Middleware.t -> unit
  (** Middleware *)

  val run_async :
    t ->
    ?transport:Transport.t ->
    ?host:string ->
    ?port:int ->
    unit ->
    unit Deferred.t
  (** Running the server *)
end

(** {1 Helper Functions} *)

val add_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> string

val has_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> bool

val remove_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> string
