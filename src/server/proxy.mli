(** Proxy Module for OxFastMCP

    Provides proxy types and managers that source tools, resources, and prompts
    from remote MCP servers in addition to local and mounted components. Uses a
    client factory pattern for session management. *)

open! Core
open! Async

(** {1 Types} *)

(** Client interface for proxy operations *)
module type Client = sig
  type t

  val connect : t -> unit Deferred.t
  val disconnect : t -> unit Deferred.t
  val new_ : t -> t
  val list_tools : t -> Yojson.Safe.t list Deferred.t

  val call_tool :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val call_tool_mcp :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val list_resources : t -> Yojson.Safe.t list Deferred.t
  val read_resource : t -> uri:string -> Yojson.Safe.t Deferred.t
  val list_resource_templates : t -> Yojson.Safe.t list Deferred.t
  val list_prompts : t -> Yojson.Safe.t list Deferred.t

  val get_prompt :
    t ->
    name:string ->
    arguments:Yojson.Safe.t option ->
    Yojson.Safe.t Deferred.t
end

type 'client client_factory = unit -> 'client Deferred.t
(** Client factory type *)

(** {1 Tool Result} *)

module Tool_result : sig
  type t = {
    content : Yojson.Safe.t list;
    structured_content : Yojson.Safe.t option;
  }

  val create :
    content:Yojson.Safe.t list -> ?structured_content:Yojson.Safe.t -> unit -> t
end

(** {1 Mirrored Component Marker} *)

module Mirrored_component : sig
  type t

  val create : ?mirrored:bool -> unit -> t
  val is_mirrored : t -> bool
end

(** {1 Proxy Tool} *)

module Proxy_tool : sig
  type 'client t = {
    name : string;
    description : string option;
    parameters : Yojson.Safe.t;
    annotations : Yojson.Safe.t option;
    output_schema : Yojson.Safe.t option;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  val create :
    client:'client ->
    name:string ->
    ?description:string ->
    parameters:Yojson.Safe.t ->
    ?annotations:Yojson.Safe.t ->
    ?output_schema:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:string list ->
    unit ->
    'client t

  val from_mcp_tool : client:'client -> Yojson.Safe.t -> 'client t

  val run :
    (module Client with type t = 'client) ->
    'client t ->
    arguments:Yojson.Safe.t ->
    Tool_result.t Deferred.t
end

(** {1 Proxy Resource} *)

module Proxy_resource : sig
  type 'client t = {
    uri : string;
    name : string;
    description : string option;
    mime_type : string;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    value : string option;
    mirrored : bool;
  }

  val create :
    client:'client ->
    uri:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?meta:Yojson.Safe.t ->
    ?tags:string list ->
    ?value:string ->
    unit ->
    'client t

  val from_mcp_resource : client:'client -> Yojson.Safe.t -> 'client t

  val read :
    (module Client with type t = 'client) -> 'client t -> string Deferred.t
end

(** {1 Proxy Resource Template} *)

module Proxy_template : sig
  type 'client t = {
    uri_template : string;
    name : string;
    description : string option;
    mime_type : string;
    parameters : Yojson.Safe.t;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  val create :
    client:'client ->
    uri_template:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?parameters:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:string list ->
    unit ->
    'client t

  val from_mcp_template : client:'client -> Yojson.Safe.t -> 'client t

  val create_resource :
    (module Client with type t = 'client) ->
    'client t ->
    params:(string * string) list ->
    'client Proxy_resource.t Deferred.t
end

(** {1 Proxy Prompt} *)

module Proxy_prompt : sig
  type argument = {
    name : string;
    description : string option;
    required : bool;
  }

  type 'client t = {
    name : string;
    description : string option;
    arguments : argument list;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  val create :
    client:'client ->
    name:string ->
    ?description:string ->
    ?arguments:argument list ->
    ?meta:Yojson.Safe.t ->
    ?tags:string list ->
    unit ->
    'client t

  val from_mcp_prompt : client:'client -> Yojson.Safe.t -> 'client t

  val render :
    (module Client with type t = 'client) ->
    'client t ->
    arguments:Yojson.Safe.t ->
    Yojson.Safe.t Deferred.t
end

(** {1 Proxy Tool Manager} *)

module Proxy_tool_manager : sig
  type 'client t

  val create :
    client_factory:'client client_factory ->
    ?transformations:(Yojson.Safe.t -> Yojson.Safe.t) list ->
    unit ->
    'client t

  val get_client : 'client t -> 'client Deferred.t

  val get_tools :
    (module Client with type t = 'client) ->
    'client t ->
    (string, 'client Proxy_tool.t) Hashtbl.t Deferred.t

  val list_tools :
    (module Client with type t = 'client) ->
    'client t ->
    'client Proxy_tool.t list Deferred.t

  val call_tool :
    (module Client with type t = 'client) ->
    'client t ->
    name:string ->
    arguments:Yojson.Safe.t ->
    Tool_result.t Deferred.t
end

(** {1 Proxy Resource Manager} *)

module Proxy_resource_manager : sig
  type 'client t

  val create : client_factory:'client client_factory -> unit -> 'client t
  val get_client : 'client t -> 'client Deferred.t

  val get_resources :
    (module Client with type t = 'client) ->
    'client t ->
    (string, 'client Proxy_resource.t) Hashtbl.t Deferred.t

  val read_resource :
    (module Client with type t = 'client) ->
    'client t ->
    uri:string ->
    string Deferred.t
end

(** {1 Proxy Prompt Manager} *)

module Proxy_prompt_manager : sig
  type 'client t

  val create : client_factory:'client client_factory -> unit -> 'client t
  val get_client : 'client t -> 'client Deferred.t

  val get_prompts :
    (module Client with type t = 'client) ->
    'client t ->
    (string, 'client Proxy_prompt.t) Hashtbl.t Deferred.t

  val render_prompt :
    (module Client with type t = 'client) ->
    'client t ->
    name:string ->
    arguments:Yojson.Safe.t option ->
    Yojson.Safe.t Deferred.t
end

(** {1 OxFastMCP Proxy Server} *)

module Ox_fast_mcp_proxy : sig
  type 'client t = {
    name : string;
    client_factory : 'client client_factory;
    tool_manager : 'client Proxy_tool_manager.t;
    resource_manager : 'client Proxy_resource_manager.t;
    prompt_manager : 'client Proxy_prompt_manager.t;
  }

  val create :
    name:string ->
    client_factory:'client client_factory ->
    ?transformations:(Yojson.Safe.t -> Yojson.Safe.t) list ->
    unit ->
    'client t
end

(** {1 Proxy Client} *)

module Proxy_client : sig
  type t

  val generate_name : unit -> string

  val create :
    transport:Yojson.Safe.t ->
    ?name:string ->
    ?roots_handler:(unit -> Yojson.Safe.t Deferred.t) ->
    ?sampling_handler:
      (Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    ?elicitation_handler:(string -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    ?log_handler:(Yojson.Safe.t -> unit Deferred.t) ->
    ?progress_handler:
      (float -> float option -> string option -> unit Deferred.t) ->
    unit ->
    t

  val default_sampling_handler :
    messages:Yojson.Safe.t -> params:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val default_elicitation_handler :
    message:string -> params:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val default_log_handler : message:Yojson.Safe.t -> unit Deferred.t

  val default_progress_handler :
    progress:float ->
    total:float option ->
    message:string option ->
    unit Deferred.t
end

(** {1 Stateful Proxy Client} *)

module Stateful_proxy_client : sig
  type session = Yojson.Safe.t
  type 'client t

  val create : client:Proxy_client.t -> 'client t
  val clear : 'client t -> unit Deferred.t

  val new_stateful :
    'client t -> session:session -> create_client:(unit -> 'client) -> 'client
end
