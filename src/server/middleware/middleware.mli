(** Core middleware types and interfaces *)

open Core
open Async

(** Result types for different operations *)
module Results : sig
  type call_tool_result = { content : Yojson.Safe.t list; is_error : bool }
  type list_tools_result = { tools : (string * Tool.t) list }
  type list_resources_result = { resources : Resource.t list }

  type list_resource_templates_result = {
    resource_templates : Resource_template.t list;
  }

  type list_prompts_result = { prompts : Prompt.t list }
end

type context = {
  message : Yojson.Safe.t;
  fastmcp_context : Context.t option;
  source : [ `Client | `Server ];
  type_ : [ `Request | `Notification ];
  method_ : string option;
  timestamp : Time.t;
  params : Yojson.Safe.t;
  id : string option;
  resource : string option;
}
(** Context for middleware operations *)

type 'a call_next = context -> 'a Deferred.t
(** Type for middleware call_next function *)

(** Base middleware type *)
module type S = sig
  type t
  (** The middleware configuration type *)

  val create : unit -> t
  (** Create a new middleware instance *)

  val call : t -> context -> 'a call_next -> 'a Deferred.t
  (** Main entry point that orchestrates the pipeline *)

  val dispatch_handler : t -> context -> 'a call_next -> 'a call_next Deferred.t
  (** Builds a chain of handlers for a given message *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Handle any message *)

  val on_request : t -> context -> 'a call_next -> 'a Deferred.t
  (** Handle request messages *)

  val on_notification : t -> context -> 'a call_next -> 'a Deferred.t
  (** Handle notification messages *)

  val on_call_tool :
    t ->
    context ->
    Results.call_tool_result call_next ->
    Results.call_tool_result Deferred.t
  (** Handle tool call requests *)

  val on_read_resource :
    t ->
    context ->
    Read_resource_result.t call_next ->
    Read_resource_result.t Deferred.t
  (** Handle resource read requests *)

  val on_get_prompt :
    t ->
    context ->
    Get_prompt_result.t call_next ->
    Get_prompt_result.t Deferred.t
  (** Handle prompt get requests *)

  val on_list_tools :
    t ->
    context ->
    Results.list_tools_result call_next ->
    Results.list_tools_result Deferred.t
  (** Handle tool list requests *)

  val on_list_resources :
    t ->
    context ->
    Results.list_resources_result call_next ->
    Results.list_resources_result Deferred.t
  (** Handle resource list requests *)

  val on_list_resource_templates :
    t ->
    context ->
    Results.list_resource_templates_result call_next ->
    Results.list_resource_templates_result Deferred.t
  (** Handle resource template list requests *)

  val on_list_prompts :
    t ->
    context ->
    Results.list_prompts_result call_next ->
    Results.list_prompts_result Deferred.t
  (** Handle prompt list requests *)
end

(** Make a middleware wrapper *)
val make_middleware_wrapper :
  (module S with type t = 't) -> 't -> 'a call_next -> 'a call_next
(** Create a wrapper that applies a single middleware to a context *)

(** Helper functions *)
val copy_context :
  context ->
  ?message:Yojson.Safe.t ->
  ?fastmcp_context:Context.t option ->
  ?source:[ `Client | `Server ] ->
  ?type_:[ `Request | `Notification ] ->
  ?method_:string option ->
  ?timestamp:Time.t ->
  ?params:Yojson.Safe.t ->
  ?id:string option ->
  ?resource:string option ->
  unit ->
  context
(** Create a copy of a context with optional updates *)
