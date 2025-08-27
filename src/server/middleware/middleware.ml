open Core
open Async
open Tools.Tool_manager
open Mcp.Types

(** FastMCP context type placeholder *)
type fastmcp_context = Yojson.Safe.t

module Results = struct
  type call_tool_result = { content : Yojson.Safe.t list; is_error : bool }
  type list_tools_result = { tools : (string * Tool.t) list }
  type list_resources_result = { resources : resource list }

  type list_resource_templates_result = {
    resource_templates : resource_template list;
  }

  type list_prompts_result = { prompts : prompt list }
end

type context = {
  message : Yojson.Safe.t;
  fastmcp_context : fastmcp_context option;
  source : [ `Client | `Server ];
  type_ : [ `Request | `Notification ];
  method_ : string option;
  timestamp : Time_ns.t;
  params : Yojson.Safe.t;
  id : string option;
  resource : string option;
}

type 'a call_next = context -> 'a Deferred.t

module type S = sig
  type t

  val create : unit -> t
  val call : t -> context -> 'a call_next -> 'a Deferred.t
  val dispatch_handler : t -> context -> 'a call_next -> 'a call_next Deferred.t
  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  val on_request : t -> context -> 'a call_next -> 'a Deferred.t
  val on_notification : t -> context -> 'a call_next -> 'a Deferred.t

  val on_call_tool :
    t ->
    context ->
    Results.call_tool_result call_next ->
    Results.call_tool_result Deferred.t

  val on_read_resource :
    t ->
    context ->
    read_resource_result call_next ->
    read_resource_result Deferred.t

  val on_get_prompt :
    t ->
    context ->
    get_prompt_result call_next ->
    get_prompt_result Deferred.t

  val on_list_tools :
    t ->
    context ->
    Results.list_tools_result call_next ->
    Results.list_tools_result Deferred.t

  val on_list_resources :
    t ->
    context ->
    Results.list_resources_result call_next ->
    Results.list_resources_result Deferred.t

  val on_list_resource_templates :
    t ->
    context ->
    Results.list_resource_templates_result call_next ->
    Results.list_resource_templates_result Deferred.t

  val on_list_prompts :
    t ->
    context ->
    Results.list_prompts_result call_next ->
    Results.list_prompts_result Deferred.t
end

let make_middleware_wrapper (type t) (module M : S with type t = t)
    (middleware : t) (call_next : 'a call_next) : 'a call_next =
 fun context -> M.call middleware context call_next

let copy_context context ?message ?fastmcp_context ?source ?type_ ?method_
    ?timestamp ?params ?id ?resource () =
  {
    message = Option.value message ~default:context.message;
    fastmcp_context =
      Option.value fastmcp_context ~default:context.fastmcp_context;
    source = Option.value source ~default:context.source;
    type_ = Option.value type_ ~default:context.type_;
    method_ = Option.value method_ ~default:context.method_;
    timestamp = Option.value timestamp ~default:context.timestamp;
    params = Option.value params ~default:context.params;
    id = Option.value id ~default:context.id;
    resource = Option.value resource ~default:context.resource;
  }

(** Base implementation that other middleware can extend *)
module Base = struct
  type t = unit [@@warning "-34"]

  let create () = () [@@warning "-32"]

  let on_message _t context call_next = call_next context
  let on_request _t context call_next = call_next context
  let on_notification _t context call_next = call_next context
  let on_call_tool _t context call_next = call_next context
  let on_read_resource _t context call_next = call_next context
  let on_get_prompt _t context call_next = call_next context
  let on_list_tools _t context call_next = call_next context
  let on_list_resources _t context call_next = call_next context
  let on_list_resource_templates _t context call_next = call_next context
  let on_list_prompts _t context call_next = call_next context

  let dispatch_handler _t context call_next =
    let handler = ref call_next in

    (match context.method_ with
    | Some "tools/call" -> handler := fun ctx -> on_call_tool _t ctx !handler
    | Some "resources/read" ->
      handler := fun ctx -> on_read_resource _t ctx !handler
    | Some "prompts/get" -> handler := fun ctx -> on_get_prompt _t ctx !handler
    | Some "tools/list" -> handler := fun ctx -> on_list_tools _t ctx !handler
    | Some "resources/list" ->
      handler := fun ctx -> on_list_resources _t ctx !handler
    | Some "resources/templates/list" ->
      handler := fun ctx -> on_list_resource_templates _t ctx !handler
    | Some "prompts/list" ->
      handler := fun ctx -> on_list_prompts _t ctx !handler
    | _ -> ());

    (match context.type_ with
    | `Request -> handler := fun ctx -> on_request _t ctx !handler
    | `Notification -> handler := fun ctx -> on_notification _t ctx !handler);

    (handler := fun ctx -> on_message _t ctx !handler);

    return !handler

  let call _t context call_next = [@@warning "-32"]
    let%bind handler = dispatch_handler _t context call_next in
    handler context
end
