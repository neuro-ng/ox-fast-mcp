(** MCP Mixin for OxFastMCP

    Provides a mixin pattern for registering class methods as tools, resources,
    and prompts with an OxFastMCP server instance.

    This is the OCaml translation of Python's fastmcp.contrib.mcp_mixin module.
    Since OCaml doesn't have decorators, registration is done via explicit item
    lists rather than attribute-based discovery. *)

open! Core
open! Async

(* Import yojson converters needed by ppx_yojson_conv *)
let yojson_of_string = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_string
let string_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.string_of_yojson
let yojson_of_bool = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_bool
let bool_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.bool_of_yojson
let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list
let list_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.list_of_yojson

(* Yojson.Safe.t alias with identity converters *)
type json = Yojson.Safe.t

let yojson_of_json (x : json) : Yojson.Safe.t = x
let json_of_yojson (x : Yojson.Safe.t) : json = x

let compare_json a b =
  String.compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)

let sexp_of_json x = Sexp.Atom (Yojson.Safe.to_string x)

let json_of_sexp sexp =
  match sexp with
  | Sexp.Atom s -> Yojson.Safe.from_string s
  | Sexp.List _ -> Yojson.Safe.from_string (Sexp.to_string sexp)

(** {1 Separators} *)

(** Default separator for tool names when using prefix *)
let default_tool_separator = "_"

(** Default separator for resource names/URIs when using prefix *)
let default_resource_separator = "+"

(** Default separator for prompt names when using prefix *)
let default_prompt_separator = "_"

(** {1 Registration Info Types}

    These types store metadata for registered items, similar to what Python's
    decorators capture in setattr. *)

type tool_registration_info = {
  name : string;
  description : string option; [@yojson.option]
  tags : string list option; [@yojson.option]
  annotations : (string * json) list option; [@yojson.option]
  exclude_args : string list option; [@yojson.option]
  meta : (string * json) list option; [@yojson.option]
  enabled : bool option; [@yojson.option]
}
[@@deriving yojson, compare, sexp]
(** Tool registration metadata *)

type resource_registration_info = {
  uri : string;
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  mime_type : string option; [@yojson.option]
  tags : string list option; [@yojson.option]
  annotations : (string * json) list option; [@yojson.option]
  meta : (string * json) list option; [@yojson.option]
  enabled : bool option; [@yojson.option]
}
[@@deriving yojson, compare, sexp]
(** Resource registration metadata *)

type prompt_registration_info = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  tags : string list option; [@yojson.option]
  meta : (string * json) list option; [@yojson.option]
  enabled : bool option; [@yojson.option]
}
[@@deriving yojson, compare, sexp]
(** Prompt registration metadata *)

(** {1 Registered Item Types}

    These bind a handler function with its registration metadata. *)

type registered_tool = {
  handler :
    Tool_types.execution_context ->
    json ->
    Fmcp_types.content_type list Deferred.t;
  info : tool_registration_info;
}
(** Registered tool - handler function paired with registration info *)

type registered_resource = {
  reader :
    unit ->
    ( Resources.Resource_types.content,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t;
  info : resource_registration_info;
}
(** Registered resource - reader function paired with registration info. NOTE:
    Our resource reader returns Result.t for error handling *)

type registered_prompt = {
  handler :
    (string * json) list ->
    ( Prompts.Prompt_types.prompt_message list,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t;
  info : prompt_registration_info;
}
(** Registered prompt - handler function paired with registration info. NOTE:
    Our prompt handler returns Result.t for error handling *)

(** {1 Helper Functions} *)

(** Apply prefix to a name using the specified separator *)
let apply_prefix ~prefix ~separator name =
  match prefix with
  | None -> name
  | Some p -> Printf.sprintf "%s%s%s" p separator name

(** {1 Registration Functions}

    These functions convert registered items to the appropriate types and
    register them with the server managers. *)

(** Register all tools from a list with the tool manager *)
let register_tools ~(tools : registered_tool list)
    ~(manager : Tool.tool_manager) ?(prefix : string option)
    ?(separator : string = default_tool_separator) () : unit =
  List.iter tools ~f:(fun { handler; info } ->
      let name = apply_prefix ~prefix ~separator info.name in
      let tool =
        Tool.create_tool ~name
          ~description:(Option.value info.description ~default:"")
          ?enabled:info.enabled ?tags:info.tags
          ?annotations:(Option.map info.annotations ~f:Option.some)
          handler
      in
      Tool.register_tool manager tool)

(** Register all resources from a list with the resource manager *)
let register_resources ~(resources : registered_resource list)
    ~(manager : Resources.Resource_manager.t) ?(prefix : string option)
    ?(separator : string = default_resource_separator) () : unit =
  List.iter resources ~f:(fun { reader; info } ->
      let name = apply_prefix ~prefix ~separator info.name in
      let uri = apply_prefix ~prefix ~separator info.uri in
      let resource =
        Resources.Resource_types.create_function_resource ~uri ~name
          ?description:info.description ?mime_type:info.mime_type
          ?tags:info.tags ?enabled:info.enabled
          (* TODO: annotations and meta support *)
          reader
      in
      don't_wait_for (Resources.Resource_manager.add manager resource))

(** Register all prompts from a list with the prompt manager *)
let register_prompts ~(prompts : registered_prompt list)
    ~(manager : Prompts.Prompt_manager.t) ?(prefix : string option)
    ?(separator : string = default_prompt_separator) () : unit =
  List.iter prompts ~f:(fun { handler; info } ->
      let name = apply_prefix ~prefix ~separator info.name in
      let prompt =
        Prompts.Prompt_types.create_function_prompt ~name
          ?description:info.description ?tags:info.tags ?enabled:info.enabled
          handler
      in
      don't_wait_for (Prompts.Prompt_manager.add manager prompt))

(** Register all items (tools, resources, prompts) with their respective
    managers *)
let register_all ~(tools : registered_tool list)
    ~(resources : registered_resource list) ~(prompts : registered_prompt list)
    ~(tool_manager : Tool.tool_manager)
    ~(resource_manager : Resources.Resource_manager.t)
    ~(prompt_manager : Prompts.Prompt_manager.t) ?(prefix : string option)
    ?(tool_separator : string = default_tool_separator)
    ?(resource_separator : string = default_resource_separator)
    ?(prompt_separator : string = default_prompt_separator) () : unit =
  register_tools ~tools ~manager:tool_manager ?prefix ~separator:tool_separator
    ();
  register_resources ~resources ~manager:resource_manager ?prefix
    ~separator:resource_separator ();
  register_prompts ~prompts ~manager:prompt_manager ?prefix
    ~separator:prompt_separator ()

(** {1 Mixin Module Type}

    Module signature for types that can provide registerable items. This is the
    OCaml equivalent of Python's MCPMixin class. *)

module type S = sig
  type t
  (** The mixin instance type *)

  val get_tools : t -> registered_tool list
  (** Get all registered tools from this mixin *)

  val get_resources : t -> registered_resource list
  (** Get all registered resources from this mixin *)

  val get_prompts : t -> registered_prompt list
  (** Get all registered prompts from this mixin *)

  val register_all :
    t ->
    tool_manager:Tool.tool_manager ->
    resource_manager:Resources.Resource_manager.t ->
    prompt_manager:Prompts.Prompt_manager.t ->
    ?prefix:string ->
    ?tool_separator:string ->
    ?resource_separator:string ->
    ?prompt_separator:string ->
    unit ->
    unit
  (** Register all items with the server managers *)
end

(** {1 Convenience Constructors}

    Helper functions to create registration info records. *)

(** Create a tool registration info record *)
let make_tool_info ~name ?description ?tags ?annotations ?exclude_args ?meta
    ?enabled () : tool_registration_info =
  { name; description; tags; annotations; exclude_args; meta; enabled }

(** Create a resource registration info record *)
let make_resource_info ~uri ~name ?title ?description ?mime_type ?tags
    ?annotations ?meta ?enabled () : resource_registration_info =
  { uri; name; title; description; mime_type; tags; annotations; meta; enabled }

(** Create a prompt registration info record *)
let make_prompt_info ~name ?title ?description ?tags ?meta ?enabled () :
    prompt_registration_info =
  { name; title; description; tags; meta; enabled }

(** {1 Bulk Tool Execution}

    Re-exports from Bulk_tool_caller for convenient access to bulk tool calling
    functionality. Use these to execute registered tools in bulk. *)

type call_tool_request = Bulk_tool_caller.call_tool_request = {
  tool : string;
  arguments : json;
}
(** Re-export bulk tool request type *)

type call_tool_request_result = Bulk_tool_caller.call_tool_request_result = {
  tool : string;
  arguments : json;
  is_error : bool;
  content : json list;
}
(** Re-export bulk tool result type *)

(** Create a bulk tool caller instance *)
let create_bulk_caller = Bulk_tool_caller.create

(** Call multiple different tools in a single batch *)
let call_tools_bulk = Bulk_tool_caller.call_tools_bulk

(** Call a single tool multiple times with different arguments *)
let call_tool_bulk = Bulk_tool_caller.call_tool_bulk

(** Convert a call_tool_result to a call_tool_request_result *)
let call_tool_request_result_from_call_tool_result =
  Bulk_tool_caller.call_tool_request_result_from_call_tool_result
