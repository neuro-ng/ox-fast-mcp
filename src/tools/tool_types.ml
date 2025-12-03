(** Tool types for OxFastMCP - Refactored per PYTHON_TO_OCAML_TYPE_MAP.md
    
    This module defines the tool type hierarchy following the specification:
    - Single authoritative tool type (no competing definitions)
    - Variant-based tool kinds (Function_tool, Transformed_tool)
    - Result.t for explicit error handling
    - Integration with component base (prepared for when component becomes polymorphic)
    
    See: COMPLIANCE_ACTION_PLAN.md Task 1.1
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 3 (lines 220-270)
*)

open! Core
open! Async
open Fmcp_types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Import Exceptions module for error types *)
module Exceptions = Ox_fast_mcp.Exceptions

(** {1 Execution Context} *)

type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
}
(** Execution context passed to tool handlers *)
(* Note: No sexp derivation due to mutable hashtable *)

(** {1 Tool Result Types} *)

(** Tool result type matching specification *)
type tool_result = {
  content : content_type list;
  structured_content : json option; [@yojson.option]
}
[@@deriving yojson]

(** Helper to create simple text result *)
let create_result_from_text text =
  { content = [ create_text_content text ]; structured_content = None }

(** Helper to create result with structured content *)
let create_result_with_structured ~content ~structured =
  { content; structured_content = Some structured }

(** {1 Tool Handler Types} *)

(** Tool handler signature with Result.t for explicit error handling 
    This matches PYTHON_TO_OCAML_TYPE_MAP.md specification *)
type tool_handler =
  execution_context ->
  json ->
  (tool_result, Exceptions.error_data) Deferred.Result.t
(* Note: No sexp derivation - function types don't support it *)

(** Legacy handler type for backward compatibility - will be phased out *)
type legacy_handler = execution_context -> json -> content_type list Deferred.t

(** Convert legacy handler to new Result-based handler *)
let handler_of_legacy (legacy : legacy_handler) : tool_handler =
  fun ctx args ->
    Monitor.try_with (fun () -> legacy ctx args)
    >>| function
    | Ok content -> Ok { content; structured_content = None }
    | Error exn ->
      Error
        {
          Exceptions.message = Exn.to_string exn;
          code = None;
          data = None;
        }

(** {1 Argument Transform} *)

(** Transformed tool arguments *)
module Arg_transform = struct
  type t = {
    name : string option;
    description : string option;
    default : json option;
    default_factory : (unit -> json) option [@sexp.opaque];
    type_ : string option;
    type_schema : json option;
    hide : bool;
    required : bool option;
    examples : json option;
  }
  (* Note: No sexp derivation due to json fields which don't support sexp *)

  let create ?name ?description ?default ?default_factory ?type_ ?type_schema
      ?(hide = false) ?required ?examples () =
    (* Validation *)
    if Option.is_some default && Option.is_some default_factory then
      invalid_arg
        "Cannot specify both 'default' and 'default_factory' in ArgTransform";
    if Option.is_some default_factory && not hide then
      invalid_arg "default_factory can only be used with hide=True";
    if
      Option.equal Bool.equal (Some true) required
      && (Option.is_some default || Option.is_some default_factory)
    then invalid_arg "Required parameters cannot have defaults";
    if hide && Option.equal Bool.equal (Some true) required then
      invalid_arg "Hidden parameters cannot be required";
    if Option.equal Bool.equal (Some false) required then
      invalid_arg "Cannot specify 'required=false'. Set a default value instead";

    {
      name;
      description;
      default;
      default_factory;
      type_;
      type_schema;
      hide;
      required;
      examples;
    }
end

(** {1 Tool Data Types} *)

(** Tool-specific data matching PYTHON_TO_OCAML_TYPE_MAP.md specification *)
type tool_data = {
  parameters : json; [@default `Null]  (** JSON schema for parameters *)
  output_schema : json option; [@yojson.option]  (** Optional output schema *)
  annotations : (string * json) list option; [@yojson.option]
      (** Tool annotations *)
  serializer : (json -> string) option; [@yojson.opaque]
      (** Optional custom serializer *)
}
[@@deriving yojson]
(* Note: No sexp derivation due to json fields *)

(** Create default tool_data *)
let default_tool_data =
  { parameters = `Null; output_schema = None; annotations = None; serializer = None }

(** {1 Tool Function Type} *)

(** Function representation matching specification *)
type tool_function = {
  name : string;
  description : string option;
  input_schema : json;  (** JSON schema for inputs *)
  output_schema : json option;
  fn : tool_handler;  (** Function with Result.t *)
  tool_data : tool_data;  (** Additional tool-specific data *)
}
(* Note: No sexp derivation due to json and function fields *)

(** {1 Tool Variants} *)

(** Tool variants following PYTHON_TO_OCAML_TYPE_MAP.md *)
type tool_kind =
  | Function_tool of tool_function
  | Transformed_tool of {
      original : tool_function;
      transform_fn : (json -> json) option [@sexp.opaque]; [@yojson.opaque]
          (** Optional function to transform result *)
      transform_args : Arg_transform.t String.Map.t;
          (** Argument transformations *)
      forwarding_fn : tool_handler;  (** Function that forwards to original with transforms *)
    }
(* Note: No sexp derivation due to function fields *)

(** {1 Unified Tool Type} *)

(** Component-specific data for tools - embeds tool_kind *)
type tool_component_data = {
  kind : tool_kind;
}

(** Main tool type - now uses polymorphic component pattern!
    See: COMPLIANCE_ACTION_PLAN.md Task 2.1 COMPLETE *)
type t = tool_component_data Components.component

(* Note: No sexp derivation due to function fields in tool_kind *)

(** {1 Tool Operations} *)

(** Get the function from a tool_kind *)
let get_function = function
  | Function_tool ft -> ft
  | Transformed_tool { original; _ } -> original

(** Get tool handler *)
let get_handler (tool : t) : tool_handler =
  match tool.data.kind with
  | Function_tool ft -> ft.fn
  | Transformed_tool { forwarding_fn; _ } -> forwarding_fn

(** Get tool name *)
let get_name (tool : t) = tool.name

(** Get tool description *)
let get_description (tool : t) = tool.description

(** Get tool parameters (JSON schema) *)
let get_parameters (tool : t) =
  let ft = get_function tool.data.kind in
  ft.input_schema

(** Check if tool is enabled *)
let is_enabled (tool : t) = tool.enabled

(** Get tool tags *)
let get_tags (tool : t) = tool.tags |> Set.to_list

(** Get tool annotations *)
let get_annotations (tool : t) =
  let ft = get_function tool.data.kind in
  ft.tool_data.annotations

(** Get tool data *)
let get_tool_data (tool : t) =
  let ft = get_function tool.data.kind in
  ft.tool_data

(** {1 Tool Modification} *)

(** Set tool enabled state *)
let set_enabled (tool : t) (enabled : bool) : t = { tool with enabled }

(** Set tool tags *)
let set_tags (tool : t) (tags : string list) : t =
  { tool with tags = String.Set.of_list tags }

(** Enable a tool *)
let enable (tool : t) : t = Components.enable tool

(** Disable a tool *)
let disable (tool : t) : t = Components.disable tool

(** Get or generate tool key *)
let key (tool : t) : string = Components.key tool

(** Set tool key *)
let with_key (tool : t) (new_key : string) : t = Components.with_key tool new_key

(** {1 Tool Execution} *)

(** Run a tool with the given arguments *)
let run (tool : t) ~(context : execution_context) ~(arguments : json) :
    (tool_result, Exceptions.error_data) Deferred.Result.t =
  if not tool.enabled then
    Deferred.return
      (Error
         {
           Exceptions.message = Printf.sprintf "Tool '%s' is disabled" tool.name;
           code = Some 403;
           data = None;
         })
  else
    let handler = get_handler tool in
    handler context arguments

(** {1 Tool Creation} *)

(** Create a function tool from a handler *)
let create_function_tool ~name ?description ?(tags = []) ?key
    ?(input_schema = `Null) ?output_schema ?annotations ?serializer
    ?(enabled = true) (fn : tool_handler) : t =
  let tool_data_inner =
    {
      parameters = input_schema;
      output_schema;
      annotations;
      serializer;
    }
  in
  let tool_function =
    { name; description; input_schema; output_schema; fn; tool_data = tool_data_inner }
  in
  let tool_component_data = { kind = Function_tool tool_function } in
  Components.create
    ~name
    ?description
    ~tags:(String.Set.of_list tags)
    ?key
    ~enabled
    ~data:tool_component_data
    ()

(** Create tool from legacy handler (for backward compatibility) *)
let create_from_legacy_handler ~name ?description ?(tags = []) ?key
    ?(input_schema = `Null) ?output_schema ?annotations ?(enabled = true)
    (legacy_fn : legacy_handler) : t =
  create_function_tool ~name ?description ~tags ?key ~input_schema ?output_schema
    ?annotations ~enabled (handler_of_legacy legacy_fn)

(** {1 Tool Transformation} *)

(** Create a transformed tool from an existing tool *)
let create_transformed_tool (base_tool : t) ?name ?description ?tags ?key
    ?transform_fn ?(transform_args = String.Map.empty) ?(enabled = true) () : t =
  let original_fn = get_function base_tool.data.kind in
  
  (* Create forwarding function that applies transforms *)
  let forwarding_fn : tool_handler =
    fun ctx args ->
      (* Apply argument transformations *)
      let transformed_args =
        match transform_fn with
        | Some tf -> tf args
        | None -> args
      in
      original_fn.fn ctx transformed_args
  in
  
  let kind =
    Transformed_tool { original = original_fn; transform_fn; transform_args; forwarding_fn }
  in
  
  let tool_component_data = { kind } in
  Components.create
    ~name:(Option.value name ~default:base_tool.name)
    ?description:(Option.first_some description base_tool.description)
    ~tags:(Option.value_map tags ~default:base_tool.tags ~f:String.Set.of_list)
    ~enabled
    ?key:(Option.first_some key base_tool.key)
    ~data:tool_component_data
    ()

(** {1 Component Integration} *)

(** Tools are now directly components - no conversion needed! *)
let to_component (tool : t) : t = tool

(** Create tool from component *)
let from_component (component : tool_component_data Components.component) : t = component
