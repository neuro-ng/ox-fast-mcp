open Utilities.Types

(** Tool handler signature *)
type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
}

type tool_handler = execution_context -> json -> content_type list Lwt.t

(** Base tool interface *)
type base_tool = {
  name : string;
  description : string;
  parameters : json;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}

(** Function tool definition *)
type function_tool = {
  base : base_tool;
  handler : tool_handler;
}

(** Transformed tool arguments *)
module Arg_transform = struct
  type t = {
    name : string option;
    description : string option;
    default : json option;
    default_factory : (unit -> json) option;
    type_ : string option;
    type_schema : json option;
    hide : bool;
    required : bool option;
    examples : json option;
  }

  let create
      ?name
      ?description
      ?default
      ?default_factory
      ?type_
      ?type_schema
      ?(hide = false)
      ?required
      ?examples
      () =
    (* Validation *)
    if Option.is_some default && Option.is_some default_factory then
      invalid_arg "Cannot specify both 'default' and 'default_factory' in ArgTransform";
    if Option.is_some default_factory && not hide then
      invalid_arg "default_factory can only be used with hide=True";
    if Option.equal Bool.equal (Some true) required &&
       (Option.is_some default || Option.is_some default_factory) then
      invalid_arg "Required parameters cannot have defaults";
    if hide && Option.equal Bool.equal (Some true) required then
      invalid_arg "Hidden parameters cannot be required";
    if Option.equal Bool.equal (Some false) required then
      invalid_arg "Cannot specify 'required=false'. Set a default value instead";

    { name
    ; description
    ; default
    ; default_factory
    ; type_
    ; type_schema
    ; hide
    ; required
    ; examples
    }
end

(** Transformed tool type *)
type transformed = {
  base : base_tool;
  parent_tool : function_tool;
  fn : tool_handler;
  forwarding_fn : tool_handler;
  transform_args : Arg_transform.t Core.String.Map.t;
}

(** Unified tool type *)
type tool =
  | Function of function_tool
  | Transformed of transformed

(** Helper functions for tool interface *)
let get_base_tool = function
  | Function ft -> ft.base
  | Transformed tt -> tt.base

let get_name tool = (get_base_tool tool).name
let get_description tool = (get_base_tool tool).description
let get_parameters tool = (get_base_tool tool).parameters
let is_enabled tool = (get_base_tool tool).enabled
let get_tags tool = (get_base_tool tool).tags
let get_annotations tool = (get_base_tool tool).annotations

let get_handler = function
  | Function ft -> ft.handler
  | Transformed tt -> tt.fn

let set_enabled tool enabled =
  match tool with
  | Function ft -> Function { ft with base = { ft.base with enabled } }
  | Transformed tt -> Transformed { tt with base = { tt.base with enabled } }

let set_tags tool tags =
  match tool with
  | Function ft -> Function { ft with base = { ft.base with tags } }
  | Transformed tt -> Transformed { tt with base = { tt.base with tags } }