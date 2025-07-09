open Utilities.Types
open Core
open Async
open Yojson.Safe
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
}
(** Tool handler signature *)

type tool_handler = execution_context -> json -> content_type list Lwt.t

type base_tool = {
  name : string;
  description : string;
  parameters : json;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}
(** Base tool interface *)

type function_tool = { base : base_tool; handler : tool_handler }
(** Function tool definition *)

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

type transformed = {
  base : base_tool;
  parent_tool : function_tool;
  fn : tool_handler;
  forwarding_fn : tool_handler;
  transform_args : Arg_transform.t Core.String.Map.t;
}
(** Transformed tool type *)

(** Unified tool type *)
type tool = Function of function_tool | Transformed of transformed

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

type content_block = {
  text : string option;
  image : string option;
  error : string option;
}
[@@deriving sexp, yojson_of]

let create_text_content text = { text = Some text; image = None; error = None }

let create_image_content image =
  { text = None; image = Some image; error = None }

let create_error_content error =
  { text = None; image = None; error = Some error }

type t = {
  key : string;
  name : string option; [@yojson.option]
  description : string option; [@yojson.option]
  tags : string list;
  annotations : (string * string) list;
  parameters : Yojson.Safe.t;
  enabled : bool;
  error : string option; [@yojson.option]
  fn : tool_handler;
}
[@@deriving fields]

let yojson_of_t t =
  `Assoc
    [
      ("key", `String t.key);
      ( "name",
        match t.name with
        | Some n -> `String n
        | None -> `Null );
      ( "description",
        match t.description with
        | Some d -> `String d
        | None -> `Null );
      ("tags", `List (List.map t.tags ~f:(fun s -> `String s)));
      ( "annotations",
        `List
          (List.map t.annotations ~f:(fun (k, v) ->
               `List [ `String k; `String v ])) );
      ("parameters", t.parameters);
      ("enabled", `Bool t.enabled);
      ( "error",
        match t.error with
        | Some e -> `String e
        | None -> `Null );
    ]

let with_key t new_key = { t with key = new_key }
let enable t = { t with enabled = true }
let disable t = { t with enabled = false }

let from_function ?name ?description ?(tags = []) ?(annotations = [])
    ?(exclude_args = []) ?serializer ?(enabled = true) fn =
  let key =
    match name with
    | Some n -> String.lowercase n
    | None -> "<function>"
  in
  {
    key;
    name;
    description;
    tags;
    annotations;
    parameters = `Assoc [];
    (* TODO: Generate JSON schema from function type *)
    enabled;
    error = None;
    fn =
      (fun args ->
        let%bind result = fn args in
        return result);
  }
