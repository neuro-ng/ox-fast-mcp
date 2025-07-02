open Utilities.Types

(** Tool handler signature *)
type tool_handler = execution_context -> json -> content_type list Lwt.t

(** Function tool definition *)
type function_tool = {
  name : string;
  description : string;
  parameters : json;
  handler : tool_handler;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}

(** Transformed tool type *)
type transformed = {
  parent_tool : function_tool;
  fn : tool_handler;
  forwarding_fn : tool_handler;
  parameters : json;
  transform_args : (string, Arg_transform.t) Map.t;
}

and module Arg_transform = struct
  type t = {
    name : string option;
    description : string option;
    default : Yojson.Safe.t option;
    default_factory : (unit -> Yojson.Safe.t) option;
    type_schema : Yojson.Safe.t option;
    hide : bool;
    required : bool option;
    examples : Yojson.Safe.t option;
  }
  [@@deriving sexp]

  let create
      ?name
      ?description
      ?default
      ?default_factory
      ?type_schema
      ?(hide = false)
      ?required
      ?examples
      () =
    (* Validation *)
    if Option.is_some default && Option.is_some default_factory then
      failwith "Cannot specify both 'default' and 'default_factory' in ArgTransform";
    if Option.is_some default_factory && not hide then
      failwith "default_factory can only be used with hide=True";
    if Option.equal Bool.equal (Some true) required &&
       (Option.is_some default || Option.is_some default_factory) then
      failwith "Required parameters cannot have defaults";
    if hide && Option.equal Bool.equal (Some true) required then
      failwith "Hidden parameters cannot be required";
    if Option.equal Bool.equal (Some false) required then
      failwith "Cannot specify 'required=false'. Set a default value instead";

    { name
    ; description
    ; default
    ; default_factory
    ; type_schema
    ; hide
    ; required
    ; examples
    }
end 