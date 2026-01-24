(** Tool Transform Configuration - JSON-serializable transformation config

    Provides a configuration model for tool transformations that can be
    serialized to/from JSON. Used for runtime tool transformation without
    requiring function callbacks. *)

open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type json = Yojson.Safe.t
(** JSON type alias with yojson converters *)

let json_of_yojson x = x
let yojson_of_json x = x

(** {1 Argument Transform Configuration} *)

type arg_transform_config = {
  name : string option; [@yojson.option]
  description : string option; [@yojson.option]
  default : json option; [@yojson.option]
  hide : bool; [@default false]
  required : bool option; [@yojson.option]
  examples : json option; [@yojson.option]
}
[@@deriving yojson]
(** JSON-serializable configuration for a single argument transform *)

let default_arg_transform_config =
  {
    name = None;
    description = None;
    default = None;
    hide = false;
    required = None;
    examples = None;
  }

(** Convert arg_transform_config to Arg_transform.t *)
let to_arg_transform (cfg : arg_transform_config) : Tool_types.Arg_transform.t =
  Tool_types.Arg_transform.create ?name:cfg.name ?description:cfg.description
    ?default:cfg.default ~hide:cfg.hide ?required:cfg.required
    ?examples:cfg.examples ()

(** {1 Tool Transform Configuration} *)

type t = {
  name : string option; [@yojson.option]  (** New name for the tool *)
  description : string option; [@yojson.option]  (** New description *)
  tags : string list option; [@yojson.option]  (** New tags *)
  transform_args : (string * arg_transform_config) list; [@default []]
      (** Argument transformations as list of (arg_name, config) pairs *)
  enabled : bool option; [@yojson.option]
      (** Whether the transformed tool is enabled *)
}
[@@deriving yojson]
(** JSON-serializable configuration for transforming a tool *)

let create ?name ?description ?tags ?(transform_args = []) ?enabled () =
  { name; description; tags; transform_args; enabled }

(** Apply this configuration to transform a tool *)
let apply (cfg : t) (tool : Tool_types.t) : Tool_types.t =
  (* Convert transform_args list to Map *)
  let transform_args_map =
    List.fold cfg.transform_args ~init:String.Map.empty
      ~f:(fun acc (arg_name, arg_cfg) ->
        Map.set acc ~key:arg_name ~data:(to_arg_transform arg_cfg))
  in
  Tool_types.create_transformed_tool tool ?name:cfg.name
    ?description:cfg.description ?tags:cfg.tags
    ~transform_args:transform_args_map
    ~enabled:(Option.value cfg.enabled ~default:(Tool_types.is_enabled tool))
    ()
