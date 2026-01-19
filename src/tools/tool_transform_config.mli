(** Tool Transform Configuration - JSON-serializable transformation config *)

(** {1 Argument Transform Configuration} *)

(** JSON-serializable configuration for a single argument transform *)
type arg_transform_config = {
  name : string option;
  description : string option;
  default : Yojson.Safe.t option;
  hide : bool;
  required : bool option;
  examples : Yojson.Safe.t option;
}

val default_arg_transform_config : arg_transform_config

val arg_transform_config_of_yojson : Yojson.Safe.t -> arg_transform_config
val yojson_of_arg_transform_config : arg_transform_config -> Yojson.Safe.t

val to_arg_transform : arg_transform_config -> Tool_types.Arg_transform.t
(** Convert arg_transform_config to Arg_transform.t *)

(** {1 Tool Transform Configuration} *)

(** JSON-serializable configuration for transforming a tool *)
type t = {
  name : string option;
  description : string option;
  tags : string list option;
  transform_args : (string * arg_transform_config) list;
  enabled : bool option;
}

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t

val create :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_args:(string * arg_transform_config) list ->
  ?enabled:bool ->
  unit ->
  t
(** Create a tool transform configuration *)

val apply : t -> Tool_types.t -> Tool_types.t
(** Apply this configuration to transform a tool *)
