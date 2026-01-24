(** Elicitation Module for OxFastMCP

    Provides types and utilities for MCP elicitation responses, including schema
    validation for MCP elicitation requirements. *)

open! Core

(** {1 Elicitation Result Types} *)

(** Action type for elicitation responses *)
module Action : sig
  type t = Accept | Decline | Cancel [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string : string -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

(** Result when user accepts the elicitation with data *)
module Accepted_elicitation : sig
  type 'a t = { action : Action.t; data : 'a } [@@deriving sexp]

  val create : data:'a -> 'a t
  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
end

(** Result when user declines the elicitation *)
module Declined_elicitation : sig
  type t = { action : Action.t; message : string option } [@@deriving sexp]

  val create : ?message:string -> unit -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

(** Result when user cancels the elicitation *)
module Cancelled_elicitation : sig
  type t = { action : Action.t; reason : string option } [@@deriving sexp]

  val create : ?reason:string -> unit -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

(** Container for scalar elicitation types *)
module Scalar_elicitation_type : sig
  type 'a t = { value : 'a } [@@deriving sexp]

  val create : value:'a -> 'a t
end

(** Elicitation result combining all possible outcomes *)
type 'a elicitation_result =
  | Accepted of 'a Accepted_elicitation.t
  | Declined of Declined_elicitation.t
  | Cancelled of Cancelled_elicitation.t
[@@deriving sexp]

(** {1 Allowed Types} *)

val allowed_types : (string, String.comparator_witness) Set.t
(** Set of allowed primitive types for elicitation schemas *)

(** {1 Schema Validation} *)

exception Elicitation_schema_error of string
(** Error for invalid elicitation schemas *)

val validate_elicitation_json_schema : Yojson.Safe.t -> unit
(** Validate that a JSON schema follows MCP elicitation requirements.

    This ensures the schema is compatible with MCP elicitation requirements:
    - Must be an object schema
    - Must only contain primitive field types (string, number, integer, boolean)
    - Must be flat (no nested objects or arrays of objects)
    - Allows const fields (for Literal types) and enum fields (for Enum types)
    - Only primitive types and their nullable variants are allowed

    @param schema The JSON schema to validate
    @raise Elicitation_schema_error
      if the schema doesn't meet MCP elicitation requirements *)

val get_elicitation_schema : Yojson.Safe.t -> Yojson.Safe.t
(** Get the schema for an elicitation response.

    This applies schema compression and validates the result against MCP
    elicitation requirements.

    @param schema The JSON schema to process
    @return The compressed and validated schema *)
