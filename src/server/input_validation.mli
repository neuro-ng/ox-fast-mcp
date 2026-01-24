open Core

(** Input validation for tool parameters with type coercion support *)

(** Validation mode *)
type mode =
  | Strict  (** Exact type matching required *)
  | Lenient  (** Attempt type coercion for compatible types *)

type coercion_error = {
  field : string;  (** Field path, e.g., "user.age" *)
  expected_type : string;  (** Expected type name *)
  actual_value : Yojson.Safe.t;  (** Actual value received *)
  message : string;  (** Human-readable error message *)
}
(** Coercion error detail *)

val coerce_value :
  expected_type:string ->
  value:Yojson.Safe.t ->
  (Yojson.Safe.t, string) Result.t
(** Coerce a single value to the expected type

    Supports coercions:
    - String → Int ("42" → 42)
    - String → Float ("3.14" → 3.14)
    - String → Bool ("true" → true, "false" → false)
    - Int → Float (42 → 42.0)

    @param expected_type
      Expected JSON type ("integer", "number", "string", "boolean")
    @param value Value to coerce
    @return Ok (coerced value) or Error (error message) *)

val validate_tool_input :
  mode:mode ->
  schema:Yojson.Safe.t ->
  input:Yojson.Safe.t ->
  (Yojson.Safe.t, coercion_error list) Result.t
(** Validate and potentially coerce tool input against a JSON schema

    In Lenient mode, attempts to coerce compatible types. In Strict mode,
    requires exact type matches.

    @param mode Validation mode
    @param schema JSON schema for the input
    @param input Input to validate
    @return Ok (validated/coerced input) or Error (list of errors) *)

val format_errors : coercion_error list -> string
(** Format a list of coercion errors into a readable message

    @param errors List of errors
    @return Formatted multi-line error message *)
