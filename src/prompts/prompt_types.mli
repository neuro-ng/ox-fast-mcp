(** Prompt types for OxFastMCP
    
    Implements prompt type system per PYTHON_TO_OCAML_TYPE_MAP.md Section 5
    Follows the same pattern as tool_types.mli
    See: COMPLIANCE_ACTION_PLAN.md Task 3.1
*)

open! Core
open! Async
open Fmcp_types

(** {1 Prompt Arguments and Messages} *)

(** Prompt argument definition - reuse from MCP types *)
type prompt_argument = Mcp.Types.prompt_argument
[@@deriving sexp]

(** Prompt message - reuse from MCP types *)
type prompt_message = Mcp.Types.prompt_message
[@@deriving sexp]

(** {1 Prompt Handler} *)

(** Prompt function signature - returns Result.t for error handling *)
type prompt_handler =
  (string * json) list
  -> (prompt_message list, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t

(** {1 Prompt Data} *)

(** Prompt-specific metadata *)
type prompt_data = {
  arguments : prompt_argument list option; [@default None] [@yojson_drop_if Option.is_none]
}
[@@deriving sexp]

(** {1 Prompt Function Type} *)

(** Function prompt representation *)
type prompt_function = {
  name : string;
  description : string option;
  prompt_data : prompt_data;
  fn : prompt_handler;
}
(* Note: No sexp derivation due to function field *)

(** {1 Prompt Kinds} *)

(** Prompt variants *)
type prompt_kind =
  | Function_prompt of prompt_function
(* Note: No sexp derivation due to function field in prompt_function *)

(** {1 Unified Prompt Type} *)

(** Component-specific data for prompts *)
type prompt_component_data = {
  kind : prompt_kind;
}
(* Note: No sexp derivation due to function field *)

(** Main prompt type - uses polymorphic component pattern! *)
type t = prompt_component_data Components.component
(* Note: No sexp derivation due to function field *)

(** {1 Prompt Operations} *)

val get_name : t -> string
(** Get prompt name *)

val get_description : t -> string option
(** Get prompt description *)

val get_arguments : t -> prompt_argument list option
(** Get prompt arguments *)

val is_enabled : t -> bool
(** Check if prompt is enabled *)

val get_tags : t -> string list
(** Get prompt tags *)

val get_handler : t -> prompt_handler
(** Get the prompt handler function *)

(** {1 Prompt Modification} *)

val enable : t -> t
(** Enable a prompt *)

val disable : t -> t
(** Disable a prompt *)

val key : t -> string
(** Get or generate prompt key *)

val with_key : t -> string -> t
(** Set prompt key *)

(** {1 Prompt Execution} *)

val render :
  t ->
  arguments:(string * json) list option ->
  (prompt_message list, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Render a prompt with the given arguments *)

(** {1 Prompt Creation} *)

val create_function_prompt :
  name:string ->
  ?description:string ->
  ?arguments:prompt_argument list ->
  ?tags:string list ->
  ?key:string ->
  ?enabled:bool ->
  prompt_handler ->
  t
(** Create a function prompt from a handler *)

(** {1 MCP Integration} *)

val to_mcp_prompt : ?_include_fastmcp_meta:bool -> t -> Mcp.Types.prompt
(** Convert to MCP prompt *)

(** {1 Component Integration} *)

val to_component : t -> t
(** Prompts are components *)

val from_component : prompt_component_data Components.component -> t
(** Create prompt from component *)

