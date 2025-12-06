(** Prompt types for OxFastMCP

    Implements prompt type system per PYTHON_TO_OCAML_TYPE_MAP.md Section 5
    Follows the same pattern as tool_types.ml See: COMPLIANCE_ACTION_PLAN.md
    Task 3.1 *)

open! Core
open! Async
open Fmcp_types

(** {1 Prompt Arguments and Messages} *)

type prompt_argument = Mcp.Types.prompt_argument [@@deriving sexp]
(** Prompt argument definition - reuse from MCP types *)

type prompt_message = Mcp.Types.prompt_message [@@deriving sexp]
(** Prompt message - reuse from MCP types *)

(** {1 Prompt Handler} *)

type prompt_handler =
  (string * json) list ->
  (prompt_message list, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Prompt function signature - returns Result.t for error handling *)

(** {1 Prompt Data} *)

type prompt_data = {
  arguments : prompt_argument list option;
      [@default None] [@yojson_drop_if Option.is_none]
}
[@@deriving sexp]
(** Prompt-specific metadata *)

(** {1 Prompt Function Type} *)

type prompt_function = {
  name : string;
  description : string option;
  prompt_data : prompt_data;
  fn : prompt_handler;
}
(** Function prompt representation *)
(* Note: No sexp derivation due to function field *)

(** {1 Prompt Kinds} *)

(** Prompt variants *)
type prompt_kind = Function_prompt of prompt_function
(* Note: No sexp derivation due to function field in prompt_function *)

(** {1 Unified Prompt Type} *)

type prompt_component_data = { kind : prompt_kind }
(** Component-specific data for prompts *)
(* Note: No sexp derivation due to function field *)

type t = prompt_component_data Components.component
(** Main prompt type - uses polymorphic component pattern! *)
(* Note: No sexp derivation due to function field *)

(** {1 Prompt Operations} *)

(** Get the function from a prompt *)
let get_function = function
  | Function_prompt pf -> pf

(** Get prompt handler *)
let get_handler (prompt : t) : prompt_handler =
  match prompt.data.kind with
  | Function_prompt pf -> pf.fn

(** Get prompt name *)
let get_name (prompt : t) = prompt.name

(** Get prompt description *)
let get_description (prompt : t) = prompt.description

(** Get prompt arguments *)
let get_arguments (prompt : t) =
  let pf = get_function prompt.data.kind in
  pf.prompt_data.arguments

(** Check if prompt is enabled *)
let is_enabled (prompt : t) = prompt.enabled

(** Get prompt tags *)
let get_tags (prompt : t) = prompt.tags |> Set.to_list

(** {1 Prompt Modification} *)

(** Enable a prompt *)
let enable (prompt : t) : t = Components.enable prompt

(** Disable a prompt *)
let disable (prompt : t) : t = Components.disable prompt

(** Get or generate prompt key *)
let key (prompt : t) : string = Components.key prompt

(** Set prompt key *)
let with_key (prompt : t) (new_key : string) : t =
  Components.with_key prompt new_key

(** {1 Prompt Execution} *)

(** Render a prompt with the given arguments *)
let render (prompt : t) ~(arguments : (string * json) list option) :
    (prompt_message list, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t =
  if not prompt.enabled then
    Deferred.return
      (Error
         {
           Ox_fast_mcp.Exceptions.message =
             Printf.sprintf "Prompt '%s' is disabled" prompt.name;
           code = Some 403;
           data = None;
         })
  else
    let handler = get_handler prompt in
    let args = Option.value arguments ~default:[] in
    handler args

(** {1 Prompt Creation} *)

(** Create a function prompt from a handler *)
let create_function_prompt ~name ?description ?(arguments = []) ?(tags = [])
    ?key ?(enabled = true) (fn : prompt_handler) : t =
  let prompt_data =
    { arguments = (if List.is_empty arguments then None else Some arguments) }
  in
  let prompt_function = { name; description; prompt_data; fn } in
  let prompt_component_data = { kind = Function_prompt prompt_function } in
  Components.create ~name ?description ~tags:(String.Set.of_list tags) ?key
    ~enabled ~data:prompt_component_data ()

(** {1 MCP Integration} *)

(** Convert to MCP prompt *)
let to_mcp_prompt ?(_include_fastmcp_meta = false) (prompt : t) :
    Mcp.Types.prompt =
  let pf = get_function prompt.data.kind in
  let base_metadata : Mcp.Types.base_metadata =
    { name = prompt.name; title = prompt.title }
  in
  {
    Mcp.Types.description = prompt.description;
    arguments = pf.prompt_data.arguments;
    meta = None;
    (* TODO: Add fastmcp metadata if requested *)
    base_metadata;
  }

(** {1 Component Integration} *)

(** Prompts are now directly components - no conversion needed! *)
let to_component (prompt : t) : t = prompt

(** Create prompt from component *)
let from_component (component : prompt_component_data Components.component) : t
    =
  component
