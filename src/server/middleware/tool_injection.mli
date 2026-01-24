(** Tool injection middleware for OxFastMCP.

    This middleware allows injecting additional tools into the MCP server
    context, enabling tools to be added without modifying the base server
    configuration. *)

open! Async

(** Tool information for injection *)
module Injectable_tool : sig
  type t

  val create :
    name:string ->
    ?description:string ->
    input_schema:Yojson.Safe.t ->
    run:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    unit ->
    t

  val name : t -> string
  val description : t -> string option
  val input_schema : t -> Yojson.Safe.t
  val run : t -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t
end

(** Tool injection middleware *)
module ToolInjectionMiddleware : sig
  type t

  val create : tools:Injectable_tool.t list -> unit -> t
  val tools : t -> Injectable_tool.t list
  val find_tool : t -> string -> Injectable_tool.t option
  val has_tool : t -> string -> bool
  val tool_names : t -> string list
  val run_tool : t -> string -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t option
end

(** Prompt tool middleware - injects list_prompts and get_prompt tools *)
module PromptToolMiddleware : sig
  type t

  val create : unit -> t
  val tool_injection : t -> ToolInjectionMiddleware.t
end

(** Resource tool middleware - injects list_resources and read_resource tools *)
module ResourceToolMiddleware : sig
  type t

  val create : unit -> t
  val tool_injection : t -> ToolInjectionMiddleware.t
end
