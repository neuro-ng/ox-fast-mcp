(** Tool injection middleware for OxFastMCP.

    This middleware allows injecting additional tools into the MCP server
    context, enabling tools to be added without modifying the base server
    configuration. *)

open! Core
open! Async

(** Tool information for injection *)
module Injectable_tool = struct
  type t = {
    name : string;
    description : string option;
    input_schema : Yojson.Safe.t;
    run : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  let create ~name ?description ~input_schema ~run () =
    { name; description; input_schema; run }

  let name t = t.name
  let description t = t.description
  let input_schema t = t.input_schema
  let run t args = t.run args
end

(** Tool injection middleware *)
module ToolInjectionMiddleware = struct
  type t = {
    tools_to_inject : Injectable_tool.t list;
    tools_by_name : (string, Injectable_tool.t) Hashtbl.t;
  }

  let create ~tools () =
    let tools_by_name = Hashtbl.create (module String) in
    List.iter tools ~f:(fun tool ->
        Hashtbl.set tools_by_name ~key:(Injectable_tool.name tool) ~data:tool);
    { tools_to_inject = tools; tools_by_name }

  let tools t = t.tools_to_inject
  let find_tool t name = Hashtbl.find t.tools_by_name name
  let has_tool t name = Hashtbl.mem t.tools_by_name name

  (** Get injected tool names *)
  let tool_names t = List.map t.tools_to_inject ~f:Injectable_tool.name

  (** Run an injected tool if it exists *)
  let run_tool t name arguments =
    match find_tool t name with
    | Some tool -> Some (Injectable_tool.run tool arguments)
    | None -> None
end

(** Prompt tool middleware - injects list_prompts and get_prompt tools *)
module PromptToolMiddleware = struct
  type t = { tool_injection : ToolInjectionMiddleware.t }

  let list_prompts_tool =
    Injectable_tool.create ~name:"list_prompts"
      ~description:"List prompts available on the server."
      ~input_schema:
        (`Assoc [ ("type", `String "object"); ("properties", `Assoc []) ])
      ~run:(fun _args ->
        (* Placeholder - requires server context *)
        return (`Assoc [ ("prompts", `List []) ]))
      ()

  let get_prompt_tool =
    Injectable_tool.create ~name:"get_prompt"
      ~description:"Render a prompt available on the server."
      ~input_schema:
        (`Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ("name", `Assoc [ ("type", `String "string") ]);
                  ("arguments", `Assoc [ ("type", `String "object") ]);
                ] );
            ("required", `List [ `String "name" ]);
          ])
      ~run:(fun _args ->
        (* Placeholder - requires server context *)
        return (`Assoc [ ("result", `Null) ]))
      ()

  let create () =
    let tools = [ list_prompts_tool; get_prompt_tool ] in
    let tool_injection = ToolInjectionMiddleware.create ~tools () in
    { tool_injection }

  let tool_injection t = t.tool_injection
end

(** Resource tool middleware - injects list_resources and read_resource tools *)
module ResourceToolMiddleware = struct
  type t = { tool_injection : ToolInjectionMiddleware.t }

  let list_resources_tool =
    Injectable_tool.create ~name:"list_resources"
      ~description:"List resources available on the server."
      ~input_schema:
        (`Assoc [ ("type", `String "object"); ("properties", `Assoc []) ])
      ~run:(fun _args ->
        (* Placeholder - requires server context *)
        return (`Assoc [ ("resources", `List []) ]))
      ()

  let read_resource_tool =
    Injectable_tool.create ~name:"read_resource"
      ~description:"Read a resource available on the server."
      ~input_schema:
        (`Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc [ ("uri", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "uri" ]);
          ])
      ~run:(fun _args ->
        (* Placeholder - requires server context *)
        return (`Assoc [ ("content", `Null) ]))
      ()

  let create () =
    let tools = [ list_resources_tool; read_resource_tool ] in
    let tool_injection = ToolInjectionMiddleware.create ~tools () in
    { tool_injection }

  let tool_injection t = t.tool_injection
end
