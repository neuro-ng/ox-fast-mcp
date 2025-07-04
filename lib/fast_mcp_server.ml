open Core
open Async
open Tools.Tool_types
open Utilities.Types
open Utilities.Content_block
open Utilities.Resource_template

type t = {
  name : string;
  instructions : string option;
  mutable tool_count : int;
  mutable resource_count : int;
  mutable prompt_count : int;
  tools : function_tool list;
  resources : string list;  (* Simplified for now *)
  resource_templates : string list;  (* Simplified for now *)
  prompts : string list;  (* Simplified for now *)
}
(** FastMCP server state *)

(** Create a new FastMCP server *)
let create ?(name="FastMCP") ?(instructions=None) () = {
  name;
  instructions;
  tool_count = 0;
  resource_count = 0;
  prompt_count = 0;
  tools = [];
  resources = [];
  resource_templates = [];
  prompts = [];
}

(** Get server name *)
let get_name server = server.name

(** Get server instructions *)
let get_instructions server = server.instructions

(** Get tool count *)
let tool_count server = server.tool_count

(** Get resource count *)
let resource_count server = server.resource_count

(** Get prompt count *)
let prompt_count server = server.prompt_count

(** Register a tool *)
let register_tool server ~name ~description ~func =
  server.tool_count <- server.tool_count + 1;
  (* For now, just increment count - real implementation would store the tool *)
  ignore (name, description, func)

(** Register a tool with context *)
let register_tool_with_context server ~name ~description ~func =
  server.tool_count <- server.tool_count + 1;
  ignore (name, description, func)

(** Register a resource *)
let register_resource server ~uri ~name ~description ~func =
  server.resource_count <- server.resource_count + 1;
  ignore (uri, name, description, func)

(** Register a resource template *)
let register_resource_template server ~uri_pattern ~name ~description ~func =
  server.resource_count <- server.resource_count + 1;
  ignore (uri_pattern, name, description, func)

(** Register a prompt *)
let register_prompt server ~name ~description ~func =
  server.prompt_count <- server.prompt_count + 1;
  ignore (name, description, func)

(** List all tools - simplified *)
let list_tools t =
  Deferred.return t.tools

(** List all resources - simplified *)
let list_resources t =
  Deferred.return t.resources

(** List all prompts - simplified *)
let list_prompts t =
  Deferred.return t.prompts

(** List all resource templates - simplified *)
let list_resource_templates t =
  Deferred.return t.resource_templates

(** Get server capabilities - simplified *)
let get_capabilities _server =
  {
    logging = None;
    prompts = Some (`Assoc [ ("listChanged", `Bool true) ]);
    resources = Some (`Assoc [ ("subscribe", `Bool true) ]);
    tools = Some (`Assoc [ ("listChanged", `Bool true) ]);
  }

let add_tool t tool =
  { t with tools = tool :: t.tools }

let add_resource t resource =
  { t with resources = resource :: t.resources }

let add_resource_template t template =
  { t with resource_templates = template :: t.resource_templates }

let add_prompt t prompt =
  { t with prompts = prompt :: t.prompts }

let list_tools_mcp t =
  let%bind tools = list_tools t in
  Deferred.return { tools }

let list_resources_mcp t =
  let%bind resources = list_resources t in
  Deferred.return { resources }

let list_resource_templates_mcp t =
  let%bind resource_templates = list_resource_templates t in
  Deferred.return { resource_templates }

let call_tool t ~name ~arguments () =
  match List.find t.tools ~f:(fun tool -> String.equal tool.name name) with
  | Some tool -> tool.f arguments
  | None -> failwith (sprintf "Tool '%s' not found" name)

let call_tool_mcp t ~name ~arguments () =
  let%bind content = call_tool t ~name ~arguments () in
  Deferred.return { content; is_error = false }

let list_prompts_mcp t =
  let%bind prompts = list_prompts t in
  Deferred.return { prompts }

let get_prompt t ~name ~arguments () =
  match List.find t.prompts ~f:(fun prompt -> String.equal prompt.name name) with
  | Some prompt ->
    let%bind text = prompt.f arguments in
    Deferred.return { messages = [text_block text]; description = "Example greeting prompt." }
  | None -> failwith (sprintf "Prompt '%s' not found" name)

let get_prompt_mcp t ~name ~arguments () =
  get_prompt t ~name ~arguments ()

let read_resource t uri =
  match List.find t.resources ~f:(fun r -> Uri.equal r.uri uri) with
  | Some resource -> resource.f ()
  | None ->
    (* Try resource templates *)
    match List.find_map t.resource_templates ~f:(fun template ->
      match match_uri template.uri_template uri with
      | Some params -> Some (template, params)
      | None -> None
    ) with
    | Some (template, params) -> template.f ~user_id:(List.Assoc.find_exn params ~equal:String.equal "user_id")
    | None -> Error.of_string (sprintf "Resource '%s' not found" (Uri.to_string uri)) |> Deferred.return

let get_tools t = t.tools

let get_tool t name =
  List.find t.tools ~f:(fun tool -> String.equal tool.base.name name)
