open Mcp_types

(** FastMCP server state *)
type t = {
  name : string;
  instructions : string option;
  mutable tool_count : int;
  mutable resource_count : int;
  mutable prompt_count : int;
}

(** Create a new FastMCP server *)
let create ?(instructions = None) ~name () = {
  name;
  instructions;
  tool_count = 0;
  resource_count = 0;
  prompt_count = 0;
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
let list_tools _server = 
  [{ name = "example_tool"; description = "An example tool"; input_schema = None }]

(** List all resources - simplified *)
let list_resources _server =
  [{ uri = "example://resource"; name = Some "Example Resource"; description = Some "An example resource"; mime_type = None }]

(** List all prompts - simplified *)
let list_prompts _server =
  [{ name = "example_prompt"; description = "An example prompt"; arguments = None }]

(** Get server capabilities - simplified *)
let get_capabilities _server = {
  logging = None;
  prompts = Some (Object [("listChanged", Bool true)]);
  resources = Some (Object [("subscribe", Bool true)]);
  tools = Some (Object [("listChanged", Bool true)]);
} 