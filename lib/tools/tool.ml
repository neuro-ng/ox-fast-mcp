(** Tool implementation for FastMCP OCaml *)

open Tool_types
open Utilities.Types

(** Tool handler signature *)
type tool_handler = execution_context -> json -> content_type list Lwt.t

(** Function tool definition *)
type function_tool = {
  name : string;
  description : string;
  parameters : json;
  handler : tool_handler;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}

(** Transformed tool type *)
type transformed = {
  parent_tool : function_tool;
  fn : tool_handler;
  forwarding_fn : tool_handler;
  parameters : json;
  transform_args : (string, Tool_transform.Arg_transform.t) Map.t;
}

(** Tool manager for managing multiple tools *)
type tool_manager = {
  mutable tools : (string, function_tool) Hashtbl.t;
  mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  mutable mask_error_details : bool;
}

(** Create a new tool manager *)
let create_manager ?(duplicate_behavior = `Warn) ?(mask_error_details = false) () =
  {
    tools = Hashtbl.create 16;
    duplicate_behavior;
    mask_error_details;
  }

(** Create a new function tool *)
let create_tool ~name ~description ?(parameters = `Null) ?(enabled = true) 
    ?(tags = []) ?(annotations = None) handler =
  {
    name;
    description;
    parameters;
    handler;
    enabled;
    tags;
    annotations;
  }

(** Convert tool to MCP tool definition *)
let to_mcp_tool tool =
  let annotations = match tool.annotations with
    | Some annots -> Some (("enabled", json_of_bool tool.enabled) :: annots)
    | None -> Some [("enabled", json_of_bool tool.enabled)]
  in
  {
    name = tool.name;
    description = tool.description;
    input_schema = Some tool.parameters;
    annotations;
  }

(** Validate JSON schema for tool parameters *)
let validate_schema schema =
  match schema with
  | `Assoc _ -> true
  | `Null -> true
  | _ -> false

(** Register a tool with the manager *)
let register_tool manager tool =
  let existing = Hashtbl.mem manager.tools tool.name in
  match existing, manager.duplicate_behavior with
  | true, `Error ->
    failwith ("Tool with name '" ^ tool.name ^ "' already exists")
  | true, `Warn ->
    Printf.eprintf "Warning: Tool '%s' already exists, replacing\n%!" tool.name;
    Hashtbl.replace manager.tools tool.name tool
  | true, `Replace ->
    Hashtbl.replace manager.tools tool.name tool
  | true, `Ignore ->
    (* Do nothing - keep existing tool *)
    ()
  | false, _ ->
    Hashtbl.add manager.tools tool.name tool

(** Remove a tool from the manager *)
let remove_tool manager name =
  Hashtbl.remove manager.tools name

(** Get a tool by name *)
let get_tool manager name =
  try
    Some (Hashtbl.find manager.tools name)
  with Not_found ->
    None

(** Get all tools *)
let get_all_tools manager =
  Hashtbl.fold (fun _name tool acc -> tool :: acc) manager.tools []

(** Get enabled tools only *)
let get_enabled_tools manager =
  Hashtbl.fold (fun _name tool acc ->
    if tool.enabled then tool :: acc else acc
  ) manager.tools []

(** Filter tools by tags *)
let filter_tools_by_tags manager tags =
  Hashtbl.fold (fun _name tool acc ->
    let has_any_tag = List.exists (fun tag ->
      List.mem tag tool.tags
    ) tags in
    if has_any_tag then tool :: acc else acc
  ) manager.tools []

(** Get tool count *)
let tool_count manager = Hashtbl.length manager.tools

(** Execute a tool *)
let execute_tool manager tool_name context args =
  match get_tool manager tool_name with
  | None ->
    Lwt.fail (Failure ("Tool not found: " ^ tool_name))
  | Some tool when not tool.enabled ->
    Lwt.fail (Failure ("Tool disabled: " ^ tool_name))
  | Some tool ->
    Lwt.catch
      (fun () -> tool.handler context args)
      (fun exn ->
        let error_msg = 
          if manager.mask_error_details then
            "Tool execution failed"
          else
            "Tool execution failed: " ^ (Printexc.to_string exn)
        in
        Lwt.fail (Failure error_msg))

(** Enable a tool *)
let enable_tool manager name =
  match get_tool manager name with
  | Some tool ->
    let enabled_tool = { tool with enabled = true } in
    Hashtbl.replace manager.tools name enabled_tool;
    true
  | None -> false

(** Disable a tool *)
let disable_tool manager name =
  match get_tool manager name with
  | Some tool ->
    let disabled_tool = { tool with enabled = false } in
    Hashtbl.replace manager.tools name disabled_tool;
    true
  | None -> false

(** Check if a tool is enabled *)
let is_tool_enabled manager name =
  match get_tool manager name with
  | Some tool -> tool.enabled
  | None -> false

(** Update tool tags *)
let update_tool_tags manager name new_tags =
  match get_tool manager name with
  | Some tool ->
    let updated_tool = { tool with tags = new_tags } in
    Hashtbl.replace manager.tools name updated_tool;
    true
  | None -> false

(** Add tags to a tool *)
let add_tool_tags manager name additional_tags =
  match get_tool manager name with
  | Some tool ->
    let combined_tags = List.rev_append additional_tags tool.tags |> 
                       List.sort_uniq String.compare in
    let updated_tool = { tool with tags = combined_tags } in
    Hashtbl.replace manager.tools name updated_tool;
    true
  | None -> false

(** Remove tags from a tool *)
let remove_tool_tags manager name tags_to_remove =
  match get_tool manager name with
  | Some tool ->
    let filtered_tags = List.filter (fun tag ->
      not (List.mem tag tags_to_remove)
    ) tool.tags in
    let updated_tool = { tool with tags = filtered_tags } in
    Hashtbl.replace manager.tools name updated_tool;
    true
  | None -> false

(** Helper: Create a simple calculator tool for testing *)
let create_calculator_tool () =
  let handler _ctx args =
    match args with
    | `Assoc [("expression", `String expr)] ->
      (* Simple expression evaluator - in real implementation, 
         you'd want a proper parser *)
      let result = match String.split_on_char '+' expr with
        | [a; b] ->
          let a_num = String.trim a |> int_of_string in
          let b_num = String.trim b |> int_of_string in
          string_of_int (a_num + b_num)
        | _ -> "Invalid expression"
      in
      Lwt.return [create_text_content result]
    | _ ->
      Lwt.return [create_text_content "Error: Invalid arguments"]
  in
  
  let parameters = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("expression", `Assoc [
        ("type", `String "string");
        ("description", `String "Mathematical expression to evaluate")
      ])
    ]);
    ("required", `List [`String "expression"])
  ] in
  
  create_tool
    ~name:"calculator"
    ~description:"Simple calculator tool"
    ~parameters
    ~tags:["math"; "calculator"]
    handler

(** Helper: Create a text processing tool *)
let create_text_processor_tool () =
  let handler _ctx args =
    match args with
    | `Assoc [("text", `String text); ("operation", `String op)] ->
      let result = match op with
        | "uppercase" -> String.uppercase_ascii text
        | "lowercase" -> String.lowercase_ascii text
        | "length" -> string_of_int (String.length text)
        | "reverse" -> 
          let len = String.length text in
          String.init len (fun i -> text.[len - 1 - i])
        | _ -> "Unknown operation"
      in
      Lwt.return [create_text_content result]
    | _ ->
      Lwt.return [create_text_content "Error: Invalid arguments"]
  in
  
  let parameters = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("text", `Assoc [
        ("type", `String "string");
        ("description", `String "Text to process")
      ]);
      ("operation", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "uppercase"; `String "lowercase"; 
                       `String "length"; `String "reverse"]);
        ("description", `String "Operation to perform")
      ])
    ]);
    ("required", `List [`String "text"; `String "operation"])
  ] in
  
  create_tool
    ~name:"text_processor"
    ~description:"Text processing tool"
    ~parameters
    ~tags:["text"; "processing"]
    handler

(** Clear all tools from manager *)
let clear_tools manager =
  Hashtbl.clear manager.tools

(** Tool statistics *)
type tool_stats = {
  total_tools : int;
  enabled_tools : int;
  disabled_tools : int;
  tags_used : string list;
}

(** Get tool statistics *)
let get_tool_stats manager =
  let all_tools = get_all_tools manager in
  let enabled_count = List.length (List.filter (fun t -> t.enabled) all_tools) in
  let all_tags = List.fold_left (fun acc tool ->
    List.rev_append tool.tags acc
  ) [] all_tools |> List.sort_uniq String.compare in
  
  {
    total_tools = List.length all_tools;
    enabled_tools = enabled_count;
    disabled_tools = List.length all_tools - enabled_count;
    tags_used = all_tags;
  }

(** Create a transformed tool *)
let create_transformed
    ?name
    ?description
    ?tags
    ?annotations
    ?serializer
    ?enabled
    ~parent_tool
    ~fn
    ~forwarding_fn
    ~parameters
    ~transform_args
    () =
  let name = Option.value name ~default:parent_tool.name in
  let description = Option.value description ~default:parent_tool.description in
  let tags = Option.value tags ~default:parent_tool.tags in
  let annotations = Option.value annotations ~default:parent_tool.annotations in
  let enabled = Option.value enabled ~default:parent_tool.enabled in

  let handler ctx args = fn ctx args in

  { parent_tool;
    fn;
    forwarding_fn;
    parameters;
    transform_args;
  }

(** Run transformed tool's forwarding function *)
let run_transformed_forwarding tool args =
  tool.forwarding_fn args

(** Add transformed tool to manager *)
let add_transformed_tool manager tool =
  let existing = Hashtbl.mem manager.tools tool.parent_tool.name in
  match existing, manager.duplicate_behavior with
  | true, `Error ->
    failwith ("Tool with name '" ^ tool.parent_tool.name ^ "' already exists")
  | true, `Warn ->
    Printf.eprintf "Warning: Tool '%s' already exists, replacing\n%!" tool.parent_tool.name;
    Hashtbl.replace manager.tools tool.parent_tool.name tool
  | true, `Replace ->
    Hashtbl.replace manager.tools tool.parent_tool.name tool
  | true, `Ignore ->
    (* Do nothing - keep existing tool *)
    ()
  | false, _ ->
    Hashtbl.add manager.tools tool.parent_tool.name tool

(** Get tool parameters *)
let parameters tool = tool.parameters

(** Run a tool *)
let run tool args = tool.handler args 