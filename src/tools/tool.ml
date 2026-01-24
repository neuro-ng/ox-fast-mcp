(** Tool implementation for FastMCP OCaml *)

open Tool_types
open Fmcp_types
open Core
open Async
open Async.Let_syntax

(* Use types from Tool_types module to avoid duplication *)

type tool_manager = {
  mutable tools : (string, Tool_types.t) Hashtbl.t;
  mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  mutable mask_error_details : bool;
}
(** Tool manager for managing multiple tools *)

(** Create a new tool manager *)
let create_manager ?(duplicate_behavior = `Warn) ?(mask_error_details = false)
    () =
  {
    tools = Hashtbl.create (module String);
    duplicate_behavior;
    mask_error_details;
  }

(** Create a new function tool *)
let create_tool ~name ~description ?(parameters = `Null) ?(enabled = true)
    ?(tags = []) ?(annotations = None) handler : Tool_types.t =
  (* Convert legacy handler to new Result-based handler *)
  let result_handler = Tool_types.handler_of_legacy handler in
  Tool_types.create_function_tool ~name ~description ~tags
    ~input_schema:parameters ?annotations ~enabled result_handler

(** Convert tool to MCP tool definition - temporarily disabled *)
let to_mcp_tool _tool =
  failwith "to_mcp_tool temporarily disabled due to type issues"

(** Validate JSON schema for tool parameters *)
let validate_schema schema =
  match schema with
  | `Assoc _ -> true
  | `Null -> true
  | _ -> false

(** Register a tool with the manager *)
let register_tool manager (tool : Tool_types.t) =
  let tool_name = Tool_types.get_name tool in
  let existing = Hashtbl.mem manager.tools tool_name in
  match (existing, manager.duplicate_behavior) with
  | true, `Error ->
    failwith ("Tool with name '" ^ tool_name ^ "' already exists")
  | true, `Warn ->
    (* Printf.eprintf "Warning: Tool '%s' already exists, replacing\n%!"
       tool_name; *)
    Hashtbl.set manager.tools ~key:tool_name ~data:tool
  | true, `Replace -> Hashtbl.set manager.tools ~key:tool_name ~data:tool
  | true, `Ignore ->
    (* Do nothing - keep existing tool *)
    ()
  | false, _ -> Hashtbl.set manager.tools ~key:tool_name ~data:tool

(** Remove a tool from the manager *)
let remove_tool manager name = Hashtbl.remove manager.tools name

(** Get a tool by name *)
let get_tool manager name = Hashtbl.find manager.tools name

(** Get all tools *)
let get_all_tools manager =
  Hashtbl.fold manager.tools ~init:[] ~f:(fun ~key:_ ~data:tool acc ->
      tool :: acc)

(** Get enabled tools only *)
let get_enabled_tools manager =
  Hashtbl.fold manager.tools ~init:[] ~f:(fun ~key:_ ~data:tool acc ->
      if Tool_types.is_enabled tool then tool :: acc else acc)

(** Filter tools by tags *)
let filter_tools_by_tags manager tags =
  Hashtbl.fold manager.tools ~init:[] ~f:(fun ~key:_ ~data:tool acc ->
      let tool_tags = Tool_types.get_tags tool in
      let has_any_tag =
        List.exists tags ~f:(fun tag ->
            List.mem tool_tags tag ~equal:String.equal)
      in
      if has_any_tag then tool :: acc else acc)

(** Get tool count *)
let tool_count manager = Hashtbl.length manager.tools

(** Execute a tool *)
let execute_tool manager tool_name context args =
  match get_tool manager tool_name with
  | None -> failwith ("Tool not found: " ^ tool_name)
  | Some tool when not (Tool_types.is_enabled tool) ->
    failwith ("Tool disabled: " ^ tool_name)
  | Some tool -> (
    Tool_types.run tool ~context ~arguments:args >>= function
    | Ok result -> return result.Tool_types.content
    | Error error_data ->
      let error_msg =
        if manager.mask_error_details then "Tool execution failed"
        else "Tool execution failed: " ^ error_data.Exceptions.message
      in
      failwith error_msg)

(** Enable a tool *)
let enable_tool manager name =
  match get_tool manager name with
  | Some tool ->
    let enabled_tool = Tool_types.set_enabled tool true in
    Hashtbl.set manager.tools ~key:name ~data:enabled_tool;
    true
  | None -> false

(** Disable a tool *)
let disable_tool manager name =
  match get_tool manager name with
  | Some tool ->
    let disabled_tool = Tool_types.set_enabled tool false in
    Hashtbl.set manager.tools ~key:name ~data:disabled_tool;
    true
  | None -> false

(** Check if a tool is enabled *)
let is_tool_enabled manager name =
  match get_tool manager name with
  | Some tool -> Tool_types.is_enabled tool
  | None -> false

(** Update tool tags *)
let update_tool_tags manager name new_tags =
  match get_tool manager name with
  | Some tool ->
    let updated_tool = Tool_types.set_tags tool new_tags in
    Hashtbl.set manager.tools ~key:name ~data:updated_tool;
    true
  | None -> false

(** Add tags to a tool *)
let add_tool_tags manager name additional_tags =
  match get_tool manager name with
  | Some tool ->
    let existing_tags = Tool_types.get_tags tool in
    let combined_tags =
      List.rev_append additional_tags existing_tags
      |> List.dedup_and_sort ~compare:String.compare
    in
    let updated_tool = Tool_types.set_tags tool combined_tags in
    Hashtbl.set manager.tools ~key:name ~data:updated_tool;
    true
  | None -> false

(** Remove tags from a tool *)
let remove_tool_tags manager name tags_to_remove =
  match get_tool manager name with
  | Some tool ->
    let existing_tags = Tool_types.get_tags tool in
    let filtered_tags =
      List.filter existing_tags ~f:(fun tag ->
          not (List.mem tags_to_remove tag ~equal:String.equal))
    in
    let updated_tool = Tool_types.set_tags tool filtered_tags in
    Hashtbl.set manager.tools ~key:name ~data:updated_tool;
    true
  | None -> false

(** Helper: Create a simple calculator tool for testing *)
let create_calculator_tool () =
  let handler _ctx args =
    match args with
    | `Assoc [ ("expression", `String expr) ] ->
      (* Simple expression evaluator - in real implementation, you'd want a
         proper parser *)
      let result =
        match String.split expr ~on:'+' with
        | [ a; b ] ->
          let a_num = String.strip a |> int_of_string in
          let b_num = String.strip b |> int_of_string in
          string_of_int (a_num + b_num)
        | _ -> "Invalid expression"
      in
      return [ create_text_content result ]
    | _ -> return [ create_text_content "Error: Invalid arguments" ]
  in

  let parameters =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "expression",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Mathematical expression to evaluate" );
                  ] );
            ] );
        ("required", `List [ `String "expression" ]);
      ]
  in

  create_tool ~name:"calculator" ~description:"Simple calculator tool"
    ~parameters ~tags:[ "math"; "calculator" ] handler

(** Helper: Create a text processing tool *)
let create_text_processor_tool () =
  let handler _ctx args =
    match args with
    | `Assoc [ ("text", `String text); ("operation", `String op) ] ->
      let result =
        match op with
        | "uppercase" -> String.uppercase text
        | "lowercase" -> String.lowercase text
        | "length" -> string_of_int (String.length text)
        | "reverse" ->
          let len = String.length text in
          String.init len ~f:(fun i -> text.[len - 1 - i])
        | _ -> "Unknown operation"
      in
      return [ create_text_content result ]
    | _ -> return [ create_text_content "Error: Invalid arguments" ]
  in

  let parameters =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "text",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Text to process");
                  ] );
              ( "operation",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "enum",
                      `List
                        [
                          `String "uppercase";
                          `String "lowercase";
                          `String "length";
                          `String "reverse";
                        ] );
                    ("description", `String "Operation to perform");
                  ] );
            ] );
        ("required", `List [ `String "text"; `String "operation" ]);
      ]
  in

  create_tool ~name:"text_processor" ~description:"Text processing tool"
    ~parameters ~tags:[ "text"; "processing" ] handler

(** Clear all tools from manager *)
let clear_tools manager = Hashtbl.clear manager.tools

type tool_stats = {
  total_tools : int;
  enabled_tools : int;
  disabled_tools : int;
  tags_used : string list;
}
(** Tool statistics *)

(** Get tool statistics *)
let get_tool_stats manager =
  let all_tools = get_all_tools manager in
  let enabled_count =
    List.length (List.filter all_tools ~f:Tool_types.is_enabled)
  in
  let all_tags =
    List.fold all_tools ~init:[] ~f:(fun acc tool ->
        List.rev_append (Tool_types.get_tags tool) acc)
    |> List.dedup_and_sort ~compare:String.compare
  in

  {
    total_tools = List.length all_tools;
    enabled_tools = enabled_count;
    disabled_tools = List.length all_tools - enabled_count;
    tags_used = all_tags;
  }

(** Add transformed tool to manager - now just registers the tool *)
let add_transformed_tool manager (tool : Tool_types.t) =
  register_tool manager tool

(** Transform a tool with the specified parameters *)
let from_tool ?name ?description ?tags ?transform_fn ?transform_args
    ?annotations:_ ?serializer:_ ?enabled (tool : Tool_types.t) : Tool_types.t =
  (* Convert list-based transform_args to Map if provided *)
  let transform_args_map =
    match transform_args with
    | None -> Core.String.Map.empty
    | Some args_list ->
      Core.List.fold args_list ~init:Core.String.Map.empty
        ~f:(fun acc (param_name, transform) ->
          Core.Map.set acc ~key:param_name ~data:transform)
  in

  (* Convert legacy transform_fn to new json -> json if provided *)
  let json_transform_fn =
    match transform_fn with
    | None -> None
    | Some _legacy_fn ->
      Some
        (fun (json : Yojson.Safe.t) ->
          (* This is a temporary shim - ideally transform_fn should be json ->
             json *)
          json)
  in

  Tool_types.create_transformed_tool tool ?name ?description ?tags
    ?transform_fn:json_transform_fn ~transform_args:transform_args_map
    ?enabled:(Some (Option.value enabled ~default:true))
    ()
