(** Example usage of MCP Mixin for OxFastMCP.

    Demonstrates how to use the mixin pattern to organize tools, resources, and
    prompts into reusable modules that can be registered with a FastMCP server. *)

open! Core
open! Async

(* Define a module that implements the Mcp_mixin.S signature. This represents a
   "mixin" - a collection of related tools, resources, and prompts. *)
module CalculatorMixin = struct
  type t = { multiplier : int }

  let create ?(multiplier = 1) () = { multiplier }

  (* Tool handlers *)
  let add_handler t _ctx (args : Yojson.Safe.t) =
    match args with
    | `Assoc fields ->
      let x =
        List.Assoc.find fields ~equal:String.equal "x"
        |> Option.value_map ~default:0 ~f:(function
             | `Int n -> n
             | _ -> 0)
      in
      let y =
        List.Assoc.find fields ~equal:String.equal "y"
        |> Option.value_map ~default:0 ~f:(function
             | `Int n -> n
             | _ -> 0)
      in
      let result = (x + y) * t.multiplier in
      return [ Fmcp_types.create_text_content (Int.to_string result) ]
    | _ -> return [ Fmcp_types.create_text_content "Invalid arguments" ]

  let multiply_handler t _ctx (args : Yojson.Safe.t) =
    match args with
    | `Assoc fields ->
      let x =
        List.Assoc.find fields ~equal:String.equal "x"
        |> Option.value_map ~default:0 ~f:(function
             | `Int n -> n
             | _ -> 0)
      in
      let y =
        List.Assoc.find fields ~equal:String.equal "y"
        |> Option.value_map ~default:0 ~f:(function
             | `Int n -> n
             | _ -> 0)
      in
      let result = x * y * t.multiplier in
      return [ Fmcp_types.create_text_content (Int.to_string result) ]
    | _ -> return [ Fmcp_types.create_text_content "Invalid arguments" ]

  (* Get all tools from this mixin *)
  let get_tools t : Mcp_mixin.registered_tool list =
    [
      {
        handler = add_handler t;
        info =
          Mcp_mixin.make_tool_info ~name:"add"
            ~description:"Add two numbers together"
            ~tags:[ "math"; "arithmetic" ] ();
      };
      {
        handler = multiply_handler t;
        info =
          Mcp_mixin.make_tool_info ~name:"multiply"
            ~description:"Multiply two numbers"
            ~tags:[ "math"; "arithmetic" ] ();
      };
    ]

  (* Get all resources (empty for this example) *)
  let get_resources _t : Mcp_mixin.registered_resource list = []

  (* Get all prompts (empty for this example) *)
  let get_prompts _t : Mcp_mixin.registered_prompt list = []

  (* Register all items with server managers *)
  let register_all t ~tool_manager ~resource_manager ~prompt_manager ?prefix
      ?tool_separator ?resource_separator ?prompt_separator () =
    Mcp_mixin.register_all ~tools:(get_tools t)
      ~resources:(get_resources t)
      ~prompts:(get_prompts t) ~tool_manager ~resource_manager ~prompt_manager
      ?prefix ?tool_separator ?resource_separator ?prompt_separator ()
end

(* Example of using bulk tool calling with the mixin *)
let example_bulk_calling () =
  let mock_call_tool ~name ~arguments =
    let content =
      [
        `Text
          {
            Mcp.Types.type_ = `Text;
            text =
              sprintf "Called %s with: %s" name (Yojson.Safe.to_string arguments);
            annotations = None;
            meta = None;
          };
      ]
    in
    return
      {
        Mcp.Types.content;
        structured_content = None;
        is_error = false;
        result = { Mcp.Types.meta = None };
      }
  in

  let caller = Mcp_mixin.create_bulk_caller () in

  (* Call add tool with multiple argument sets using bulk execution *)
  let tool_arguments =
    [
      `Assoc [ ("x", `Int 1); ("y", `Int 2) ];
      `Assoc [ ("x", `Int 10); ("y", `Int 20) ];
      `Assoc [ ("x", `Int 100); ("y", `Int 200) ];
    ]
  in

  let%bind results =
    Mcp_mixin.call_tool_bulk caller ~tool:"add" ~tool_arguments
      ~continue_on_error:true ~call_tool:mock_call_tool ()
  in

  List.iter results ~f:(fun result ->
      printf "Tool: %s, Error: %b\n" result.tool result.is_error);

  return ()

(* Example of registering mixin with a server *)
let example_registration () =
  let calculator = CalculatorMixin.create ~multiplier:2 () in
  let tool_manager = Tool.create_tool_manager () in
  let resource_manager = Resources.Resource_manager.create () in
  let prompt_manager = Prompts.Prompt_manager.create () in

  (* Register with a prefix to namespace the tools *)
  CalculatorMixin.register_all calculator ~tool_manager ~resource_manager
    ~prompt_manager ~prefix:"calc" ~tool_separator:"_" ();

  (* Now tools are available as "calc_add" and "calc_multiply" *)
  let%bind has_add = Tool.has_tool tool_manager "calc_add" in
  let%bind has_multiply = Tool.has_tool tool_manager "calc_multiply" in

  printf "calc_add registered: %b\n" has_add;
  printf "calc_multiply registered: %b\n" has_multiply;

  return ()

(* To run the examples:

   let () = Command.async ~summary:"MCP Mixin Example" (Command.Param.return
   (fun () -> let%bind () = example_registration () in example_bulk_calling ()))
   |> Command_unix.run *)
