(** Example usage of Bulk Tool Caller for OxFastMCP.

    Demonstrates how to use the bulk tool caller to call multiple tools or the
    same tool with different arguments in a single batch. *)

open! Core
open! Async

(* Example: Create some mock tool results for demonstration *)
let mock_call_tool ~name ~arguments =
  (* In a real implementation, this would call the actual tool *)
  let content =
    [
      `Text
        {
          Mcp.Types.type_ = `Text;
          text =
            sprintf "Called %s with args: %s" name
              (Yojson.Safe.to_string arguments);
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

let example_call_tools_bulk () =
  let caller = Bulk_tool_caller.create () in

  (* Define tool calls *)
  let tool_calls =
    [
      {
        Bulk_tool_caller.tool = "echo_tool";
        arguments = `Assoc [ ("text", `String "Hello") ];
      };
      {
        Bulk_tool_caller.tool = "echo_tool";
        arguments = `Assoc [ ("text", `String "World") ];
      };
      {
        Bulk_tool_caller.tool = "another_tool";
        arguments = `Assoc [ ("value", `Int 42) ];
      };
    ]
  in

  (* Call all tools in bulk *)
  let%bind results =
    Bulk_tool_caller.call_tools_bulk caller ~tool_calls ~continue_on_error:true
      ~call_tool:mock_call_tool ()
  in

  (* Print results *)
  List.iter results ~f:(fun result ->
      printf "Tool: %s, Error: %b\n" result.tool result.is_error);

  return ()

let example_call_tool_bulk () =
  let caller = Bulk_tool_caller.create () in

  (* Define arguments for same tool *)
  let tool_arguments =
    [
      `Assoc [ ("text", `String "First") ];
      `Assoc [ ("text", `String "Second") ];
      `Assoc [ ("text", `String "Third") ];
    ]
  in

  (* Call the same tool with different arguments *)
  let%bind results =
    Bulk_tool_caller.call_tool_bulk caller ~tool:"echo_tool" ~tool_arguments
      ~continue_on_error:true ~call_tool:mock_call_tool ()
  in

  (* Print results *)
  List.iter results ~f:(fun result ->
      printf "Tool: %s, Args: %s, Error: %b\n" result.tool
        (Yojson.Safe.to_string result.arguments)
        result.is_error);

  return ()

(* To run the examples: let () = Command.async ~summary:"Bulk Tool Caller
   Example" (Command.Param.return (fun () -> let%bind () =
   example_call_tools_bulk () in example_call_tool_bulk ())) |>
   Command_unix.run *)
