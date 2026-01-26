open Core
open Async
open Testing_demo_lib

(* Helper to extract content text from tool result *)
let get_text_content result =
  match result with
  | `Assoc fields ->
    List.Assoc.find fields ~equal:String.equal "content"
    |> Option.value_map ~default:"<no content>" ~f:(function
         | `List items ->
           List.map items ~f:(function
             | `Assoc item_fields ->
               List.Assoc.find item_fields ~equal:String.equal "text"
               |> Option.value_map ~default:"" ~f:(function
                    | `String s -> s
                    | _ -> "")
             | _ -> "")
           |> String.concat ~sep:"\n"
         | _ -> "<invalid content>")
  | _ -> "<invalid result>"

let%expect_test "add tool" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.call_tool server ~name:"add"
      ~arguments:(`Assoc [ ("a", `Int 2); ("b", `Int 3) ])
  in
  print_endline (get_text_content result);
  [%expect {| 5 |}];
  return ()

let%expect_test "greet tool default" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.call_tool server ~name:"greet"
      ~arguments:(`Assoc [ ("name", `String "Alice") ])
  in
  print_endline (get_text_content result);
  [%expect {| Hello, Alice! |}];
  return ()

let%expect_test "greet tool custom" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.call_tool server ~name:"greet"
      ~arguments:
        (`Assoc [ ("name", `String "Bob"); ("greeting", `String "Hi") ])
  in
  print_endline (get_text_content result);
  [%expect {| Hi, Bob! |}];
  return ()

let%expect_test "async multiply tool" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.call_tool server ~name:"async_multiply"
      ~arguments:(`Assoc [ ("x", `Float 3.5); ("y", `Float 2.0) ])
  in
  print_endline (get_text_content result);
  [%expect {| 7. |}];
  return ()

let%expect_test "server info resource" =
  let server = Demo_server.create () in
  let%bind content =
    Server.Ox_fast_mcp.read_resource server ~uri:"demo://info"
  in
  print_endline content;
  [%expect {| This is the FastMCP Testing Demo server |}];
  return ()

(* Note: Dynamic resource templates are not yet fully supported by read_resource
   in Ox_fast_mcp v1 skipping template test *)

(* Helper to extract text from prompt result *)
let get_prompt_text result =
  match result with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "messages" with
    | Some (`List (`Assoc message_fields :: _)) -> (
      match List.Assoc.find message_fields ~equal:String.equal "content" with
      | Some (`Assoc content_fields) ->
        List.Assoc.find content_fields ~equal:String.equal "text"
        |> Option.value_map ~default:"<no text>" ~f:(function
             | `String s -> s
             | _ -> "<not string>")
      | _ -> "<no content>")
    | _ -> "<no messages>")
  | _ -> "<invalid result>"

let%expect_test "hello prompt default" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.get_prompt server ~name:"hello" ~arguments:(`Assoc [])
  in
  print_endline (get_prompt_text result);
  [%expect {| Say hello to World in a friendly way. |}];
  return ()

let%expect_test "hello prompt custom" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.get_prompt server ~name:"hello"
      ~arguments:(`Assoc [ ("name", `String "Dave") ])
  in
  print_endline (get_prompt_text result);
  [%expect {| Say hello to Dave in a friendly way. |}];
  return ()

let%expect_test "explain prompt simple" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.get_prompt server ~name:"explain"
      ~arguments:
        (`Assoc
          [ ("topic", `String "MCP"); ("detail_level", `String "simple") ])
  in
  print_endline (get_prompt_text result);
  [%expect {| Explain MCP in simple terms for beginners. |}];
  return ()

let%expect_test "explain prompt detailed" =
  let server = Demo_server.create () in
  let%bind result =
    Server.Ox_fast_mcp.get_prompt server ~name:"explain"
      ~arguments:
        (`Assoc
          [ ("topic", `String "MCP"); ("detail_level", `String "detailed") ])
  in
  print_endline (get_prompt_text result);
  [%expect {| Provide a detailed, technical explanation of MCP. |}];
  return ()

let%expect_test "list tools" =
  let server = Demo_server.create () in
  let tools = Server.Ox_fast_mcp.list_tools_mcp server in
  let names =
    List.map tools ~f:(fun t ->
        match t with
        | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal "name"
          |> Option.value_map ~default:"" ~f:(function
               | `String s -> s
               | _ -> "")
        | _ -> "")
    |> List.sort ~compare:String.compare
  in
  print_s [%sexp (names : string list)];
  [%expect {| (add async_multiply greet) |}];
  return ()

let%expect_test "list resources" =
  let server = Demo_server.create () in
  let resources = Server.Ox_fast_mcp.list_resources_mcp server in
  let uris =
    List.map resources ~f:(fun r ->
        match r with
        | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal "uri"
          |> Option.value_map ~default:"" ~f:(function
               | `String s -> s
               | _ -> "")
        | _ -> "")
    |> List.sort ~compare:String.compare
  in
  print_s [%sexp (uris : string list)];
  (* Note: Template-instantiated resources (demo://greeting/{name}) are dynamic
     and not listed in static list_resources unless explicitly added. The static
     resource 'demo://info' should be listed. *)
  [%expect {| (demo://info) |}];
  return ()

let%expect_test "list prompts" =
  let server = Demo_server.create () in
  let prompts = Server.Ox_fast_mcp.list_prompts_mcp server in
  let names =
    List.map prompts ~f:(fun p ->
        match p with
        | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal "name"
          |> Option.value_map ~default:"" ~f:(function
               | `String s -> s
               | _ -> "")
        | _ -> "")
    |> List.sort ~compare:String.compare
  in
  print_s [%sexp (names : string list)];
  [%expect {| (explain hello) |}];
  return ()
