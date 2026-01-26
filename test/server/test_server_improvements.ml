(** Server Improvements Tests

    Tests for:
    - Server description and inspection
    - Component search and discovery
    - Similar name suggestions
    - Server validation *)

open Core
open Async

(** {1 Server Description Tests} *)

let%expect_test "describe_server_basic_info" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server =
    create ~name:"test-server" ~version:"1.0.0"
      ~instructions:"Test instructions" ()
  in
  let description = describe_server server in
  print_endline (Yojson.Safe.to_string description);
  [%expect
    {|
    {"name":"test-server","version":"1.0.0","instructions":"Test instructions","tool_count":0,"resource_count":0,"template_count":0,"prompt_count":0,"mounted_server_count":0,"middleware_count":0,"strict_input_validation":false,"include_fastmcp_meta":true} |}];
  return ()

let%expect_test "describe_server_with_components" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"full-server" () in
  (* Add some components *)
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return (`String "ok")) server;
  add_simple_resource ~uri:"file:///test" ~name:"res1"
    ~reader:(fun () -> return "data")
    server;
  add_simple_prompt ~name:"prompt1"
    ~render:(fun _ -> return (`String "rendered"))
    server;

  let description = describe_server server in
  (match description with
  | `Assoc fields ->
    let tool_count =
      List.Assoc.find_exn fields ~equal:String.equal "tool_count"
    in
    let resource_count =
      List.Assoc.find_exn fields ~equal:String.equal "resource_count"
    in
    let prompt_count =
      List.Assoc.find_exn fields ~equal:String.equal "prompt_count"
    in
    print_endline (Yojson.Safe.to_string tool_count);
    print_endline (Yojson.Safe.to_string resource_count);
    print_endline (Yojson.Safe.to_string prompt_count)
  | _ -> print_endline "Unexpected format");
  [%expect {|
    1
    1
    1 |}];
  return ()

(** {1 Component Search Tests} *)

let%expect_test "find_tools_by_tag" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let tool1 =
    Ox_fast_mcp_server.Server.Tool.create ~name:"tool1"
      ~tags:(String.Set.of_list [ "api"; "v1" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  let tool2 =
    Ox_fast_mcp_server.Server.Tool.create ~name:"tool2"
      ~tags:(String.Set.of_list [ "api"; "v2" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  let tool3 =
    Ox_fast_mcp_server.Server.Tool.create ~name:"tool3"
      ~tags:(String.Set.of_list [ "internal" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  add_tool server tool1;
  add_tool server tool2;
  add_tool server tool3;

  let api_tools = find_tools_by_tag server ~tag:"api" in
  let v1_tools = find_tools_by_tag server ~tag:"v1" in
  let internal_tools = find_tools_by_tag server ~tag:"internal" in

  print_s [%sexp (List.length api_tools : int)];
  print_s [%sexp (List.length v1_tools : int)];
  print_s [%sexp (List.length internal_tools : int)];
  [%expect {|
    2
    1
    1 |}];
  return ()

let%expect_test "find_resources_by_scheme" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let file_res =
    Ox_fast_mcp_server.Server.Resource.create ~uri:"file:///path/to/file"
      ~name:"file"
      ~reader:(fun () -> return "data")
      ()
  in
  let http_res =
    Ox_fast_mcp_server.Server.Resource.create ~uri:"http://example.com/api"
      ~name:"http"
      ~reader:(fun () -> return "data")
      ()
  in
  let https_res =
    Ox_fast_mcp_server.Server.Resource.create ~uri:"https://secure.com/data"
      ~name:"https"
      ~reader:(fun () -> return "data")
      ()
  in
  add_resource server file_res;
  add_resource server http_res;
  add_resource server https_res;

  let file_resources = find_resources_by_scheme server ~scheme:"file" in
  let http_resources = find_resources_by_scheme server ~scheme:"http" in
  let https_resources = find_resources_by_scheme server ~scheme:"https" in

  print_s [%sexp (List.length file_resources : int)];
  print_s [%sexp (List.length http_resources : int)];
  print_s [%sexp (List.length https_resources : int)];
  [%expect {|
    1
    1
    1 |}];
  return ()

let%expect_test "find_prompts_by_tag" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let prompt1 =
    Ox_fast_mcp_server.Server.Prompt.create ~name:"prompt1"
      ~tags:(String.Set.of_list [ "creative" ])
      ~render:(fun _ -> return `Null)
      ()
  in
  let prompt2 =
    Ox_fast_mcp_server.Server.Prompt.create ~name:"prompt2"
      ~tags:(String.Set.of_list [ "creative"; "long" ])
      ~render:(fun _ -> return `Null)
      ()
  in
  let prompt3 =
    Ox_fast_mcp_server.Server.Prompt.create ~name:"prompt3"
      ~tags:(String.Set.of_list [ "technical" ])
      ~render:(fun _ -> return `Null)
      ()
  in
  add_prompt server prompt1;
  add_prompt server prompt2;
  add_prompt server prompt3;

  let creative_prompts = find_prompts_by_tag server ~tag:"creative" in
  let long_prompts = find_prompts_by_tag server ~tag:"long" in

  print_s [%sexp (List.length creative_prompts : int)];
  print_s [%sexp (List.length long_prompts : int)];
  [%expect {|
    2
    1 |}];
  return ()

(** {1 Similar Name Suggestion Tests} *)

let%expect_test "suggest_similar_names_exact_match" =
  let available = [ "user_profile"; "user_settings"; "admin_panel" ] in
  let suggestions =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.suggest_similar_names "user_pro"
      available
  in
  List.iter suggestions ~f:print_endline;
  [%expect {| |}];
  return ()

let%expect_test "suggest_similar_names_typo" =
  let available = [ "calculate"; "calibrate"; "validate"; "navigate" ] in
  let suggestions =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.suggest_similar_names "calulate"
      available
  in
  List.iter suggestions ~f:print_endline;
  [%expect {|
    calculate
    calibrate
    validate
    |}];
  return ()

let%expect_test "suggest_similar_names_no_matches" =
  let available = [ "apple"; "banana"; "cherry" ] in
  let suggestions =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.suggest_similar_names "zebra"
      available
  in
  print_s [%sexp (List.length suggestions : int)];
  [%expect {| 0 |}];
  return ()

let%expect_test "suggest_similar_names_multiple_close" =
  let available = [ "tool1"; "tool2"; "tool3"; "cool1"; "pool1" ] in
  let suggestions =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.suggest_similar_names "tol1" available
  in
  List.iter suggestions ~f:print_endline;
  [%expect {|
    tool1
    tool2
    tool3
    cool1
    pool1
    |}];
  return ()

(** {1 Server Validation Tests} *)

let%expect_test "validate_server_valid_config" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"valid-server" () in
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return `Null) server;
  add_simple_resource ~uri:"file:///valid" ~name:"res"
    ~reader:(fun () -> return "ok")
    server;

  let result = validate_server server in
  (match result with
  | Ok () -> print_endline "Server is valid"
  | Error errors ->
    print_endline "Validation errors:";
    List.iter errors ~f:print_endline);
  [%expect {| Server is valid |}];
  return ()

let%expect_test "validate_server_invalid_uri" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  (* Manually create resource with invalid URI to test validation *)
  let invalid_res =
    Ox_fast_mcp_server.Server.Resource.create ~uri:"no_scheme" ~name:"bad"
      ~reader:(fun () -> return "data")
      ()
  in
  add_resource server invalid_res;

  let result = validate_server server in
  (match result with
  | Ok () -> print_endline "Unexpected: Server validated"
  | Error errors ->
    print_endline "Validation found errors:";
    List.iter errors ~f:print_endline);
  [%expect
    {|
    Validation found errors:
    Invalid resource URI: Resource URI must contain a scheme (e.g., 'file://', 'http://') |}];
  return ()

(** {1 Integration Tests} *)

let%expect_test "search_then_validate" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"integration-test" () in

  (* Add tagged components *)
  add_simple_tool ~name:"api_tool" ~handler:(fun _ -> return `Null) server;
  let tool_with_tag =
    Ox_fast_mcp_server.Server.Tool.create ~name:"tagged_tool"
      ~tags:(String.Set.of_list [ "production" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  add_tool server tool_with_tag;

  (* Search by tag *)
  let prod_tools = find_tools_by_tag server ~tag:"production" in
  printf "%s: " "Found production tools";
  print_s [%sexp (List.length prod_tools : int)];

  (* Validate server *)
  let validation = validate_server server in
  printf "%s: " "Server valid";
  print_s [%sexp (Result.is_ok validation : bool)];

  (* Describe server *)
  let description = describe_server server in
  (match description with
  | `Assoc fields ->
    let count = List.Assoc.find_exn fields ~equal:String.equal "tool_count" in
    printf "Total tools: %s\n" (Yojson.Safe.to_string count)
  | _ -> ());

  [%expect
    {|
    Found production tools: 1
    Server valid: true
    Total tools: 2 |}];
  return ()

let%expect_test "suggest_on_missing_component" =
  let open Ox_fast_mcp_server.Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"calculate_sum" ~handler:(fun _ -> return `Null) server;
  add_simple_tool ~name:"calculate_product"
    ~handler:(fun _ -> return `Null)
    server;
  add_simple_tool ~name:"format_data" ~handler:(fun _ -> return `Null) server;

  (* Simulate looking for a typo *)
  let available_tools = get_tools server |> Hashtbl.keys |> List.of_list in
  let typo_name = "caculate_sum" in
  let suggestions = suggest_similar_names typo_name available_tools in

  printf "Looking for: %s\n" typo_name;
  printf "Suggestions:\n";
  List.iter suggestions ~f:(fun s -> printf "  - %s\n" s);

  [%expect
    {|
    Looking for: caculate_sum
    Suggestions:
      - calculate_sum |}];
  return ()
