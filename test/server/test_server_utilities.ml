(** Server Statistics and Utilities Tests

    Tests for statistics tracking and debug utilities. *)

open Core
open Async

(** {1 Component Listing Tests} *)

let%expect_test "list_all_component_names_empty" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let names = list_all_component_names server in
  print_endline (Yojson.Safe.to_string names);
  [%expect {| {"tools":[],"resources":[],"prompts":[],"templates":[]} |}];
  return ()

let%expect_test "list_all_component_names_with_components" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return `Null) server;
  add_simple_resource ~uri:"file:///test" ~name:"res1"
    ~reader:(fun () -> return "data")
    server;
  add_simple_prompt ~name:"prompt1" ~render:(fun _ -> return `Null) server;

  let names = list_all_component_names server in
  (match names with
  | `Assoc fields ->
    let tools = List.Assoc.find_exn fields ~equal:String.equal "tools" in
    let resources =
      List.Assoc.find_exn fields ~equal:String.equal "resources"
    in
    let prompts = List.Assoc.find_exn fields ~equal:String.equal "prompts" in
    printf "Tools: %s\n" (Yojson.Safe.to_string tools);
    printf "Resources: %s\n" (Yojson.Safe.to_string resources);
    printf "Prompts: %s\n" (Yojson.Safe.to_string prompts)
  | _ -> print_endline "Unexpected format");
  [%expect
    {|
    Tools: ["tool1"]
    Resources: ["res1"]
    Prompts: ["prompt1"]
    |}];
  return ()

(** {1 Component Count by Tag Tests} *)

let%expect_test "component_count_by_tag_no_tags" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return `Null) server;

  let counts = component_count_by_tag server in
  print_s [%sexp (List.length counts : int)];
  [%expect {| 0 |}];
  return ()

let%expect_test "component_count_by_tag_with_tags" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let tool1 =
    Server.Tool.create ~name:"tool1"
      ~tags:(String.Set.of_list [ "api"; "v1" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  let tool2 =
    Server.Tool.create ~name:"tool2"
      ~tags:(String.Set.of_list [ "api"; "v2" ])
      ~handler:(fun _ -> return `Null)
      ()
  in
  let resource1 =
    Server.Resource.create ~uri:"file:///test" ~name:"res1"
      ~tags:(String.Set.of_list [ "api" ])
      ~reader:(fun () -> return "data")
      ()
  in
  add_tool server tool1;
  add_tool server tool2;
  add_resource server resource1;

  let counts = component_count_by_tag server in
  List.iter counts ~f:(fun (tag, count) -> printf "%s: %d\n" tag count);
  [%expect {|
    api: 3
    v2: 1
    v1: 1
    |}];
  return ()

(** {1 Statistics Tests} *)

let%expect_test "get_tool_stats_initial" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let stats = get_tool_stats server in
  print_s [%sexp (List.length stats : int)];
  [%expect {| 0 |}];
  return ()

let%expect_test "reset_stats" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  reset_stats server;
  let tool_stats = get_tool_stats server in
  let resource_stats = get_resource_stats server in
  printf "Tool stats: %d\n" (List.length tool_stats);
  printf "Resource stats: %d\n" (List.length resource_stats);
  [%expect {|
    Tool stats: 0
    Resource stats: 0 |}];
  return ()

(** {1 Health Check Tests} *)

let%expect_test "health_check_healthy_server" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"healthy-server" () in
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return `Null) server;

  let result = health_check server in
  (match result with
  | Ok health -> (
    match health with
    | `Assoc fields ->
      let status = List.Assoc.find_exn fields ~equal:String.equal "status" in
      printf "Status: %s\n" (Yojson.Safe.to_string status);
      let tool_count =
        List.Assoc.find_exn fields ~equal:String.equal "tool_count"
      in
      printf "Tool count: %s\n" (Yojson.Safe.to_string tool_count)
    | _ -> print_endline "Unexpected format")
  | Error err -> printf "Error: %s\n" err);
  [%expect {|
    Status: "healthy"
    Tool count: 1 |}];
  return ()

let%expect_test "health_check_degraded_server" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  (* Add invalid resource to cause degraded state *)
  let invalid_res =
    Server.Resource.create ~uri:"no_scheme" ~name:"bad"
      ~reader:(fun () -> return "data")
      ()
  in
  add_resource server invalid_res;

  let result = health_check server in
  (match result with
  | Ok health -> (
    match health with
    | `Assoc fields ->
      let status = List.Assoc.find_exn fields ~equal:String.equal "status" in
      printf "Status: %s\n" (Yojson.Safe.to_string status)
    | _ -> print_endline "Unexpected format")
  | Error err -> printf "Error: %s\n" err);
  [%expect {| Status: "degraded" |}];
  return ()
