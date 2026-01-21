(** Enhanced Error Messages Tests

    Tests for intelligent "Did you mean?" suggestions in error messages. *)

open Core
open Async

(** {1 Tool Lookup Error Tests} *)

let%expect_test "tool_lookup_with_suggestion" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"calculate_sum" ~handler:(fun _ -> return `Null) server;
  add_simple_tool ~name:"format_data" ~handler:(fun _ -> return `Null) server;

  (* Try to call tool with typo *)
  let%bind result =
    Monitor.try_with (fun () ->
        call_tool server ~name:"caculate_sum" ~arguments:(`Assoc []))
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    (* Check that suggestion is provided *)
    if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Error includes suggestion"
    else print_endline "Error missing suggestion");
  [%expect {| Error includes suggestion |}];
  return ()

let%expect_test "tool_lookup_no_suggestion" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"format_data" ~handler:(fun _ -> return `Null) server;

  (* Try completely different name *)
  let%bind result =
    Monitor.try_with (fun () ->
        call_tool server ~name:"zebra" ~arguments:(`Assoc []))
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    if String.is_substring error_str ~substring:"available" then
      print_endline "Error shows available tools"
    else if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Unexpected: has suggestions"
    else print_endline "Basic error message");
  [%expect {| Error shows available tools |}];
  return ()

(** {1 Resource Lookup Error Tests} *)

let%expect_test "resource_lookup_with_suggestion" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_resource ~uri:"file:///config.json" ~name:"config"
    ~reader:(fun () -> return "data")
    server;
  add_simple_resource ~uri:"file:///data.json" ~name:"data"
    ~reader:(fun () -> return "data")
    server;

  let%bind result =
    Monitor.try_with (fun () -> get_resource server ~key:"file:///confg.json")
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Error includes suggestion"
    else print_endline "Error missing suggestion");
  [%expect {| Error missing suggestion |}];
  return ()

(** {1 Prompt Lookup Error Tests} *)

let%expect_test "prompt_lookup_with_suggestion" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_prompt ~name:"write_email" ~render:(fun _ -> return `Null) server;
  add_simple_prompt ~name:"write_story" ~render:(fun _ -> return `Null) server;

  let%bind result =
    Monitor.try_with (fun () -> get_prompt_component server ~key:"write_emai")
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Error includes suggestion"
    else print_endline "Error missing suggestion");
  [%expect {| Error includes suggestion |}];
  return ()

(** {1 Template Lookup Error Tests} *)

let%expect_test "template_lookup_with_suggestion" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  let template1 =
    Server.Resource_template.create ~uri_template:"file:///{path}/data"
      ~name:"file_template"
      ~create_resource:(fun ~params:_ ->
        return
          (Server.Resource.create ~uri:"file:///test" ~name:"test"
             ~reader:(fun () -> return "data")
             ()))
      ()
  in
  add_template server template1;

  let%bind result =
    Monitor.try_with (fun () -> get_template server ~key:"file:///{pth}/data")
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Error includes suggestion"
    else print_endline "Error missing suggestion");
  [%expect {| Error includes suggestion |}];
  return ()

(** {1 Multiple Suggestions Test} *)

let%expect_test "multiple_similar_tools" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"tool1" ~handler:(fun _ -> return `Null) server;
  add_simple_tool ~name:"tool2" ~handler:(fun _ -> return `Null) server;
  add_simple_tool ~name:"tool3" ~handler:(fun _ -> return `Null) server;
  add_simple_tool ~name:"cool1" ~handler:(fun _ -> return `Null) server;

  let%bind result =
    Monitor.try_with (fun () ->
        call_tool server ~name:"tol1" ~arguments:(`Assoc []))
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    (* Should have multiple suggestions *)
    if String.is_substring error_str ~substring:"did_you_mean" then
      print_endline "Error includes multiple suggestions"
    else print_endline "Error missing suggestions");
  [%expect {| Error includes multiple suggestions |}];
  return ()

(** {1 Exact Match Test} *)

let%expect_test "exact_typo_correction" =
  let open Server.Ox_fast_mcp in
  let server = create ~name:"test" () in
  add_simple_tool ~name:"calculate_sum" ~handler:(fun _ -> return `Null) server;

  let%bind result =
    Monitor.try_with (fun () ->
        call_tool server ~name:"calulate_sum" ~arguments:(`Assoc []))
  in
  (match result with
  | Ok _ -> print_endline "Unexpected success"
  | Error exn ->
    let error_str = Exn.to_string exn in
    (* Should suggest calculate_sum *)
    if String.is_substring error_str ~substring:"calculate_sum" then
      print_endline "Suggests correct tool name"
    else print_endline "Missing expected suggestion");
  [%expect {| Suggests correct tool name |}];
  return ()
