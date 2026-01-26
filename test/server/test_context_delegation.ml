(** Tests for Context delegation methods *)

open! Core
open! Async
open! Expect_test_helpers_core

(* Access Context using internal module name *)
module Context = Ox_fast_mcp_server__Context

let create_test_context () = Context.create ()

(** {1 Delegation Error Tests} *)

let%expect_test "list_resources with no server raises error" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () -> Context.list_resources ctx)
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot list resources: no server reference. Context must be created with server delegation.")
    |}];
  return ()

let%expect_test "list_prompts with no server raises error" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () -> Context.list_prompts ctx)
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot list prompts: no server reference. Context must be created with server delegation.")
    |}];
  return ()

let%expect_test "read_resource with no server raises error" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.read_resource ctx ~uri:"test://resource")
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot read resource: no server reference. Context must be created with server delegation.")
    |}];
  return ()

let%expect_test "get_prompt with no server raises error" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.get_prompt ctx ~name:"test" ~arguments:(`Assoc []))
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot get prompt: no server reference. Context must be created with server delegation.")
    |}];
  return ()

(** {1 Delegation Success Tests} *)

let%expect_test "delegation with callbacks works" =
  let%bind () =
    (* Create mock callbacks *)
    let list_resources_fn () =
      return [ `Assoc [ ("uri", `String "test://resource1") ] ]
    in
    let list_prompts_fn () =
      return [ `Assoc [ ("name", `String "prompt1") ] ]
    in
    let read_resource_fn ~uri = return (sprintf "Content of %s" uri) in
    let get_prompt_fn ~name ~arguments:_ =
      return (`Assoc [ ("name", `String name); ("result", `String "rendered") ])
    in

    (* Create context with callbacks *)
    let ctx =
      Context.create ~list_resources_fn:(Some list_resources_fn)
        ~list_prompts_fn:(Some list_prompts_fn)
        ~read_resource_fn:(Some read_resource_fn)
        ~get_prompt_fn:(Some get_prompt_fn) ()
    in

    (* Test list_resources *)
    let%bind resources = Context.list_resources ctx in
    print_endline (Yojson.Safe.to_string (`List resources));
    [%expect {| [{"uri":"test://resource1"}] |}];

    (* Test list_prompts *)
    let%bind prompts = Context.list_prompts ctx in
    print_endline (Yojson.Safe.to_string (`List prompts));
    [%expect {| [{"name":"prompt1"}] |}];

    (* Test read_resource *)
    let%bind content = Context.read_resource ctx ~uri:"test://myresource" in
    print_endline content;
    [%expect {| Content of test://myresource |}];

    (* Test get_prompt *)
    let%bind prompt_result =
      Context.get_prompt ctx ~name:"myprompt" ~arguments:(`Assoc [])
    in
    print_endline (Yojson.Safe.to_string prompt_result);
    [%expect {| {"name":"myprompt","result":"rendered"} |}];

    return ()
  in
  return ()
