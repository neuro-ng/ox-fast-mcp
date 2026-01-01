(** Tests for session notification functions in Context module *)

open! Core
open! Async
open! Expect_test_helpers_core

(* Access Context using internal module name *)
module Context = Server__Context
open Context

let create_test_context ?session () = create ?session ()

let%expect_test "send_resources_list_changed with no session" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind () = send_resources_list_changed ctx in
    (* Should complete without error *)
    print_endline "send_resources_list_changed completed with no session";
    [%expect {| send_resources_list_changed completed with no session |}];
    return ()
  in
  return ()

let%expect_test "send_tools_list_changed with no session" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind () = send_tools_list_changed ctx in
    (* Should complete without error *)
    print_endline "send_tools_list_changed completed with no session";
    [%expect {| send_tools_list_changed completed with no session |}];
    return ()
  in
  return ()

let%expect_test "send_prompts_list_changed with no session" =
  let%bind () =
    let ctx = create_test_context () in
    let%bind () = send_prompts_list_changed ctx in
    (* Should complete without error *)
    print_endline "send_prompts_list_changed completed with no session";
    [%expect {| send_prompts_list_changed completed with no session |}];
    return ()
  in
  return ()

let%expect_test "change tracking flags work correctly" =
  let ctx = create_test_context () in
  (* Initially no changes *)
  print_s [%sexp (has_changes ctx : bool)];
  [%expect {| false |}];

  (* Queue a tool change *)
  queue_tool_list_changed ctx;
  print_s [%sexp (has_changes ctx : bool)];
  [%expect {| true |}];

  (* Queue resource change *)
  queue_resource_list_changed ctx;
  let changed_lists = get_changed_lists ctx in
  print_s [%sexp (changed_lists : string list)];
  [%expect {| (tools resources) |}];

  (* Reset changes *)
  reset_changes ctx;
  print_s [%sexp (has_changes ctx : bool)];
  [%expect {| false |}];
  return ()

let%expect_test "notification queue tracking" =
  let ctx = create_test_context () in

  (* Queue notifications *)
  queue_tool_list_changed ctx;
  queue_resource_list_changed ctx;
  queue_prompt_list_changed ctx;

  (* Check pending notifications *)
  let pending = get_pending_notifications ctx in
  print_s [%sexp (List.length pending : int)];
  [%expect {| 3 |}];

  (* Reset clears queue *)
  reset_changes ctx;
  let pending_after = get_pending_notifications ctx in
  print_s [%sexp (List.length pending_after : int)];
  [%expect {| 0 |}];
  return ()
