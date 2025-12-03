(** Tests for Resource Manager
    
    Basic tests for resource manager functionality.
    Uses expect_test framework with Async support.
*)

open! Core
open! Async

(** {1 Test Helpers} *)

let create_test_resource name uri =
  Resources.Resource_types.create_text_resource
    ~uri
    ~name
    ~content:"Test content"
    ~description:"Test resource"
    ~tags:["test"]
    ()

(** {1 Manager Creation Tests} *)

let%expect_test "create resource manager" =
  let manager = Resources.Resource_manager.create () in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

let%expect_test "create manager with options" =
  let manager = Resources.Resource_manager.create
    ~duplicate_behavior:Resources.Resource_manager.DuplicateBehavior.Replace
    ~mask_error_details:true
    () in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Resource Addition Tests} *)

let%expect_test "add resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 1 |}];
  return ()

let%expect_test "add multiple resources" =
  let manager = Resources.Resource_manager.create () in
  let resource1 = create_test_resource "resource1" "file:///test1" in
  let resource2 = create_test_resource "resource2" "file:///test2" in
  let%bind () = Resources.Resource_manager.add manager resource1 in
  let%bind () = Resources.Resource_manager.add manager resource2 in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 2 |}];
  return ()

(** {1 Resource Retrieval Tests} *)

let%expect_test "get resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  let retrieved = Resources.Resource_manager.get manager "test_resource" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  [%expect {| true |}];
  return ()

let%expect_test "get nonexistent resource" =
  let manager = Resources.Resource_manager.create () in
  let retrieved = Resources.Resource_manager.get manager "nonexistent" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  [%expect {| false |}];
  return ()

let%expect_test "has_resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  print_s [%sexp (Resources.Resource_manager.has_resource manager "test_resource" : bool)];
  [%expect {| true |}];
  print_s [%sexp (Resources.Resource_manager.has_resource manager "nonexistent" : bool)];
  [%expect {| false |}];
  return ()

(** {1 Resource Listing Tests} *)

let%expect_test "list resources" =
  let manager = Resources.Resource_manager.create () in
  let resource1 = create_test_resource "resource1" "file:///test1" in
  let resource2 = create_test_resource "resource2" "file:///test2" in
  let%bind () = Resources.Resource_manager.add manager resource1 in
  let%bind () = Resources.Resource_manager.add manager resource2 in
  let resources = Resources.Resource_manager.list manager in
  print_s [%sexp (List.length resources : int)];
  [%expect {| 2 |}];
  return ()

let%expect_test "list enabled resources" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  let enabled = Resources.Resource_manager.list_enabled manager in
  print_s [%sexp (List.length enabled : int)];
  [%expect {| 1 |}];
  return ()

(** {1 Resource Enable/Disable Tests} *)

let%expect_test "enable/disable resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  let disabled = Resources.Resource_manager.disable manager "test_resource" in
  print_s [%sexp (disabled : bool)];
  [%expect {| true |}];
  let is_enabled = Resources.Resource_manager.is_enabled manager "test_resource" in
  print_s [%sexp (is_enabled : bool)];
  [%expect {| false |}];
  let enabled = Resources.Resource_manager.enable manager "test_resource" in
  print_s [%sexp (enabled : bool)];
  [%expect {| true |}];
  return ()

(** {1 Resource Removal Tests} *)

let%expect_test "remove resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 1 |}];
  let%bind () = Resources.Resource_manager.remove manager "test_resource" in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Clear Tests} *)

let%expect_test "clear all resources" =
  let manager = Resources.Resource_manager.create () in
  let resource1 = create_test_resource "resource1" "file:///test1" in
  let resource2 = create_test_resource "resource2" "file:///test2" in
  let%bind () = Resources.Resource_manager.add manager resource1 in
  let%bind () = Resources.Resource_manager.add manager resource2 in
  let%bind () = Resources.Resource_manager.clear manager in
  print_s [%sexp (Resources.Resource_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Resource Reading Tests} *)

let%expect_test "read text resource" =
  let manager = Resources.Resource_manager.create () in
  let resource = create_test_resource "test_resource" "file:///test" in
  let%bind () = Resources.Resource_manager.add manager resource in
  let%bind result = Resources.Resource_manager.read_resource manager "test_resource" in
  (match result with
  | Ok (Resources.Resource_types.Text _content) ->
    print_endline "Text content read successfully";
    [%expect {| Text content read successfully |}]
  | Ok (Resources.Resource_types.Binary _) ->
    print_endline "Binary content";
    [%expect {| |}]
  | Error _error ->
    print_endline "Error";
    [%expect {| |}]);
  return ()

let%expect_test "read nonexistent resource" =
  let manager = Resources.Resource_manager.create () in
  let%bind result = Resources.Resource_manager.read_resource manager "nonexistent" in
  (match result with
  | Ok _ ->
    print_endline "Should not succeed";
    [%expect {| |}]
  | Error _error ->
    print_endline "Error as expected";
    [%expect {| Error as expected |}]);
  return ()

