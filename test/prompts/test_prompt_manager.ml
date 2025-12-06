(** Tests for Prompt Manager

    Basic tests for prompt manager functionality. Uses expect_test framework
    with Async support. *)

open! Core
open! Async

(** {1 Test Helpers} *)

let create_test_handler name _args =
  return
    (Ok
       [
         {
           Mcp.Types.role = `User;
           content =
             `Text
               {
                 Mcp.Types.type_ = `Text;
                 text = Printf.sprintf "Test prompt: %s" name;
                 annotations = None;
                 meta = None;
               };
         };
       ])

let create_test_prompt name description =
  Prompts.Prompt_types.create_function_prompt ~name ~description
    ~tags:[ "test" ] (create_test_handler name)

(** {1 Manager Creation Tests} *)

let%expect_test "create prompt manager" =
  let manager = Prompts.Prompt_manager.create () in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

let%expect_test "create manager with options" =
  let manager =
    Prompts.Prompt_manager.create
      ~duplicate_behavior:Prompts.Prompt_manager.DuplicateBehavior.Replace
      ~mask_error_details:true ()
  in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Prompt Addition Tests} *)

let%expect_test "add prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 1 |}];
  return ()

let%expect_test "add multiple prompts" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt1 = create_test_prompt "prompt1" "First prompt" in
  let prompt2 = create_test_prompt "prompt2" "Second prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt1 in
  let%bind () = Prompts.Prompt_manager.add manager prompt2 in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 2 |}];
  return ()

(** {1 Prompt Retrieval Tests} *)

let%expect_test "get prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  let retrieved = Prompts.Prompt_manager.get manager "test_prompt" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  [%expect {| true |}];
  return ()

let%expect_test "get nonexistent prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let retrieved = Prompts.Prompt_manager.get manager "nonexistent" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  [%expect {| false |}];
  return ()

let%expect_test "has_prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  print_s
    [%sexp (Prompts.Prompt_manager.has_prompt manager "test_prompt" : bool)];
  [%expect {| true |}];
  print_s
    [%sexp (Prompts.Prompt_manager.has_prompt manager "nonexistent" : bool)];
  [%expect {| false |}];
  return ()

(** {1 Prompt Listing Tests} *)

let%expect_test "list prompts" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt1 = create_test_prompt "prompt1" "First prompt" in
  let prompt2 = create_test_prompt "prompt2" "Second prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt1 in
  let%bind () = Prompts.Prompt_manager.add manager prompt2 in
  let prompts = Prompts.Prompt_manager.list manager in
  print_s [%sexp (List.length prompts : int)];
  [%expect {| 2 |}];
  return ()

let%expect_test "list enabled prompts" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  let enabled = Prompts.Prompt_manager.list_enabled manager in
  print_s [%sexp (List.length enabled : int)];
  [%expect {| 1 |}];
  return ()

(** {1 Prompt Enable/Disable Tests} *)

let%expect_test "enable/disable prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  let disabled = Prompts.Prompt_manager.disable manager "test_prompt" in
  print_s [%sexp (disabled : bool)];
  [%expect {| true |}];
  let is_enabled = Prompts.Prompt_manager.is_enabled manager "test_prompt" in
  print_s [%sexp (is_enabled : bool)];
  [%expect {| false |}];
  let enabled = Prompts.Prompt_manager.enable manager "test_prompt" in
  print_s [%sexp (enabled : bool)];
  [%expect {| true |}];
  return ()

(** {1 Prompt Removal Tests} *)

let%expect_test "remove prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 1 |}];
  let%bind () = Prompts.Prompt_manager.remove manager "test_prompt" in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Clear Tests} *)

let%expect_test "clear all prompts" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt1 = create_test_prompt "prompt1" "First prompt" in
  let prompt2 = create_test_prompt "prompt2" "Second prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt1 in
  let%bind () = Prompts.Prompt_manager.add manager prompt2 in
  let%bind () = Prompts.Prompt_manager.clear manager in
  print_s [%sexp (Prompts.Prompt_manager.count manager : int)];
  [%expect {| 0 |}];
  return ()

(** {1 Prompt Rendering Tests} *)

let%expect_test "render prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let prompt = create_test_prompt "test_prompt" "Test prompt" in
  let%bind () = Prompts.Prompt_manager.add manager prompt in
  let%bind result =
    Prompts.Prompt_manager.render_prompt manager "test_prompt" ~arguments:None
  in
  (match result with
  | Ok messages ->
    print_s [%sexp (List.length messages : int)];
    [%expect {| 1 |}]
  | Error error ->
    print_endline error.Ox_fast_mcp.Exceptions.message;
    [%expect {| |}]);
  return ()

let%expect_test "render nonexistent prompt" =
  let manager = Prompts.Prompt_manager.create () in
  let%bind result =
    Prompts.Prompt_manager.render_prompt manager "nonexistent" ~arguments:None
  in
  (match result with
  | Ok _ ->
    print_endline "Should not succeed";
    [%expect {| |}]
  | Error _error ->
    print_endline "Error as expected";
    [%expect {| Error as expected |}]);
  return ()
