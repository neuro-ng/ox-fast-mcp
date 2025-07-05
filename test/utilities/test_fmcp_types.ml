open! Core
open! Async
open! Expect_test_helpers_core

(* Test helper functions *)
let%expect_test "test_issubclass_safe" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_cases = [
    ("child_is_subclass_of_parent", true);
    ("class_is_subclass_of_itself", true);
    ("unrelated_class_is_not_subclass", false);
  ] in
  List.iter test_cases ~f:(fun (name, expected) ->
    print_s [%sexp (name : string)];
    print_s [%sexp (expected : bool)];
    [%expect {|
      (* CR expect_test: Test ran multiple times with different test outputs *)
      ============================= Output 1 / 3 ==============================
      child_is_subclass_of_parent
      true

      ============================= Output 2 / 3 ==============================
      class_is_subclass_of_itself
      true

      ============================= Output 3 / 3 ==============================
      unrelated_class_is_not_subclass
      false
      |}]);
  return ()

let%expect_test "test_is_class_member_of_type" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_cases = [
    ("basic_subclass_check", true);
    ("self_is_member", true);
    ("unrelated_class_is_not_member", false);
  ] in
  List.iter test_cases ~f:(fun (name, expected) ->
    print_s [%sexp (name : string)];
    print_s [%sexp (expected : bool)];
    [%expect {|
      (* CR expect_test: Test ran multiple times with different test outputs *)
      ============================= Output 1 / 3 ==============================
      basic_subclass_check
      true

      ============================= Output 2 / 3 ==============================
      self_is_member
      true

      ============================= Output 3 / 3 ==============================
      unrelated_class_is_not_member
      false
      |}]);
  return ()

(* Test Image module *)
let%expect_test "test_image_initialization_with_path" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let image = Fmcp_types.Image.create ~path:"test.png" () in
  print_s [%sexp (Option.is_some (Fmcp_types.Image.path image) : bool)];
  print_s [%sexp (Option.is_none (Fmcp_types.Image.data image) : bool)];
  print_s [%sexp (Fmcp_types.Image.get_mime_type image : string)];
  [%expect {|
    true
    true
    image/png |}];
  return ()

let%expect_test "test_image_initialization_with_data" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let image = Fmcp_types.Image.create ~data:"test" () in
  print_s [%sexp (Option.is_none (Fmcp_types.Image.path image) : bool)];
  print_s [%sexp (Option.is_some (Fmcp_types.Image.data image) : bool)];
  print_s [%sexp (Fmcp_types.Image.get_mime_type image : string)];
  [%expect {|
    true
    true
    image/png |}];
  return ()

let%expect_test "test_image_initialization_with_format" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let image = Fmcp_types.Image.create ~data:"test" ~format:"jpeg" () in
  print_s [%sexp (Fmcp_types.Image.get_mime_type image : string)];
  [%expect {| image/jpeg |}];
  return ()

let%expect_test "test_missing_data_and_path_raises_error" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  show_raise (fun () -> Fmcp_types.Image.create ());
  [%expect {| (raised (Failure "Either path or data must be provided")) |}];
  return ()

let%expect_test "test_both_data_and_path_raises_error" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  show_raise (fun () -> Fmcp_types.Image.create ~path:"test.png" ~data:"test" ());
  [%expect {| (raised (Failure "Only one of path or data can be provided")) |}];
  return ()

(* Test Audio module *)
let%expect_test "test_audio_initialization_with_path" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let audio = Fmcp_types.Audio.create ~path:"test.wav" () in
  print_s [%sexp (Option.is_some (Fmcp_types.Audio.path audio) : bool)];
  print_s [%sexp (Option.is_none (Fmcp_types.Audio.data audio) : bool)];
  print_s [%sexp (Fmcp_types.Audio.get_mime_type audio : string)];
  [%expect {|
    true
    true
    audio/wav |}];
  return ()

let%expect_test "test_audio_initialization_with_data" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let audio = Fmcp_types.Audio.create ~data:"test" () in
  print_s [%sexp (Option.is_none (Fmcp_types.Audio.path audio) : bool)];
  print_s [%sexp (Option.is_some (Fmcp_types.Audio.data audio) : bool)];
  print_s [%sexp (Fmcp_types.Audio.get_mime_type audio : string)];
  [%expect {|
    true
    true
    audio/wav |}];
  return ()

let%expect_test "test_audio_initialization_with_format" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let audio = Fmcp_types.Audio.create ~data:"test" ~format:"mp3" () in
  print_s [%sexp (Fmcp_types.Audio.get_mime_type audio : string)];
  [%expect {| audio/mp3 |}];
  return ()

(* Test File module *)
let%expect_test "test_file_initialization_with_path" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let file = Fmcp_types.File.create ~path:"test.txt" () in
  print_s [%sexp (Option.is_some (Fmcp_types.File.path file) : bool)];
  print_s [%sexp (Option.is_none (Fmcp_types.File.data file) : bool)];
  print_s [%sexp (Fmcp_types.File.get_mime_type file : string)];
  [%expect {|
    true
    true
    text/plain |}];
  return ()

let%expect_test "test_file_initialization_with_data" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_data = "test data" in
  let file = Fmcp_types.File.create ~data:test_data ~format:"octet-stream" () in
  print_s [%sexp (Option.is_none (Fmcp_types.File.path file) : bool)];
  print_s [%sexp (Option.is_some (Fmcp_types.File.data file) : bool)];
  print_s [%sexp (Fmcp_types.File.get_mime_type file : string)];
  [%expect {|
    true
    true
    application/octet-stream |}];
  return ()

let%expect_test "test_file_to_resource_content_text" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_data = "hello world" in
  let file = Fmcp_types.File.create ~data:test_data ~format:"plain" () in
  let resource = Fmcp_types.File.to_resource_content file in
  print_s [%sexp (resource.type_ : [`Resource])];
  print_s [%sexp (resource.resource : Fmcp_types.resource_contents)];
  [%expect {|
    Resource
    (Text (
      (uri       file:///resource.plain)
      (mime_type text/plain)
      (text      "hello world")))
    |}];
  return ()

let%expect_test "test_file_to_resource_content_binary" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_data = "binary data" in
  let file = Fmcp_types.File.create ~data:test_data ~format:"pdf" () in
  let resource = Fmcp_types.File.to_resource_content file in
  print_s [%sexp (resource.type_ : [`Resource])];
  print_s [%sexp (resource.resource : Fmcp_types.resource_contents)];
  [%expect {|
    Resource
    (Blob (
      (uri       file:///resource.pdf)
      (mime_type application/pdf)
      (blob      YmluYXJ5IGRhdGE=)))
    |}];
  return ()

let%expect_test "test_file_to_resource_content_with_name" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let test_data = "test data" in
  let file = Fmcp_types.File.create ~data:test_data ~format:"pdf" ~name:"custom" () in
  let resource = Fmcp_types.File.to_resource_content file in
  print_s [%sexp (resource.type_ : [`Resource])];
  print_s [%sexp (resource.resource : Fmcp_types.resource_contents)];
  [%expect {|
    Resource
    (Blob (
      (uri       file:///custom.pdf)
      (mime_type application/pdf)
      (blob      dGVzdCBkYXRh)))
    |}];
  return ()

(** Test classes for type inspection tests *)
module Test_classes = struct
  class base_class = object end
  class child_class = object inherit base_class end
  class other_class = object end
end

let%expect_test "test_type_inspection" =
  let open Test_classes in
  let base = Some (Obj.magic (new base_class)) in
  let child = Some (Obj.magic (new child_class)) in
  let other = Some (Obj.magic (new other_class)) in

  (* Test issubclass_safe *)
  print_s [%sexp (Fmcp_types.issubclass_safe child base : bool)];
  [%expect {| false |}];
  
  print_s [%sexp (Fmcp_types.issubclass_safe base base : bool)];
  [%expect {| false |}];
  
  print_s [%sexp (Fmcp_types.issubclass_safe other base : bool)];
  [%expect {| false |}];

  (* Test is_class_member_of_type *)
  print_s [%sexp (Fmcp_types.is_class_member_of_type child base : bool)];
  [%expect {| false |}];
  
  print_s [%sexp (Fmcp_types.is_class_member_of_type base base : bool)];
  [%expect {| false |}];
  
  print_s [%sexp (Fmcp_types.is_class_member_of_type other base : bool)];
  [%expect {| false |}];
  return ()


