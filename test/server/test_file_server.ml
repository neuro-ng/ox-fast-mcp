open Async

(* Test file server functionality *)

(* Helper to create a temporary test directory with files *)
let create_test_dir () =
  let%bind temp_dir = Unix.mkdtemp "/tmp/test_files_XXXXXX" in
  let%bind () =
    Writer.save
      (Filename.concat temp_dir "example.py")
      ~contents:"print('hello world')"
  in
  let%bind () =
    Writer.save
      (Filename.concat temp_dir "readme.md")
      ~contents:"# Test Directory\nThis is a test."
  in
  let%bind () =
    Writer.save
      (Filename.concat temp_dir "config.json")
      ~contents:"{\"test\": true}"
  in
  return temp_dir

(* Helper to cleanup test directory *)
let cleanup_test_dir dir =
  let%bind files = Sys.readdir dir in
  let%bind () =
    Deferred.List.iter ~how:`Sequential (Array.to_list files) ~f:(fun file ->
        Unix.unlink (Filename.concat dir file))
  in
  Unix.rmdir dir

let%expect_test "list_resources" =
  (* Test listing resources *)
  let%bind test_dir = create_test_dir () in
  let%bind () = return () in
  print_endline "TODO: Implement test_list_resources";
  [%expect {|
    TODO: Implement test_list_resources
  |}];
  let%bind () = cleanup_test_dir test_dir in
  return ()

let%expect_test "read_resource_dir" =
  (* Test reading a directory resource *)
  let%bind test_dir = create_test_dir () in
  let%bind () = return () in
  print_endline "TODO: Implement test_read_resource_dir";
  [%expect {|
    TODO: Implement test_read_resource_dir
  |}];
  let%bind () = cleanup_test_dir test_dir in
  return ()

let%expect_test "read_resource_file" =
  (* Test reading a file resource *)
  let%bind test_dir = create_test_dir () in
  let%bind () = return () in
  print_endline "TODO: Implement test_read_resource_file";
  [%expect {|
    TODO: Implement test_read_resource_file
  |}];
  let%bind () = cleanup_test_dir test_dir in
  return ()

let%expect_test "delete_file" =
  (* Test deleting a file via tool *)
  let%bind test_dir = create_test_dir () in
  let%bind () = return () in
  print_endline "TODO: Implement test_delete_file";
  [%expect {|
    TODO: Implement test_delete_file
  |}];
  let%bind () = cleanup_test_dir test_dir in
  return ()

let%expect_test "delete_file_and_check_resources" =
  (* Test that resource reports file not found after deletion *)
  let%bind test_dir = create_test_dir () in
  let%bind () = return () in
  print_endline "TODO: Implement test_delete_file_and_check_resources";
  [%expect {|
    TODO: Implement test_delete_file_and_check_resources
  |}];
  let%bind () = cleanup_test_dir test_dir in
  return ()
