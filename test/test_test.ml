open OUnit2
open Core
open Tests

let test_temporary_settings _ =
  (* Test that settings are properly changed and restored *)
  assert_equal "INFO" global_settings.log_level;
  
  let test_fn () =
    assert_equal "DEBUG" global_settings.log_level;
    ()
  in
  
  with_temporary_settings [`Log_level "DEBUG"] test_fn;
  assert_equal "INFO" global_settings.log_level

let test_find_available_port _ =
  (* Test that we can get an available port *)
  let port = find_available_port () in
  assert_bool "Port should be positive" (port > 0);
  assert_bool "Port should be less than 65536" (port < 65536)

let test_with_server _ =
  (* Test server context manager *)
  let test_url = ref "" in
  with_server (fun url ->
    test_url := url;
    assert_bool "URL should start with http://" (String.is_prefix ~prefix:"http://" url);
    assert_bool "URL should contain localhost" (String.is_substring ~substring:"127.0.0.1" url);
    true
  ) |> ignore;
  assert_bool "URL should have been set" (!test_url <> "")

let suite =
  "test_suite" >::: [
    "test_temporary_settings" >:: test_temporary_settings;
    "test_find_available_port" >:: test_find_available_port;
    "test_with_server" >:: test_with_server;
  ]

let () = run_test_tt_main suite 