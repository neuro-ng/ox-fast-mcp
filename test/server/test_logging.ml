(** Logging configuration tests for OxFastMCP

    Tests for uvicorn logging configuration in HTTP transport. *)

open Async

let%expect_test "uvicorn_logging_default_level" =
  (* Tests that OxFastMCP passes log_level to HTTP server if no log_config is
     given *)
  print_endline "TODO: Implement test_uvicorn_logging_default_level";
  [%expect {| TODO: Implement test_uvicorn_logging_default_level |}];
  return ()

let%expect_test "uvicorn_logging_with_custom_log_config" =
  (* Tests that OxFastMCP passes log_config to HTTP server and not log_level *)
  print_endline "TODO: Implement test_uvicorn_logging_with_custom_log_config";
  [%expect {| TODO: Implement test_uvicorn_logging_with_custom_log_config |}];
  return ()

let%expect_test "uvicorn_logging_custom_log_config_overrides_log_level_param" =
  (* Tests log_config precedence if log_level is also passed to
     run_http_async *)
  print_endline
    "TODO: Implement \
     test_uvicorn_logging_custom_log_config_overrides_log_level_param";
  [%expect
    {| TODO: Implement test_uvicorn_logging_custom_log_config_overrides_log_level_param |}];
  return ()
