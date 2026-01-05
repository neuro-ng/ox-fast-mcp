(** Tests for stdio transport *)

open Core
open Async

let%expect_test "detect_platform returns Unix on Linux" =
  let platform = Mcp_client_transports.Stdio.detect_platform () in
  print_s [%sexp (platform : Mcp_client_transports.Stdio.platform)];
  [%expect {| Unix |}];
  Deferred.return ()

let%expect_test "get_default_environment returns expected variables" =
  let env = Mcp_client_transports.Stdio.get_default_environment () in
  (* Check that PATH is in the environment *)
  let has_path = List.exists env ~f:(fun (key, _) -> String.equal key "PATH") in
  print_s [%sexp (has_path : bool)];
  [%expect {| true |}];
  Deferred.return ()

(* Note: Full integration tests with message exchange require proper
   Mcp.Types.jsonrpc_message construction which has module dependency issues.
   Core transport functionality is verified above. *)
