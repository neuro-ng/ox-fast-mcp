open Alcotest

module TestTemporarySettings = struct
  let test_temporary_settings () =
    (* Check initial log level *)
    let initial_level = FastMCP.Settings.get_log_level () in
    Alcotest.(check string) "initial log level" "DEBUG" initial_level;
    
    (* Use temporary settings *)
    FastMCP.Utilities.Tests.with_temporary_settings 
      [("log_level", "ERROR")]
      (fun () ->
        let temp_level = FastMCP.Settings.get_log_level () in
        Alcotest.(check string) "temporary log level" "ERROR" temp_level
      );
    
    (* Check that settings are restored *)
    let restored_level = FastMCP.Settings.get_log_level () in
    Alcotest.(check string) "restored log level" "DEBUG" restored_level
end

let () =
  run "Tests Utilities Tests" [
    "temporary_settings", [
      test_case "temporary settings" `Quick TestTemporarySettings.test_temporary_settings;
    ];
  ] 