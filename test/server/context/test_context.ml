(** Tests for context module. *)

open! Core
open! Expect_test_helpers_core

(* Context is accessible directly since we depend on ox-fast-mcp.server
   library *)
module Context = Server__Context
open Context

(* =============================================================================
   Tests for Log_level
   ============================================================================= *)

let%expect_test "Log_level - to_string" =
  printf "debug: %s\n" (Log_level.to_string Debug);
  printf "info: %s\n" (Log_level.to_string Info);
  printf "notice: %s\n" (Log_level.to_string Notice);
  printf "warning: %s\n" (Log_level.to_string Warning);
  printf "error: %s\n" (Log_level.to_string Error);
  printf "critical: %s\n" (Log_level.to_string Critical);
  printf "alert: %s\n" (Log_level.to_string Alert);
  printf "emergency: %s\n" (Log_level.to_string Emergency);
  [%expect
    {|
    debug: debug
    info: info
    notice: notice
    warning: warning
    error: error
    critical: critical
    alert: alert
    emergency: emergency
    |}]

let%expect_test "Log_level - of_string" =
  printf "debug: %s\n" (Log_level.to_string (Log_level.of_string "debug"));
  printf "info: %s\n" (Log_level.to_string (Log_level.of_string "info"));
  printf "warning: %s\n" (Log_level.to_string (Log_level.of_string "warning"));
  printf "error: %s\n" (Log_level.to_string (Log_level.of_string "error"));
  [%expect
    {|
    debug: debug
    info: info
    warning: warning
    error: error
    |}]

let%expect_test "Log_level - to_logs_level" =
  let log_level_name level =
    match Log_level.to_logs_level level with
    | Logs.Debug -> "debug"
    | Logs.Info -> "info"
    | Logs.Warning -> "warning"
    | Logs.Error -> "error"
    | Logs.App -> "app"
  in
  printf "Debug -> %s\n" (log_level_name Debug);
  printf "Info -> %s\n" (log_level_name Info);
  printf "Notice -> %s\n" (log_level_name Notice);
  printf "Warning -> %s\n" (log_level_name Warning);
  printf "Error -> %s\n" (log_level_name Error);
  printf "Critical -> %s\n" (log_level_name Critical);
  [%expect
    {|
    Debug -> debug
    Info -> info
    Notice -> info
    Warning -> warning
    Error -> error
    Critical -> error
    |}]

(* =============================================================================
   Tests for Log_data
   ============================================================================= *)

let%expect_test "Log_data - create without extra" =
  let data = Log_data.create ~msg:"Test message" () in
  printf "msg: %s\n" data.msg;
  printf "has extra: %b\n" (Option.is_some data.extra);
  [%expect {|
    msg: Test message
    has extra: false
    |}]

let%expect_test "Log_data - create with extra" =
  let data =
    Log_data.create ~msg:"Test message"
      ~extra:[ ("key1", `String "value1"); ("key2", `Int 42) ]
      ()
  in
  printf "msg: %s\n" data.msg;
  printf "has extra: %b\n" (Option.is_some data.extra);
  printf "extra length: %d\n"
    (Option.value_map data.extra ~default:0 ~f:List.length);
  [%expect
    {|
    msg: Test message
    has extra: true
    extra length: 2
    |}]

(* =============================================================================
   Tests for Model_hint
   ============================================================================= *)

let%expect_test "Model_hint - create" =
  let hint = Model_hint.create ~name:"claude-3-sonnet" in
  printf "name: %s\n" hint.name;
  [%expect {| name: claude-3-sonnet |}]

(* =============================================================================
   Tests for Model_preferences
   ============================================================================= *)

let%expect_test "Model_preferences - create empty" =
  let prefs = Model_preferences.empty in
  printf "hints count: %d\n" (List.length prefs.hints);
  [%expect {| hints count: 0 |}]

let%expect_test "Model_preferences - create with hints" =
  let prefs =
    Model_preferences.create
      ~hints:
        [
          Model_hint.create ~name:"claude-3-sonnet";
          Model_hint.create ~name:"claude";
        ]
  in
  printf "hints count: %d\n" (List.length prefs.hints);
  List.iter prefs.hints ~f:(fun hint -> printf "hint: %s\n" hint.name);
  [%expect
    {|
    hints count: 2
    hint: claude-3-sonnet
    hint: claude
    |}]

(* =============================================================================
   Tests for parse_model_preferences
   ============================================================================= *)

let%expect_test "parse_model_preferences - None" =
  let result = parse_model_preferences `None in
  printf "is none: %b\n" (Option.is_none result);
  [%expect {| is none: true |}]

let%expect_test "parse_model_preferences - String" =
  let result = parse_model_preferences (`String "claude-3-sonnet") in
  (match result with
  | Some prefs ->
    printf "hints count: %d\n" (List.length prefs.hints);
    List.iter prefs.hints ~f:(fun hint -> printf "hint: %s\n" hint.name)
  | None -> printf "none\n");
  [%expect {|
    hints count: 1
    hint: claude-3-sonnet
    |}]

let%expect_test "parse_model_preferences - List" =
  let result =
    parse_model_preferences (`List [ "claude-3-sonnet"; "claude" ])
  in
  (match result with
  | Some prefs ->
    printf "hints count: %d\n" (List.length prefs.hints);
    List.iter prefs.hints ~f:(fun hint -> printf "hint: %s\n" hint.name)
  | None -> printf "none\n");
  [%expect
    {|
    hints count: 2
    hint: claude-3-sonnet
    hint: claude
    |}]

let%expect_test "parse_model_preferences - Preferences object" =
  let original =
    Model_preferences.create ~hints:[ Model_hint.create ~name:"gpt-4" ]
  in
  let result = parse_model_preferences (`Preferences original) in
  (match result with
  | Some prefs ->
    printf "hints count: %d\n" (List.length prefs.hints);
    List.iter prefs.hints ~f:(fun hint -> printf "hint: %s\n" hint.name)
  | None -> printf "none\n");
  [%expect {|
    hints count: 1
    hint: gpt-4
    |}]

(* =============================================================================
   Tests for Context creation
   ============================================================================= *)

let%expect_test "Context - create default" =
  let ctx = create () in
  printf "request_id: %s\n" (Option.value (get_request_id ctx) ~default:"none");
  printf "client_id: %s\n" (Option.value (get_client_id ctx) ~default:"none");
  printf "session_id: %s\n" (Option.value (get_session_id ctx) ~default:"none");
  [%expect
    {|
    request_id: none
    client_id: none
    session_id: none
    |}]

let%expect_test "Context - create with ids" =
  let ctx =
    create ~request_id:"req-123" ~client_id:"client-456" ~session_id:"sess-789"
      ()
  in
  printf "request_id: %s\n" (Option.value (get_request_id ctx) ~default:"none");
  printf "client_id: %s\n" (Option.value (get_client_id ctx) ~default:"none");
  printf "session_id: %s\n" (Option.value (get_session_id ctx) ~default:"none");
  [%expect
    {|
    request_id: req-123
    client_id: client-456
    session_id: sess-789
    |}]

(* =============================================================================
   Tests for Context state
   ============================================================================= *)

let%expect_test "Context - state get/set" =
  let ctx = create () in
  printf "initial: %s\n"
    (Option.value_map (get_state ctx "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  set_state ctx "key1" (`String "value1");
  printf "after set: %s\n"
    (Option.value_map (get_state ctx "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  set_state ctx "key1" (`String "new_value");
  printf "after override: %s\n"
    (Option.value_map (get_state ctx "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  [%expect
    {|
    initial: none
    after set: "value1"
    after override: "new_value"
    |}]

let%expect_test "Context - state inheritance" =
  let parent = create () in
  set_state parent "key1" (`String "parent-key1");
  set_state parent "key2" (`String "parent-key2");

  let child = create () in
  let child = with_inherited_state parent child in

  (* Child inherits parent state *)
  printf "child key1: %s\n"
    (Option.value_map (get_state child "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  printf "child key2: %s\n"
    (Option.value_map (get_state child "key2") ~default:"none"
       ~f:Yojson.Safe.to_string);

  (* Modify child state *)
  set_state child "key1" (`String "child-key1");

  (* Parent unaffected *)
  printf "parent key1 after child mod: %s\n"
    (Option.value_map (get_state parent "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  printf "child key1 after mod: %s\n"
    (Option.value_map (get_state child "key1") ~default:"none"
       ~f:Yojson.Safe.to_string);
  [%expect
    {|
    child key1: "parent-key1"
    child key2: "parent-key2"
    parent key1 after child mod: "parent-key1"
    child key1 after mod: "child-key1"
    |}]

(* =============================================================================
   Tests for Context change notifications
   ============================================================================= *)

let%expect_test "Context - change notifications" =
  let ctx = create () in
  printf "has changes initially: %b\n" (has_changes ctx);
  queue_tool_list_changed ctx;
  printf "has changes after tools: %b\n" (has_changes ctx);
  printf "changed lists: %s\n" (String.concat ~sep:", " (get_changed_lists ctx));
  queue_resource_list_changed ctx;
  queue_prompt_list_changed ctx;
  printf "all changed lists: %s\n"
    (String.concat ~sep:", " (get_changed_lists ctx));
  reset_changes ctx;
  printf "has changes after reset: %b\n" (has_changes ctx);
  [%expect
    {|
    has changes initially: false
    has changes after tools: true
    changed lists: tools
    all changed lists: tools, resources, prompts
    has changes after reset: false
    |}]

let%expect_test "Context - pending notifications" =
  let ctx = create () in
  printf "pending initially: %d\n" (List.length (get_pending_notifications ctx));
  queue_tool_list_changed ctx;
  queue_resource_list_changed ctx;
  let pending = get_pending_notifications ctx in
  printf "pending count: %d\n" (List.length pending);
  printf "has tools notification: %b\n"
    (List.mem pending "notifications/tools/list_changed" ~equal:String.equal);
  printf "has resources notification: %b\n"
    (List.mem pending "notifications/resources/list_changed" ~equal:String.equal);
  [%expect
    {|
    pending initially: 0
    pending count: 2
    has tools notification: true
    has resources notification: true
    |}]

(* =============================================================================
   Tests for Session data
   ============================================================================= *)

let%expect_test "Context - session data" =
  let ctx = create () in
  printf "initial: %s\n"
    (Option.value_map
       (get_session_value ctx "data1")
       ~default:"none" ~f:Yojson.Safe.to_string);
  set_session_value ctx "data1" (`Int 42);
  printf "after set: %s\n"
    (Option.value_map
       (get_session_value ctx "data1")
       ~default:"none" ~f:Yojson.Safe.to_string);
  remove_session_value ctx "data1";
  printf "after remove: %s\n"
    (Option.value_map
       (get_session_value ctx "data1")
       ~default:"none" ~f:Yojson.Safe.to_string);
  [%expect {|
    initial: none
    after set: 42
    after remove: none
    |}]

(* =============================================================================
   Tests for Context with_* functions
   ============================================================================= *)

let%expect_test "Context - with_request_id" =
  let ctx = create () in
  let ctx2 = with_request_id ctx "new-req-id" in
  printf "original request_id: %s\n"
    (Option.value (get_request_id ctx) ~default:"none");
  printf "new request_id: %s\n"
    (Option.value (get_request_id ctx2) ~default:"none");
  [%expect
    {|
    original request_id: none
    new request_id: new-req-id
    |}]

let%expect_test "Context - with_client_id" =
  let ctx = create () in
  let ctx2 = with_client_id ctx "new-client-id" in
  printf "original client_id: %s\n"
    (Option.value (get_client_id ctx) ~default:"none");
  printf "new client_id: %s\n"
    (Option.value (get_client_id ctx2) ~default:"none");
  [%expect
    {|
    original client_id: none
    new client_id: new-client-id
    |}]

let%expect_test "Context - with_session_id" =
  let ctx = create () in
  let ctx2 = with_session_id ctx "new-sess-id" in
  printf "original session_id: %s\n"
    (Option.value (get_session_id ctx) ~default:"none");
  printf "new session_id: %s\n"
    (Option.value (get_session_id ctx2) ~default:"none");
  [%expect
    {|
    original session_id: none
    new session_id: new-sess-id
    |}]

(* =============================================================================
   Tests for Progress
   ============================================================================= *)

let%expect_test "Progress - create" =
  let progress =
    Progress.create ~progress:50.0 ~total:100.0 ~message:"Processing" ()
  in
  printf "progress: %.1f\n" progress.progress;
  printf "total: %s\n"
    (Option.value_map progress.total ~default:"none" ~f:(Printf.sprintf "%.1f"));
  printf "message: %s\n" (Option.value progress.message ~default:"none");
  [%expect
    {|
    progress: 50.0
    total: 100.0
    message: Processing
    |}]

let%expect_test "Context - report_progress" =
  let ctx = create ~request_id:"req-123" () in
  let progress =
    report_progress ctx ~progress:75.0 ~total:100.0 ~message:"Almost done" ()
  in
  printf "progress: %.1f\n" progress.progress;
  printf "request_id: %s\n" (Option.value progress.request_id ~default:"none");
  [%expect {|
    progress: 75.0
    request_id: req-123
    |}]
