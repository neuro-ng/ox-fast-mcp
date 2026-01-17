open Base
open Expect_test_helpers_core
open Mcp.Types

let%expect_test "progress_token yojson" =
  let token_string = `String "token" in
  let token_int = `Int 123 in
  let yojson_string = progress_token_to_yojson token_string in
  let yojson_int = progress_token_to_yojson token_int in
  print_endline (Yojson.Safe.to_string yojson_string);
  print_endline (Yojson.Safe.to_string yojson_int);
  [%expect {|
    "token"
    123
  |}];
  (* Test round-trip conversion *)
  let roundtrip_string = Mcp.Types.progress_token_of_yojson yojson_string in
  let roundtrip_int = Mcp.Types.progress_token_of_yojson yojson_int in
  require ~here:[%here] (phys_equal token_string roundtrip_string);
  require ~here:[%here] (phys_equal token_int roundtrip_int)

let%expect_test "role yojson" =
  let user_role = `User in
  let assistant_role = `Assistant in
  let yojson_user = role_to_yojson user_role in
  let yojson_assistant = role_to_yojson assistant_role in
  print_endline (Yojson.Safe.to_string yojson_user);
  print_endline (Yojson.Safe.to_string yojson_assistant);
  [%expect {|
    "user"
    "assistant"
  |}];
  let roundtrip_user = Mcp.Types.role_of_yojson yojson_user in
  let roundtrip_assistant = Mcp.Types.role_of_yojson yojson_assistant in
  require ~here:[%here] (phys_equal user_role roundtrip_user);
  require ~here:[%here] (phys_equal assistant_role roundtrip_assistant)

let%expect_test "request_id yojson" =
  let id_string = `String "id" in
  let id_int = `Int 123 in
  let yojson_string = request_id_to_yojson id_string in
  let yojson_int = request_id_to_yojson id_int in
  print_endline (Yojson.Safe.to_string yojson_string);
  print_endline (Yojson.Safe.to_string yojson_int);
  [%expect {|
    "id"
    123
  |}];
  let roundtrip_string = Mcp.Types.request_id_of_yojson yojson_string in
  let roundtrip_int = Mcp.Types.request_id_of_yojson yojson_int in
  require ~here:[%here] (phys_equal id_string roundtrip_string);
  require ~here:[%here] (phys_equal id_int roundtrip_int)

let%expect_test "jsonrpc_message yojson" =
  let request =
    `Request { jsonrpc = "2.0"; id = `Int 1; method_ = "test"; params = None }
  in
  let notification =
    `Notification { jsonrpc = "2.0"; method_ = "test"; params = None }
  in
  let response = `Response { jsonrpc = "2.0"; id = `Int 1; result = `Null } in
  let error =
    `Error
      {
        jsonrpc = "2.0";
        id = `Int 1;
        error = { code = 1; message = "error"; data = None };
      }
  in
  let yojson_request = jsonrpc_message_to_yojson request in
  let yojson_notification = jsonrpc_message_to_yojson notification in
  let yojson_response = jsonrpc_message_to_yojson response in
  let yojson_error = jsonrpc_message_to_yojson error in
  print_endline (Yojson.Safe.to_string yojson_request);
  print_endline (Yojson.Safe.to_string yojson_notification);
  print_endline (Yojson.Safe.to_string yojson_response);
  print_endline (Yojson.Safe.to_string yojson_error);
  [%expect
    {|
    ["Request",{"jsonrpc":"2.0","id":1,"method":"test"}]
    ["Notification",{"jsonrpc":"2.0","method":"test"}]
    ["Response",{"jsonrpc":"2.0","id":1,"result":null}]
    ["Error",{"jsonrpc":"2.0","id":1,"error":{"code":1,"message":"error"}}]
  |}];
  let roundtrip_request = Mcp.Types.jsonrpc_message_of_yojson yojson_request in
  let roundtrip_notification =
    Mcp.Types.jsonrpc_message_of_yojson yojson_notification
  in
  let roundtrip_response =
    Mcp.Types.jsonrpc_message_of_yojson yojson_response
  in
  let roundtrip_error = Mcp.Types.jsonrpc_message_of_yojson yojson_error in
  require ~here:[%here] (phys_equal request roundtrip_request);
  require ~here:[%here] (phys_equal notification roundtrip_notification);
  require ~here:[%here] (phys_equal response roundtrip_response);
  require ~here:[%here] (phys_equal error roundtrip_error)

(* Resource type tests - uses exported resource_to_yojson *)
let%expect_test "resource yojson" =
  let resource =
    {
      uri = "file:///path/to/resource.txt";
      description = Some "A test resource";
      mime_type = Some "text/plain";
      size = Some 1024;
      annotations = None;
      icons = None;
      meta = None;
      base_metadata = { name = "test-resource"; title = Some "Test Resource" };
    }
  in
  let json = resource_to_yojson resource in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"uri":"file:///path/to/resource.txt","description":"A test resource","mimeType":"text/plain","size":1024,"name":"test-resource","title":"Test Resource"} |}];
  let roundtrip = resource_of_yojson json in
  require ~here:[%here] (String.equal resource.uri roundtrip.uri);
  require ~here:[%here]
    (String.equal resource.base_metadata.name roundtrip.base_metadata.name)

let%expect_test "resource with annotations" =
  let resource =
    {
      uri = "file:///test.txt";
      description = None;
      mime_type = None;
      size = None;
      annotations = Some { audience = Some [ `User ]; priority = Some 0.5 };
      icons = None;
      meta = None;
      base_metadata = { name = "annotated"; title = None };
    }
  in
  let json = resource_to_yojson resource in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"uri":"file:///test.txt","annotations":{"audience":["user"],"priority":0.5},"name":"annotated"} |}];
  let roundtrip = resource_of_yojson json in
  require ~here:[%here] (Option.is_some roundtrip.annotations)

(* Server capabilities - uses exported server_capabilities_to_yojson *)
let%expect_test "server_capabilities yojson" =
  let caps =
    {
      experimental = None;
      logging = None;
      prompts = Some { list_changed = Some true };
      resources = Some { subscribe = Some true; list_changed = Some false };
      tools = Some { list_changed = Some true };
    }
  in
  let json = server_capabilities_to_yojson caps in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"prompts":{"listChanged":true},"resources":{"subscribe":true,"listChanged":false},"tools":{"listChanged":true}} |}];
  let roundtrip = server_capabilities_of_yojson json in
  require ~here:[%here] (Option.is_some roundtrip.prompts)

(* Error data - uses exported error_data_to_yojson *)
let%expect_test "error_data yojson" =
  let error = { code = 404; message = "Not found"; data = None } in
  let json = error_data_to_yojson error in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"code":404,"message":"Not found"} |}];
  let roundtrip = error_data_of_yojson json in
  require ~here:[%here] (Int.equal error.code roundtrip.code);
  require ~here:[%here] (String.equal error.message roundtrip.message)

let%expect_test "error_data with data" =
  let error =
    {
      code = 500;
      message = "Internal error";
      data = Some (`Assoc [ ("details", `String "stack trace") ]);
    }
  in
  let json = error_data_to_yojson error in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"code":500,"message":"Internal error","data":{"details":"stack trace"}} |}];
  let roundtrip = error_data_of_yojson json in
  require ~here:[%here] (Option.is_some roundtrip.data)

(* JSONRPC request - uses exported jsonrpc_request_to_yojson *)
let%expect_test "jsonrpc_request yojson" =
  let request =
    {
      jsonrpc = "2.0";
      id = `String "req-1";
      method_ = "initialize";
      params = Some (`Assoc [ ("version", `String "1.0") ]);
    }
  in
  let json = jsonrpc_request_to_yojson request in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"jsonrpc":"2.0","id":"req-1","method":"initialize","params":{"version":"1.0"}} |}];
  let roundtrip = jsonrpc_request_of_yojson json in
  require ~here:[%here] (String.equal request.method_ roundtrip.method_)

(* Resources capability - uses exported resources_capability_to_yojson *)
let%expect_test "resources_capability yojson" =
  let cap = { subscribe = Some true; list_changed = Some false } in
  let json = resources_capability_to_yojson cap in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"subscribe":true,"listChanged":false} |}];
  let roundtrip = resources_capability_of_yojson json in
  require ~here:[%here]
    (Option.equal Bool.equal cap.subscribe roundtrip.subscribe)

(* Tools capability - uses exported tools_capability_to_yojson *)
let%expect_test "tools_capability yojson" =
  let cap = { list_changed = Some true } in
  let json = tools_capability_to_yojson cap in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"listChanged":true} |}];
  let roundtrip = tools_capability_of_yojson json in
  require ~here:[%here]
    (Option.equal Bool.equal cap.list_changed roundtrip.list_changed)

(* ============================================= *)
(* Tasks API Tests                               *)
(* ============================================= *)

let%expect_test "task_status sexp" =
  let statuses = [ `Working; `Input_required; `Completed; `Failed; `Cancelled ] in
  List.iter statuses ~f:(fun status ->
      print_s [%sexp (status : task_status)]);
  [%expect {|
    Working
    Input_required
    Completed
    Failed
    Cancelled
  |}]

let%expect_test "task type construction" =
  let task : task = {
    task_id = "task-123";
    status = `Working;
    status_message = Some "Processing...";
    created_at = "2026-01-16T12:00:00Z";
    last_updated_at = "2026-01-16T12:01:00Z";
    ttl = Some 3600;
    poll_interval = Some 5;
  } in
  print_s [%sexp { task_id = (task.task_id : string); status = (task.status : task_status) }];
  [%expect {| ((task_id task-123) (status Working)) |}]

let%expect_test "get_task_request sexp" =
  let params : get_task_request_params = {
    task_id = "task-456";
    request_params = { meta = None };
  } in
  let request : get_task_request = {
    method_ = "tasks/get";
    params;
  } in
  print_s [%sexp {
    method_ = (request.method_ : string);
    task_id = (request.params.task_id : string)
  }];
  [%expect {| ((method_ tasks/get) (task_id task-456)) |}]

let%expect_test "cancel_task_request sexp" =
  let params : cancel_task_request_params = {
    task_id = "task-789";
    request_params = { meta = None };
  } in
  let request : cancel_task_request = {
    method_ = "tasks/cancel";
    params;
  } in
  print_s [%sexp {
    method_ = (request.method_ : string);
    task_id = (request.params.task_id : string)
  }];
  [%expect {| ((method_ tasks/cancel) (task_id task-789)) |}]

let%expect_test "task_status_notification sexp" =
  let params : task_status_notification_params = {
    task_id = "task-notify";
    status = `Completed;
    status_message = Some "Done";
    created_at = "2026-01-16T12:00:00Z";
    last_updated_at = "2026-01-16T12:05:00Z";
    ttl = None;
    poll_interval = None;
    notification_params = { meta = None };
  } in
  print_s [%sexp {
    task_id = (params.task_id : string);
    status = (params.status : task_status)
  }];
  [%expect {| ((task_id task-notify) (status Completed)) |}]

(* ============================================= *)
(* Sampling Types Tests                          *)
(* ============================================= *)

let%expect_test "sampling_message construction" =
  let text_content : text_content = {
    type_ = `Text;
    text = "Hello, world!";
    annotations = None;
    meta = None;
  } in
  let message : sampling_message = {
    role = `User;
    content = `Text text_content;
    meta = None;
  } in
  (* Verify construction succeeds and role is correctly set *)
  let role_str = match message.role with `User -> "User" | `Assistant -> "Assistant" in
  print_endline role_str;
  [%expect {| User |}]

let%expect_test "tool_use_content sexp" =
  let content : tool_use_content = {
    type_ = `Tool_use;
    name = "calculator";
    id = "tool-call-1";
    input = `Assoc [("expression", `String "2+2")];
    meta = None;
  } in
  print_s [%sexp {
    name = (content.name : string);
    id = (content.id : string)
  }];
  [%expect {| ((name calculator) (id tool-call-1)) |}]

let%expect_test "tool_result_content sexp" =
  let content : tool_result_content = {
    type_ = `Tool_result;
    tool_use_id = "tool-call-1";
    content = `String "4";
    structured_content = Some (`Assoc [("result", `Int 4)]);
    is_error = Some false;
    meta = None;
  } in
  print_s [%sexp {
    tool_use_id = (content.tool_use_id : string);
    is_error = (content.is_error : bool option)
  }];
  [%expect {| ((tool_use_id tool-call-1) (is_error (false))) |}]

let%expect_test "model_preferences sexp" =
  let prefs : model_preferences = {
    hints = Some [{ name = Some "claude-3" }];
    cost_priority = Some 0.5;
    speed_priority = Some 0.3;
    intelligence_priority = Some 0.8;
  } in
  print_s [%sexp {
    cost_priority = (prefs.cost_priority : float option);
    intelligence_priority = (prefs.intelligence_priority : float option)
  }];
  [%expect {| ((cost_priority (0.5)) (intelligence_priority (0.8))) |}]

let%expect_test "stop_reason sexp" =
  let reasons : stop_reason list = [
    `EndTurn;
    `StopSequence;
    `MaxTokens;
    `String "custom"
  ] in
  List.iter reasons ~f:(fun reason ->
      print_s [%sexp (reason : stop_reason)]);
  [%expect {|
    EndTurn
    StopSequence
    MaxTokens
    (String custom)
  |}]

(* Note: Elicitation schema types are defined in types.ml but not exported 
   in types.mli, so we skip testing those internal types here. *)

(* ============================================= *)
(* Client/Server Aggregation Tests               *)
(* ============================================= *)

let%expect_test "client_request includes task types" =
  let get_task : client_request = `GetTask {
    method_ = "tasks/get";
    params = { task_id = "t1"; request_params = { meta = None } };
  } in
  let cancel_task : client_request = `CancelTask {
    method_ = "tasks/cancel";
    params = { task_id = "t2"; request_params = { meta = None } };
  } in
  print_s [%sexp ("GetTask variant exists" : string)];
  print_s [%sexp ("CancelTask variant exists" : string)];
  (* Just verify they compile - pattern match to ensure variants exist *)
  (match get_task with `GetTask _ -> () | _ -> ());
  (match cancel_task with `CancelTask _ -> () | _ -> ());
  [%expect {|
    "GetTask variant exists"
    "CancelTask variant exists"
  |}]

let%expect_test "server_notification includes TaskStatus" =
  let task_notif : server_notification = `TaskStatus {
    method_ = "notifications/tasks/status";
    params = {
      task_id = "t1";
      status = `Completed;
      status_message = None;
      created_at = "2026-01-16T12:00:00Z";
      last_updated_at = "2026-01-16T12:00:00Z";
      ttl = None;
      poll_interval = None;
      notification_params = { meta = None };
    };
  } in
  print_s [%sexp ("TaskStatus variant exists" : string)];
  (match task_notif with `TaskStatus _ -> () | _ -> ());
  [%expect {| "TaskStatus variant exists" |}]
