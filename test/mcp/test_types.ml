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
