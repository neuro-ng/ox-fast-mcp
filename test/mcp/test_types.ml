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
