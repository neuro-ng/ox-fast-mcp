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
  require_equal (module (struct type t = progress_token [@@deriving sexp] let compare = Mcp.Types.compare_progress_token end)) token_string (Mcp.Types.progress_token_of_yojson yojson_string |> Or_error.ok_exn);
  require_equal (module (struct type t = progress_token [@@deriving sexp] let compare = Mcp.Types.compare_progress_token end)) token_int (Mcp.Types.progress_token_of_yojson yojson_int |> Or_error.ok_exn)

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
  require_equal (module (struct type t = role [@@deriving sexp] let compare = Mcp.Types.compare_role end)) user_role (Mcp.Types.role_of_yojson yojson_user |> Or_error.ok_exn);
  require_equal (module (struct type t = role [@@deriving sexp] let compare = Mcp.Types.compare_role end)) assistant_role (Mcp.Types.role_of_yojson yojson_assistant |> Or_error.ok_exn)

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
  require_equal (module (struct type t = request_id [@@deriving sexp] let compare = Mcp.Types.compare_request_id end)) id_string (Mcp.Types.request_id_of_yojson yojson_string |> Or_error.ok_exn);
  require_equal (module (struct type t = request_id [@@deriving sexp] let compare = Mcp.Types.compare_request_id end)) id_int (Mcp.Types.request_id_of_yojson yojson_int |> Or_error.ok_exn)

let%expect_test "jsonrpc_message yojson" = 
  let request = `Request { jsonrpc = "2.0"; id = `Int 1; method_ = "test"; params = None } in
  let notification = `Notification { jsonrpc = "2.0"; method_ = "test"; params = None } in
  let response = `Response { jsonrpc = "2.0"; id = `Int 1; result = `Null } in
  let error = `Error { jsonrpc = "2.0"; id = `Int 1; error = { code = 1; message = "error"; data = None } } in
  let yojson_request = jsonrpc_message_to_yojson request in
  let yojson_notification = jsonrpc_message_to_yojson notification in
  let yojson_response = jsonrpc_message_to_yojson response in
  let yojson_error = jsonrpc_message_to_yojson error in
  print_endline (Yojson.Safe.to_string yojson_request);
  print_endline (Yojson.Safe.to_string yojson_notification);
  print_endline (Yojson.Safe.to_string yojson_response);
  print_endline (Yojson.Safe.to_string yojson_error);
  [%expect {|
    ["Request",{"jsonrpc":"2.0","id":1,"method":"test"}]
    ["Notification",{"jsonrpc":"2.0","method":"test"}]
    ["Response",{"jsonrpc":"2.0","id":1,"result":null}]
    ["Error",{"jsonrpc":"2.0","id":1,"error":{"code":1,"message":"error"}}]
  |}];
  require_equal (module (struct type t = jsonrpc_message [@@deriving sexp] let compare = Mcp.Types.compare_jsonrpc_message end)) request (Mcp.Types.jsonrpc_message_of_yojson yojson_request |> Or_error.ok_exn);
  require_equal (module (struct type t = jsonrpc_message [@@deriving sexp] let compare = Mcp.Types.compare_jsonrpc_message end)) notification (Mcp.Types.jsonrpc_message_of_yojson yojson_notification |> Or_error.ok_exn);
  require_equal (module (struct type t = jsonrpc_message [@@deriving sexp] let compare = Mcp.Types.compare_jsonrpc_message end)) response (Mcp.Types.jsonrpc_message_of_yojson yojson_response |> Or_error.ok_exn);
  require_equal (module (struct type t = jsonrpc_message [@@deriving sexp] let compare = Mcp.Types.compare_jsonrpc_message end)) error (Mcp.Types.jsonrpc_message_of_yojson yojson_error |> Or_error.ok_exn)
