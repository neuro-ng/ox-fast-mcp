(** Tests for base middleware module. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Middleware

(* =============================================================================
   Tests for context creation
   ============================================================================= *)

let%expect_test "context - default values" =
  let context =
    {
      message = `Assoc [ ("test", `String "data") ];
      fastmcp_context = None;
      source = `Client;
      type_ = `Request;
      method_ = Some "tools/call";
      timestamp = Time_ns.epoch;
      params = `Null;
      id = Some "test-id";
      resource = None;
    }
  in
  printf "source: %s\n"
    (match context.source with
    | `Client -> "client"
    | `Server -> "server");
  printf "type: %s\n"
    (match context.type_ with
    | `Request -> "request"
    | `Notification -> "notification");
  printf "method: %s\n" (Option.value context.method_ ~default:"none");
  printf "id: %s\n" (Option.value context.id ~default:"none");
  printf "fastmcp_context is None: %b\n"
    (Option.is_none context.fastmcp_context);
  printf "resource is None: %b\n" (Option.is_none context.resource);
  [%expect
    {|
    source: client
    type: request
    method: tools/call
    id: test-id
    fastmcp_context is None: true
    resource is None: true
    |}]

let%expect_test "context - notification type" =
  let context =
    {
      message = `Null;
      fastmcp_context = None;
      source = `Server;
      type_ = `Notification;
      method_ = Some "notifications/initialized";
      timestamp = Time_ns.epoch;
      params = `Null;
      id = None;
      resource = None;
    }
  in
  printf "type: %s\n"
    (match context.type_ with
    | `Request -> "request"
    | `Notification -> "notification");
  printf "source: %s\n"
    (match context.source with
    | `Client -> "client"
    | `Server -> "server");
  [%expect {|
    type: notification
    source: server
    |}]

(* =============================================================================
   Tests for copy_context
   ============================================================================= *)

let%expect_test "copy_context - preserves all fields when no changes" =
  let original =
    {
      message = `Assoc [ ("key", `String "value") ];
      fastmcp_context = Some (`String "ctx");
      source = `Server;
      type_ = `Notification;
      method_ = Some "notifications/initialized";
      timestamp = Time_ns.epoch;
      params = `Assoc [ ("param", `Int 42) ];
      id = Some "original-id";
      resource = Some "resource://test";
    }
  in
  let copied = copy_context original () in
  printf "message preserved: %b\n"
    (Yojson.Safe.equal copied.message original.message);
  printf "source preserved: %b\n" (phys_equal copied.source original.source);
  printf "type preserved: %b\n" (phys_equal copied.type_ original.type_);
  printf "method preserved: %b\n"
    (Option.equal String.equal copied.method_ original.method_);
  printf "id preserved: %b\n" (Option.equal String.equal copied.id original.id);
  printf "resource preserved: %b\n"
    (Option.equal String.equal copied.resource original.resource);
  [%expect
    {|
    message preserved: true
    source preserved: true
    type preserved: true
    method preserved: true
    id preserved: true
    resource preserved: true
    |}]

let%expect_test "copy_context - updates specified fields" =
  let original =
    {
      message = `Null;
      fastmcp_context = None;
      source = `Client;
      type_ = `Request;
      method_ = Some "tools/call";
      timestamp = Time_ns.epoch;
      params = `Null;
      id = Some "original";
      resource = None;
    }
  in
  let copied =
    copy_context original ~message:(`String "new message") ~source:`Server
      ~type_:`Notification ~method_:(Some "new/method") ~id:(Some "new-id")
      ~resource:(Some "resource://new") ()
  in
  printf "message updated: %b\n"
    (Yojson.Safe.equal copied.message (`String "new message"));
  printf "source updated: %b\n" (phys_equal copied.source `Server);
  printf "type updated: %b\n" (phys_equal copied.type_ `Notification);
  printf "method updated: %b\n"
    (Option.equal String.equal copied.method_ (Some "new/method"));
  printf "id updated: %b\n"
    (Option.equal String.equal copied.id (Some "new-id"));
  printf "resource updated: %b\n"
    (Option.equal String.equal copied.resource (Some "resource://new"));
  [%expect
    {|
    message updated: true
    source updated: true
    type updated: true
    method updated: true
    id updated: true
    resource updated: true
    |}]

let%expect_test "copy_context - partial update" =
  let original =
    {
      message = `String "original";
      fastmcp_context = None;
      source = `Client;
      type_ = `Request;
      method_ = Some "tools/call";
      timestamp = Time_ns.epoch;
      params = `Null;
      id = Some "orig-id";
      resource = None;
    }
  in
  let copied = copy_context original ~method_:(Some "new/method") () in
  printf "method changed: %b\n"
    (Option.equal String.equal copied.method_ (Some "new/method"));
  printf "message unchanged: %b\n"
    (Yojson.Safe.equal copied.message original.message);
  printf "source unchanged: %b\n" (phys_equal copied.source original.source);
  printf "id unchanged: %b\n" (Option.equal String.equal copied.id original.id);
  [%expect
    {|
    method changed: true
    message unchanged: true
    source unchanged: true
    id unchanged: true
    |}]

(* =============================================================================
   Tests for Results types
   ============================================================================= *)

let%expect_test "Results.call_tool_result - success" =
  let result = { Results.content = [ `String "output" ]; is_error = false } in
  printf "is_error: %b\n" result.is_error;
  printf "content count: %d\n" (List.length result.content);
  [%expect {|
    is_error: false
    content count: 1
    |}]

let%expect_test "Results.call_tool_result - error" =
  let result =
    { Results.content = [ `String "error message" ]; is_error = true }
  in
  printf "is_error: %b\n" result.is_error;
  printf "content count: %d\n" (List.length result.content);
  [%expect {|
    is_error: true
    content count: 1
    |}]

let%expect_test "Results.call_tool_result - multiple content" =
  let result =
    {
      Results.content = [ `String "line1"; `String "line2"; `String "line3" ];
      is_error = false;
    }
  in
  printf "content count: %d\n" (List.length result.content);
  [%expect {| content count: 3 |}]

let%expect_test "Results.list_tools_result - empty" =
  let result = { Results.tools = [] } in
  printf "tools count: %d\n" (List.length result.tools);
  [%expect {| tools count: 0 |}]

let%expect_test "Results.list_resources_result - empty" =
  let result = { Results.resources = [] } in
  printf "resources count: %d\n" (List.length result.resources);
  [%expect {| resources count: 0 |}]

let%expect_test "Results.list_resource_templates_result - empty" =
  let result = { Results.resource_templates = [] } in
  printf "templates count: %d\n" (List.length result.resource_templates);
  [%expect {| templates count: 0 |}]

let%expect_test "Results.list_prompts_result - empty" =
  let result = { Results.prompts = [] } in
  printf "prompts count: %d\n" (List.length result.prompts);
  [%expect {| prompts count: 0 |}]

(* =============================================================================
   Tests for source and type variants
   ============================================================================= *)

let%expect_test "context - client source" =
  let context =
    {
      message = `Null;
      fastmcp_context = None;
      source = `Client;
      type_ = `Request;
      method_ = None;
      timestamp = Time_ns.epoch;
      params = `Null;
      id = None;
      resource = None;
    }
  in
  printf "source is client: %b\n"
    (match context.source with
    | `Client -> true
    | `Server -> false);
  [%expect {| source is client: true |}]

let%expect_test "context - server source" =
  let context =
    {
      message = `Null;
      fastmcp_context = None;
      source = `Server;
      type_ = `Notification;
      method_ = None;
      timestamp = Time_ns.epoch;
      params = `Null;
      id = None;
      resource = None;
    }
  in
  printf "source is server: %b\n"
    (match context.source with
    | `Server -> true
    | `Client -> false);
  [%expect {| source is server: true |}]

(* =============================================================================
   Tests for method routing values
   ============================================================================= *)

let%expect_test "method routing - known methods" =
  let methods =
    [
      "initialize";
      "tools/call";
      "resources/read";
      "prompts/get";
      "tools/list";
      "resources/list";
      "resources/templates/list";
      "prompts/list";
    ]
  in
  List.iter methods ~f:(fun method_ -> printf "method: %s - valid\n" method_);
  [%expect
    {|
    method: initialize - valid
    method: tools/call - valid
    method: resources/read - valid
    method: prompts/get - valid
    method: tools/list - valid
    method: resources/list - valid
    method: resources/templates/list - valid
    method: prompts/list - valid
    |}]

(* =============================================================================
   Tests for message types
   ============================================================================= *)

let%expect_test "context - JSON message types" =
  let contexts =
    [
      ( "null",
        {
          message = `Null;
          fastmcp_context = None;
          source = `Client;
          type_ = `Request;
          method_ = None;
          timestamp = Time_ns.epoch;
          params = `Null;
          id = None;
          resource = None;
        } );
      ( "string",
        {
          message = `String "test";
          fastmcp_context = None;
          source = `Client;
          type_ = `Request;
          method_ = None;
          timestamp = Time_ns.epoch;
          params = `Null;
          id = None;
          resource = None;
        } );
      ( "object",
        {
          message = `Assoc [ ("key", `String "value") ];
          fastmcp_context = None;
          source = `Client;
          type_ = `Request;
          method_ = None;
          timestamp = Time_ns.epoch;
          params = `Null;
          id = None;
          resource = None;
        } );
    ]
  in
  List.iter contexts ~f:(fun (name, ctx) ->
      printf "message type %s: %s\n" name
        (match ctx.message with
        | `Null -> "null"
        | `String _ -> "string"
        | `Assoc _ -> "object"
        | _ -> "other"));
  [%expect
    {|
    message type null: null
    message type string: string
    message type object: object
    |}]

(* =============================================================================
   Tests for Base module creation
   ============================================================================= *)

let%expect_test "Base.create - creates unit instance" =
  let base = Base.create () in
  printf "base created: %b\n" (phys_equal base ());
  [%expect {| base created: true |}]
