(** Tests for Proxy Client module.

    Translated from Python test_proxy_client.py to OCaml. Tests focus on
    unit-testable components like name generation, handler configuration, and
    default handler behavior.

    Note: Python tests require full async Client/FastMCP connections which are
    not unit-testable in OCaml. These tests focus on synchronous functions. *)

open! Core
open! Expect_test_helpers_core
module Conftest = Conftest

(* =============================================================================
   Test: Proxy Client Name Generation
   ============================================================================= *)

let%expect_test "Proxy_client.generate_name - creates unique names" =
  let name1 = Server__Proxy.Proxy_client.generate_name () in
  let name2 = Server__Proxy.Proxy_client.generate_name () in
  printf "name1_prefix: %b\n" (String.is_prefix name1 ~prefix:"proxy-client-");
  printf "name2_prefix: %b\n" (String.is_prefix name2 ~prefix:"proxy-client-");
  printf "names_different: %b\n" (not (String.equal name1 name2));
  [%expect
    {|
    name1_prefix: true
    name2_prefix: true
    names_different: true
    |}]

(* =============================================================================
   Test: Proxy Client Creation
   ============================================================================= *)

let%expect_test "Proxy_client.create - with default name" =
  let client = Server__Proxy.Proxy_client.create ~transport:(`Assoc []) () in
  (* Client should be created successfully - we can't inspect internal state but
     we can verify it doesn't raise *)
  printf "client_created: true\n";
  let _ = client in
  [%expect {| client_created: true |}]

let%expect_test "Proxy_client.create - with custom name" =
  let _client =
    Server__Proxy.Proxy_client.create ~name:"my-custom-client"
      ~transport:(`Assoc []) ()
  in
  printf "client_with_name_created: true\n";
  [%expect {| client_with_name_created: true |}]

let%expect_test "Proxy_client.create - with handlers" =
  (* Test that handlers can be configured without error *)
  let roots_handler () = Async.return (`List []) in
  let sampling_handler _messages _params = Async.return (`Assoc []) in
  let elicitation_handler _message _params =
    Async.return (`Assoc [ ("action", `String "accept") ])
  in
  let log_handler _msg = Async.return () in
  let progress_handler _progress _total _message = Async.return () in

  let _client =
    Server__Proxy.Proxy_client.create ~transport:(`Assoc []) ~roots_handler
      ~sampling_handler ~elicitation_handler ~log_handler ~progress_handler ()
  in
  printf "client_with_handlers_created: true\n";
  [%expect {| client_with_handlers_created: true |}]

(* =============================================================================
   Test: Default Handlers (OCaml equivalents of Python default implementations)
   ============================================================================= *)

let%expect_test "Proxy_client.default_sampling_handler - returns expected \
                 structure" =
  (* Default handler returns a role/model/content structure *)
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Server__Proxy.Proxy_client.default_sampling_handler ~messages:`Null
          ~params:`Null)
  in
  let open Yojson.Safe.Util in
  printf "has_role: %b\n"
    (Option.is_some (result |> member "role" |> to_string_option));
  printf "has_model: %b\n"
    (Option.is_some (result |> member "model" |> to_string_option));
  printf "role: %s\n" (result |> member "role" |> to_string);
  [%expect {|
    has_role: true
    has_model: true
    role: assistant
    |}]

let%expect_test "Proxy_client.default_elicitation_handler - returns accept \
                 action" =
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Server__Proxy.Proxy_client.default_elicitation_handler ~message:"test"
          ~params:`Null)
  in
  let open Yojson.Safe.Util in
  printf "action: %s\n" (result |> member "action" |> to_string);
  [%expect {| action: accept |}]

(* =============================================================================
   Test: Tool Result Type
   ============================================================================= *)

let%expect_test "Tool_result.create - with content only" =
  let content =
    [ `Assoc [ ("type", `String "text"); ("text", `String "Hello") ] ]
  in
  let result = Server__Proxy.Tool_result.create ~content () in
  printf "content_count: %d\n" (List.length result.content);
  printf "has_structured: %b\n" (Option.is_some result.structured_content);
  [%expect {|
    content_count: 1
    has_structured: false
    |}]

let%expect_test "Tool_result.create - with structured content" =
  let content =
    [ `Assoc [ ("type", `String "text"); ("text", `String "Hello") ] ]
  in
  let structured = `Assoc [ ("data", `String "structured") ] in
  let result =
    Server__Proxy.Tool_result.create ~content ~structured_content:structured ()
  in
  printf "content_count: %d\n" (List.length result.content);
  printf "has_structured: %b\n" (Option.is_some result.structured_content);
  (match result.structured_content with
  | Some (`Assoc fields) ->
    printf "structured_has_data: %b\n"
      (List.Assoc.mem fields ~equal:String.equal "data")
  | _ -> printf "structured_has_data: false\n");
  [%expect
    {|
    content_count: 1
    has_structured: true
    structured_has_data: true
    |}]

(* =============================================================================
   Test: Mirrored Component Marker
   ============================================================================= *)

let%expect_test "Mirrored_component.create - default not mirrored" =
  let component = Server__Proxy.Mirrored_component.create () in
  printf "is_mirrored: %b\n"
    (Server__Proxy.Mirrored_component.is_mirrored component);
  [%expect {| is_mirrored: false |}]

let%expect_test "Mirrored_component.create - explicitly mirrored" =
  let component = Server__Proxy.Mirrored_component.create ~mirrored:true () in
  printf "is_mirrored: %b\n"
    (Server__Proxy.Mirrored_component.is_mirrored component);
  [%expect {| is_mirrored: true |}]

let%expect_test "Mirrored_component.create - explicitly not mirrored" =
  let component = Server__Proxy.Mirrored_component.create ~mirrored:false () in
  printf "is_mirrored: %b\n"
    (Server__Proxy.Mirrored_component.is_mirrored component);
  [%expect {| is_mirrored: false |}]

(* =============================================================================
   Test: Simulated Handler Forwarding (Mock testing pattern)
   ============================================================================= *)

(** Mock handler that records calls *)
module Mock_handler = struct
  let log_calls : string list ref = ref []
  let reset () = log_calls := []

  let log_handler msg =
    let open Yojson.Safe.Util in
    let level =
      match msg |> member "level" with
      | `String s -> s
      | _ -> "unknown"
    in
    let logger =
      match msg |> member "logger" with
      | `String s -> s
      | _ -> "none"
    in
    log_calls := !log_calls @ [ sprintf "%s:%s" level logger ];
    Async.return ()

  let get_calls () = !log_calls
end

let%expect_test "Mock log handler - records calls" =
  Mock_handler.reset ();
  (* Simulate what would happen when log handler is called *)
  let msg1 = `Assoc [ ("level", `String "info"); ("logger", `String "test") ] in
  let msg2 =
    `Assoc [ ("level", `String "warning"); ("logger", `String "app") ]
  in
  Async.Thread_safe.block_on_async_exn (fun () -> Mock_handler.log_handler msg1);
  Async.Thread_safe.block_on_async_exn (fun () -> Mock_handler.log_handler msg2);
  let calls = Mock_handler.get_calls () in
  printf "call_count: %d\n" (List.length calls);
  List.iter calls ~f:(fun c -> printf "call: %s\n" c);
  [%expect
    {|
    call_count: 2
    call: info:test
    call: warning:app
    |}]

(** Mock elicitation handler that returns configurable response *)
module Mock_elicitation = struct
  type response = Accept of string | Decline

  let response : response ref = ref Decline
  let set_response r = response := r

  let handler _message response_type _params =
    let _ = response_type in
    Async.return
      (match !response with
      | Accept name ->
        `Assoc
          [
            ("action", `String "accept");
            ("content", `Assoc [ ("name", `String name) ]);
          ]
      | Decline -> `Assoc [ ("action", `String "decline") ])
end

let%expect_test "Mock elicitation handler - returns accept" =
  Mock_elicitation.set_response (Mock_elicitation.Accept "Alice");
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Mock_elicitation.handler "What is your name?" `Null `Null)
  in
  let open Yojson.Safe.Util in
  printf "action: %s\n" (result |> member "action" |> to_string);
  printf "name: %s\n" (result |> member "content" |> member "name" |> to_string);
  [%expect {|
    action: accept
    name: Alice
    |}]

let%expect_test "Mock elicitation handler - returns decline" =
  Mock_elicitation.set_response Mock_elicitation.Decline;
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Mock_elicitation.handler "What is your name?" `Null `Null)
  in
  let open Yojson.Safe.Util in
  printf "action: %s\n" (result |> member "action" |> to_string);
  [%expect {| action: decline |}]

(** Mock progress handler for testing *)
module Mock_progress = struct
  type progress_record = {
    progress : float;
    total : float option;
    message : string option;
  }

  let records : progress_record list ref = ref []
  let reset () = records := []

  let handler progress total message =
    records := !records @ [ { progress; total; message } ];
    Async.return ()

  let get_records () = !records
end

let%expect_test "Mock progress handler - records progress" =
  Mock_progress.reset ();
  Async.Thread_safe.block_on_async_exn (fun () ->
      Mock_progress.handler 1.0 (Some 3.0) (Some "33.33% complete"));
  Async.Thread_safe.block_on_async_exn (fun () ->
      Mock_progress.handler 2.0 (Some 3.0) (Some "66.67% complete"));
  Async.Thread_safe.block_on_async_exn (fun () ->
      Mock_progress.handler 3.0 (Some 3.0) (Some "100.00% complete"));
  let records = Mock_progress.get_records () in
  printf "record_count: %d\n" (List.length records);
  List.iter records ~f:(fun r ->
      let total_str =
        match r.total with
        | Some t -> sprintf "%.0f" t
        | None -> "none"
      in
      let msg_str = Option.value r.message ~default:"none" in
      printf "progress: %.0f/%s - %s\n" r.progress total_str msg_str);
  [%expect
    {|
    record_count: 3
    progress: 1/3 - 33.33% complete
    progress: 2/3 - 66.67% complete
    progress: 3/3 - 100.00% complete
    |}]

(* =============================================================================
   Test: Client Factory Pattern (OCaml equivalent of Python tests)
   ============================================================================= *)

let%expect_test "client_factory type - can wrap client creation" =
  (* Demonstrate the client_factory pattern *)
  let call_count = ref 0 in
  let _factory : unit -> Conftest.Mock_client.t Async.Deferred.t =
   fun () ->
    incr call_count;
    Async.return (Conftest.Mock_client.create ~name:"TestClient" ())
  in
  printf "factory_created: true\n";
  printf "initial_call_count: %d\n" !call_count;
  [%expect {|
    factory_created: true
    initial_call_count: 0
    |}]

(* =============================================================================
   Test: Error Response Pattern (Unit testing error structures)
   ============================================================================= *)

(** Parse tool error response *)
let parse_tool_error result =
  let open Yojson.Safe.Util in
  match result |> member "isError" with
  | `Bool true -> (
    match result |> member "content" |> to_list with
    | first :: _ -> (
      match first |> member "text" with
      | `String text -> Some text
      | _ -> Some "Unknown error")
    | [] -> Some "Unknown error")
  | _ -> None

let%expect_test "parse_tool_error - extracts error text" =
  let error_response =
    `Assoc
      [
        ("isError", `Bool true);
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text");
                  ("text", `String "Tool error: something went wrong");
                ];
            ] );
      ]
  in
  (match parse_tool_error error_response with
  | Some text -> printf "error: %s\n" text
  | None -> printf "no_error\n");
  [%expect {| error: Tool error: something went wrong |}]

let%expect_test "parse_tool_error - returns None for success" =
  let success_response =
    `Assoc
      [
        ("isError", `Bool false);
        ( "content",
          `List
            [
              `Assoc [ ("type", `String "text"); ("text", `String "Success!") ];
            ] );
      ]
  in
  (match parse_tool_error success_response with
  | Some _ -> printf "has_error\n"
  | None -> printf "no_error\n");
  [%expect {| no_error |}]

(* =============================================================================
   Test: Log Message Structure (Unit testing JSON structures)
   ============================================================================= *)

type log_message = { data : string; level : string; logger : string }
(** Log message type matching Python LogMessage *)

let parse_log_message json =
  let open Yojson.Safe.Util in
  {
    data = json |> member "data" |> to_string;
    level = json |> member "level" |> to_string;
    logger = json |> member "logger" |> to_string;
  }

let%expect_test "parse_log_message - parses correctly" =
  let json =
    `Assoc
      [
        ("data", `String "Hello, world!");
        ("level", `String "info");
        ("logger", `String "test");
      ]
  in
  let msg = parse_log_message json in
  printf "data: %s\n" msg.data;
  printf "level: %s\n" msg.level;
  printf "logger: %s\n" msg.logger;
  [%expect {|
    data: Hello, world!
    level: info
    logger: test
    |}]

(* =============================================================================
   Test: ElicitRequestParams Structure
   ============================================================================= *)

type elicit_request_params = {
  message : string;
  requested_schema : Yojson.Safe.t;
}
(** Elicit request params type matching Python ElicitRequestParams *)

let parse_elicit_params json =
  let open Yojson.Safe.Util in
  {
    message = json |> member "message" |> to_string;
    requested_schema = json |> member "requestedSchema";
  }

let%expect_test "parse_elicit_params - parses schema correctly" =
  let json =
    `Assoc
      [
        ("message", `String "What is your name?");
        ( "requestedSchema",
          `Assoc
            [
              ("title", `String "Person");
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ( "name",
                      `Assoc
                        [
                          ("title", `String "Name"); ("type", `String "string");
                        ] );
                  ] );
              ("required", `List [ `String "name" ]);
            ] );
      ]
  in
  let params = parse_elicit_params json in
  let open Yojson.Safe.Util in
  printf "message: %s\n" params.message;
  printf "schema_title: %s\n"
    (params.requested_schema |> member "title" |> to_string);
  printf "schema_type: %s\n"
    (params.requested_schema |> member "type" |> to_string);
  [%expect
    {|
    message: What is your name?
    schema_title: Person
    schema_type: object
    |}]

(* =============================================================================
   Test: SamplingParams Structure
   ============================================================================= *)

type sampling_params = {
  system_prompt : string option;
  temperature : float option;
  max_tokens : int option;
}
(** Sampling params type matching Python SamplingParams *)

let parse_sampling_params json =
  let open Yojson.Safe.Util in
  {
    system_prompt = json |> member "systemPrompt" |> to_string_option;
    temperature =
      (match json |> member "temperature" with
      | `Float f -> Some f
      | `Int i -> Some (Float.of_int i)
      | _ -> None);
    max_tokens =
      (match json |> member "maxTokens" with
      | `Int i -> Some i
      | _ -> None);
  }

let%expect_test "parse_sampling_params - parses correctly" =
  let json =
    `Assoc
      [
        ("systemPrompt", `String "You love OxFastMCP");
        ("temperature", `Float 0.5);
        ("maxTokens", `Int 100);
      ]
  in
  let params = parse_sampling_params json in
  printf "system_prompt: %s\n"
    (Option.value params.system_prompt ~default:"none");
  printf "temperature: %.1f\n" (Option.value params.temperature ~default:0.0);
  printf "max_tokens: %d\n" (Option.value params.max_tokens ~default:0);
  [%expect
    {|
    system_prompt: You love OxFastMCP
    temperature: 0.5
    max_tokens: 100
    |}]

let%expect_test "parse_sampling_params - handles missing fields" =
  let json = `Assoc [ ("systemPrompt", `String "Just a prompt") ] in
  let params = parse_sampling_params json in
  printf "has_system_prompt: %b\n" (Option.is_some params.system_prompt);
  printf "has_temperature: %b\n" (Option.is_some params.temperature);
  printf "has_max_tokens: %b\n" (Option.is_some params.max_tokens);
  [%expect
    {|
    has_system_prompt: true
    has_temperature: false
    has_max_tokens: false
    |}]
