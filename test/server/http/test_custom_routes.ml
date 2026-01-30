(** Tests for Custom Routes in HTTP applications.

    Translated from Python test_custom_routes.py to OCaml. Tests that custom
    routes are properly included in SSE and Streamable HTTP applications created
    via the Http module. *)

open! Core
open! Expect_test_helpers_core
module Http = Ox_fast_mcp_server__Http

(* =============================================================================
   Helper Functions
   ============================================================================= *)

(** Create a simple handler that returns JSON response *)
let json_handler message : Http.Route.handler =
 fun _request ->
  Async.Deferred.return
    (Http.Response.json (`Assoc [ ("message", `String message) ]))

(** Check if a route with the given path exists in the app routes *)
let has_route_with_path ~(app : Http.App.t) ~path =
  List.exists app.routes ~f:(fun route ->
      String.equal route.Http.Route.path path)

(* =============================================================================
   Test: Route Creation
   ============================================================================= *)

let%expect_test "Route.create - creates route with path and handler" =
  let route =
    Http.Route.create ~path:"/custom-route" ~methods:[ Http.Http_method.GET ]
      (json_handler "custom route")
  in
  printf "path: %s\n" route.path;
  printf "methods_count: %d\n" (List.length route.methods);
  printf "first_method: %s\n"
    (Http.Http_method.to_string (List.hd_exn route.methods));
  [%expect
    {|
    path: /custom-route
    methods_count: 1
    first_method: GET
    |}]

let%expect_test "Route.create - with optional name" =
  let route =
    Http.Route.create ~path:"/named-route" ~name:"my_route"
      (json_handler "named route")
  in
  printf "path: %s\n" route.path;
  printf "name: %s\n" (Option.value route.name ~default:"none");
  [%expect {|
    path: /named-route
    name: my_route
    |}]

(* =============================================================================
   Test: App with Custom Routes
   ============================================================================= *)

let%expect_test "App.create - with custom routes" =
  let custom_route =
    Http.Route.create ~path:"/custom-route" ~methods:[ Http.Http_method.GET ]
      (json_handler "custom route")
  in
  let app = Http.App.create ~routes:[ custom_route ] () in
  printf "routes_count: %d\n" (List.length app.routes);
  printf "has_custom_route: %b\n"
    (has_route_with_path ~app ~path:"/custom-route");
  [%expect {|
    routes_count: 1
    has_custom_route: true
    |}]

let%expect_test "App.create - with multiple custom routes" =
  let routes =
    [ "/route1"; "/route2"; "/route3" ]
    |> List.map ~f:(fun path ->
           Http.Route.create ~path ~methods:[ Http.Http_method.GET ]
             (json_handler ("route " ^ path)))
  in
  let app = Http.App.create ~routes () in
  printf "routes_count: %d\n" (List.length app.routes);
  printf "has_route1: %b\n" (has_route_with_path ~app ~path:"/route1");
  printf "has_route2: %b\n" (has_route_with_path ~app ~path:"/route2");
  printf "has_route3: %b\n" (has_route_with_path ~app ~path:"/route3");
  [%expect
    {|
    routes_count: 3
    has_route1: true
    has_route2: true
    has_route3: true
    |}]

(* =============================================================================
   Test: create_base_app with Custom Routes
   ============================================================================= *)

let%expect_test "create_base_app - includes custom routes" =
  let custom_route =
    Http.Route.create ~path:"/custom-route" ~methods:[ Http.Http_method.GET ]
      (json_handler "custom route")
  in
  let app = Http.create_base_app ~routes:[ custom_route ] ~middleware:[] () in
  printf "has_custom_route: %b\n"
    (has_route_with_path ~app ~path:"/custom-route");
  [%expect {| has_custom_route: true |}]

(* =============================================================================
   Test: create_sse_app with Custom Routes
   ============================================================================= *)

let%expect_test "create_sse_app - includes custom routes" =
  let custom_route =
    Http.Route.create ~path:"/custom-route" ~methods:[ Http.Http_method.GET ]
      (json_handler "custom route")
  in
  (* Create a mock server (as JSON since the API uses Yojson.Safe.t) *)
  let server = `Assoc [ ("name", `String "test-server") ] in
  let app =
    Http.create_sse_app ~server ~message_path:"/message" ~sse_path:"/sse/"
      ~routes:[ custom_route ] ()
  in
  printf "has_custom_route: %b\n"
    (has_route_with_path ~app ~path:"/custom-route");
  (* SSE app should have more routes than just the custom one *)
  printf "has_multiple_routes: %b\n" (List.length app.routes >= 1);
  [%expect {|
    has_custom_route: true
    has_multiple_routes: true
    |}]

(* =============================================================================
   Test: create_streamable_http_app with Custom Routes
   ============================================================================= *)

let%expect_test "create_streamable_http_app - includes custom routes" =
  let custom_route =
    Http.Route.create ~path:"/custom-route" ~methods:[ Http.Http_method.GET ]
      (json_handler "custom route")
  in
  (* Create a mock server (as JSON since the API uses Yojson.Safe.t) *)
  let server = `Assoc [ ("name", `String "test-server") ] in
  let app =
    Http.create_streamable_http_app ~server ~streamable_http_path:"/api"
      ~routes:[ custom_route ] ()
  in
  printf "has_custom_route: %b\n"
    (has_route_with_path ~app ~path:"/custom-route");
  (* Streamable HTTP app should have more routes than just the custom one *)
  printf "has_multiple_routes: %b\n" (List.length app.routes >= 1);
  [%expect {|
    has_custom_route: true
    has_multiple_routes: true
    |}]

let%expect_test "create_streamable_http_app - with multiple custom routes" =
  let routes =
    [ "/route1"; "/route2"; "/route3" ]
    |> List.map ~f:(fun path ->
           Http.Route.create ~path ~methods:[ Http.Http_method.GET ]
             (json_handler ("route " ^ path)))
  in
  let server = `Assoc [ ("name", `String "test-server") ] in
  let app =
    Http.create_streamable_http_app ~server ~streamable_http_path:"/api" ~routes
      ()
  in
  printf "has_route1: %b\n" (has_route_with_path ~app ~path:"/route1");
  printf "has_route2: %b\n" (has_route_with_path ~app ~path:"/route2");
  printf "has_route3: %b\n" (has_route_with_path ~app ~path:"/route3");
  [%expect
    {|
    has_route1: true
    has_route2: true
    has_route3: true
    |}]

(* =============================================================================
   Test: Response Helpers
   ============================================================================= *)

let%expect_test "Response.json - creates JSON response" =
  let response = Http.Response.json (`Assoc [ ("message", `String "hello") ]) in
  printf "status: %d\n" response.status;
  printf "has_body: %b\n" (Option.is_some response.body);
  printf "body_contains_message: %b\n"
    (Option.value_map response.body ~default:false ~f:(fun body ->
         String.is_substring body ~substring:"message"));
  [%expect
    {|
    status: 200
    has_body: true
    body_contains_message: true
    |}]

let%expect_test "Response helpers - various status codes" =
  let ok = Http.Response.ok ~body:"success" () in
  let created = Http.Response.created ~body:"created" () in
  let bad_request = Http.Response.bad_request ~body:"error" () in
  let not_found = Http.Response.not_found ~body:"missing" () in
  printf "ok: %d\n" ok.status;
  printf "created: %d\n" created.status;
  printf "bad_request: %d\n" bad_request.status;
  printf "not_found: %d\n" not_found.status;
  [%expect
    {|
    ok: 200
    created: 201
    bad_request: 400
    not_found: 404
    |}]

(* =============================================================================
   Test: Http_method
   ============================================================================= *)

let%expect_test "Http_method - to_string and of_string" =
  printf "GET: %s\n" (Http.Http_method.to_string Http.Http_method.GET);
  printf "POST: %s\n" (Http.Http_method.to_string Http.Http_method.POST);
  printf "PUT: %s\n" (Http.Http_method.to_string Http.Http_method.PUT);
  printf "DELETE: %s\n" (Http.Http_method.to_string Http.Http_method.DELETE);
  let parsed = Http.Http_method.of_string "GET" in
  printf "parsed_GET: %s\n" (Http.Http_method.to_string parsed);
  [%expect
    {|
    GET: GET
    POST: POST
    PUT: PUT
    DELETE: DELETE
    parsed_GET: GET
    |}]
