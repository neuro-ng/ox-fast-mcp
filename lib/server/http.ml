open Core
open Cohttp
open Lwt
open Lwt.Syntax
open Mcp.Server.Auth.Middleware

(** Types *)
type app = {
  routes: route_list;
  middleware: middleware_list;
  debug: bool;
  lifespan: (unit -> unit Lwt.t) option;
}

and route = {
  path: string;
  methods: string list;
  handler: (Request.t -> (Response.t * Body.t) Lwt.t);
}

and route_list = route list

and middleware = {
  name: string;
  handler: (Request.t -> (Response.t * Body.t) Lwt.t) -> (Request.t -> (Response.t * Body.t) Lwt.t);
}

and middleware_list = middleware list

(** Current HTTP request context *)
let current_http_request : Request.t option Lwt.key =
  Lwt.new_key ()

(** Set the current HTTP request in context *)
let with_http_request request f =
  Lwt.with_value current_http_request (Some request) f

(** Request context middleware *)
let request_context_middleware = {
  name = "request_context";
  handler = fun next request ->
    with_http_request request (fun () -> next request);
}

(** Setup auth middleware and routes *)
let setup_auth_middleware_and_routes ~auth =
  let open Mcp.Server.Auth.Provider in
  let middleware = [
    { name = "bearer_auth";
      handler = (fun next request ->
        let* auth_result = Bearer_auth_backend.authenticate 
          { token_verifier = (module auth : TOKEN_VERIFIER) }
          request
        in
        Auth_context_middleware.handle auth_result request (next request)
      )
    }
  ] in

  let required_scopes = auth.required_scopes in

  let auth_routes = [
    { path = "/.well-known/oauth-authorization-server";
      methods = ["GET"];
      handler = (fun _req ->
        let body = `Assoc [
          ("issuer", `String auth.issuer_url);
          ("service_documentation", match auth.service_documentation_url with
            | Some url -> `String url
            | None -> `Null);
        ] in
        let body_string = Yojson.Safe.to_string body in
        let headers = Header.init_with [
          ("Content-Type", "application/json");
          ("Content-Length", string_of_int (String.length body_string));
        ] in
        let response = Response.make ~status:`OK ~headers () in
        Lwt.return (response, Body.of_string body_string)
      )
    }
  ] in

  (middleware, auth_routes, required_scopes)

(** Create a base application *)
let create_base_app ~routes ~middleware ?(debug=false) ?lifespan () =
  let middleware = middleware @ [request_context_middleware] in
  { routes; middleware; debug; lifespan }

(** Create an SSE application *)
let create_sse_app ~server ~message_path ~sse_path ?auth ?(debug=false) ?routes ?middleware () =
  let message_path = if String.is_suffix ~suffix:"/" message_path
    then message_path else message_path ^ "/" in

  let server_routes = ref [] in
  let server_middleware = ref [] in

  (* Set up SSE transport *)
  let sse = Mcp.Server.Sse.create ~message_path () in

  (* SSE connection handler *)
  let handle_sse request =
    let* streams = Mcp.Server.Sse.connect_sse sse request in
    let* () = Mcp.Server.run server streams in
    Lwt.return (Response.make ~status:`OK (), Body.empty)
  in

  (* Add routes and middleware based on auth *)
  begin match auth with
  | Some auth ->
    let auth_middleware, auth_routes, required_scopes = setup_auth_middleware_and_routes ~auth in
    server_routes := !server_routes @ auth_routes;
    server_middleware := !server_middleware @ auth_middleware;
    
    (* Add protected SSE routes *)
    server_routes := !server_routes @ [
      { path = sse_path;
        methods = ["GET"];
        handler = (fun request ->
          let* auth_result = Bearer_auth_backend.authenticate 
            { token_verifier = (module auth : Mcp.Server.Auth.Provider.TOKEN_VERIFIER) }
            request
          in
          Require_auth_middleware.handle
            { required_scopes; resource_metadata_url = None }
            auth_result
            request
            (handle_sse request)
        )
      };
      { path = message_path;
        methods = ["POST"];
        handler = (fun request ->
          let* auth_result = Bearer_auth_backend.authenticate
            { token_verifier = (module auth : Mcp.Server.Auth.Provider.TOKEN_VERIFIER) }
            request
          in
          Require_auth_middleware.handle
            { required_scopes; resource_metadata_url = None }
            auth_result
            request
            (Mcp.Server.Sse.handle_post_message sse request)
        )
      }
    ]

  | None ->
    (* Add unprotected SSE routes *)
    server_routes := !server_routes @ [
      { path = sse_path;
        methods = ["GET"];
        handler = handle_sse
      };
      { path = message_path;
        methods = ["POST"];
        handler = Mcp.Server.Sse.handle_post_message sse
      }
    ]
  end;

  (* Add custom routes *)
  begin match routes with
  | Some r -> server_routes := !server_routes @ r
  | None -> ()
  end;

  (* Add custom middleware *)
  begin match middleware with
  | Some m -> server_middleware := !server_middleware @ m
  | None -> ()
  end;

  create_base_app
    ~routes:!server_routes
    ~middleware:!server_middleware
    ~debug
    ()

(** Create a streamable HTTP application *)
let create_streamable_http_app
    ~server
    ~streamable_http_path
    ?event_store
    ?auth
    ?(json_response=false)
    ?(stateless_http=false)
    ?(debug=false)
    ?routes
    ?middleware
    () =
  let streamable_http_path = if String.is_suffix ~suffix:"/" streamable_http_path
    then streamable_http_path else streamable_http_path ^ "/" in

  let server_routes = ref [] in
  let server_middleware = ref [] in

  (* Create session manager *)
  let session_manager = Mcp.Server.Streamable_http.create_session_manager
    ~server
    ?event_store
    ~json_response
    ~stateless:stateless_http
    ()
  in

  (* Streamable HTTP handler *)
  let handle_streamable_http request =
    try%lwt
      Mcp.Server.Streamable_http.handle_request session_manager request
    with
    | Runtime_error msg as e ->
      if String.equal msg "Task group is not initialized. Make sure to use run()." then
        let new_msg = "FastMCP's StreamableHTTPSessionManager task group was not initialized. \
          This commonly occurs when the FastMCP application's lifespan is not \
          passed to the parent ASGI application (e.g., FastAPI or Starlette). \
          Please ensure you are setting `lifespan=mcp_app.lifespan` in your \
          parent app's constructor, where `mcp_app` is the application instance \
          returned by `fastmcp_instance.http_app()`. \n\
          For more details, see the FastMCP ASGI integration documentation: \
          https://gofastmcp.com/deployment/asgi\n\
          Original error: " ^ msg in
        Lwt.fail (Runtime_error new_msg)
      else
        Lwt.fail e
  in

  (* Add routes and middleware based on auth *)
  begin match auth with
  | Some auth ->
    let auth_middleware, auth_routes, required_scopes = setup_auth_middleware_and_routes ~auth in
    server_routes := !server_routes @ auth_routes;
    server_middleware := !server_middleware @ auth_middleware;
    
    (* Add protected streamable HTTP route *)
    server_routes := !server_routes @ [
      { path = streamable_http_path;
        methods = ["POST"; "GET"];
        handler = (fun request ->
          let* auth_result = Bearer_auth_backend.authenticate
            { token_verifier = (module auth : Mcp.Server.Auth.Provider.TOKEN_VERIFIER) }
            request
          in
          Require_auth_middleware.handle
            { required_scopes; resource_metadata_url = None }
            auth_result
            request
            (handle_streamable_http request)
        )
      }
    ]

  | None ->
    (* Add unprotected streamable HTTP route *)
    server_routes := !server_routes @ [
      { path = streamable_http_path;
        methods = ["POST"; "GET"];
        handler = handle_streamable_http
      }
    ]
  end;

  (* Add custom routes *)
  begin match routes with
  | Some r -> server_routes := !server_routes @ r
  | None -> ()
  end;

  (* Add custom middleware *)
  begin match middleware with
  | Some m -> server_middleware := !server_middleware @ m
  | None -> ()
  end;

  (* Create lifespan function *)
  let lifespan () =
    let* () = Mcp.Server.Streamable_http.start session_manager in
    let* () = Lwt_unix.sleep 0.1 in (* Give time for startup *)
    let* () = Mcp.Server.Streamable_http.stop session_manager in
    Lwt.return_unit
  in

  create_base_app
    ~routes:!server_routes
    ~middleware:!server_middleware
    ~debug
    ~lifespan
    () 