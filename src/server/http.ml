(** HTTP Application Module for OxFastMCP

    Provides HTTP application types and utilities for creating SSE and
    Streamable HTTP server applications. This module handles request context
    management, authentication middleware integration, and ASGI-style
    application creation.

    Note: This is a Jane Street style implementation using Core and Async. *)

open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 Types} *)

(** HTTP methods *)
module Http_method = struct
  type t = GET | POST | PUT | DELETE | PATCH | OPTIONS | HEAD
  [@@deriving sexp, compare, equal, enumerate]

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.of_string s)
  let yojson_of_t t = `String (to_string t)
  let t_of_yojson json = of_string (string_of_yojson json)
end

(** HTTP request representation *)
module Request = struct
  type t = {
    method_ : Http_method.t;
    path : string;
    headers : (string * string) list;
    query_params : (string * string) list;
    body : string option;
    scope : Yojson.Safe.t option;
  }

  let create ~method_ ~path ?(headers = []) ?(query_params = []) ?body ?scope ()
      =
    { method_; path; headers; query_params; body; scope }

  let get_header t ~name =
    List.find_map t.headers ~f:(fun (k, v) ->
        if String.Caseless.equal k name then Some v else None)

  let get_query_param t ~name =
    List.Assoc.find t.query_params ~equal:String.equal name
end

(** HTTP response representation *)
module Response = struct
  type t = {
    status : int;
    headers : (string * string) list;
    body : string option;
  }
  [@@deriving sexp]

  let create ?(status = 200) ?(headers = []) ?body () =
    { status; headers; body }

  let ok ?body () = create ~status:200 ?body ()
  let created ?body () = create ~status:201 ?body ()
  let no_content () = create ~status:204 ()
  let bad_request ?body () = create ~status:400 ?body ()
  let unauthorized ?body () = create ~status:401 ?body ()
  let forbidden ?body () = create ~status:403 ?body ()
  let not_found ?body () = create ~status:404 ?body ()
  let internal_server_error ?body () = create ~status:500 ?body ()

  let json ?(status = 200) (json : Yojson.Safe.t) =
    let body = Yojson.Safe.to_string json in
    create ~status ~headers:[ ("Content-Type", "application/json") ] ~body ()
end

(** Route definition *)
module Route = struct
  type handler = Request.t -> Response.t Deferred.t

  type t = {
    path : string;
    methods : Http_method.t list;
    handler : handler;
    name : string option;
  }

  let create ~path ?(methods = [ Http_method.GET ]) ?name handler =
    { path; methods; handler; name }
end

(** Middleware definition *)
module Middleware = struct
  type next = Request.t -> Response.t Deferred.t
  type t = next -> next

  let identity : t = fun next request -> next request

  let compose (middlewares : t list) : t =
    List.fold_right middlewares ~init:identity ~f:(fun mw acc next ->
        mw (acc next))
end

(** Application state *)
module App_state = struct
  type t = {
    mutable fastmcp_server : Yojson.Safe.t option;
    mutable path : string option;
    mutable custom_data : (string, Yojson.Safe.t) Hashtbl.t;
  }

  let create () =
    {
      fastmcp_server = None;
      path = None;
      custom_data = Hashtbl.create (module String);
    }

  let set_fastmcp_server t server = t.fastmcp_server <- Some server
  let get_fastmcp_server t = t.fastmcp_server
  let set_path t path = t.path <- Some path
  let get_path t = t.path
  let set_data t ~key ~data = Hashtbl.set t.custom_data ~key ~data
  let get_data t ~key = Hashtbl.find t.custom_data key
end

(** HTTP Application *)
module App = struct
  type lifespan = unit -> unit Deferred.t

  type t = {
    routes : Route.t list;
    middleware : Middleware.t list;
    debug : bool;
    lifespan : lifespan option;
    state : App_state.t;
  }

  let create ?(routes = []) ?(middleware = []) ?(debug = false) ?lifespan () =
    { routes; middleware; debug; lifespan; state = App_state.create () }

  let with_state t ~f =
    f t.state;
    t

  let get_lifespan t = t.lifespan
end

(** {1 Request Context Management} *)

(** Current HTTP request context variable *)
let current_http_request : Request.t option ref = ref None

(** Get the current HTTP request from context *)
let get_current_request () = !current_http_request

(** Set HTTP request in context for the duration of a function call *)
let with_http_request (request : Request.t) ~(f : unit -> 'a Deferred.t) :
    'a Deferred.t =
  let old_request = !current_http_request in
  current_http_request := Some request;
  Monitor.protect
    (fun () -> f ())
    ~finally:(fun () ->
      current_http_request := old_request;
      return ())

(** {1 Request Context Middleware} *)

(** Middleware that stores each request in a context variable *)
let request_context_middleware : Middleware.t =
 fun next request -> with_http_request request ~f:(fun () -> next request)

(** {1 Streamable HTTP ASGI App Wrapper} *)

(** ASGI application wrapper for Streamable HTTP server transport. Handles
    runtime errors and provides helpful error messages. *)
module Streamable_http_asgi_app = struct
  type session_manager = { handle_request : Request.t -> Response.t Deferred.t }
  type t = { session_manager : session_manager }

  let create ~session_manager = { session_manager }

  let handle_request t (request : Request.t) : Response.t Deferred.t =
    Monitor.try_with (fun () -> t.session_manager.handle_request request)
    >>= function
    | Ok response -> return response
    | Error exn ->
      let error_msg = Exn.to_string exn in
      if
        String.is_substring error_msg ~substring:"Task group is not initialized"
      then
        let new_error_message =
          "OxFastMCP's StreamableHTTPSessionManager task group was not \
           initialized. This commonly occurs when the OxFastMCP application's \
           lifespan is not passed to the parent ASGI application (e.g., a web \
           framework). Please ensure you are setting \
           `lifespan=mcp_app.lifespan` in your parent app's constructor, where \
           `mcp_app` is the application instance returned by \
           `oxfastmcp_instance.http_app()`. \n\
           For more details, see the OxFastMCP ASGI integration documentation.\n\
           Original error: " ^ error_msg
        in
        raise_s
          [%message "StreamableHTTP initialization error" new_error_message]
      else raise exn
end

(** {1 Application Creation} *)

(** Create a base application with common middleware and routes.

    @param routes List of routes to include in the app
    @param middleware List of middleware to include in the app
    @param debug Whether to enable debug mode
    @param lifespan Optional lifespan manager for the app
    @return A configured HTTP application *)
let create_base_app ~(routes : Route.t list) ~(middleware : Middleware.t list)
    ?(debug = false) ?(lifespan : App.lifespan option) () : App.t =
  (* Always add RequestContextMiddleware as the outermost middleware *)
  let middleware = request_context_middleware :: middleware in
  App.create ~routes ~middleware ~debug ?lifespan ()

(** Placeholder for SSE transport type *)
module Sse_transport = struct
  type t = { message_path : string }

  let create ~message_path = { message_path }

  let connect_sse _t _request : (unit -> unit Deferred.t) Deferred.t =
    (* TODO: Implement SSE connection handling *)
    return (fun () -> return ())

  let handle_post_message _t (request : Request.t) : Response.t Deferred.t =
    (* TODO: Implement SSE message handling *)
    let _ = request in
    return (Response.ok ())
end

(** Placeholder for session manager type *)
module Session_manager = struct
  type t = { json_response : bool; stateless : bool }

  let create ?(json_response = false) ?(stateless = false) () =
    { json_response; stateless }

  let run _t : (unit -> unit Deferred.t) Deferred.t =
    (* TODO: Implement session manager run *)
    return (fun () -> return ())
end

(** Authentication configuration placeholder *)
module Auth_config = struct
  type t = {
    required_scopes : string list;
    get_middleware : unit -> Middleware.t list;
    get_routes : mcp_path:string -> Route.t list;
    get_resource_url : string -> string option;
  }

  let get_middleware t = t.get_middleware ()
  let get_routes t ~mcp_path = t.get_routes ~mcp_path
  let get_resource_url t path = t.get_resource_url path
end

(** Build RFC 9728-compliant resource metadata URL *)
let build_resource_metadata_url (resource_url : string) : string =
  (* TODO: Implement proper RFC 9728 URL building *)
  resource_url ^ "/.well-known/oauth-protected-resource"

(** Require authentication middleware wrapper *)
let require_auth_middleware ~(handler : Route.handler)
    ~(required_scopes : string list) ~(resource_metadata_url : string option) :
    Route.handler =
  let _ = required_scopes in
  let _ = resource_metadata_url in
  (* TODO: Implement actual auth checking *)
  handler

(** Create an SSE (Server-Sent Events) application.

    @param server The OxFastMCP server instance (as JSON for now)
    @param message_path Path for SSE messages
    @param sse_path Path for SSE connections
    @param auth Optional authentication configuration
    @param debug Whether to enable debug mode
    @param routes Optional list of custom routes
    @param middleware Optional list of middleware
    @return A configured SSE HTTP application *)
let create_sse_app ~(server : Yojson.Safe.t) ~(message_path : string)
    ~(sse_path : string) ?(auth : Auth_config.t option) ?(debug = false)
    ?(routes : Route.t list option) ?(middleware : Middleware.t list option) ()
    : App.t =
  let server_routes = ref [] in
  let server_middleware = ref [] in

  (* Set up SSE transport *)
  let sse = Sse_transport.create ~message_path in

  (* Create handler for SSE connections *)
  let handle_sse (request : Request.t) : Response.t Deferred.t =
    let%bind _cleanup = Sse_transport.connect_sse sse request in
    (* TODO: Run MCP server with streams *)
    return (Response.ok ())
  in

  (* Set up auth if enabled *)
  (match auth with
  | Some auth_config ->
    (* Get auth middleware from the provider *)
    let auth_middleware = Auth_config.get_middleware auth_config in

    (* Get auth provider's own routes *)
    let auth_routes = Auth_config.get_routes auth_config ~mcp_path:sse_path in
    server_routes := !server_routes @ auth_routes;
    server_middleware := !server_middleware @ auth_middleware;

    (* Build RFC 9728-compliant metadata URL *)
    let resource_metadata_url =
      match Auth_config.get_resource_url auth_config sse_path with
      | Some url -> Some (build_resource_metadata_url url)
      | None -> None
    in

    (* Create protected SSE endpoint route *)
    let protected_sse_handler =
      require_auth_middleware ~handler:handle_sse
        ~required_scopes:auth_config.required_scopes ~resource_metadata_url
    in
    server_routes :=
      !server_routes
      @ [
          Route.create ~path:sse_path ~methods:[ Http_method.GET ]
            protected_sse_handler;
        ];

    (* Wrap the SSE message endpoint with auth *)
    let protected_message_handler =
      require_auth_middleware
        ~handler:(Sse_transport.handle_post_message sse)
        ~required_scopes:auth_config.required_scopes ~resource_metadata_url
    in
    server_routes :=
      !server_routes
      @ [
          Route.create ~path:message_path ~methods:[ Http_method.POST ]
            protected_message_handler;
        ]
  | None ->
    (* No auth required *)
    server_routes :=
      !server_routes
      @ [
          Route.create ~path:sse_path ~methods:[ Http_method.GET ] handle_sse;
          Route.create ~path:message_path ~methods:[ Http_method.POST ]
            (Sse_transport.handle_post_message sse);
        ]);

  (* Add custom routes with lowest precedence *)
  (match routes with
  | Some r -> server_routes := !server_routes @ r
  | None -> ());

  (* Add custom middleware *)
  (match middleware with
  | Some m -> server_middleware := !server_middleware @ m
  | None -> ());

  (* Create lifespan function *)
  let lifespan () =
    (* TODO: Implement proper lifespan with server lifecycle *)
    return ()
  in

  (* Create and return the app *)
  let app =
    create_base_app ~routes:!server_routes ~middleware:!server_middleware ~debug
      ~lifespan ()
  in

  (* Store the OxFastMCP server instance on the app state *)
  App_state.set_fastmcp_server app.state server;
  App_state.set_path app.state sse_path;

  app

(** Create a Streamable HTTP application.

    @param server The OxFastMCP server instance (as JSON for now)
    @param streamable_http_path Path for Streamable HTTP connections
    @param event_store Optional event store for session management
    @param auth Optional authentication configuration
    @param json_response Whether to use JSON response format
    @param stateless_http
      Whether to use stateless mode (new transport per request)
    @param debug Whether to enable debug mode
    @param routes Optional list of custom routes
    @param middleware Optional list of middleware
    @return A Streamable HTTP application *)
let create_streamable_http_app ~(server : Yojson.Safe.t)
    ~(streamable_http_path : string) ?(_event_store : Yojson.Safe.t option)
    ?(auth : Auth_config.t option) ?(json_response = false)
    ?(stateless_http = false) ?(debug = false) ?(routes : Route.t list option)
    ?(middleware : Middleware.t list option) () : App.t =
  let server_routes = ref [] in
  let server_middleware = ref [] in

  (* Create session manager *)
  let session_manager =
    Session_manager.create ~json_response ~stateless:stateless_http ()
  in

  (* Create the ASGI app wrapper *)
  let streamable_http_app =
    Streamable_http_asgi_app.create
      ~session_manager:
        {
          handle_request =
            (fun _request ->
              (* TODO: Implement actual request handling *)
              return (Response.ok ()));
        }
  in

  (* Streamable HTTP handler *)
  let handle_streamable_http (request : Request.t) : Response.t Deferred.t =
    Streamable_http_asgi_app.handle_request streamable_http_app request
  in

  (* Set up auth if enabled *)
  (match auth with
  | Some auth_config ->
    (* Get auth middleware from the provider *)
    let auth_middleware = Auth_config.get_middleware auth_config in

    (* Get auth provider's own routes *)
    let auth_routes =
      Auth_config.get_routes auth_config ~mcp_path:streamable_http_path
    in
    server_routes := !server_routes @ auth_routes;
    server_middleware := !server_middleware @ auth_middleware;

    (* Build RFC 9728-compliant metadata URL *)
    let resource_metadata_url =
      match Auth_config.get_resource_url auth_config streamable_http_path with
      | Some url -> Some (build_resource_metadata_url url)
      | None -> None
    in

    (* Create protected HTTP endpoint route *)
    let protected_handler =
      require_auth_middleware ~handler:handle_streamable_http
        ~required_scopes:auth_config.required_scopes ~resource_metadata_url
    in
    server_routes :=
      !server_routes
      @ [
          Route.create ~path:streamable_http_path
            ~methods:[ Http_method.GET; Http_method.POST; Http_method.DELETE ]
            protected_handler;
        ]
  | None ->
    (* No auth required *)
    server_routes :=
      !server_routes
      @ [
          Route.create ~path:streamable_http_path
            ~methods:[ Http_method.GET; Http_method.POST; Http_method.DELETE ]
            handle_streamable_http;
        ]);

  (* Add custom routes with lowest precedence *)
  (match routes with
  | Some r -> server_routes := !server_routes @ r
  | None -> ());

  (* Add custom middleware *)
  (match middleware with
  | Some m -> server_middleware := !server_middleware @ m
  | None -> ());

  (* Create lifespan function that manages session manager lifecycle *)
  let lifespan () =
    let%bind _cleanup = Session_manager.run session_manager in
    (* TODO: Implement proper lifespan with server lifecycle *)
    return ()
  in

  (* Create and return the app with lifespan *)
  let app =
    create_base_app ~routes:!server_routes ~middleware:!server_middleware ~debug
      ~lifespan ()
  in

  (* Store the OxFastMCP server instance on the app state *)
  App_state.set_fastmcp_server app.state server;
  App_state.set_path app.state streamable_http_path;

  app
