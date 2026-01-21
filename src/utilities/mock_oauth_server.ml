(** Mock OAuth Server for Testing

    A simple HTTP server that mimics OAuth 2.0 authorization server endpoints
    for use in integration tests. *)

open Core
open Async

(** {1 Types} *)

type t = {
  port : int;
  mutable valid_codes : (string, string) List.Assoc.t; (* code -> client_id *)
  mutable issued_tokens : string list;
  mutable running : bool;
}

type token_response = {
  access_token : string;
  token_type : string;
  expires_in : int;
  refresh_token : string option;
  scope : string option;
}

let yojson_of_token_response r =
  let fields =
    [
      ("access_token", `String r.access_token);
      ("token_type", `String r.token_type);
      ("expires_in", `Int r.expires_in);
    ]
  in
  let fields =
    match r.refresh_token with
    | Some rt -> ("refresh_token", `String rt) :: fields
    | None -> fields
  in
  let fields =
    match r.scope with
    | Some s -> ("scope", `String s) :: fields
    | None -> fields
  in
  `Assoc fields

type error_response = { error : string; error_description : string option }

let yojson_of_error_response e =
  let fields = [ ("error", `String e.error) ] in
  let fields =
    match e.error_description with
    | Some desc -> ("error_description", `String desc) :: fields
    | None -> fields
  in
  `Assoc fields

(** {1 Mock Server Implementation} *)

let create ~port () =
  { port; valid_codes = []; issued_tokens = []; running = false }

let add_valid_code t ~code ~client_id =
  t.valid_codes <- (code, client_id) :: t.valid_codes

let oauth_metadata ~issuer =
  `Assoc
    [
      ("issuer", `String issuer);
      ("authorization_endpoint", `String (sprintf "%s/authorize" issuer));
      ("token_endpoint", `String (sprintf "%s/token" issuer));
      ("registration_endpoint", `String (sprintf "%s/register" issuer));
      ("response_types_supported", `List [ `String "code" ]);
      ( "grant_types_supported",
        `List [ `String "authorization_code"; `String "refresh_token" ] );
      ("code_challenge_methods_supported", `List [ `String "S256" ]);
    ]

let handle_token_request t ~body =
  (* Parse form-encoded body *)
  let params = Uri.query_of_encoded body in
  let get_param name =
    List.Assoc.find params name ~equal:String.equal |> Option.bind ~f:List.hd
  in

  let grant_type = get_param "grant_type" in
  let code = get_param "code" in
  let _client_id = get_param "client_id" in

  match grant_type with
  | Some "authorization_code" -> (
    match code with
    | Some c when List.Assoc.mem t.valid_codes c ~equal:String.equal ->
      (* Valid code - issue token *)
      let access_token =
        sprintf "mock_access_%s"
          (Mcp_client_auth.Crypto_utils.generate_state_token ())
      in
      let refresh_token =
        sprintf "mock_refresh_%s"
          (Mcp_client_auth.Crypto_utils.generate_state_token ())
      in
      t.issued_tokens <- access_token :: t.issued_tokens;
      (* Remove used code (one-time use) *)
      t.valid_codes <- List.Assoc.remove t.valid_codes c ~equal:String.equal;
      let response =
        {
          access_token;
          token_type = "Bearer";
          expires_in = 3600;
          refresh_token = Some refresh_token;
          scope = Some "read write";
        }
      in
      (`OK, yojson_of_token_response response)
    | Some _ ->
      let error =
        {
          error = "invalid_grant";
          error_description = Some "Invalid authorization code";
        }
      in
      (`Bad_request, yojson_of_error_response error)
    | None ->
      let error =
        {
          error = "invalid_request";
          error_description = Some "Missing code parameter";
        }
      in
      (`Bad_request, yojson_of_error_response error))
  | Some "refresh_token" ->
    (* Issue new token for refresh *)
    let access_token =
      sprintf "mock_refreshed_%s"
        (Mcp_client_auth.Crypto_utils.generate_state_token ())
    in
    t.issued_tokens <- access_token :: t.issued_tokens;
    let response =
      {
        access_token;
        token_type = "Bearer";
        expires_in = 3600;
        refresh_token = get_param "refresh_token";
        scope = Some "read write";
      }
    in
    (`OK, yojson_of_token_response response)
  | Some gt ->
    let error =
      {
        error = "unsupported_grant_type";
        error_description = Some (sprintf "Unsupported: %s" gt);
      }
    in
    (`Bad_request, yojson_of_error_response error)
  | None ->
    let error =
      {
        error = "invalid_request";
        error_description = Some "Missing grant_type";
      }
    in
    (`Bad_request, yojson_of_error_response error)

let handle_registration t =
  (* Dynamic client registration - return mock client info *)
  ignore t;
  let client_id =
    sprintf "mock_client_%s"
      (String.prefix (Mcp_client_auth.Crypto_utils.generate_state_token ()) 8)
  in
  let client_secret =
    sprintf "mock_secret_%s"
      (Mcp_client_auth.Crypto_utils.generate_state_token ())
  in
  let response =
    `Assoc
      [
        ("client_id", `String client_id);
        ("client_secret", `String client_secret);
        ("token_endpoint_auth_method", `String "client_secret_post");
      ]
  in
  (`Created, response)

let start t =
  let issuer = sprintf "http://localhost:%d" t.port in

  let handler ~body _sock request =
    let uri = Cohttp.Request.uri request in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth request in

    let%bind body_str = Cohttp_async.Body.to_string body in

    let status, response_body =
      match (meth, path) with
      | `GET, "/.well-known/oauth-authorization-server" ->
        (`OK, oauth_metadata ~issuer)
      | `GET, "/.well-known/openid-configuration" ->
        (`OK, oauth_metadata ~issuer)
      | `POST, "/token" -> handle_token_request t ~body:body_str
      | `POST, "/register" -> handle_registration t
      | `GET, "/authorize" ->
        (* Authorization endpoint - in real flow, would show login page *)
        (* For mock, we just return info about expected redirect *)
        let redirect_uri = Uri.get_query_param uri "redirect_uri" in
        let state = Uri.get_query_param uri "state" in
        let info =
          `Assoc
            [
              ( "message",
                `String
                  "Mock auth endpoint - use add_valid_code to simulate user \
                   auth" );
              ( "redirect_uri",
                `String (Option.value redirect_uri ~default:"none") );
              ("state", `String (Option.value state ~default:"none"));
            ]
        in
        (`OK, info)
      | _ -> (`Not_found, `Assoc [ ("error", `String "Not found") ])
    in

    let headers =
      Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
    in
    let body = Yojson.Safe.to_string response_body in
    Cohttp_async.Server.respond_string ~headers ~status body
  in

  let%bind _server =
    Cohttp_async.Server.create ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port t.port)
      handler
  in
  t.running <- true;
  Logs.info (fun m ->
      m "Mock OAuth server started on http://localhost:%d" t.port);
  return ()

let stop t =
  (* Note: In a full implementation, we'd track the server and close it. For
     tests, server runs until test process exits. *)
  t.running <- false;
  Logs.info (fun m -> m "Mock OAuth server marked as stopped");
  return ()

let is_token_valid t ~token = List.mem t.issued_tokens token ~equal:String.equal
let get_issuer t = sprintf "http://localhost:%d" t.port
