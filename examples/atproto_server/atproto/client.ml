(** AT Protocol HTTP client with authentication and session management *)

open! Core
open! Async
open Atproto_types

type session = {
  did : string;  (** Decentralized identifier *)
  access_jwt : string;  (** Access token *)
  refresh_jwt : string;  (** Refresh token *)
  handle : string;  (** User handle *)
}
[@@deriving sexp]
(** AT Protocol session information *)

type t = { base_url : string; mutable session : session option }
(** Client state *)

(** Create a new client *)
let create ~base_url = { base_url; session = None }

(** Authenticate and create session *)
let login t ~handle ~password =
  let open Deferred.Let_syntax in
  let url = sprintf "%s/xrpc/com.atproto.server.createSession" t.base_url in
  let body =
    `Assoc [ ("identifier", `String handle); ("password", `String password) ]
    |> Yojson.Safe.to_string
  in

  let%bind resp, resp_body =
    Cohttp_async.Client.post
      ~body:(Cohttp_async.Body.of_string body)
      ~headers:(Cohttp.Header.of_list [ ("Content-Type", "application/json") ])
      (Uri.of_string url)
  in

  let status = Cohttp.Response.status resp in
  if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then
    let%bind error_body = Cohttp_async.Body.to_string resp_body in
    failwithf "Authentication failed with status %s: %s"
      (Cohttp.Code.string_of_status status)
      error_body ()
  else
    let%bind json_str = Cohttp_async.Body.to_string resp_body in
    let json = Yojson.Safe.from_string json_str in

    let session =
      {
        did = Yojson.Safe.Util.(member "did" json |> to_string);
        access_jwt = Yojson.Safe.Util.(member "accessJwt" json |> to_string);
        refresh_jwt = Yojson.Safe.Util.(member "refreshJwt" json |> to_string);
        handle = Yojson.Safe.Util.(member "handle" json |> to_string);
      }
    in

    t.session <- Some session;
    return session

(** Make authenticated GET request *)
let api_get t ~endpoint =
  let open Deferred.Let_syntax in
  match t.session with
  | None -> failwith "Client not authenticated. Call login first."
  | Some session ->
    let url = sprintf "%s/xrpc/%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [ ("Authorization", sprintf "Bearer %s" session.access_jwt) ]
    in

    let%bind resp, resp_body =
      Cohttp_async.Client.get ~headers (Uri.of_string url)
    in

    let status = Cohttp.Response.status resp in
    let%bind body_str = Cohttp_async.Body.to_string resp_body in

    if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then
      failwithf "API GET request to %s failed with status %s: %s" endpoint
        (Cohttp.Code.string_of_status status)
        body_str ()
    else return body_str

(** Make authenticated POST request *)
let api_post t ~endpoint ~body =
  let open Deferred.Let_syntax in
  match t.session with
  | None -> failwith "Client not authenticated. Call login first."
  | Some session ->
    let url = sprintf "%s/xrpc/%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [
          ("Content-Type", "application/json");
          ("Authorization", sprintf "Bearer %s" session.access_jwt);
        ]
    in

    let body_str = Yojson.Safe.to_string body in
    let%bind resp, resp_body =
      Cohttp_async.Client.post ~headers
        ~body:(Cohttp_async.Body.of_string body_str)
        (Uri.of_string url)
    in

    let status = Cohttp.Response.status resp in
    let%bind result_str = Cohttp_async.Body.to_string resp_body in

    if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then
      failwithf "API POST request to %s failed with status %s: %s" endpoint
        (Cohttp.Code.string_of_status status)
        result_str ()
    else return result_str

(** Make authenticated POST request with binary blob data **)
let api_post_blob t ~endpoint ~content_type ~body =
  let open Deferred.Let_syntax in
  match t.session with
  | None -> failwith "Client not authenticated. Call login first."
  | Some session ->
    let url = sprintf "%s%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [
          ("Content-Type", content_type);
          ("Authorization", sprintf "Bearer %s" session.access_jwt);
        ]
    in

    let%bind resp, resp_body =
      Cohttp_async.Client.post ~headers
        ~body:(Cohttp_async.Body.of_string body)
        (Uri.of_string url)
    in

    let status = Cohttp.Response.status resp in
    let%bind result_str = Cohttp_async.Body.to_string resp_body in

    if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then
      failwithf "Blob POST request to %s failed with status %s: %s" endpoint
        (Cohttp.Code.string_of_status status)
        result_str ()
    else return (Yojson.Safe.from_string result_str)

(** Singleton client instance *)
let _client : t option ref = ref None

(** Get or create authenticated client *)
let get_client settings =
  let open Deferred.Let_syntax in
  match !_client with
  | Some c -> return c
  | None ->
    let client = create ~base_url:settings.Settings.atproto_pds_url in
    let%bind _session =
      login client ~handle:settings.atproto_handle
        ~password:settings.atproto_password
    in
    _client := Some client;
    return client

(** Get current session (for testing/debugging) *)
let get_session t = t.session

(** Convenience wrappers using singleton client **)

let authenticated_get ~endpoint =
  let open Deferred.Let_syntax in
  let settings = Settings.get_settings () in
  let%bind client = get_client settings in
  api_get client ~endpoint

let authenticated_post ~endpoint ~body =
  let open Deferred.Let_syntax in
  let settings = Settings.get_settings () in
  let%bind client = get_client settings in
  api_post client ~endpoint ~body

let authenticated_post_blob ~endpoint ~content_type ~body =
  let open Deferred.Let_syntax in
  let settings = Settings.get_settings () in
  let%bind client = get_client settings in
  api_post_blob client ~endpoint ~content_type ~body
