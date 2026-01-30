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

type t = {
  base_url : string;
  mutable sessions : (string, session) Hashtbl.t;
  mutable active_handle : string option;
}
(** Client state *)

(** Create a new client *)
let create ~base_url =
  { base_url; sessions = Hashtbl.create (module String); active_handle = None }

(** Session storage path *)
let session_file = ".atproto_session.json"

(** Save sessions to disk *)
let save_session t =
  let sessions_json =
    Hashtbl.data t.sessions
    |> List.map ~f:(fun session ->
           `Assoc
             [
               ("did", `String session.did);
               ("accessJwt", `String session.access_jwt);
               ("refreshJwt", `String session.refresh_jwt);
               ("handle", `String session.handle);
             ])
  in
  let json =
    `Assoc
      [
        ("sessions", `List sessions_json);
        ( "active_handle",
          match t.active_handle with
          | None -> `Null
          | Some h -> `String h );
      ]
  in
  Writer.save session_file ~contents:(Yojson.Safe.to_string json)

(** Load sessions from disk *)
let load_session t =
  let open Deferred.Let_syntax in
  match%bind Sys.file_exists session_file with
  | `No | `Unknown -> return ()
  | `Yes -> (
    try
      let%bind contents = Reader.file_contents session_file in
      let json = Yojson.Safe.from_string contents in

      (* Handle legacy format or new format *)
      match Yojson.Safe.Util.member "sessions" json with
      | `Null -> (
        (* Try legacy format try *)
        try
          let did = Yojson.Safe.Util.(member "did" json |> to_string) in
          let access_jwt =
            Yojson.Safe.Util.(member "accessJwt" json |> to_string)
          in
          let refresh_jwt =
            Yojson.Safe.Util.(member "refreshJwt" json |> to_string)
          in
          let handle = Yojson.Safe.Util.(member "handle" json |> to_string) in
          let session = { did; access_jwt; refresh_jwt; handle } in
          Hashtbl.set t.sessions ~key:handle ~data:session;
          t.active_handle <- Some handle;
          return ()
        with _ -> return ())
      | `List sessions_list ->
        let active =
          Yojson.Safe.Util.(member "active_handle" json |> to_string_option)
        in
        t.active_handle <- active;
        List.iter sessions_list ~f:(fun s_json ->
            try
              let did = Yojson.Safe.Util.(member "did" s_json |> to_string) in
              let access_jwt =
                Yojson.Safe.Util.(member "accessJwt" s_json |> to_string)
              in
              let refresh_jwt =
                Yojson.Safe.Util.(member "refreshJwt" s_json |> to_string)
              in
              let handle =
                Yojson.Safe.Util.(member "handle" s_json |> to_string)
              in
              let session = { did; access_jwt; refresh_jwt; handle } in
              Hashtbl.set t.sessions ~key:handle ~data:session
            with _ -> ());
        return ()
      | _ -> return ()
    with _ -> return ())

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

    Hashtbl.set t.sessions ~key:session.handle ~data:session;
    t.active_handle <- Some session.handle;
    let%bind () = save_session t in
    return session

(** Get current active session *)
let get_session t =
  match t.active_handle with
  | None -> None
  | Some handle -> Hashtbl.find t.sessions handle

(** Switch active account *)
let switch_account t ~handle =
  if Hashtbl.mem t.sessions handle then (
    t.active_handle <- Some handle;
    let open Deferred.Let_syntax in
    let%bind () = save_session t in
    return true)
  else return false

(** List available accounts *)
let list_accounts t = Hashtbl.keys t.sessions

(** Refresh session using refresh token *)
let refresh_session t =
  let open Deferred.Let_syntax in
  match get_session t with
  | None -> return false
  | Some session -> (
    let url = sprintf "%s/xrpc/com.atproto.server.refreshSession" t.base_url in
    let headers =
      Cohttp.Header.of_list
        [ ("Authorization", sprintf "Bearer %s" session.refresh_jwt) ]
    in

    let%bind resp, resp_body =
      Cohttp_async.Client.post ~headers (Uri.of_string url)
    in

    let status = Cohttp.Response.status resp in
    if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then
      return false
    else
      let%bind json_str = Cohttp_async.Body.to_string resp_body in
      try
        let json = Yojson.Safe.from_string json_str in
        let new_access_jwt =
          Yojson.Safe.Util.(member "accessJwt" json |> to_string)
        in
        let new_refresh_jwt =
          Yojson.Safe.Util.(member "refreshJwt" json |> to_string)
        in
        (* Update session with new tokens *)
        let new_session =
          {
            session with
            access_jwt = new_access_jwt;
            refresh_jwt = new_refresh_jwt;
          }
        in
        Hashtbl.set t.sessions ~key:session.handle ~data:new_session;
        let%bind () = save_session t in
        return true
      with _ -> return false)

(** Make authenticated request with retry on 401 *)
let make_authenticated_request t ~req_fn =
  let open Deferred.Let_syntax in
  match get_session t with
  | None -> failwith "Client not authenticated. Call login first."
  | Some session ->
    let%bind resp, resp_body = req_fn session in
    let status = Cohttp.Response.status resp in

    (* If 401 Unauthorized, try to refresh token *)
    if Cohttp.Code.code_of_status status = 401 then
      let%bind refreshed = refresh_session t in
      if refreshed then
        (* Retry with new session *)
        match get_session t with
        | None -> failwith "Session lost during refresh"
        | Some new_session ->
          let%bind resp, resp_body = req_fn new_session in
          return (resp, resp_body)
      else
        (* Refresh failed, return original 401 response *)
        return (resp, resp_body)
    else return (resp, resp_body)

(** Make authenticated GET request *)
let api_get t ~endpoint =
  let open Deferred.Let_syntax in
  let req_fn session =
    let url = sprintf "%s/xrpc/%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [ ("Authorization", sprintf "Bearer %s" session.access_jwt) ]
    in
    Cohttp_async.Client.get ~headers (Uri.of_string url)
  in

  let%bind resp, resp_body = make_authenticated_request t ~req_fn in
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
  let body_str = Yojson.Safe.to_string body in

  let req_fn session =
    let url = sprintf "%s/xrpc/%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [
          ("Content-Type", "application/json");
          ("Authorization", sprintf "Bearer %s" session.access_jwt);
        ]
    in
    Cohttp_async.Client.post ~headers
      ~body:(Cohttp_async.Body.of_string body_str)
      (Uri.of_string url)
  in

  let%bind resp, resp_body = make_authenticated_request t ~req_fn in
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
  let req_fn session =
    let url = sprintf "%s%s" t.base_url endpoint in
    let headers =
      Cohttp.Header.of_list
        [
          ("Content-Type", content_type);
          ("Authorization", sprintf "Bearer %s" session.access_jwt);
        ]
    in
    Cohttp_async.Client.post ~headers
      ~body:(Cohttp_async.Body.of_string body)
      (Uri.of_string url)
  in

  let%bind resp, resp_body = make_authenticated_request t ~req_fn in
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
  | None -> (
    let client = create ~base_url:settings.Settings.atproto_pds_url in
    (* Try to load session from disk first *)
    let%bind () = load_session client in
    match get_session client with
    | Some _ ->
      _client := Some client;
      return client
    | None ->
      (* Fallback to login with default settings if no active session *)
      let%bind _session =
        login client ~handle:settings.atproto_handle
          ~password:settings.atproto_password
      in
      _client := Some client;
      return client)

(** Get current session (for testing/debugging) *)

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
