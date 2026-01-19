(** OAuth Callback Server

    HTTP server to handle OAuth browser authorization callbacks.
    Listens for the OAuth redirect containing authorization code and state. *)

open Core
open Async

(** {1 Types} *)

type callback_result = {
  code : string option;
  state : string option;
  error : string option;
  error_description : string option;
}

(** {1 HTML Templates} *)

let success_html =
  {|<!DOCTYPE html>
<html>
<head>
  <title>Authorization Successful</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    }
    .card {
      background: white;
      padding: 40px 60px;
      border-radius: 16px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.2);
      text-align: center;
    }
    .checkmark {
      width: 60px;
      height: 60px;
      background: #4CAF50;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 auto 20px;
    }
    .checkmark::after {
      content: '✓';
      color: white;
      font-size: 32px;
    }
    h1 { color: #333; margin: 0 0 10px; }
    p { color: #666; margin: 0; }
  </style>
</head>
<body>
  <div class="card">
    <div class="checkmark"></div>
    <h1>Authorization Successful</h1>
    <p>You can close this window and return to the application.</p>
  </div>
</body>
</html>|}

let error_html ~error =
  let template_start = {|<!DOCTYPE html>
<html>
<head>
  <title>Authorization Failed</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #e74c3c 0, #c0392b 100%);
    }
    .card {
      background: white;
      padding: 40px 60px;
      border-radius: 16px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.2);
      text-align: center;
    }
    .error-icon {
      width: 60px;
      height: 60px;
      background: #e74c3c;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 auto 20px;
    }
    .error-icon::after {
      content: 'X';
      color: white;
      font-size: 32px;
      font-weight: bold;
    }
    h1 { color: #333; margin: 0 0 10px; }
    p { color: #666; margin: 0; }
    .error-detail { color: #e74c3c; font-size: 14px; margin-top: 15px; }
  </style>
</head>
<body>
  <div class="card">
    <div class="error-icon"></div>
    <h1>Authorization Failed</h1>
    <p>Something went wrong during authorization.</p>
    <p class="error-detail">|} in
  let template_end = {|</p>
  </div>
</body>
</html>|} in
  template_start ^ error ^ template_end

(** {1 Callback Server} *)

let start_callback_server ~port ~timeout =
  let result_ivar = Ivar.create () in
  let server_ref = ref None in

  (* Handler for HTTP requests *)
  let handler ~body:_ _sock request =
    let uri = Cohttp.Request.uri request in
    let path = Uri.path uri in

    match path with
    | "/callback" | "/callback/" ->
      let query = Uri.query uri in
      let get_param name =
        List.find_map query ~f:(fun (k, vs) ->
            if String.equal k name then List.hd vs else None)
      in

      let code = get_param "code" in
      let state = get_param "state" in
      let error = get_param "error" in
      let error_description = get_param "error_description" in

      let result = { code; state; error; error_description } in

      (* Determine response based on whether we got code or error *)
      let response_body, status =
        match (code, error) with
        | Some _, _ ->
          Logs.info (fun m -> m "OAuth callback received authorization code");
          (success_html, `OK)
        | None, Some err ->
          let desc = Option.value error_description ~default:"Unknown error" in
          Logs.warn (fun m -> m "OAuth callback error: %s - %s" err desc);
          (error_html ~error:(sprintf "%s: %s" err desc), `Bad_request)
        | None, None ->
          Logs.warn (fun m -> m "OAuth callback missing code and error");
          ( error_html ~error:"Missing authorization code",
            `Bad_request )
      in

      (* Fill the result ivar (only once) *)
      if Ivar.is_empty result_ivar then Ivar.fill_exn result_ivar result;

      (* Return HTML response *)
      let headers =
        Cohttp.Header.of_list
          [
            ("Content-Type", "text/html; charset=utf-8");
            ("Connection", "close");
          ]
      in
      Cohttp_async.Server.respond_string ~headers ~status response_body

    | _ ->
      (* 404 for other paths *)
      Cohttp_async.Server.respond_string ~status:`Not_found
        "Not found - expecting /callback"
  in

  (* Start the server *)
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port port)
      handler
  in
  server_ref := Some server;

  Logs.info (fun m -> m "OAuth callback server started on http://localhost:%d" port);

  (* Wait for result or timeout *)
  let timeout_result =
    let%map () = Clock_ns.after timeout in
    { code = None; state = None; error = Some "timeout"; error_description = Some "Authorization timed out" }
  in

  let result_or_timeout =
    Deferred.any [ Ivar.read result_ivar; timeout_result ]
  in

  (* Get result and cleanup *)
  let%bind result = result_or_timeout in

  (* Stop the server *)
  let%bind () =
    match !server_ref with
    | Some s -> Cohttp_async.Server.close s
    | None -> return ()
  in

  Logs.info (fun m -> m "OAuth callback server stopped");

  return result
