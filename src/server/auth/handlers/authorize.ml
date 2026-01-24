(** Enhanced authorization handler with improved error responses.

    This module provides an enhanced authorization handler that wraps the MCP
    SDK's AuthorizationHandler to provide better error messages when clients
    attempt to authorize with unregistered client IDs.

    The enhancement adds:
    - Content negotiation: HTML for browsers, JSON for API clients
    - Enhanced JSON responses with registration endpoint hints
    - Styled HTML error pages with registration links/forms
    - Link headers pointing to registration endpoints *)

open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Logging

let logger = Logger.get_logger "Authorize"

type authorization_error_response = {
  error : string;
  error_description : string option; [@yojson.option]
  state : string option; [@yojson.option]
  registration_endpoint : string option; [@yojson.option]
  authorization_server_metadata : string option; [@yojson.option]
}
[@@deriving yojson, sexp, compare]
(** Authorization error response type *)

type config = {
  base_url : string;
  server_name : string option;
  server_icon_url : string option;
}
(** Configuration for the authorization handler *)

(** Create an authorization error response *)
let create_error_response ~error ?error_description ?state
    ?registration_endpoint ?authorization_server_metadata () =
  {
    error;
    error_description;
    state;
    registration_endpoint;
    authorization_server_metadata;
  }

(** HTML styles for info boxes *)
let info_box_styles =
  {|
    .info-box {
      padding: 16px;
      border-radius: 8px;
      border-left: 4px solid;
      margin: 16px 0;
      background: #f8fafc;
      border-color: #3b82f6;
    }
    .info-box p {
      margin: 0;
      line-height: 1.5;
    }
    .info-box.error {
      background: #fef2f2;
      border-color: #f87171;
    }
    .info-box.error strong {
      color: #991b1b;
    }
    .info-box.warning {
      background: #fffbeb;
      border-color: #fbbf24;
    }
    .info-box.warning strong {
      color: #92400e;
    }
    .info-box code {
      background: rgba(0, 0, 0, 0.05);
      padding: 2px 6px;
      border-radius: 3px;
      font-family: 'SF Mono', Monaco, 'Cascadia Code', monospace;
      font-size: 0.9em;
    }
    .info-box ul {
      margin: 10px 0;
      padding-left: 20px;
    }
    .info-box li {
      margin: 6px 0;
    }
  |}

(** HTML styles for tooltips *)
let tooltip_styles =
  {|
    .help-link-container {
      margin-top: 24px;
      text-align: center;
    }
    .help-link {
      color: #6b7280;
      font-size: 0.9em;
      cursor: help;
      position: relative;
      display: inline-block;
    }
    .help-link .tooltip {
      visibility: hidden;
      width: 300px;
      background-color: #1f2937;
      color: #fff;
      text-align: left;
      border-radius: 8px;
      padding: 12px;
      position: absolute;
      z-index: 1;
      bottom: 125%;
      left: 50%;
      margin-left: -150px;
      opacity: 0;
      transition: opacity 0.3s;
      font-size: 0.85em;
      line-height: 1.4;
    }
    .help-link:hover .tooltip {
      visibility: visible;
      opacity: 1;
    }
  |}

(** Escape HTML special characters *)
let escape_html s =
  String.concat_map s ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | '\'' -> "&#x27;"
    | c -> String.make 1 c)

(** Create a logo HTML element *)
let create_logo ?icon_url ~alt_text () =
  match icon_url with
  | Some url ->
    sprintf
      {|<img src="%s" alt="%s" class="logo" style="max-width: 80px; margin-bottom: 16px;">|}
      (escape_html url) (escape_html alt_text)
  | None ->
    sprintf
      {|<div class="logo-placeholder" style="font-size: 2em; margin-bottom: 16px;">🔐</div>|}

(** Create HTML page wrapper *)
let create_page ~content ~title ~additional_styles =
  sprintf
    {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <style>
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 20px;
    }
    .container {
      background: white;
      border-radius: 16px;
      padding: 40px;
      max-width: 500px;
      width: 100%%;
      box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
      text-align: center;
    }
    h1 {
      color: #1f2937;
      margin-bottom: 24px;
      font-size: 1.5em;
    }
    %s
  </style>
</head>
<body>
  %s
</body>
</html>|}
    (escape_html title) additional_styles content

(** Create styled HTML error page for unregistered client attempts *)
let create_unregistered_client_html ~client_id ~registration_endpoint
    ~discovery_endpoint ?server_name ?server_icon_url
    ?(title = "Client Not Registered") () =
  let client_id_escaped = escape_html client_id in
  let _ = registration_endpoint in
  (* Will be used in future for registration form *)
  let _ = discovery_endpoint in

  (* Will be used in future for metadata link *)

  (* Main error message *)
  let error_box =
    sprintf
      {|<div class="info-box error">
        <p>The client ID <code>%s</code> was not found in the server's client registry.</p>
      </div>|}
      client_id_escaped
  in

  (* What to do - yellow warning box *)
  let warning_box =
    {|<div class="info-box warning">
      <p>Your MCP client opened this page to complete OAuth authorization,
      but the server did not recognize its client ID. To fix this:</p>
      <ul>
        <li>Close this browser window</li>
        <li>Clear authentication tokens in your MCP client (or restart it)</li>
        <li>Try connecting again - your client should automatically re-register</li>
      </ul>
    </div>|}
  in

  (* Help link with tooltip *)
  let help_link =
    {|<div class="help-link-container">
      <span class="help-link">
        Why am I seeing this?
        <span class="tooltip">
          OAuth 2.0 requires clients to register before authorization.
          This server returned a 400 error because the provided client
          ID was not found.
          <br><br>
          In browser-delegated OAuth flows, your application cannot
          detect this error automatically; it's waiting for a
          callback that will never arrive. You must manually clear
          auth tokens and reconnect.
        </span>
      </span>
    </div>|}
  in

  (* Build page content *)
  let alt_text = Option.value server_name ~default:"OxFastMCP" in
  let content =
    sprintf
      {|<div class="container">
        %s
        <h1>%s</h1>
        %s
        %s
      </div>
      %s|}
      (create_logo ?icon_url:server_icon_url ~alt_text ())
      (escape_html title) error_box warning_box help_link
  in

  (* Use same styles as consent page *)
  let additional_styles = info_box_styles ^ tooltip_styles in

  create_page ~content ~title ~additional_styles

(** Authorization handler module type *)
module type AUTHORIZATION_HANDLER = sig
  type request
  type response

  val handle : request -> response Lwt.t
end

(** Create the enhanced authorization handler *)
module Make_authorization_handler
    (Provider : Mcp_server_auth.Provider.OAUTH_AUTHORIZATION_SERVER_PROVIDER) =
struct
  (** Check if a client is registered *)
  let is_client_registered client_id =
    Lwt.bind (Provider.get_client client_id) (fun client ->
        Lwt.return (Option.is_some client))

  (** Create an enhanced error response for unregistered client *)
  let create_enhanced_error_response ~client_id ~accept_header ~state ~config =
    let registration_endpoint = config.base_url ^ "/register" in
    let discovery_endpoint =
      config.base_url ^ "/.well-known/oauth-authorization-server"
    in

    (* Check Accept header for content negotiation *)
    let prefers_html =
      String.is_substring accept_header ~substring:"text/html"
    in

    if prefers_html then (
      (* Return HTML for browsers *)
      let html =
        create_unregistered_client_html ~client_id ~registration_endpoint
          ~discovery_endpoint ?server_name:config.server_name
          ?server_icon_url:config.server_icon_url ()
      in
      let headers =
        [
          ("Content-Type", "text/html; charset=utf-8");
          ("Cache-Control", "no-store");
          ( "Link",
            sprintf {|<%s>; rel="http://oauth.net/core/2.1/#registration"|}
              registration_endpoint );
          ("X-Content-Type-Options", "nosniff");
          ("X-Frame-Options", "DENY");
        ]
      in
      Logger.info logger
        (sprintf "Unregistered client_id=%s, returned HTML error response"
           client_id);
      (400, "text/html", html, headers))
    else
      (* Return enhanced JSON for API clients *)
      let error_response =
        create_error_response ~error:"invalid_request"
          ~error_description:
            (sprintf
               "Client ID '%s' is not registered with this server. MCP clients \
                should automatically re-register by sending a POST request to \
                the registration_endpoint and retry authorization. If this \
                persists, clear cached authentication tokens and reconnect."
               client_id)
          ?state ~registration_endpoint
          ~authorization_server_metadata:discovery_endpoint ()
      in
      let json =
        Yojson.Safe.to_string
          (yojson_of_authorization_error_response error_response)
      in
      let headers =
        [
          ("Content-Type", "application/json");
          ("Cache-Control", "no-store");
          ( "Link",
            sprintf {|<%s>; rel="http://oauth.net/core/2.1/#registration"|}
              registration_endpoint );
        ]
      in
      Logger.info logger
        (sprintf "Unregistered client_id=%s, returned JSON error response"
           client_id);
      (400, "application/json", json, headers)
end
