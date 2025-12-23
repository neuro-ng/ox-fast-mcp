(** Utilities for validating client redirect URIs in OAuth flows. *)

open! Core

(** Default patterns for localhost-only validation *)
let default_localhost_patterns =
  [ "http://localhost:*"; "http://127.0.0.1:*" ]

(** Check if a URI matches an allowed pattern with wildcard support.

    Patterns support * wildcard matching:
    - http://localhost:* matches any localhost port
    - http://127.0.0.1:* matches any 127.0.0.1 port
    - https://*.example.com/* matches any subdomain of example.com
    - https://app.example.com/auth/* matches any path under /auth/

    @param uri The redirect URI to validate
    @param pattern The allowed pattern (may contain wildcards)
    @return true if the URI matches the pattern *)
let matches_allowed_pattern ~uri ~pattern =
  (* Convert fnmatch-style pattern to regular expression *)
  let regex_pattern =
    pattern
    (* Escape regex special characters except * *)
    |> String.concat_map ~f:(fun c ->
           match c with
           | '*' -> ".*"
           | '.' -> "\\."
           | '?' -> "\\?"
           | '+' -> "\\+"
           | '^' -> "\\^"
           | '$' -> "\\$"
           | '(' -> "\\("
           | ')' -> "\\)"
           | '[' -> "\\["
           | ']' -> "\\]"
           | '{' -> "\\{"
           | '}' -> "\\}"
           | '|' -> "\\|"
           | '\\' -> "\\\\"
           | c -> String.make 1 c)
  in
  (* Add anchors to match entire string *)
  let full_pattern = "^" ^ regex_pattern ^ "$" in
  let re = Re.Pcre.regexp full_pattern in
  Re.execp re uri

(** Validate a redirect URI against allowed patterns.

    @param redirect_uri The redirect URI to validate (None is allowed)
    @param allowed_patterns
      List of allowed patterns. If None, all URIs are allowed (for DCR
      compatibility). If empty list, no URIs are allowed. To restrict to
      localhost only, explicitly pass default_localhost_patterns.
    @return true if the redirect URI is allowed *)
let validate_redirect_uri ~redirect_uri ~allowed_patterns =
  match redirect_uri with
  | None ->
    (* None is allowed - will use client's default *)
    true
  | Some uri_str -> (
    match allowed_patterns with
    | None ->
      (* If no patterns specified, allow all for DCR compatibility *)
      true
    | Some patterns ->
      (* Check if URI matches any allowed pattern *)
      List.exists patterns ~f:(fun pattern ->
          matches_allowed_pattern ~uri:uri_str ~pattern))
