(** Utilities for validating client redirect URIs in OAuth flows. *)

val default_localhost_patterns : string list
(** Default patterns for localhost-only validation. Includes:
    - http://localhost:*
    - http://127.0.0.1:* *)

val matches_allowed_pattern : uri:string -> pattern:string -> bool
(** Check if a URI matches an allowed pattern with wildcard support.

    Patterns support * wildcard matching:
    - http://localhost:* matches any localhost port
    - http://127.0.0.1:* matches any 127.0.0.1 port
    - https://*.example.com/* matches any subdomain of example.com
    - https://app.example.com/auth/* matches any path under /auth/

    @param uri The redirect URI to validate
    @param pattern The allowed pattern (may contain wildcards)
    @return true if the URI matches the pattern *)

val validate_redirect_uri :
  redirect_uri:string option ->
  allowed_patterns:string list option ->
  bool
(** Validate a redirect URI against allowed patterns.

    @param redirect_uri The redirect URI to validate (None is allowed)
    @param allowed_patterns
      List of allowed patterns. If None, all URIs are allowed (for DCR
      compatibility). If empty list, no URIs are allowed. To restrict to
      localhost only, explicitly pass default_localhost_patterns.
    @return true if the redirect URI is allowed *)
