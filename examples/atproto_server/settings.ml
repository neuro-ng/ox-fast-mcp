(** Configuration management for ATProto server *)

open Core

type t = {
  atproto_handle : string;
  atproto_password : string;
  atproto_pds_url : string;
  notifications_default_limit : int;
  timeline_default_limit : int;
  search_default_limit : int;
}

let from_env () =
  let get_env key ~default = Option.value (Sys.getenv key) ~default in
  let get_env_exn key =
    match Sys.getenv key with
    | Some v -> v
    | None ->
      failwith
        (sprintf
           "Missing required environment variable: %s. Please set it in your \
            environment or .env file"
           key)
  in
  {
    atproto_handle = get_env_exn "ATPROTO_HANDLE";
    atproto_password = get_env_exn "ATPROTO_PASSWORD";
    atproto_pds_url = get_env "ATPROTO_PDS_URL" ~default:"https://bsky.social";
    notifications_default_limit = 10;
    timeline_default_limit = 10;
    search_default_limit = 10;
  }

(* Global settings instance - initialized on first access *)
let settings_ref : t option ref = ref None

let get_settings () =
  match !settings_ref with
  | Some s -> s
  | None ->
    let s = from_env () in
    settings_ref := Some s;
    s
