(** File-Based Token Storage

    Persistent token storage using JSON files on disk.
    Tokens are stored in ~/.ox-fast-mcp/auth/ by default. *)

open Core
open Async

(** {1 Types} *)

type t = {
  base_path : string;
  tokens_file : string;
  client_info_file : string;
}

(** {1 Helpers} *)

let hash_server_url server_url =
  (* Create short hash from server URL for filename *)
  let digest = Digestif.SHA256.digest_string server_url in
  let hex = Digestif.SHA256.to_hex digest in
  String.prefix hex 16

let ensure_directory_exists path =
  Monitor.try_with (fun () ->
      Unix.mkdir ~p:() path)
  >>| function
  | Ok () -> ()
  | Error _ -> () (* Directory may already exist *)

let default_base_path () =
  let home = Sys.getenv "HOME" |> Option.value ~default:"/tmp" in
  Filename.concat home ".ox-fast-mcp/auth"

(** {1 Token Storage Implementation} *)

let create ~base_path ~server_url =
  let hash = hash_server_url server_url in
  let tokens_file = Filename.concat base_path (sprintf "%s_tokens.json" hash) in
  let client_info_file = Filename.concat base_path (sprintf "%s_client.json" hash) in
  { base_path; tokens_file; client_info_file }

let create_default ~server_url =
  create ~base_path:(default_base_path ()) ~server_url

let read_json_file file ~of_yojson =
  Monitor.try_with (fun () ->
      Reader.file_contents file
      >>| fun content ->
      let json = Yojson.Safe.from_string content in
      of_yojson json)
  >>| function
  | Ok result -> Some result
  | Error _ -> None

let write_json_file file json =
  let%bind () = ensure_directory_exists (Filename.dirname file) in
  let content = Yojson.Safe.to_string ~std:true json in
  (* Atomic write: write to temp file then rename *)
  let temp_file = file ^ ".tmp" in
  let%bind () = Writer.save temp_file ~contents:content in
  Unix.rename ~src:temp_file ~dst:file

let get_tokens t =
  read_json_file t.tokens_file ~of_yojson:Mcp_shared.Auth.oauth_token_of_yojson
  >>| function
  | Some token ->
    (* Check if token is expired *)
    (match token.expires_in with
    | None -> Some token
    | Some _ttl ->
      (* For file storage, we rely on the OAuth2 context to check expiry *)
      (* The context tracks token_expiry_time separately *)
      Some token)
  | None -> None

let set_tokens t tokens =
  let json = Mcp_shared.Auth.yojson_of_oauth_token tokens in
  write_json_file t.tokens_file json

let get_client_info t =
  read_json_file t.client_info_file 
    ~of_yojson:Mcp_shared.Auth.oauth_client_information_full_of_yojson

let set_client_info t client_info =
  let json = Mcp_shared.Auth.yojson_of_oauth_client_information_full client_info in
  write_json_file t.client_info_file json

(** {1 Additional Utilities} *)

let clear t =
  let%bind () =
    Monitor.try_with (fun () -> Unix.unlink t.tokens_file)
    >>| fun _ -> ()
  in
  Monitor.try_with (fun () -> Unix.unlink t.client_info_file)
  >>| fun _ -> ()

let exists t =
  Sys.file_exists t.tokens_file
  >>| function
  | `Yes -> true
  | `No | `Unknown -> false
