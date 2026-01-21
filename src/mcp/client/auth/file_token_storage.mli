(** File-Based Token Storage Interface

    Persistent token storage using JSON files on disk. *)

type t
(** Storage implementation that saves tokens to JSON files *)

val create : base_path:string -> server_url:string -> t
(** Create file-based storage.

    @param base_path Base directory for auth files (e.g., ~/.ox-fast-mcp/auth)
    @param server_url Server URL (used to derive filename) *)

val create_default : server_url:string -> t
(** Create file storage with default base path (~/.ox-fast-mcp/auth) *)

val clear : t -> unit Async.Deferred.t
(** Delete stored tokens and client info files *)

val exists : t -> bool Async.Deferred.t
(** Check if token file exists *)

include Oauth2.Token_storage with type t := t
