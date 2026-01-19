(** Mock OAuth Server Interface

    HTTP server for OAuth integration testing. *)

(** Mock OAuth server instance *)
type t

val create : port:int -> unit -> t
(** Create mock server on specified port *)

val add_valid_code : t -> code:string -> client_id:string -> unit
(** Add a valid authorization code that the mock server will accept *)

val start : t -> unit Async.Deferred.t
(** Start the mock server *)

val stop : t -> unit Async.Deferred.t
(** Stop the mock server *)

val is_token_valid : t -> token:string -> bool
(** Check if a token was issued by this mock server *)

val get_issuer : t -> string
(** Get the issuer URL (http://localhost:{port}) *)

(** Token response from mock server *)
type token_response = {
  access_token : string;
  token_type : string;
  expires_in : int;
  refresh_token : string option;
  scope : string option;
}

val yojson_of_token_response : token_response -> Yojson.Safe.t
