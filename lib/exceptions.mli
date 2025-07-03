(**
   Custom exceptions for FastMCP.
   This module provides exception types used throughout the FastMCP framework.
*)

open Core

module Exception_group : sig
  type t = {
    message : string;
    exceptions : exn list;
  } [@@deriving sexp]

  exception Exception_group of t

  (** Create a new exception group *)
  val create : ?message:string -> exn list -> exn

  (** Convert an exception group to a string *)
  val to_string : t -> string

  (** Get the first error from the group *)
  val get_first_error : t -> exn option
end

module Tool_error : sig
  type t = {
    tool_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Tool_error of t

  (** Create a new tool error *)
  val create : ?details:(string * string) list -> tool_name:string -> string -> exn

  (** Convert a tool error to a string *)
  val to_string : t -> string
end

module Resource_error : sig
  type t = {
    resource_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Resource_error of t

  (** Create a new resource error *)
  val create : ?details:(string * string) list -> resource_name:string -> string -> exn

  (** Convert a resource error to a string *)
  val to_string : t -> string
end

module Prompt_error : sig
  type t = {
    prompt_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Prompt_error of t

  (** Create a new prompt error *)
  val create : ?details:(string * string) list -> prompt_name:string -> string -> exn

  (** Convert a prompt error to a string *)
  val to_string : t -> string
end

(** Handle errors and convert them to Result.t *)
val handle_errors : (unit -> 'a) -> ('a, string) Result.t

(** Base error for FastMCP *)
exception Fast_mcp_error of string

(** Error in validating parameters or return values *)
exception Validation_error of string

(** Error in resource operations *)
exception Resource_error of string

(** Error in tool operations *)
exception Tool_error of string

(** Error in prompt operations *)
exception Prompt_error of string

(** Invalid signature for use with FastMCP *)
exception Invalid_signature of string

(** Error in client operations *)
exception Client_error of string

(** Object not found *)
exception Not_found_error of string

(** Object is disabled *)
exception Disabled_error of string

(** Create a FastMCP error *)
val fast_mcp_error : string -> exn

(** Create a validation error *)
val validation_error : string -> exn

(** Create a resource error *)
val resource_error : string -> exn

(** Create a tool error *)
val tool_error : string -> exn

(** Create a prompt error *)
val prompt_error : string -> exn

(** Create an invalid signature error *)
val invalid_signature : string -> exn

(** Create a client error *)
val client_error : string -> exn

(** Create a not found error *)
val not_found_error : string -> exn

(** Create a disabled error *)
val disabled_error : string -> exn

(** Convert an exception to a human-readable string *)
val error_to_string : exn -> string 