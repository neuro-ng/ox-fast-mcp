open! Core
open! Types

(** Error types and utilities for MCP *)

(** Error data type for MCP errors *)
type error_data = {
  code : int;  (** Error code *)
  message : string;  (** Error message *)
  data : Yojson.Safe.t option;  (** Optional error details *)
} [@@deriving yojson]

(** MCP error type *)
type t = [
  | `Parse_error of string
  | `Invalid_request of string
  | `Method_not_found of string
  | `Invalid_params of string
  | `Internal_error of string
  | `Connection_closed of string
  | `Request_timeout of string
  | `Protocol_error of string * int
] [@@deriving sexp, yojson]

(** Exception type raised when an error arrives over an MCP connection *)
exception Mcp_error of error_data

(** Create a new MCP error
    @param error The error data
    @return A new MCP error exception
*)
val create : error_data -> exn

(** Convert error to error_data *)
val to_error_data : t -> error_data

(** Convert error to string *)
val to_string : t -> string

(** Convert error_data to error *)
val of_error_data : error_data -> t

(** Raise error as exception *)
val raise : t -> 'a 