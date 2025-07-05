(**
   Custom exceptions for FastMCP.
   This module provides exception types used throughout the FastMCP framework.
*)

open Core

(** Base error for FastMCP *)
exception Fast_mcp_error of string

(** Error in validating parameters or return values *)
exception Validation_error of string

(** Error in resource operations *)
exception Resource_error of string

(** Error in tool operations *)
exception Tool_error of string * exn option

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

(** Helper functions to create exceptions *)
let fast_mcp_error msg = Fast_mcp_error msg
let validation_error msg = Validation_error msg
let resource_error msg = Resource_error msg
let tool_error ?exn msg = raise (Tool_error (msg, exn))
let prompt_error msg = Prompt_error msg
let invalid_signature msg = Invalid_signature msg
let client_error msg = Client_error msg
let not_found_error msg = raise (Not_found_error msg)
let disabled_error msg = Disabled_error msg

(** Helper functions to convert exceptions to strings *)
let error_to_string = function
  | Fast_mcp_error msg -> sprintf "FastMCP error: %s" msg
  | Validation_error msg -> sprintf "Validation error: %s" msg
  | Resource_error msg -> sprintf "Resource error: %s" msg
  | Tool_error (msg, Some exn) -> sprintf "Tool error: %s (%s)" msg (Exn.to_string exn)
  | Tool_error (msg, None) -> sprintf "Tool error: %s" msg
  | Prompt_error msg -> sprintf "Prompt error: %s" msg
  | Invalid_signature msg -> sprintf "Invalid signature: %s" msg
  | Client_error msg -> sprintf "Client error: %s" msg
  | Not_found_error msg -> sprintf "Not found: %s" msg
  | Disabled_error msg -> sprintf "Disabled: %s" msg
  | exn -> sprintf "Unknown error: %s" (Exn.to_string exn)

exception Not_found_s of Sexp.t

let () =
  Caml.Printexc.register_printer (function
    | Tool_error (msg, Some exn) -> Some (sprintf "Tool error: %s (%s)" msg (Exn.to_string exn))
    | Tool_error (msg, None) -> Some (sprintf "Tool error: %s" msg)
    | Not_found_error msg -> Some (sprintf "Not found: %s" msg)
    | _ -> None)

module Exception_group = struct
  type t = {
    message : string;
    exceptions : exn list;
  } [@@deriving sexp]

  exception Exception_group of t

  let create ?(message = "Multiple exceptions occurred") exceptions =
    Exception_group { message; exceptions }

  let to_string { message; exceptions } =
    let exception_strings =
      List.map exceptions ~f:(fun e -> Exn.to_string e)
    in
    sprintf "%s:\n%s" message
      (String.concat ~sep:"\n"
         (List.mapi exception_strings ~f:(fun i s ->
              sprintf "%d) %s" (i + 1) s)))

  let get_first_error { exceptions; _ } = List.hd exceptions
end

module Tool_error = struct
  type t = {
    tool_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Tool_error of t

  let create ?(details = []) ~tool_name message =
    Tool_error { tool_name; message; details }

  let to_string { tool_name; message; details } =
    let details_str =
      if List.is_empty details then ""
      else
        "\nDetails:\n"
        ^ String.concat ~sep:"\n"
            (List.map details ~f:(fun (k, v) -> sprintf "%s: %s" k v))
    in
    sprintf "Tool error in %s: %s%s" tool_name message details_str
end

module Resource_error = struct
  type t = {
    resource_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Resource_error of t

  let create ?(details = []) ~resource_name message =
    Resource_error { resource_name; message; details }

  let to_string { resource_name; message; details } =
    let details_str =
      if List.is_empty details then ""
      else
        "\nDetails:\n"
        ^ String.concat ~sep:"\n"
            (List.map details ~f:(fun (k, v) -> sprintf "%s: %s" k v))
    in
    sprintf "Resource error in %s: %s%s" resource_name message details_str
end

module Prompt_error = struct
  type t = {
    prompt_name : string;
    message : string;
    details : (string * string) list;
  } [@@deriving sexp, yojson_of, yojson]

  exception Prompt_error of t

  let create ?(details = []) ~prompt_name message =
    Prompt_error { prompt_name; message; details }

  let to_string { prompt_name; message; details } =
    let details_str =
      if List.is_empty details then ""
      else
        "\nDetails:\n"
        ^ String.concat ~sep:"\n"
            (List.map details ~f:(fun (k, v) -> sprintf "%s: %s" k v))
    in
    sprintf "Prompt error in %s: %s%s" prompt_name message details_str
end

let handle_errors f =
  try f () with
  | Exception_group.Exception_group e -> Error (Exception_group.to_string e)
  | Tool_error.Tool_error e -> Error (Tool_error.to_string e)
  | Resource_error.Resource_error e -> Error (Resource_error.to_string e)
  | Prompt_error.Prompt_error e -> Error (Prompt_error.to_string e)
  | e -> Error (Exn.to_string e) 