(** Exceptions Module

    This module provides exception types used throughout the MCP framework. *)

open Mcp.Types

(* Error code for URL elicitation required - from MCP spec *)
let url_elicitation_required_code = -32008

exception Mcp_error of error_data
(** Exception type raised when an error arrives over an MCP connection *)

exception
  Url_elicitation_required of {
    elicitations : Yojson.Safe.t list;
        (* Using Yojson until elicit_request_url_params type is defined *)
    message : string;
  }
(** Exception for when a tool requires URL mode elicitation(s) before proceeding *)

(** Create a new MCP error exception *)
let create_error error = Mcp_error error

(** Get the error data from an MCP error exception *)
let get_error = function
  | Mcp_error error -> error
  | _ -> failwith "Not an MCP error"

(** Get the error message from an MCP error exception *)
let get_error_message = function
  | Mcp_error error -> error.message
  | _ -> failwith "Not an MCP error"

(** Create a URL elicitation required error *)
let url_elicitation_required elicitations ?message () =
  let msg =
    match message with
    | Some m -> m
    | None ->
      if List.length elicitations > 1 then "URL elicitations required"
      else "URL elicitation required"
  in
  Url_elicitation_required { elicitations; message = msg }

(** Reconstruct URL elicitation error from ErrorData received over the wire *)
let url_elicitation_from_error (error : error_data) =
  if error.code <> url_elicitation_required_code then
    failwith
      (Printf.sprintf "Expected error code %d, got %d"
         url_elicitation_required_code error.code)
  else
    match error.data with
    | None -> failwith "Missing elicitations data in error"
    | Some (`Assoc fields) -> (
      match List.assoc_opt "elicitations" fields with
      | Some (`List elicit_list) ->
        Url_elicitation_required
          { elicitations = elicit_list; message = error.message }
      | _ -> failwith "Invalid elicitations format in error data")
    | _ -> failwith "Invalid error data format"
