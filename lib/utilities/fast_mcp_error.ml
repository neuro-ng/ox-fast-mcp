open Core

(** Base error type for FastMCP *)
type error_data = {
  code : int;
  message : string;
  details : Yojson.Safe.t option;
}

(** Base FastMCP exception *)
exception Fast_mcp_error of error_data

(** Validation error *)
exception Validation_error of error_data

(** Resource error *)
exception Resource_error of error_data

(** Tool error *)
exception Tool_error of error_data

(** Prompt error *)
exception Prompt_error of error_data

(** Invalid signature error *)
exception Invalid_signature_error of error_data

(** Client error *)
exception Client_error of error_data

(** Not found error *)
exception Not_found_error of error_data

(** Disabled error *)
exception Disabled_error of error_data

(** Create a new FastMCP error *)
let create_error ?(details=None) code message =
  { code; message; details }

(** Create a new FastMCP error exception *)
let create_fast_mcp_error ?details code message =
  Fast_mcp_error (create_error ?details code message)

(** Create a new validation error *)
let create_validation_error ?details code message =
  Validation_error (create_error ?details code message)

(** Create a new resource error *)
let create_resource_error ?details code message =
  Resource_error (create_error ?details code message)

(** Create a new tool error *)
let create_tool_error ?details code message =
  Tool_error (create_error ?details code message)

(** Create a new prompt error *)
let create_prompt_error ?details code message =
  Prompt_error (create_error ?details code message)

(** Create a new invalid signature error *)
let create_invalid_signature_error ?details code message =
  Invalid_signature_error (create_error ?details code message)

(** Create a new client error *)
let create_client_error ?details code message =
  Client_error (create_error ?details code message)

(** Create a new not found error *)
let create_not_found_error ?details code message =
  Not_found_error (create_error ?details code message)

(** Create a new disabled error *)
let create_disabled_error ?details code message =
  Disabled_error (create_error ?details code message)

(** Convert error data to JSON *)
let error_data_to_yojson error_data =
  `Assoc [
    ("code", `Int error_data.code);
    ("message", `String error_data.message);
    ("details", Option.value ~default:`Null error_data.details)
  ]

(** Parse error data from JSON *)
let error_data_of_yojson = function
  | `Assoc fields ->
    (match List.Assoc.find fields ~equal:String.equal "code",
           List.Assoc.find fields ~equal:String.equal "message",
           List.Assoc.find fields ~equal:String.equal "details" with
     | Some (`Int code), Some (`String message), details ->
       Ok { code; message; details = Option.filter details ~f:(fun x -> not (x = `Null)) }
     | _ -> Error "Invalid error data format")
  | _ -> Error "Invalid error data format"

(** Extract error data from an exception *)
let error_data_of_exn = function
  | Fast_mcp_error data -> Some data
  | Validation_error data -> Some data
  | Resource_error data -> Some data
  | Tool_error data -> Some data
  | Prompt_error data -> Some data
  | Invalid_signature_error data -> Some data
  | Client_error data -> Some data
  | Not_found_error data -> Some data
  | Disabled_error data -> Some data
  | _ -> None 