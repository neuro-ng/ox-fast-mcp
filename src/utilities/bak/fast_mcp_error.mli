(** Error handling types and functions for FastMCP *)

type error_data = {
  code : int;  (** Error code *)
  message : string;  (** Error message *)
  details : Yojson.Safe.t option;  (** Optional error details *)
}
(** Base error type for FastMCP *)

exception Fast_mcp_error of error_data
(** Base FastMCP exception *)

exception Validation_error of error_data
(** Validation error *)

exception Resource_error of error_data
(** Resource error *)

exception Tool_error of error_data
(** Tool error *)

exception Prompt_error of error_data
(** Prompt error *)

exception Invalid_signature_error of error_data
(** Invalid signature error *)

exception Client_error of error_data
(** Client error *)

exception Not_found_error of error_data
(** Not found error *)

exception Disabled_error of error_data
(** Disabled error *)

val create_error : ?details:Yojson.Safe.t option -> int -> string -> error_data
(** Create a new FastMCP error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Error data *)

val create_fast_mcp_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new FastMCP error exception
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return FastMCP error exception *)

val create_validation_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new validation error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Validation error exception *)

val create_resource_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new resource error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Resource error exception *)

val create_tool_error : ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new tool error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Tool error exception *)

val create_prompt_error : ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new prompt error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Prompt error exception *)

val create_invalid_signature_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new invalid signature error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Invalid signature error exception *)

val create_client_error : ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new client error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Client error exception *)

val create_not_found_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new not found error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Not found error exception *)

val create_disabled_error :
  ?details:Yojson.Safe.t option -> int -> string -> exn
(** Create a new disabled error
    @param details Optional error details
    @param code Error code
    @param message Error message
    @return Disabled error exception *)

val error_data_to_yojson : error_data -> Yojson.Safe.t
(** Convert error data to JSON
    @param error_data Error data to convert
    @return JSON representation of error data *)

val error_data_of_yojson : Yojson.Safe.t -> (error_data, string) result
(** Parse error data from JSON
    @param json JSON to parse
    @return Result containing error data or error message *)

val error_data_of_exn : exn -> error_data option
(** Extract error data from an exception
    @param exn Exception to extract data from
    @return Optional error data *)
