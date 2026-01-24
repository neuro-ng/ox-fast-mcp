(** Bulk Tool Caller for OxFastMCP

    Provides bulk tool calling functionality for OxFastMCP servers. Enables
    calling multiple tools in a single request for improved performance. *)

open! Core
open! Async

(* Import yojson converters *)
let yojson_of_string = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_string
let string_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.string_of_yojson
let yojson_of_bool = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_bool
let bool_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.bool_of_yojson
let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list
let list_of_yojson = Ppx_yojson_conv_lib.Yojson_conv.list_of_yojson

(* Yojson.Safe.t is its own JSON representation - use identity converters and
   custom compare/sexp since Yojson.Safe doesn't derive these *)
type json = Yojson.Safe.t

let yojson_of_json (x : json) : Yojson.Safe.t = x
let json_of_yojson (x : Yojson.Safe.t) : json = x

let compare_json (a : json) (b : json) =
  String.compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)

let sexp_of_json (x : json) = Sexp.Atom (Yojson.Safe.to_string x)

let json_of_sexp sexp =
  match sexp with
  | Sexp.Atom s -> Yojson.Safe.from_string s
  | Sexp.List _ -> Yojson.Safe.from_string (Sexp.to_string sexp)

(** {1 Types} *)

type call_tool_request = {
  tool : string;  (** The name of the tool to call *)
  arguments : json;  (** Arguments for the tool call *)
}
[@@deriving yojson, compare, sexp]
(** A request to call a single tool with specific arguments *)

type call_tool_request_result = {
  tool : string;  (** The name of the tool that was called *)
  arguments : json;  (** The arguments used for the tool call *)
  is_error : bool; [@key "isError"]
      (** Whether the call resulted in an error *)
  content : json list;  (** The result content as JSON *)
}
[@@deriving yojson, compare, sexp]
(** Result of a bulk tool call, extending call_tool_result with request info *)

(** Create a call_tool_request_result from a call_tool_result *)
let call_tool_request_result_from_call_tool_result
    ~(result : Mcp.Types.call_tool_result) ~(tool : string)
    ~(arguments : Yojson.Safe.t) : call_tool_request_result =
  (* Serialize result to extract content as JSON *)
  let result_json = Mcp.Types.yojson_of_call_tool_result result in
  let content_json =
    match result_json with
    | `Assoc fields -> (
      match List.Assoc.find fields "content" ~equal:String.equal with
      | Some (`List items) -> items
      | _ -> [])
    | _ -> []
  in
  { tool; arguments; is_error = result.is_error; content = content_json }

(** {1 Bulk Tool Caller} *)

(** Bulk tool caller module type *)
module type S = sig
  type t

  val create : unit -> t
  (** Create a new bulk tool caller *)

  val call_tools_bulk :
    t ->
    tool_calls:call_tool_request list ->
    ?continue_on_error:bool ->
    call_tool:
      (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
    unit ->
    call_tool_request_result list Deferred.t
  (** Call multiple tools in a single request.
      @param tool_calls List of tool calls to execute
      @param continue_on_error Whether to continue on errors (default: true)
      @param call_tool Function to call individual tools *)

  val call_tool_bulk :
    t ->
    tool:string ->
    tool_arguments:json list ->
    ?continue_on_error:bool ->
    call_tool:
      (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
    unit ->
    call_tool_request_result list Deferred.t
  (** Call a single tool multiple times with different arguments.
      @param tool The name of the tool to call
      @param tool_arguments List of argument sets for each call
      @param continue_on_error Whether to continue on errors (default: true)
      @param call_tool Function to call the tool *)
end

(** Default implementation of bulk tool caller *)
module Bulk_tool_caller : S = struct
  type t = unit

  let create () = ()

  let call_tools_bulk _t ~(tool_calls : call_tool_request list)
      ?(continue_on_error = true) ~call_tool () =
    let rec process_calls acc = function
      | [] -> return (List.rev acc)
      | (call : call_tool_request) :: rest ->
        let%bind result = call_tool ~name:call.tool ~arguments:call.arguments in
        let request_result =
          call_tool_request_result_from_call_tool_result ~result ~tool:call.tool
            ~arguments:call.arguments
        in
        let acc = request_result :: acc in
        if request_result.is_error && not continue_on_error then
          return (List.rev acc)
        else process_calls acc rest
    in
    process_calls [] tool_calls

  let call_tool_bulk _t ~tool ~tool_arguments ?(continue_on_error = true)
      ~call_tool () =
    let rec process_args acc = function
      | [] -> return (List.rev acc)
      | args :: rest ->
        let%bind result = call_tool ~name:tool ~arguments:args in
        let request_result =
          call_tool_request_result_from_call_tool_result ~result ~tool
            ~arguments:args
        in
        let acc = request_result :: acc in
        if request_result.is_error && not continue_on_error then
          return (List.rev acc)
        else process_args acc rest
    in
    process_args [] tool_arguments
end

(** Convenience functions using the default implementation *)

let create = Bulk_tool_caller.create
let call_tools_bulk = Bulk_tool_caller.call_tools_bulk
let call_tool_bulk = Bulk_tool_caller.call_tool_bulk
