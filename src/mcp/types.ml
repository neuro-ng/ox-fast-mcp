open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type json = Yojson.Safe.t
(** Basic JSON representation with proper OCaml types *)

(* Manual yojson conversion functions for json type *)
let json_of_yojson (j : Yojson.Safe.t) : json = j
let yojson_of_json (j : json) : Yojson.Safe.t = j

(* Manual sexp conversion functions for json type *)
let json_of_sexp (s : Sexp.t) : json = 
  Yojson.Safe.from_string (Sexp.to_string_hum s)
let sexp_of_json (j : json) : Sexp.t = 
  Sexp.of_string (Yojson.Safe.to_string j)

type json_value =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of json_value list
  | `Assoc of (string * json_value) list ]
[@@deriving yojson]

(** Latest protocol version *)
let latest_protocol_version = "2025-06-18"

(** Default negotiated version *)
let default_negotiated_version = "2025-03-26"

(** Standard JSON-RPC error codes *)
let parse_error = -32700

let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603

(** SDK error codes *)
let connection_closed = -32000

type progress_token = string [@@deriving yojson]
(** Basic types *)

type cursor = string [@@deriving yojson]
type role = [ `User | `Assistant ] [@@deriving yojson]
type request_id = [ `Int of int | `String of string ] [@@deriving yojson]

type meta = {
  progress_token : progress_token option; [@key "progressToken"] [@yojson.option]
}
[@@deriving yojson]
(** Request and notification types *)

type request_params = { meta : meta option [@key "_meta"] [@yojson.option] }
[@@deriving yojson]

type paginated_request_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
  cursor : cursor option; [@yojson.option]
}
[@@deriving yojson]

type notification_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type implementation = {
  name : string;
  title : string option; [@yojson.option]
  version : string;
}
[@@deriving yojson]
(** Implementation info *)

type roots_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
}
[@@deriving yojson]
(** Capabilities *)

type sampling_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit; [@default ()]
}
[@@deriving yojson]

type elicitation_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit; [@default ()]
}
[@@deriving yojson]

type client_capabilities = {
  experimental : (string * (string * json) list) list option;
  sampling : sampling_capability option; [@yojson.option]
  elicitation : elicitation_capability option; [@yojson.option]
  roots : roots_capability option; [@yojson.option]
}
[@@deriving yojson]

type prompts_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
}
[@@deriving yojson]

type resources_capability = {
  subscribe : bool option; [@yojson.option]
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
}
[@@deriving yojson]

type tools_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
}
[@@deriving yojson]

type logging_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit; [@default ()]
}
[@@deriving yojson]

type server_capabilities = {
  experimental : (string * json) list option; [@yojson.option]
  logging : logging_capability option; [@yojson.option]
  prompts : prompts_capability option; [@yojson.option]
  resources : resources_capability option; [@yojson.option]
  tools : tools_capability option; [@yojson.option]
}
[@@deriving yojson]

type initialize_request_params = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : client_capabilities;
  client_info : implementation; [@key "clientInfo"]
}
[@@deriving yojson]
(** Initialize types *)

type initialize_result = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : server_capabilities;
  server_info : implementation; [@key "serverInfo"]
  instructions : string option; [@yojson.option]
}
[@@deriving yojson]

type progress_notification_params = {
  progress_token : progress_token; [@key "progressToken"]
  progress : float;
  total : float option; [@yojson.option]
  message : string option; [@yojson.option]
}
[@@deriving yojson]
(** Progress notification *)

type annotations = {
  audience : role list option; [@yojson.option]
  priority : float option; [@yojson.option]
}
[@@deriving yojson]
(** Resource types *)

type resource = {
  name : string;
  title : string option; [@yojson.option]
  uri : string;
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  size : int option; [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type resource_template = {
  name : string;
  title : string option; [@yojson.option]
  uri_template : string; [@key "uriTemplate"]
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type blob_resource_contents = {
  uri : string;
  mime_type : string;
  blob : string; (* Base64 encoded data *)
}
[@@deriving yojson, compare]
(** Resource contents *)

type text_resource_contents = {
  uri : string;
  mime_type : string;
  text : string;
}
[@@deriving yojson, compare]

type resource_contents =
  | Blob of blob_resource_contents
  | Text of text_resource_contents
[@@deriving yojson, compare]

let create_blob_resource ~uri ~mime_type ~blob () =
  Blob { uri; mime_type; blob }

let create_text_resource ~uri ~mime_type ~text () =
  Text { uri; mime_type; text }

let get_mime_type = function
  | Blob { mime_type; _ } -> mime_type
  | Text { mime_type; _ } -> mime_type

let get_uri = function
  | Blob { uri; _ } -> uri
  | Text { uri; _ } -> uri

type tool_annotations = (string * string) list [@@deriving yojson]
(** Tool types *)

type tool = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  input_schema : json; [@key "inputSchema"]
  output_schema : json option; [@key "outputSchema"] [@yojson.option]
  annotations : tool_annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type text_content = {
  type_ : [ `Text ]; [@key "type"]
  text : string;
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Content types *)

type image_content = {
  type_ : [ `Image ]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type audio_content = {
  type_ : [ `Audio ]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type content =
  [ `Text of text_content | `Image of image_content | `Audio of audio_content ]
[@@deriving yojson]

type message = { role : role; content : content } [@@deriving yojson]
(** Message types *)

type model_hint = { name : string option [@yojson.option] } [@@deriving yojson]
(** Model types *)

type model_preferences = {
  hints : model_hint list option; [@yojson.option]
  cost_priority : float option; [@key "costPriority"] [@yojson.option]
  speed_priority : float option; [@key "speedPriority"] [@yojson.option]
  intelligence_priority : float option;
      [@key "intelligencePriority"] [@yojson.option]
}
[@@deriving yojson]

type completion_argument = { name : string; value : string } [@@deriving yojson]
(** Completion types *)

type completion_context = {
  arguments : (string * string) list option; [@yojson.option]
}
[@@deriving yojson]

type completion = {
  values : string list;
  total : int option; [@yojson.option]
  has_more : bool option; [@key "hasMore"] [@yojson.option]
}
[@@deriving yojson]

type root = {
  uri : string;
  name : string option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Root types *)

type error_data = {
  code : int;
  message : string;
  data : json option; [@yojson.option]
}
[@@deriving yojson]
(** Error types *)

type jsonrpc_request = {
  jsonrpc : string;
  id : request_id;
  method_ : string; [@key "method"]
  params : json option; [@yojson.option]
}
[@@deriving yojson]
(** JSON-RPC types *)

type jsonrpc_notification = {
  jsonrpc : string;
  method_ : string; [@key "method"]
  params : json option; [@yojson.option]
}
[@@deriving yojson]

type jsonrpc_response = { jsonrpc : string; id : request_id; result : json }
[@@deriving yojson]

type jsonrpc_error_response = {
  jsonrpc : string;
  id : request_id;
  error : error_data;
}
[@@deriving yojson]

type jsonrpc_message =
  [ `Request of jsonrpc_request
  | `Notification of jsonrpc_notification
  | `Response of jsonrpc_response
  | `Error of jsonrpc_error_response ]
[@@deriving yojson]

type logging_level =
  [ `Debug
  | `Info
  | `Notice
  | `Warning
  | `Error
  | `Critical
  | `Alert
  | `Emergency ]
[@@deriving yojson]
(** Logging types *)

type set_level_request_params = { level : logging_level } [@@deriving yojson]

type logging_message_notification_params = {
  level : logging_level;
  logger : string option; [@yojson.option]
  data : json;
}
[@@deriving yojson]

type include_context = [ `None | `ThisServer | `AllServers ] [@@deriving yojson]
(** Include context types *)

type resource_template_reference = {
  type_ : [ `Ref_resource ]; [@key "type"]
  uri : string;
}
[@@deriving yojson]
(** Resource reference types *)

type prompt_reference = { type_ : [ `Ref_prompt ]; [@key "type"] name : string }
[@@deriving yojson]

type elicit_request_params = {
  message : string;
  requested_schema : json; [@key "requestedSchema"]
}
[@@deriving yojson]
(** Elicitation types *)

type elicit_result = {
  action : [ `Accept | `Decline | `Cancel ];
  content :
    (string
    * [ `String of string
      | `Int of int
      | `Float of float
      | `Bool of bool
      | `Null ])
    list
    option;
      [@yojson.option]
}
[@@deriving yojson]

type create_message_request_params = {
  messages : message list;
  model_preferences : model_preferences option;
      [@key "modelPreferences"] [@yojson.option]
  system_prompt : string option; [@key "systemPrompt"] [@yojson.option]
  include_context : include_context option;
      [@key "includeContext"] [@yojson.option]
  temperature : float option; [@yojson.option]
  max_tokens : int; [@key "maxTokens"]
  stop_sequences : string list option; [@key "stopSequences"] [@yojson.option]
  metadata : json option; [@yojson.option]
}
[@@deriving yojson]
(** Sampling types *)

type stop_reason = [ `EndTurn | `StopSequence | `MaxTokens | `Other of string ]
[@@deriving yojson]

type create_message_result = {
  role : role;
  content : content;
  model : string;
  stop_reason : stop_reason option; [@key "stopReason"] [@yojson.option]
}
[@@deriving yojson]

type client_request =
  [ `Ping of request_params option
  | `Initialize of initialize_request_params
  | `Complete of completion_argument
  | `SetLevel of set_level_request_params
  | `GetPrompt of string * (string * string) list option
  | `ListPrompts of paginated_request_params option
  | `ListResources of paginated_request_params option
  | `ListResourceTemplates of paginated_request_params option
  | `ReadResource of string
  | `Subscribe of string
  | `Unsubscribe of string
  | `CallTool of string * json option
  | `ListTools of paginated_request_params option ]
[@@deriving yojson]
(** Client request types *)

type client_notification =
  [ `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `Initialized
  | `RootsListChanged ]
[@@deriving yojson]
(** Client notification types *)

type server_request =
  [ `Ping of request_params option
  | `CreateMessage of create_message_request_params
  | `ListRoots of request_params option
  | `Elicit of elicit_request_params ]
[@@deriving yojson]
(** Server request types *)

type server_notification =
  [ `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `LoggingMessage of logging_message_notification_params
  | `ResourceUpdated of string
  | `ResourceListChanged
  | `ToolListChanged
  | `PromptListChanged ]
[@@deriving yojson]
(** Server notification types *)

type prompt_argument = {
  name : string;
  description : string option; [@yojson.option]
  required : bool option; [@yojson.option]
}
[@@deriving yojson]
(** Prompt types *)

type prompt = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  arguments : prompt_argument list option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type prompt_message = { role : role; content : content }
[@@deriving yojson]

type embedded_resource_content = 
  | Text_resource of text_resource_contents
  | Blob_resource of blob_resource_contents
[@@deriving yojson]

type embedded_resource = {
  type_ : [ `Resource ]; [@key "type"]
  resource : embedded_resource_content;
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Resource embedded content *)

type resource_link = {
  type_ : [ `Resource_link ]; [@key "type"]
  name : string;
  title : string option; [@yojson.option]
  uri : string;
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  size : int option; [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type content_block = {
  text : string option; [@yojson.option]
  image : string option; [@yojson.option]
  error : string option; [@yojson.option]
}
[@@deriving fields, yojson]

type empty_result = { meta : json option [@key "_meta"] [@yojson.option] }
[@@deriving yojson]
(** Empty result type *)

type paginated_result = {
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Paginated result types *)

type list_resources_result = {
  resources : resource list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Resource operation results *)

type list_resource_templates_result = {
  resource_templates : resource_template list; [@key "resourceTemplates"]
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type read_resource_result = {
  contents : embedded_resource_content list;
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type list_tools_result = {
  tools : tool list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Tool operation results *)

type call_tool_result = {
  content : content list;
  structured_content : json option; [@key "structuredContent"] [@yojson.option]
  is_error : bool; [@key "isError"]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type list_prompts_result = {
  prompts : prompt list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Prompt operation results *)

type get_prompt_result = {
  description : string option; [@yojson.option]
  messages : prompt_message list;
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]

type list_roots_result = {
  roots : root list;
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Root operation results *)

type cancelled_notification_params = {
  request_id : request_id; [@key "requestId"]
  reason : string option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Cancelled notification types *)

type resource_updated_notification_params = {
  uri : string;
  meta : json option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Resource update notification types *)

type client_result =
  [ `Empty of empty_result
  | `CreateMessage of create_message_result
  | `ListRoots of list_roots_result
  | `Elicit of elicit_result ]
[@@deriving yojson]
(** Client result types *)

type server_result =
  [ `Empty of empty_result
  | `Initialize of initialize_result
  | `Complete of completion
  | `GetPrompt of get_prompt_result
  | `ListPrompts of list_prompts_result
  | `ListResources of list_resources_result
  | `ListResourceTemplates of list_resource_templates_result
  | `ReadResource of read_resource_result
  | `CallTool of call_tool_result
  | `ListTools of list_tools_result ]
[@@deriving yojson]
(** Server result types *)

type sampling_message = { role : role; content : content }
[@@deriving yojson]
(** Sampling message type *)

type complete_request_params = {
  ref : resource_template_reference; [@key "ref"]
  argument : completion_argument;
  context : completion_context option; [@yojson.option]
}
[@@deriving yojson]
(** Complete request params *)

type subscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Subscribe request params *)

type unsubscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
}
[@@deriving yojson]
(** Unsubscribe request params *)



(* Helper constructor functions to match the .mli interface *)
let create_text_content text = 
  { text = Some text; image = None; error = None }

let create_image_content image = 
  { text = None; image = Some image; error = None }

let create_audio_content _data _mime_type ?annotations:_ ?meta:_ () = 
  `Audio { type_ = `Audio; data = _data; mime_type = _mime_type; annotations = None; meta = None }

let create_resource_link ~name ~uri ?title:_ ?description:_ ?mime_type:_ ?size:_ ?annotations:_ ?meta:_ () = 
  { type_ = `Resource_link; name; title = None; uri; description = None; mime_type = None; size = None; annotations = None; meta = None }

let create_embedded_resource resource ?annotations:_ ?meta:_ () = 
  { type_ = `Resource; resource; annotations = None; meta = None }

let create_message ~role ~content : message = 
  { role; content }

let create_sampling_message ~role ~content : sampling_message = 
  { role; content }

let create_prompt_message ~role ~content : message = 
  { role; content }

let create_error_data ~code ~message ?data:_ () = 
  { code; message; data = None }

let create_request_params ?progress_token () : request_params = 
  let meta = match progress_token with 
    | None -> None 
    | Some token -> Some { progress_token = Some token } 
  in
  { meta }

let create_paginated_request_params ?progress_token ?cursor () : paginated_request_params = 
  let meta = match progress_token with 
    | None -> None 
    | Some token -> Some { progress_token = Some token } 
  in
  { meta; cursor }

let create_notification_params ?progress_token () : notification_params = 
  let meta = match progress_token with 
    | None -> None 
    | Some token -> Some { progress_token = Some token } 
  in
  { meta }

let create_progress_notification_params ~progress_token ~progress ?total ?message () = 
  { progress_token; progress; total; message }

let create_cancelled_notification_params ~request_id ?reason ?meta () = 
  { request_id; reason; meta }

let create_resource_updated_notification_params ~uri ?meta () : resource_updated_notification_params = 
  { uri; meta }

let create_error_content error = 
  { text = None; image = None; error = Some error }
