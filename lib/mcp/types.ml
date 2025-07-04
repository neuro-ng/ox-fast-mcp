open Core
open Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Basic JSON representation with proper OCaml types *)
type json = Yojson.Safe.t

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

(** Basic types *)
type progress_token = string [@@deriving yojson]
type cursor = string [@@deriving yojson]
type role = [ `User | `Assistant ] [@@deriving yojson]
type request_id = [ `Int of int | `String of string ] [@@deriving yojson]

(** Request and notification types *)
type meta = {
  progress_token : progress_token option; [@key "progressToken"] [@yojson.option]
} [@@deriving yojson]

type request_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type paginated_request_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
  cursor : cursor option; [@yojson.option]
} [@@deriving yojson]

type notification_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Implementation info *)
type implementation = {
  name : string;
  title : string option; [@yojson.option]
  version : string;
} [@@deriving yojson]

(** Capabilities *)
type roots_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving yojson]

type sampling_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit [@default ()];
} [@@deriving yojson]

type elicitation_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit [@default ()];
} [@@deriving yojson]

type client_capabilities = {
  experimental : (string * json) list option; [@yojson.option]
  sampling : sampling_capability option; [@yojson.option]
  elicitation : elicitation_capability option; [@yojson.option]
  roots : roots_capability option; [@yojson.option]
} [@@deriving yojson]

type prompts_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving yojson]

type resources_capability = {
  subscribe : bool option; [@yojson.option]
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving yojson]

type tools_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving yojson]

type logging_capability = {
  (* Empty for now but may have fields in future *)
  _dummy : unit [@default ()];
} [@@deriving yojson]

type server_capabilities = {
  experimental : (string * json) list option; [@yojson.option]
  logging : logging_capability option; [@yojson.option]
  prompts : prompts_capability option; [@yojson.option]
  resources : resources_capability option; [@yojson.option]
  tools : tools_capability option; [@yojson.option]
} [@@deriving yojson]

(** Initialize types *)
type initialize_request_params = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : client_capabilities;
  client_info : implementation; [@key "clientInfo"]
} [@@deriving yojson]

type initialize_result = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : server_capabilities;
  server_info : implementation; [@key "serverInfo"]
  instructions : string option; [@yojson.option]
} [@@deriving yojson]

(** Progress notification *)
type progress_notification_params = {
  progress_token : progress_token; [@key "progressToken"]
  progress : float;
  total : float option; [@yojson.option]
  message : string option; [@yojson.option]
} [@@deriving yojson]

(** Resource types *)
type annotations = {
  audience : role list option; [@yojson.option]
  priority : float option; [@yojson.option]
} [@@deriving yojson]

type resource = {
  name : string;
  title : string option; [@yojson.option]
  uri : string;
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  size : int option; [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type resource_template = {
  name : string;
  title : string option; [@yojson.option]
  uri_template : string; [@key "uriTemplate"]
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource contents *)
type resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type text_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
  text : string;
} [@@deriving yojson]

type blob_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
  blob : string;
} [@@deriving yojson]

(** Tool types *)
type tool_annotations = (string * string) list [@@deriving yojson]

type tool = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  input_schema : json; [@key "inputSchema"]
  output_schema : json option; [@key "outputSchema"] [@yojson.option]
  annotations : tool_annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Content types *)
type text_content = {
  type_ : [`Text]; [@key "type"]
  text : string;
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type image_content = {
  type_ : [`Image]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type audio_content = {
  type_ : [`Audio]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type content = [
  | `Text of text_content
  | `Image of image_content
  | `Audio of audio_content
] [@@deriving yojson]

(** Message types *)
type message = {
  role : role;
  content : content;
} [@@deriving yojson]

(** Model types *)
type model_hint = {
  name : string option; [@yojson.option]
} [@@deriving yojson]

type model_preferences = {
  hints : model_hint list option; [@yojson.option]
  cost_priority : float option; [@key "costPriority"] [@yojson.option]
  speed_priority : float option; [@key "speedPriority"] [@yojson.option]
  intelligence_priority : float option; [@key "intelligencePriority"] [@yojson.option]
} [@@deriving yojson]

(** Completion types *)
type completion_argument = {
  name : string;
  value : string;
} [@@deriving yojson]

type completion_context = {
  arguments : (string * string) list option; [@yojson.option]
} [@@deriving yojson]

type completion = {
  values : string list;
  total : int option; [@yojson.option]
  has_more : bool option; [@key "hasMore"] [@yojson.option]
} [@@deriving yojson]

(** Root types *)
type root = {
  uri : string;
  name : string option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Error types *)
type error_data = {
  code : int;
  message : string;
  data : json option; [@yojson.option]
} [@@deriving yojson]

(** JSON-RPC types *)
type jsonrpc_request = {
  jsonrpc : string;
  id : request_id;
  method_ : string; [@key "method"]
  params : json option; [@yojson.option]
} [@@deriving yojson]

type jsonrpc_notification = {
  jsonrpc : string;
  method_ : string; [@key "method"]
  params : json option; [@yojson.option]
} [@@deriving yojson]

type jsonrpc_response = {
  jsonrpc : string;
  id : request_id;
  result : json;
} [@@deriving yojson]

type jsonrpc_error_response = {
  jsonrpc : string;
  id : request_id;
  error : error_data;
} [@@deriving yojson]

type jsonrpc_message = [
  | `Request of jsonrpc_request
  | `Notification of jsonrpc_notification
  | `Response of jsonrpc_response
  | `Error of jsonrpc_error_response
] [@@deriving yojson]

(** Logging types *)
type logging_level = [
  | `Debug
  | `Info
  | `Notice
  | `Warning
  | `Error
  | `Critical
  | `Alert
  | `Emergency
] [@@deriving yojson]

type set_level_request_params = {
  level : logging_level;
} [@@deriving yojson]

type logging_message_notification_params = {
  level : logging_level;
  logger : string option; [@yojson.option]
  data : json;
} [@@deriving yojson]

(** Include context types *)
type include_context = [
  | `None
  | `ThisServer
  | `AllServers
] [@@deriving yojson]

(** Resource reference types *)
type resource_template_reference = {
  type_ : [`Ref_resource]; [@key "type"]
  uri : string;
} [@@deriving yojson]

type prompt_reference = {
  type_ : [`Ref_prompt]; [@key "type"]
  name : string;
} [@@deriving yojson]

(** Elicitation types *)
type elicit_request_params = {
  message : string;
  requested_schema : json; [@key "requestedSchema"]
} [@@deriving yojson]

type elicit_result = {
  action : [`Accept | `Decline | `Cancel];
  content : (string * [`String of string | `Int of int | `Float of float | `Bool of bool | `Null]) list option; [@yojson.option]
} [@@deriving yojson]

(** Sampling types *)
type create_message_request_params = {
  messages : message list;
  model_preferences : model_preferences option; [@key "modelPreferences"] [@yojson.option]
  system_prompt : string option; [@key "systemPrompt"] [@yojson.option]
  include_context : include_context option; [@key "includeContext"] [@yojson.option]
  temperature : float option; [@yojson.option]
  max_tokens : int; [@key "maxTokens"]
  stop_sequences : string list option; [@key "stopSequences"] [@yojson.option]
  metadata : json option; [@yojson.option]
} [@@deriving yojson]

type stop_reason = [
  | `EndTurn
  | `StopSequence
  | `MaxTokens
  | `Other of string
] [@@deriving yojson]

type create_message_result = {
  role : role;
  content : content;
  model : string;
  stop_reason : stop_reason option; [@key "stopReason"] [@yojson.option]
} [@@deriving yojson]

(** Client request types *)
type client_request = [
  | `Ping of request_params option
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
  | `ListTools of paginated_request_params option
] [@@deriving yojson]

(** Client notification types *)
type client_notification = [
  | `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `Initialized
  | `RootsListChanged
] [@@deriving yojson]

(** Server request types *)
type server_request = [
  | `Ping of request_params option
  | `CreateMessage of create_message_request_params
  | `ListRoots of request_params option
  | `Elicit of elicit_request_params
] [@@deriving yojson]

(** Server notification types *)
type server_notification = [
  | `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `LoggingMessage of logging_message_notification_params
  | `ResourceUpdated of string
  | `ResourceListChanged
  | `ToolListChanged
  | `PromptListChanged
] [@@deriving yojson]

(** Prompt types *)
type prompt_argument = {
  name : string;
  description : string option; [@yojson.option]
  required : bool option; [@yojson.option]
} [@@deriving sexp, yojson]

type prompt = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  arguments : prompt_argument list option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type prompt_message = {
  role : role;
  content : content;
} [@@deriving sexp, yojson]

(** Resource embedded content *)
type embedded_resource = {
  type_ : [`Resource]; [@key "type"]
  resource : (text_resource_contents, blob_resource_contents) Either.t;
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type resource_link = {
  type_ : [`Resource_link]; [@key "type"]
  name : string;
  title : string option; [@yojson.option]
  uri : string;
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  size : int option; [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type content_block = {
  text : string option [@yojson.option];
  image : string option [@yojson.option];
  error : string option [@yojson.option];
} [@@deriving fields, yojson]

(** Empty result type *)
type empty_result = {
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Paginated result types *)
type paginated_result = {
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource operation results *)
type list_resources_result = {
  resources : resource list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type list_resource_templates_result = {
  resource_templates : resource_template list; [@key "resourceTemplates"]
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type read_resource_result = {
  contents : (text_resource_contents, blob_resource_contents) Either.t list;
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Tool operation results *)
type list_tools_result = {
  tools : tool list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type call_tool_result = {
  content : content list;
  structured_content : json option; [@key "structuredContent"] [@yojson.option]
  is_error : bool; [@key "isError"]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Prompt operation results *)
type list_prompts_result = {
  prompts : prompt list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type get_prompt_result = {
  description : string option; [@yojson.option]
  messages : prompt_message list;
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Root operation results *)
type list_roots_result = {
  roots : root list;
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Cancelled notification types *)
type cancelled_notification_params = {
  request_id : request_id; [@key "requestId"]
  reason : string option; [@yojson.option]
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource update notification types *)
type resource_updated_notification_params = {
  uri : string;
  meta : json option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Client result types *)
type client_result = [
  | `Empty of empty_result
  | `CreateMessage of create_message_result
  | `ListRoots of list_roots_result
  | `Elicit of elicit_result
] [@@deriving sexp, yojson]

(** Server result types *)
type server_result = [
  | `Empty of empty_result
  | `Initialize of initialize_result
  | `Complete of completion
  | `GetPrompt of get_prompt_result
  | `ListPrompts of list_prompts_result
  | `ListResources of list_resources_result
  | `ListResourceTemplates of list_resource_templates_result
  | `ReadResource of read_resource_result
  | `CallTool of call_tool_result
  | `ListTools of list_tools_result
] [@@deriving sexp, yojson]

(** Sampling message type *)
type sampling_message = {
  role : role;
  content : content;
} [@@deriving sexp, yojson]

(** Complete request params *)
type complete_request_params = {
  ref : resource_template_reference [@key "ref"];
  argument : completion_argument;
  context : completion_context option; [@yojson.option]
} [@@deriving sexp, yojson]

(** Subscribe request params *)
type subscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Unsubscribe request params *)
type unsubscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Deprecated resource reference type *)
type resource_reference = resource_template_reference [@@deriving sexp, yojson] [@@deprecated "Use resource_template_reference instead"]

(** Helper functions for common operations *)

let create_text_content text = { text = Some text; image = None; error = None }
let create_image_content image = { text = None; image = Some image; error = None }
let create_error_content error = { text = None; image = None; error = Some error }