open Core

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
type progress_token = string [@@deriving sexp, yojson]
type cursor = string [@@deriving sexp, yojson]
type role = [ `User | `Assistant ] [@@deriving sexp, yojson]
type request_id = [ `Int of int | `String of string ] [@@deriving sexp, yojson]

(** Request and notification types *)
type meta = {
  progress_token : progress_token option; [@key "progressToken"] [@yojson.option]
} [@@deriving sexp, yojson]

type request_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type paginated_request_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
  cursor : cursor option; [@yojson.option]
} [@@deriving sexp, yojson]

type notification_params = {
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Implementation info *)
type implementation = {
  name : string;
  title : string option; [@yojson.option]
  version : string;
} [@@deriving sexp, yojson]

(** Capabilities *)
type roots_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving sexp, yojson]

type sampling_capability = {
  (* Empty for now but may have fields in future *)
} [@@deriving sexp, yojson]

type elicitation_capability = {
  (* Empty for now but may have fields in future *)
} [@@deriving sexp, yojson]

type client_capabilities = {
  experimental : (string * Yojson.Safe.t) list option; [@yojson.option]
  sampling : sampling_capability option; [@yojson.option]
  elicitation : elicitation_capability option; [@yojson.option]
  roots : roots_capability option; [@yojson.option]
} [@@deriving sexp, yojson]

type prompts_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving sexp, yojson]

type resources_capability = {
  subscribe : bool option; [@yojson.option]
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving sexp, yojson]

type tools_capability = {
  list_changed : bool option; [@key "listChanged"] [@yojson.option]
} [@@deriving sexp, yojson]

type logging_capability = {
  (* Empty for now but may have fields in future *)
} [@@deriving sexp, yojson]

type server_capabilities = {
  experimental : (string * Yojson.Safe.t) list option; [@yojson.option]
  logging : logging_capability option; [@yojson.option]
  prompts : prompts_capability option; [@yojson.option]
  resources : resources_capability option; [@yojson.option]
  tools : tools_capability option; [@yojson.option]
} [@@deriving sexp, yojson]

(** Initialize types *)
type initialize_request_params = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : client_capabilities;
  client_info : implementation; [@key "clientInfo"]
} [@@deriving sexp, yojson]

type initialize_result = {
  protocol_version : string; [@key "protocolVersion"]
  capabilities : server_capabilities;
  server_info : implementation; [@key "serverInfo"]
  instructions : string option; [@yojson.option]
} [@@deriving sexp, yojson]

(** Progress notification *)
type progress_notification_params = {
  progress_token : progress_token; [@key "progressToken"]
  progress : float;
  total : float option; [@yojson.option]
  message : string option; [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource types *)
type annotations = {
  audience : role list option; [@yojson.option]
  priority : float option; [@yojson.option]
} [@@deriving sexp, yojson]

type resource = {
  name : string;
  title : string option; [@yojson.option]
  uri : string;
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  size : int option; [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type resource_template = {
  name : string;
  title : string option; [@yojson.option]
  uri_template : string; [@key "uriTemplate"]
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource contents *)
type resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type text_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  text : string;
} [@@deriving sexp, yojson]

type blob_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  blob : string;
} [@@deriving sexp, yojson]

(** Tool types *)
type tool_annotations = {
  title : string option; [@yojson.option]
  read_only_hint : bool option; [@key "readOnlyHint"] [@yojson.option]
  destructive_hint : bool option; [@key "destructiveHint"] [@yojson.option]
  idempotent_hint : bool option; [@key "idempotentHint"] [@yojson.option]
  open_world_hint : bool option; [@key "openWorldHint"] [@yojson.option]
} [@@deriving sexp, yojson]

type tool = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  input_schema : Yojson.Safe.t; [@key "inputSchema"]
  output_schema : Yojson.Safe.t option; [@key "outputSchema"] [@yojson.option]
  annotations : tool_annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Content types *)
type text_content = {
  type_ : [`Text]; [@key "type"]
  text : string;
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type image_content = {
  type_ : [`Image]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type audio_content = {
  type_ : [`Audio]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type content = [
  | `Text of text_content
  | `Image of image_content
  | `Audio of audio_content
] [@@deriving sexp, yojson]

(** Message types *)
type message = {
  role : role;
  content : content;
} [@@deriving sexp, yojson]

(** Model types *)
type model_hint = {
  name : string option; [@yojson.option]
} [@@deriving sexp, yojson]

type model_preferences = {
  hints : model_hint list option; [@yojson.option]
  cost_priority : float option; [@key "costPriority"] [@yojson.option]
  speed_priority : float option; [@key "speedPriority"] [@yojson.option]
  intelligence_priority : float option; [@key "intelligencePriority"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Completion types *)
type completion_argument = {
  name : string;
  value : string;
} [@@deriving sexp, yojson]

type completion_context = {
  arguments : (string * string) list option; [@yojson.option]
} [@@deriving sexp, yojson]

type completion = {
  values : string list;
  total : int option; [@yojson.option]
  has_more : bool option; [@key "hasMore"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Root types *)
type root = {
  uri : string;
  name : string option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Error types *)
type error_data = {
  code : int;
  message : string;
  data : Yojson.Safe.t option; [@yojson.option]
} [@@deriving sexp, yojson]

(** JSON-RPC types *)
type jsonrpc_request = {
  jsonrpc : string;
  id : request_id;
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@yojson.option]
} [@@deriving sexp, yojson]

type jsonrpc_notification = {
  jsonrpc : string;
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@yojson.option]
} [@@deriving sexp, yojson]

type jsonrpc_response = {
  jsonrpc : string;
  id : request_id;
  result : Yojson.Safe.t;
} [@@deriving sexp, yojson]

type jsonrpc_error_response = {
  jsonrpc : string;
  id : request_id;
  error : error_data;
} [@@deriving sexp, yojson]

type jsonrpc_message = [
  | `Request of jsonrpc_request
  | `Notification of jsonrpc_notification
  | `Response of jsonrpc_response
  | `Error of jsonrpc_error_response
] [@@deriving sexp, yojson]

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
] [@@deriving sexp, yojson]

type set_level_request_params = {
  level : logging_level;
} [@@deriving sexp, yojson]

type logging_message_notification_params = {
  level : logging_level;
  logger : string option; [@yojson.option]
  data : Yojson.Safe.t;
} [@@deriving sexp, yojson]

(** Include context types *)
type include_context = [
  | `None
  | `ThisServer
  | `AllServers
] [@@deriving sexp, yojson]

(** Resource reference types *)
type resource_template_reference = {
  type_ : [`Ref_resource]; [@key "type"]
  uri : string;
} [@@deriving sexp, yojson]

type prompt_reference = {
  type_ : [`Ref_prompt]; [@key "type"]
  name : string;
} [@@deriving sexp, yojson]

(** Elicitation types *)
type elicit_request_params = {
  message : string;
  requested_schema : Yojson.Safe.t; [@key "requestedSchema"]
} [@@deriving sexp, yojson]

type elicit_result = {
  action : [`Accept | `Decline | `Cancel];
  content : (string * [`String of string | `Int of int | `Float of float | `Bool of bool | `Null]) list option; [@yojson.option]
} [@@deriving sexp, yojson]

(** Sampling types *)
type create_message_request_params = {
  messages : message list;
  model_preferences : model_preferences option; [@key "modelPreferences"] [@yojson.option]
  system_prompt : string option; [@key "systemPrompt"] [@yojson.option]
  include_context : include_context option; [@key "includeContext"] [@yojson.option]
  temperature : float option; [@yojson.option]
  max_tokens : int; [@key "maxTokens"]
  stop_sequences : string list option; [@key "stopSequences"] [@yojson.option]
  metadata : Yojson.Safe.t option; [@yojson.option]
} [@@deriving sexp, yojson]

type stop_reason = [
  | `EndTurn
  | `StopSequence
  | `MaxTokens
  | `Other of string
] [@@deriving sexp, yojson]

type create_message_result = {
  role : role;
  content : content;
  model : string;
  stop_reason : stop_reason option; [@key "stopReason"] [@yojson.option]
} [@@deriving sexp, yojson]

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
  | `CallTool of string * Yojson.Safe.t option
  | `ListTools of paginated_request_params option
] [@@deriving sexp, yojson]

(** Client notification types *)
type client_notification = [
  | `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `Initialized
  | `RootsListChanged
] [@@deriving sexp, yojson]

(** Server request types *)
type server_request = [
  | `Ping of request_params option
  | `CreateMessage of create_message_request_params
  | `ListRoots of request_params option
  | `Elicit of elicit_request_params
] [@@deriving sexp, yojson]

(** Server notification types *)
type server_notification = [
  | `Cancelled of request_id * string option
  | `Progress of progress_notification_params
  | `LoggingMessage of logging_message_notification_params
  | `ResourceUpdated of string
  | `ResourceListChanged
  | `ToolListChanged
  | `PromptListChanged
] [@@deriving sexp, yojson]

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
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
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
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
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
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type content_block = [
  | content
  | `Resource_link of resource_link
  | `Embedded_resource of embedded_resource
] [@@deriving sexp, yojson]

(** Empty result type *)
type empty_result = {
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Paginated result types *)
type paginated_result = {
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource operation results *)
type list_resources_result = {
  resources : resource list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type list_resource_templates_result = {
  resource_templates : resource_template list; [@key "resourceTemplates"]
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type read_resource_result = {
  contents : (text_resource_contents, blob_resource_contents) Either.t list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Tool operation results *)
type list_tools_result = {
  tools : tool list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type call_tool_result = {
  content : content list;
  structured_content : Yojson.Safe.t option; [@key "structuredContent"] [@yojson.option]
  is_error : bool; [@key "isError"]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Prompt operation results *)
type list_prompts_result = {
  prompts : prompt list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

type get_prompt_result = {
  description : string option; [@yojson.option]
  messages : prompt_message list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Root operation results *)
type list_roots_result = {
  roots : root list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Cancelled notification types *)
type cancelled_notification_params = {
  request_id : request_id; [@key "requestId"]
  reason : string option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving sexp, yojson]

(** Resource update notification types *)
type resource_updated_notification_params = {
  uri : string;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
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

(** Create a text content block *)
let create_text_content text ?annotations ?meta () =
  `Text { type_ = `Text; text; annotations; meta }

(** Create an image content block *)
let create_image_content data mime_type ?annotations ?meta () =
  `Image { type_ = `Image; data; mime_type; annotations; meta }

(** Create an audio content block *)
let create_audio_content data mime_type ?annotations ?meta () =
  `Audio { type_ = `Audio; data; mime_type; annotations; meta }

(** Create a resource link content block *)
let create_resource_link ~name ~uri ?title ?description ?mime_type ?size ?annotations ?meta () =
  `Resource_link {
    type_ = `Resource_link;
    name;
    title;
    uri;
    description;
    mime_type;
    size;
    annotations;
    meta;
  }

(** Create an embedded resource content block *)
let create_embedded_resource resource ?annotations ?meta () =
  `Embedded_resource {
    type_ = `Resource;
    resource;
    annotations;
    meta;
  }

(** Create a message *)
let create_message ~role ~content =
  { role; content }

(** Create a sampling message *)
let create_sampling_message ~role ~content =
  { role; content }

(** Create a prompt message *)
let create_prompt_message ~role ~content =
  { role; content }

(** Create an error data *)
let create_error_data ~code ~message ?data () =
  { code; message; data }

(** Create a request params with meta *)
let create_request_params ?progress_token () =
  { meta = Option.map progress_token ~f:(fun token -> { progress_token = Some token }) }

(** Create a paginated request params *)
let create_paginated_request_params ?progress_token ?cursor () =
  { meta = Option.map progress_token ~f:(fun token -> { progress_token = Some token });
    cursor }

(** Create a notification params with meta *)
let create_notification_params ?progress_token () =
  { meta = Option.map progress_token ~f:(fun token -> { progress_token = Some token }) }

(** Create a progress notification params *)
let create_progress_notification_params ~progress_token ~progress ?total ?message () =
  { progress_token; progress; total; message }

(** Create a cancelled notification params *)
let create_cancelled_notification_params ~request_id ?reason ?meta () =
  { request_id; reason; meta }

(** Create a resource updated notification params *)
let create_resource_updated_notification_params ~uri ?meta () =
  { uri; meta }
