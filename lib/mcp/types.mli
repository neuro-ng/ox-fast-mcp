(** Latest protocol version *)
val latest_protocol_version : string

(** Default negotiated version *)
val default_negotiated_version : string

(** Standard JSON-RPC error codes *)
val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int

(** SDK error codes *)
val connection_closed : int

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
  experimental : (string * Yojson.Safe.t) list option; [@yojson.option]
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
  experimental : (string * Yojson.Safe.t) list option; [@yojson.option]
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
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type resource_template = {
  name : string;
  title : string option; [@yojson.option]
  uri_template : string; [@key "uriTemplate"]
  description : string option; [@yojson.option]
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource contents *)
type resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type text_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  text : string;
} [@@deriving yojson]

type blob_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  blob : string;
} [@@deriving yojson]

(** Tool types *)
type tool_annotations = (string * string) list [@@deriving yojson]

type tool = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  input_schema : Yojson.Safe.t; [@key "inputSchema"]
  output_schema : Yojson.Safe.t option; [@key "outputSchema"] [@yojson.option]
  annotations : tool_annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Content types *)
type text_content = {
  type_ : [`Text]; [@key "type"]
  text : string;
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type image_content = {
  type_ : [`Image]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type audio_content = {
  type_ : [`Audio]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
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
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Error types *)
type error_data = {
  code : int;
  message : string;
  data : Yojson.Safe.t option; [@yojson.option]
} [@@deriving yojson]

(** JSON-RPC types *)
type jsonrpc_request = {
  jsonrpc : string;
  id : request_id;
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@yojson.option]
} [@@deriving yojson]

type jsonrpc_notification = {
  jsonrpc : string;
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@yojson.option]
} [@@deriving yojson]

type jsonrpc_response = {
  jsonrpc : string;
  id : request_id;
  result : Yojson.Safe.t;
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
  data : Yojson.Safe.t;
} [@@deriving yojson]

(** Include context types *)
type include_context = [
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
  requested_schema : Yojson.Safe.t; [@key "requestedSchema"]
} [@@deriving yojson]

type elicit_result = {
  action : [`Accept | `Decline | `Cancel];
  content : (string * [`String of string | `Int of int | `Float of float | `Bool of bool | `Null]) list option; [@yojson.option]
} [@@deriving yojson]

(** Sampling types *)
type create_message_request_params = {
  messages : message list;
  model_preferences : model_preferences option; [@key "modelPreferences"] [@yojson.option]
  stop_sequences : string list option; [@key "stopSequences"] [@yojson.option]
  metadata : Yojson.Safe.t option; [@yojson.option]
} [@@deriving yojson]

type stop_reason = [
  | `Stop
  | `Length
  | `MaxTokens
  | `Other of string
] [@@deriving yojson]

type create_message_result = {
  messages : message list;
  model : string;
  stop_reason : stop_reason option; [@key "stopReason"] [@yojson.option]
} [@@deriving yojson]

(** Client request types *)
type client_request = [
  | `Initialize of initialize_request_params
  | `SetLogLevel of set_level_request_params
  | `CallTool of string * Yojson.Safe.t option
  | `ListTools of paginated_request_params option
] [@@deriving yojson]

(** Client notification types *)
type client_notification = [
  | `Initialized
  | `RootsListChanged
] [@@deriving yojson]

(** Server request types *)
type server_request = [
  | `ListRoots of request_params option
  | `Elicit of elicit_request_params
] [@@deriving yojson]

(** Server notification types *)
type server_notification = [
  | `Progress of progress_notification_params
  | `LoggingMessage of logging_message_notification_params
  | `ToolListChanged
  | `PromptListChanged
] [@@deriving yojson]

(** Prompt types *)
type prompt = {
  name : string;
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
  arguments : (string * Yojson.Safe.t) list option; [@yojson.option]
  annotations : (string * string) list option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource embedded content *)
type embedded_resource = {
  type_ : [`Resource]; [@key "type"]
  resource : (text_resource_contents, blob_resource_contents) Either.t;
  annotations : annotations option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

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
} [@@deriving yojson]

type content_block = {
  text : string option [@yojson.option];
  image : string option [@yojson.option];
  error : string option [@yojson.option];
} [@@deriving fields, yojson]

(** Empty result type *)
type empty_result = {
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Paginated result types *)
type paginated_result = {
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource operation results *)
type list_resources_result = {
  resources : resource list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type list_resource_templates_result = {
  resource_templates : resource_template list; [@key "resourceTemplates"]
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type read_resource_result = {
  contents : (text_resource_contents, blob_resource_contents) Either.t list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Tool operation results *)
type list_tools_result = {
  tools : tool list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type call_tool_result = {
  content : content list;
  structured_content : Yojson.Safe.t option; [@key "structuredContent"] [@yojson.option]
  is_error : bool; [@key "isError"]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Prompt operation results *)
type list_prompts_result = {
  prompts : prompt list;
  next_cursor : cursor option; [@key "nextCursor"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

type get_prompt_result = {
  description : string option; [@yojson.option]
  messages : message list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Root operation results *)
type list_roots_result = {
  roots : root list;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Cancelled notification types *)
type cancelled_notification_params = {
  request_id : request_id; [@key "requestId"]
  reason : string option; [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource update notification types *)
type resource_updated_notification_params = {
  uri : string;
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Client result types *)
type client_result = [
  | `Empty of empty_result
  | `CreateMessage of create_message_result
  | `ListRoots of list_roots_result
  | `Elicit of elicit_result
] [@@deriving yojson]

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
] [@@deriving yojson]

(** Sampling message type *)
type sampling_message = {
  role : role;
  content : content;
} [@@deriving yojson]

(** Complete request params *)
type complete_request_params = {
  ref : resource_template_reference [@key "ref"];
  argument : completion_argument;
  context : completion_context option; [@yojson.option]
} [@@deriving yojson]

(** Subscribe request params *)
type subscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Unsubscribe request params *)
type unsubscribe_request_params = {
  uri : string;
  meta : meta option; [@key "_meta"] [@yojson.option]
} [@@deriving yojson]

(** Resource reference type (deprecated) *)
[@@@ocaml.warning "-3"]
type resource_reference = resource_template_reference [@@deriving yojson]
[@@deprecated "Use resource_template_reference instead"]
[@@@ocaml.warning "+3"]

(** Helper functions for common operations *)

(** Create a text content block *)
val create_text_content : string -> content_block

(** Create an image content block *)
val create_image_content : string -> content_block

(** Create an audio content block *)
val create_audio_content : string -> string -> ?annotations:annotations -> ?meta:Yojson.Safe.t -> unit -> content

(** Create a resource link content block *)
val create_resource_link :
  name:string ->
  uri:string ->
  ?title:string ->
  ?description:string ->
  ?mime_type:string ->
  ?size:int ->
  ?annotations:annotations ->
  ?meta:Yojson.Safe.t ->
  unit ->
  content_block

(** Create an embedded resource content block *)
val create_embedded_resource :
  (text_resource_contents, blob_resource_contents) Either.t ->
  ?annotations:annotations ->
  ?meta:Yojson.Safe.t ->
  unit ->
  content_block

(** Create a message *)
val create_message : role:role -> content:content -> message

(** Create a sampling message *)
val create_sampling_message : role:role -> content:content -> sampling_message

(** Create a prompt message *)
val create_prompt_message : role:role -> content:content -> message

(** Create an error data *)
val create_error_data : code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> error_data

(** Create a request params with meta *)
val create_request_params : ?progress_token:progress_token -> unit -> request_params

(** Create a paginated request params *)
val create_paginated_request_params :
  ?progress_token:progress_token ->
  ?cursor:cursor ->
  unit ->
  paginated_request_params

(** Create a notification params with meta *)
val create_notification_params : ?progress_token:progress_token -> unit -> notification_params

(** Create a progress notification params *)
val create_progress_notification_params :
  progress_token:progress_token ->
  progress:float ->
  ?total:float ->
  ?message:string ->
  unit ->
  progress_notification_params

(** Create a cancelled notification params *)
val create_cancelled_notification_params :
  request_id:request_id ->
  ?reason:string ->
  ?meta:Yojson.Safe.t ->
  unit ->
  cancelled_notification_params

(** Create a resource updated notification params *)
val create_resource_updated_notification_params :
  uri:string ->
  ?meta:Yojson.Safe.t ->
  unit ->
  resource_updated_notification_params

(** Create an error content block *)
val create_error_content : string -> content_block 