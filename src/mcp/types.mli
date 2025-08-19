open Base


(* Constants *)
val latest_protocol_version : string
val default_negotiated_version : string

(* Basic Types *)
type json = Yojson.Safe.t
type progress_token = [ `String of string | `Int of int ]
val progress_token_of_yojson : Yojson.Safe.t -> progress_token
val progress_token_to_yojson : progress_token -> Yojson.Safe.t
type cursor = string
val cursor_of_yojson : Yojson.Safe.t -> cursor
val cursor_to_yojson : cursor -> Yojson.Safe.t
type role = [ `User | `Assistant ]
val role_of_yojson : Yojson.Safe.t -> role
val role_to_yojson : role -> Yojson.Safe.t
type request_id = [ `Int of int | `String of string ]
val request_id_of_yojson : Yojson.Safe.t -> request_id
val request_id_to_yojson : request_id -> Yojson.Safe.t
val yojson_of_request_id : request_id -> Yojson.Safe.t
type any_function = unit -> unit (* A placeholder, as the original is very generic *)
val any_function_of_yojson : Yojson.Safe.t -> any_function
val any_function_to_yojson : any_function -> Yojson.Safe.t

(* Manual yojson conversion functions for json type *)
val json_of_yojson : Yojson.Safe.t -> json
val yojson_of_json : json -> Yojson.Safe.t

(* Manual sexp conversion functions for json type *)
val json_of_sexp : Sexp.t -> json
val sexp_of_json : json -> Sexp.t

(* Request/Notification/Result Base Types *)

module Request_params : sig
  module Meta : sig
    type t = {
      progress_token: progress_token option [@key "progressToken"] [@yojson.option];
    } [@@deriving compare, sexp]
  end

  type t = {
    meta: Meta.t option [@key "_meta"] [@yojson.option];
  } [@@deriving compare, sexp]
end

type paginated_request_params = {
  cursor: cursor option [@default None] [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]


module Notification_params : sig
    module Meta : sig
        type t = unit [@@deriving compare, sexp]
    end

    type t = {
        meta: Meta.t option [@key "_meta"] [@default None] [@yojson.option];
    } [@@deriving compare, sexp]
end

(* Alias used by shared modules for request metadata *)
type meta = json
val meta_of_yojson : Yojson.Safe.t -> meta
val meta_to_yojson : meta -> Yojson.Safe.t
val yojson_of_meta : meta -> Yojson.Safe.t

type 'params request = {
  method_ : string [@key "method"];
  params: 'params;
} [@@deriving compare, sexp]

type 'params paginated_request = 'params request [@@deriving compare, sexp]

type 'params notification = {
  method_ : string [@key "method"];
  params: 'params;
} [@@deriving compare, sexp]

type result = {
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type paginated_result = {
  next_cursor: cursor option [@key "nextCursor"] [@default None] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]


(* JSON-RPC Types *)

type jsonrpc_request = {
  jsonrpc: string;
  id: request_id;
  method_: string [@key "method"];
  params: Yojson.Safe.t option [@default None] [@yojson.option];
} [@@deriving compare, sexp]
val jsonrpc_request_of_yojson : Yojson.Safe.t -> jsonrpc_request
val jsonrpc_request_to_yojson : jsonrpc_request -> Yojson.Safe.t
val yojson_of_jsonrpc_request : jsonrpc_request -> Yojson.Safe.t
type jsonrpc_notification = {
  jsonrpc: string;
  method_: string [@key "method"];
  params: Yojson.Safe.t option [@default None] [@yojson.option];
} [@@deriving compare, sexp]
val jsonrpc_notification_of_yojson : Yojson.Safe.t -> jsonrpc_notification
val jsonrpc_notification_to_yojson : jsonrpc_notification -> Yojson.Safe.t
val yojson_of_jsonrpc_notification : jsonrpc_notification -> Yojson.Safe.t
type jsonrpc_response = {
  jsonrpc: string;
  id: request_id;
  result: Yojson.Safe.t;
} [@@deriving compare, sexp]
val jsonrpc_response_of_yojson : Yojson.Safe.t -> jsonrpc_response
val jsonrpc_response_to_yojson : jsonrpc_response -> Yojson.Safe.t
val yojson_of_jsonrpc_response : jsonrpc_response -> Yojson.Safe.t
type error_data = {
  code: int;
  message: string;
  data: Yojson.Safe.t option [@default None] [@yojson.option];
} [@@deriving compare, sexp]
val error_data_of_yojson : Yojson.Safe.t -> error_data
val error_data_to_yojson : error_data -> Yojson.Safe.t
val yojson_of_error_data : error_data -> Yojson.Safe.t
type jsonrpc_error = {
  jsonrpc: string;
  id: request_id;
  error: error_data;
} [@@deriving compare, sexp]
val jsonrpc_error_of_yojson : Yojson.Safe.t -> jsonrpc_error
val jsonrpc_error_to_yojson : jsonrpc_error -> Yojson.Safe.t
val yojson_of_jsonrpc_error : jsonrpc_error -> Yojson.Safe.t
type jsonrpc_message =
  [ `Request of jsonrpc_request
  | `Notification of jsonrpc_notification
  | `Response of jsonrpc_response
  | `Error of jsonrpc_error
  ] [@@deriving compare, sexp]
val jsonrpc_message_of_yojson : Yojson.Safe.t -> jsonrpc_message
val jsonrpc_message_to_yojson : jsonrpc_message -> Yojson.Safe.t
val yojson_of_jsonrpc_message : jsonrpc_message -> Yojson.Safe.t

type empty_result = result [@@deriving compare, sexp]

(* Metadata and Capabilities *)

type base_metadata = {
  name: string;
  title: string option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type implementation = {
  version: string;
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving compare, sexp]

type roots_capability = {
  list_changed: bool option [@key "listChanged"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type sampling_capability = Yojson.Safe.t [@@deriving compare, sexp]
type elicitation_capability = Yojson.Safe.t [@@deriving compare, sexp]

type client_capabilities = {
  experimental: (string, Yojson.Safe.t) List.Assoc.t option [@default None] [@yojson.option];
  sampling: sampling_capability option [@default None] [@yojson.option];
  elicitation: elicitation_capability option [@default None] [@yojson.option];
  roots: roots_capability option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type prompts_capability = {
  list_changed: bool option [@key "listChanged"] [@default None] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type resources_capability = {
  subscribe: bool option [@default None] [@yojson.option];
  list_changed: bool option [@key "listChanged"] [@default None] [@yojson.option];
} [@@deriving yojson, compare, sexp]
val resources_capability_of_yojson : Yojson.Safe.t -> resources_capability
val resources_capability_to_yojson : resources_capability -> Yojson.Safe.t

type tools_capability = {
  list_changed: bool option [@key "listChanged"] [@default None] [@yojson.option];
} [@@deriving yojson, compare, sexp]
val tools_capability_of_yojson : Yojson.Safe.t -> tools_capability
val tools_capability_to_yojson : tools_capability -> Yojson.Safe.t

type logging_capability = Yojson.Safe.t [@@deriving yojson, compare, sexp]
val logging_capability_of_yojson : Yojson.Safe.t -> logging_capability
val logging_capability_to_yojson : logging_capability -> Yojson.Safe.t

type server_capabilities = {
  experimental: (string, Yojson.Safe.t) List.Assoc.t option [@default None] [@yojson.option];
  logging: logging_capability option [@default None] [@yojson.option];
  prompts: prompts_capability option [@default None] [@yojson.option];
  resources: resources_capability option [@default None] [@yojson.option];
  tools: tools_capability option [@default None] [@yojson.option];
} [@@deriving yojson, compare, sexp]
val server_capabilities_of_yojson : Yojson.Safe.t -> server_capabilities
val server_capabilities_to_yojson : server_capabilities -> Yojson.Safe.t

(* Initialization *)

type initialize_request_params = {
  protocol_version: string [@key "protocolVersion"];
  capabilities: client_capabilities;
  client_info: implementation [@key "clientInfo"];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type initialize_request = initialize_request_params request [@@deriving compare, sexp]

type initialize_result = {
  protocol_version: string [@key "protocolVersion"];
  capabilities: server_capabilities;
  server_info: implementation [@key "serverInfo"];
  instructions: string option [@default None] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

type initialized_notification = (Notification_params.t option) notification [@@deriving compare, sexp]

(* Ping *)
type ping_request = (Request_params.t option) request [@@deriving compare, sexp]

(* Progress *)
type progress_notification_params = {
  progress_token: progress_token [@key "progressToken"];
  progress: float;
  total: float option [@default None] [@yojson.option];
  message: string option [@default None] [@yojson.option];
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving compare, sexp]

type progress_notification = progress_notification_params notification [@@deriving compare, sexp]

(* Resources *)
type list_resources_request = paginated_request_params paginated_request [@@deriving compare, sexp]

type annotations = {
  audience: role list option [@default None] [@yojson.option];
  priority: float option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type resource = {
  uri: string; (* Using string for URI for now, can be refined with Uri.t *)
  description: string option [@default None] [@yojson.option];
  mime_type: string option [@key "mimeType"] [@default None] [@yojson.option];
  size: int option [@default None] [@yojson.option];
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]
val resource_of_yojson : Yojson.Safe.t -> resource
val resource_to_yojson : resource -> Yojson.Safe.t

type resource_template = {
  uri_template: string [@key "uriTemplate"];
  description: string option [@default None] [@yojson.option];
  mime_type: string option [@key "mimeType"] [@default None] [@yojson.option];
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving compare, sexp]

type list_resources_result = {
  resources: resource list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving compare, sexp]

type list_resource_templates_request = paginated_request_params paginated_request [@@deriving compare, sexp]

type list_resource_templates_result = {
  resource_templates: resource_template list [@key "resourceTemplates"];
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving compare, sexp]

type read_resource_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type read_resource_request = read_resource_request_params request [@@deriving compare, sexp]

type resource_contents = {
  uri: string;
  mime_type: string option [@key "mimeType"] [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type text_resource_contents = {
  text: string;
  resource_contents: resource_contents [@to_yojson resource_contents_to_yojson] [@of_yojson resource_contents_of_yojson];
} [@@deriving compare, sexp]

type blob_resource_contents = {
  blob: string;
  resource_contents: resource_contents [@to_yojson resource_contents_to_yojson] [@of_yojson resource_contents_of_yojson];
} [@@deriving compare, sexp]

type read_resource_result = {
  contents: [ `Text of text_resource_contents | `Blob of blob_resource_contents ] list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

type resource_list_changed_notification = (Notification_params.t option) notification [@@deriving compare, sexp]

type subscribe_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type subscribe_request = subscribe_request_params request [@@deriving compare, sexp]

type unsubscribe_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type unsubscribe_request = unsubscribe_request_params request [@@deriving compare, sexp]

type resource_updated_notification_params = {
  uri: string;
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving compare, sexp]

type resource_updated_notification = resource_updated_notification_params notification [@@deriving compare, sexp]

(* Prompts *)
type list_prompts_request = paginated_request_params paginated_request [@@deriving compare, sexp]

type prompt_argument = {
  name: string;
  description: string option [@default None] [@yojson.option];
  required: bool option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type prompt = {
  description: string option [@default None] [@yojson.option];
  arguments: prompt_argument list option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving compare, sexp]

type list_prompts_result = {
  prompts: prompt list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving compare, sexp]

type get_prompt_request_params = {
  name: string;
  arguments: (string, string) List.Assoc.t option [@default None] [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type get_prompt_request = get_prompt_request_params request [@@deriving compare, sexp]

type text_content = {
  type_: [ `Text ] [@key "type"];
  text: string;
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type image_content = {
  type_: [ `Image ] [@key "type"];
  data: string;
  mime_type: string [@key "mimeType"];
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type audio_content = {
  type_: [ `Audio ] [@key "type"];
  data: string;
  mime_type: string [@key "mimeType"];
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type sampling_message = {
  role: role;
  content: [ `Text of text_content | `Image of image_content | `Audio of audio_content ];
} [@@deriving compare, sexp]

type embedded_resource = {
  type_: [ `Resource ] [@key "type"];
  resource: [ `Text of text_resource_contents | `Blob of blob_resource_contents ];
  annotations: annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type resource_link = {
  type_: [ `Resource_link ] [@key "type"];
  resource: resource [@to_yojson resource_to_yojson] [@of_yojson resource_of_yojson];
} [@@deriving compare, sexp]

type content_block =
  [ `Text of text_content
  | `Image of image_content
  | `Audio of audio_content
  | `Resource_link of resource_link
  | `Embedded_resource of embedded_resource
  ] [@@deriving compare, sexp]

type content = content_block [@@deriving compare, sexp]

type prompt_message = {
  role: role;
  content: content_block;
} [@@deriving compare, sexp]

type get_prompt_result = {
  description: string option [@default None] [@yojson.option];
  messages: prompt_message list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

type prompt_list_changed_notification = (Notification_params.t option) notification [@@deriving compare, sexp]

(* Tools *)
type list_tools_request = paginated_request_params paginated_request [@@deriving compare, sexp]

type tool_annotations = {
  title: string option [@default None] [@yojson.option];
  read_only_hint: bool option [@key "readOnlyHint"] [@default None] [@yojson.option];
  destructive_hint: bool option [@key "destructiveHint"] [@default None] [@yojson.option];
  idempotent_hint: bool option [@key "idempotentHint"] [@default None] [@yojson.option];
  open_world_hint: bool option [@key "openWorldHint"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type tool = {
  description: string option [@default None] [@yojson.option];
  input_schema: Yojson.Safe.t [@key "inputSchema"];
  output_schema: Yojson.Safe.t option [@key "outputSchema"] [@default None] [@yojson.option];
  annotations: tool_annotations option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving compare, sexp]

type list_tools_result = {
  tools: tool list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving compare, sexp]

type call_tool_request_params = {
  name: string;
  arguments: Yojson.Safe.t option [@default None] [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type call_tool_request = call_tool_request_params request [@@deriving compare, sexp]

type call_tool_result = {
  content: content_block list;
  structured_content: Yojson.Safe.t option [@key "structuredContent"] [@default None] [@yojson.option];
  is_error: bool [@key "isError"] [@default false];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

type tool_list_changed_notification = (Notification_params.t option) notification [@@deriving compare, sexp]

(* Logging *)
type logging_level = [ `Debug | `Info | `Notice | `Warning | `Error | `Critical | `Alert | `Emergency ] [@@deriving compare, sexp]

type set_level_request_params = {
  level: logging_level;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type set_level_request = set_level_request_params request [@@deriving compare, sexp]

type logging_message_notification_params = {
  level: logging_level;
  logger: string option [@default None] [@yojson.option];
  data: Yojson.Safe.t;
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving compare, sexp]

type logging_message_notification = logging_message_notification_params notification [@@deriving compare, sexp]

(* Sampling *)
type include_context = [ `None | `ThisServer | `AllServers ] [@@deriving compare, sexp]

type model_hint = {
  name: string option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type model_preferences = {
  hints: model_hint list option [@default None] [@yojson.option];
  cost_priority: float option [@key "costPriority"] [@default None] [@yojson.option];
  speed_priority: float option [@key "speedPriority"] [@default None] [@yojson.option];
  intelligence_priority: float option [@key "intelligencePriority"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type create_message_request_params = {
  messages: sampling_message list;
  model_preferences: model_preferences option [@key "modelPreferences"] [@default None] [@yojson.option];
  system_prompt: string option [@key "systemPrompt"] [@default None] [@yojson.option];
  include_context: include_context option [@key "includeContext"] [@default None] [@yojson.option];
  temperature: float option [@default None] [@yojson.option];
  max_tokens: int [@key "maxTokens"];
  stop_sequences: string list option [@key "stopSequences"] [@default None] [@yojson.option];
  metadata: Yojson.Safe.t option [@default None] [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type create_message_request = create_message_request_params request [@@deriving compare, sexp]

type stop_reason = [ `EndTurn | `StopSequence | `MaxTokens | `String of string ] [@@deriving compare, sexp]

type create_message_result = {
  role: role;
  content: [ `Text of text_content | `Image of image_content | `Audio of audio_content ];
  model: string;
  stop_reason: stop_reason option [@key "stopReason"] [@default None] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

(* Completion *)
type resource_template_reference = {
  type_: [ `Ref_resource ] [@key "type"];
  uri: string;
} [@@deriving compare, sexp]

type resource_reference = resource_template_reference [@@deriving compare, sexp]

type prompt_reference = {
  type_: [ `Ref_prompt ] [@key "type"];
  name: string;
} [@@deriving compare, sexp]

type completion_argument = {
  name: string;
  value: string;
} [@@deriving compare, sexp]

type completion_context = {
  arguments: (string, string) List.Assoc.t option [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type complete_request_params = {
  ref: [ `Resource of resource_template_reference | `Prompt of prompt_reference ];
  argument: completion_argument;
  context: completion_context option [@default None] [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type complete_request = complete_request_params request [@@deriving compare, sexp]

type completion = {
  values: string list;
  total: int option [@default None] [@yojson.option];
  has_more: bool option [@key "hasMore"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type complete_result = {
  completion: completion;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

(* Roots *)
type list_roots_request = (Request_params.t option) request [@@deriving compare, sexp]

type root = {
  uri: string; (* Should be FileUrl, using string for now *)
  name: string option [@default None] [@yojson.option];
  meta: Yojson.Safe.t option [@key "_meta"] [@default None] [@yojson.option];
} [@@deriving compare, sexp]

type list_roots_result = {
  roots: root list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

type roots_list_changed_notification = (Notification_params.t option) notification [@@deriving compare, sexp]

(* Cancellation *)
type cancelled_notification_params = {
  request_id: request_id [@key "requestId"];
  reason: string option [@default None] [@yojson.option];
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving compare, sexp]

type cancelled_notification = cancelled_notification_params notification [@@deriving compare, sexp]

(* Elicitation *)
type elicit_requested_schema = Yojson.Safe.t [@@deriving compare, sexp]

type elicit_request_params = {
  message: string;
  requested_schema: elicit_requested_schema [@key "requestedSchema"];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving compare, sexp]

type elicit_request = elicit_request_params request [@@deriving compare, sexp]

type elicit_result = {
  action: [ `Accept | `Decline | `Cancel ];
  content: Yojson.Safe.t option [@default None] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving compare, sexp]

(* Message Aggregations *)
type client_request =
  [ `Ping of ping_request
  | `Initialize of initialize_request
  | `Complete of complete_request
  | `SetLevel of set_level_request
  | `GetPrompt of get_prompt_request
  | `ListPrompts of list_prompts_request
  | `ListResources of list_resources_request
  | `ListResourceTemplates of list_resource_templates_request
  | `ReadResource of read_resource_request
  | `Subscribe of subscribe_request
  | `Unsubscribe of unsubscribe_request
  | `CallTool of call_tool_request
  | `ListTools of list_tools_request
  ] [@@deriving compare, sexp]

type client_notification =
  [ `Cancelled of cancelled_notification
  | `Progress of progress_notification
  | `Initialized of initialized_notification
  | `RootsListChanged of roots_list_changed_notification
  ] [@@deriving compare, sexp]

type client_result =
  [ `Empty of empty_result
  | `CreateMessage of create_message_result
  | `ListRoots of list_roots_result
  | `Elicit of elicit_result
  ] [@@deriving compare, sexp]

type server_request =
  [ `Ping of ping_request
  | `CreateMessage of create_message_request
  | `ListRoots of list_roots_request
  | `Elicit of elicit_request
  ] [@@deriving compare, sexp]

type server_notification =
  [ `Cancelled of cancelled_notification
  | `Progress of progress_notification
  | `LoggingMessage of logging_message_notification
  | `ResourceUpdated of resource_updated_notification
  | `ResourceListChanged of resource_list_changed_notification
  | `ToolListChanged of tool_list_changed_notification
  | `PromptListChanged of prompt_list_changed_notification
  ] [@@deriving compare, sexp]

type server_result =
  [ `Empty of empty_result
  | `Initialize of initialize_result
  | `Complete of complete_result
  | `GetPrompt of get_prompt_result
  | `ListPrompts of list_prompts_result
  | `ListResources of list_resources_result
  | `ListResourceTemplates of list_resource_templates_result
  | `ReadResource of read_resource_result
  | `CallTool of call_tool_result
  | `ListTools of list_tools_result
  ] [@@deriving yojson, compare, sexp]