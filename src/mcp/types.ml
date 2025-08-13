open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Constants *)
let latest_protocol_version = "2025-06-18"
let default_negotiated_version = "2025-03-26"

(* Basic Types *)
type json = Yojson.Safe.t
type progress_token = [ `String of string | `Int of int ] [@@deriving yojson, compare, sexp]
type cursor = string [@@deriving yojson, compare, sexp]
type role = [ `User | `Assistant ] [@@deriving yojson, compare, sexp]
type request_id = [ `Int of int | `String of string ] [@@deriving yojson, compare, sexp]
type any_function = unit -> unit (* A placeholder, as the original is very generic *)


(* Manual yojson conversion functions for json type *)
let json_of_yojson (j : Yojson.Safe.t) : json = j
let yojson_of_json (j : json) : Yojson.Safe.t = j

(* Aliases required by the .mli signatures *)
let progress_token_to_yojson = yojson_of_progress_token
let cursor_to_yojson = yojson_of_cursor
let role_to_yojson = yojson_of_role
let request_id_to_yojson = yojson_of_request_id
let any_function_of_yojson (_ : Yojson.Safe.t) : any_function = fun () -> ()
let any_function_to_yojson (_ : any_function) : Yojson.Safe.t = `String "<function>"

(* Provide compare for json to support [@@deriving compare] in downstream types *)
let compare_json (a : json) (b : json) : int =
  String.compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)

(* Manual sexp conversion functions for json type *)
let json_of_sexp (s : Sexp.t) : json =
  Yojson.Safe.from_string (Sexp.to_string_hum s)

let sexp_of_json (j : json) : Sexp.t =
  Parsexp.Single.parse_string_exn (Yojson.Safe.to_string j)

type json_value =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of json_value list
  | `Assoc of (string * json_value) list ]
[@@deriving yojson]


(* Request/Notification/Result Base Types *)

module Request_params = struct
  module Meta = struct
    type t = {
      progress_token: progress_token option [@key "progressToken"] [@yojson.option];
    } [@@deriving yojson, compare, sexp]
  end

  type t = {
    meta: Meta.t option [@key "_meta"] [@yojson.option];
  } [@@deriving yojson, compare, sexp]
end

type paginated_request_params = {
  cursor: cursor option [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]


module Notification_params = struct
    module Meta = struct
        type t = unit [@@deriving yojson, compare, sexp]
    end

    type t = {
        meta: Meta.t option [@key "_meta"] [@yojson.option];
    } [@@deriving yojson, compare, sexp]
end

type 'params request = {
  method_ : string [@key "method"];
  params: 'params;
} [@@deriving yojson, compare, sexp]

type 'params paginated_request = 'params request [@@deriving yojson, compare, sexp]

type 'params notification = {
  method_ : string [@key "method"];
  params: 'params;
} [@@deriving yojson, compare, sexp]

type meta = json [@@deriving yojson, sexp]
let compare_meta = compare_json
let meta_of_yojson (j : Yojson.Safe.t) : meta = j
let meta_to_yojson (m : meta) : Yojson.Safe.t = m
let yojson_of_meta (m : meta) : Yojson.Safe.t = m

type result = { meta: meta option [@key "_meta"] [@yojson.option] }
[@@deriving yojson, sexp]
let compare_result (a : result) (b : result) : int =
  Option.compare compare_meta a.meta b.meta

type paginated_result = {
  next_cursor: cursor option [@key "nextCursor"] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]


(* JSON-RPC Types *)

type jsonrpc_request = {
  jsonrpc: string;
  id: request_id;
  method_: string [@key "method"];
  params: json option [@yojson.option];
} [@@deriving yojson, compare, sexp]
let jsonrpc_request_to_yojson = yojson_of_jsonrpc_request

type jsonrpc_notification = {
  jsonrpc: string;
  method_: string [@key "method"];
  params: json option [@yojson.option];
} [@@deriving yojson, compare, sexp]
let jsonrpc_notification_to_yojson = yojson_of_jsonrpc_notification

type jsonrpc_response = {
  jsonrpc: string;
  id: request_id;
  result: json;
} [@@deriving yojson, compare, sexp]
let jsonrpc_response_to_yojson = yojson_of_jsonrpc_response

type error_data = {
  code: int;
  message: string;
  data: json option [@yojson.option];
} [@@deriving yojson, compare, sexp]
let error_data_to_yojson = yojson_of_error_data

type jsonrpc_error = {
  jsonrpc: string;
  id: request_id;
  error: error_data;
} [@@deriving yojson, compare, sexp]
let jsonrpc_error_to_yojson = yojson_of_jsonrpc_error

type jsonrpc_message =
  [ `Request of jsonrpc_request
  | `Notification of jsonrpc_notification
  | `Response of jsonrpc_response
  | `Error of jsonrpc_error
  ] [@@deriving yojson, compare, sexp]
let jsonrpc_message_to_yojson = yojson_of_jsonrpc_message

type empty_result = result [@@deriving yojson, compare, sexp]

(* Metadata and Capabilities *)

type base_metadata = {
  name: string;
  title: string option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type implementation = {
  version: string;
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]

type roots_capability = {
  list_changed: bool option [@key "listChanged"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type sampling_capability = json [@@deriving yojson, compare, sexp]
type elicitation_capability = json [@@deriving yojson, compare, sexp]

type client_capabilities = {
  experimental: (string * json) list option [@yojson.option];
  sampling: sampling_capability option [@yojson.option];
  elicitation: elicitation_capability option [@yojson.option];
  roots: roots_capability option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type prompts_capability = {
  list_changed: bool option [@key "listChanged"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type resources_capability = {
  subscribe: bool option [@yojson.option];
  list_changed: bool option [@key "listChanged"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type tools_capability = {
  list_changed: bool option [@key "listChanged"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type logging_capability = json [@@deriving yojson, compare, sexp]

type server_capabilities = {
  experimental: (string * json) list option [@yojson.option];
  logging: logging_capability option [@yojson.option];
  prompts: prompts_capability option [@yojson.option];
  resources: resources_capability option [@yojson.option];
  tools: tools_capability option [@yojson.option];
} [@@deriving yojson, compare, sexp]

(* Initialization *)

type initialize_request_params = {
  protocol_version: string [@key "protocolVersion"];
  capabilities: client_capabilities;
  client_info: implementation [@key "clientInfo"];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type initialize_request = initialize_request_params request [@@deriving yojson, compare, sexp]

type initialize_result = {
  protocol_version: string [@key "protocolVersion"];
  capabilities: server_capabilities;
  server_info: implementation [@key "serverInfo"];
  instructions: string option [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

type initialized_notification = (Notification_params.t option) notification [@@deriving yojson, compare, sexp]

(* Ping *)
type ping_request = (Request_params.t option) request [@@deriving yojson, compare, sexp]

(* Progress *)
type progress_notification_params = {
  progress_token: progress_token [@key "progressToken"];
  progress: float;
  total: float option [@yojson.option];
  message: string option [@yojson.option];
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type progress_notification = progress_notification_params notification [@@deriving yojson, compare, sexp]

(* Resources *)
type list_resources_request = paginated_request_params paginated_request [@@deriving yojson, compare, sexp]

type annotations = {
  audience: role list option [@yojson.option];
  priority: float option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type resource = {
  uri: string; (* Using string for URI for now, can be refined with Uri.t *)
  description: string option [@yojson.option];
  mime_type: string option [@key "mimeType"] [@yojson.option];
  size: int option [@yojson.option];
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]
let base_metadata_to_yojson = yojson_of_base_metadata
let base_metadata_of_yojson = base_metadata_of_yojson

type resource_template = {
  uri_template: string [@key "uriTemplate"];
  description: string option [@yojson.option];
  mime_type: string option [@key "mimeType"] [@yojson.option];
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]

type list_resources_result = {
  resources: resource list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving yojson, compare, sexp]

type list_resource_templates_request = paginated_request_params paginated_request [@@deriving yojson, compare, sexp]

type list_resource_templates_result = {
  resource_templates: resource_template list [@key "resourceTemplates"];
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving yojson, compare, sexp]

type read_resource_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type read_resource_request = read_resource_request_params request [@@deriving yojson, compare, sexp]

type resource_contents = {
  uri: string;
  mime_type: string option [@key "mimeType"] [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type text_resource_contents = {
  text: string;
  resource_contents: resource_contents [@to_yojson resource_contents_to_yojson] [@of_yojson resource_contents_of_yojson];
} [@@deriving yojson, compare, sexp]

type blob_resource_contents = {
  blob: string;
  resource_contents: resource_contents [@to_yojson resource_contents_to_yojson] [@of_yojson resource_contents_of_yojson];
} [@@deriving yojson, compare, sexp]

type read_resource_result = {
  contents: [ `Text of text_resource_contents | `Blob of blob_resource_contents ] list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

type resource_list_changed_notification = (Notification_params.t option) notification [@@deriving yojson, compare, sexp]

type subscribe_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type subscribe_request = subscribe_request_params request [@@deriving yojson, compare, sexp]

type unsubscribe_request_params = {
  uri: string;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type unsubscribe_request = unsubscribe_request_params request [@@deriving yojson, compare, sexp]

type resource_updated_notification_params = {
  uri: string;
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type resource_updated_notification = resource_updated_notification_params notification [@@deriving yojson, compare, sexp]

(* Prompts *)
type list_prompts_request = paginated_request_params paginated_request [@@deriving yojson, compare, sexp]

type prompt_argument = {
  name: string;
  description: string option [@yojson.option];
  required: bool option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type prompt = {
  description: string option [@yojson.option];
  arguments: prompt_argument list option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]

type list_prompts_result = {
  prompts: prompt list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving yojson, compare, sexp]

type get_prompt_request_params = {
  name: string;
  arguments: (string * string) list option [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type get_prompt_request = get_prompt_request_params request [@@deriving yojson, compare, sexp]

type text_content = {
  type_: [ `Text ] [@key "type"];
  text: string;
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type image_content = {
  type_: [ `Image ] [@key "type"];
  data: string;
  mime_type: string [@key "mimeType"];
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type audio_content = {
  type_: [ `Audio ] [@key "type"];
  data: string;
  mime_type: string [@key "mimeType"];
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type sampling_message = {
  role: role;
  content: [ `Text of text_content | `Image of image_content | `Audio of audio_content ];
} [@@deriving yojson, compare, sexp]

type embedded_resource = {
  type_: [ `Resource ] [@key "type"];
  resource: [ `Text of text_resource_contents | `Blob of blob_resource_contents ];
  annotations: annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type resource_link = {
  type_: [ `Resource_link ] [@key "type"];
  resource: resource [@to_yojson resource_to_yojson] [@of_yojson resource_of_yojson];
} [@@deriving yojson, compare, sexp]

type content_block =
  [ `Text of text_content
  | `Image of image_content
  | `Audio of audio_content
  | `Resource_link of resource_link
  | `Embedded_resource of embedded_resource
  ] [@@deriving yojson, compare, sexp]

type content = content_block [@@deriving yojson, compare, sexp]

type prompt_message = {
  role: role;
  content: content_block;
} [@@deriving yojson, compare, sexp]

type get_prompt_result = {
  description: string option [@yojson.option];
  messages: prompt_message list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

type prompt_list_changed_notification = (Notification_params.t option) notification [@@deriving yojson, compare, sexp]

(* Tools *)
type list_tools_request = paginated_request_params paginated_request [@@deriving yojson, compare, sexp]

type tool_annotations = {
  title: string option [@yojson.option];
  read_only_hint: bool option [@key "readOnlyHint"] [@yojson.option];
  destructive_hint: bool option [@key "destructiveHint"] [@yojson.option];
  idempotent_hint: bool option [@key "idempotentHint"] [@yojson.option];
  open_world_hint: bool option [@key "openWorldHint"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type tool = {
  description: string option [@yojson.option];
  input_schema: json [@key "inputSchema"];
  output_schema: json option [@key "outputSchema"] [@yojson.option];
  annotations: tool_annotations option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
  base_metadata: base_metadata [@to_yojson base_metadata_to_yojson] [@of_yojson base_metadata_of_yojson];
} [@@deriving yojson, compare, sexp]

type list_tools_result = {
  tools: tool list;
  paginated_result: paginated_result [@to_yojson paginated_result_to_yojson] [@of_yojson paginated_result_of_yojson];
} [@@deriving yojson, compare, sexp]

type call_tool_request_params = {
  name: string;
  arguments: json option [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type call_tool_request = call_tool_request_params request [@@deriving yojson, compare, sexp]

type call_tool_result = {
  content: content_block list;
  structured_content: json option [@key "structuredContent"] [@yojson.option];
  is_error: bool [@key "isError"] [@default false];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

type tool_list_changed_notification = (Notification_params.t option) notification [@@deriving yojson, compare, sexp]

(* Logging *)
type logging_level = [ `Debug | `Info | `Notice | `Warning | `Error | `Critical | `Alert | `Emergency ] [@@deriving yojson, compare, sexp]

type set_level_request_params = {
  level: logging_level;
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type set_level_request = set_level_request_params request [@@deriving yojson, compare, sexp]

type logging_message_notification_params = {
  level: logging_level;
  logger: string option [@yojson.option];
  data: json;
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type logging_message_notification = logging_message_notification_params notification [@@deriving yojson, compare, sexp]

(* Sampling *)
type include_context = [ `None | `ThisServer | `AllServers ] [@@deriving yojson, compare, sexp]

type model_hint = {
  name: string option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type model_preferences = {
  hints: model_hint list option [@yojson.option];
  cost_priority: float option [@key "costPriority"] [@yojson.option];
  speed_priority: float option [@key "speedPriority"] [@yojson.option];
  intelligence_priority: float option [@key "intelligencePriority"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type create_message_request_params = {
  messages: sampling_message list;
  model_preferences: model_preferences option [@key "modelPreferences"] [@yojson.option];
  system_prompt: string option [@key "systemPrompt"] [@yojson.option];
  include_context: include_context option [@key "includeContext"] [@yojson.option];
  temperature: float option [@yojson.option];
  max_tokens: int [@key "maxTokens"];
  stop_sequences: string list option [@key "stopSequences"] [@yojson.option];
  metadata: json option [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type create_message_request = create_message_request_params request [@@deriving yojson, compare, sexp]

type stop_reason = [ `EndTurn | `StopSequence | `MaxTokens | `String of string ] [@@deriving yojson, compare, sexp]

type create_message_result = {
  role: role;
  content: [ `Text of text_content | `Image of image_content | `Audio of audio_content ];
  model: string;
  stop_reason: stop_reason option [@key "stopReason"] [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

(* Completion *)
type resource_template_reference = {
  type_: [ `Ref_resource ] [@key "type"];
  uri: string;
} [@@deriving yojson, compare, sexp]

type resource_reference = resource_template_reference [@@deriving yojson, compare, sexp]

type prompt_reference = {
  type_: [ `Ref_prompt ] [@key "type"];
  name: string;
} [@@deriving yojson, compare, sexp]

type completion_argument = {
  name: string;
  value: string;
} [@@deriving yojson, compare, sexp]

type completion_context = {
  arguments: (string * string) list option [@yojson.option];
} [@@deriving yojson, compare, sexp]

type complete_request_params = {
  ref: [ `Resource of resource_template_reference | `Prompt of prompt_reference ];
  argument: completion_argument;
  context: completion_context option [@yojson.option];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type complete_request = complete_request_params request [@@deriving yojson, compare, sexp]

type completion = {
  values: string list;
  total: int option [@yojson.option];
  has_more: bool option [@key "hasMore"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type complete_result = {
  completion: completion;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

(* Roots *)
type list_roots_request = (Request_params.t option) request [@@deriving yojson, compare, sexp]

type root = {
  uri: string; (* Should be FileUrl, using string for now *)
  name: string option [@yojson.option];
  meta: json option [@key "_meta"] [@yojson.option];
} [@@deriving yojson, compare, sexp]

type list_roots_result = {
  roots: root list;
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

type roots_list_changed_notification = (Notification_params.t option) notification [@@deriving yojson, compare, sexp]

(* Cancellation *)
type cancelled_notification_params = {
  request_id: request_id [@key "requestId"];
  reason: string option [@yojson.option];
  notification_params: Notification_params.t [@to_yojson Notification_params.to_yojson] [@of_yojson Notification_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type cancelled_notification = cancelled_notification_params notification [@@deriving yojson, compare, sexp]

(* Elicitation *)
type elicit_requested_schema = json [@@deriving yojson, compare, sexp]

type elicit_request_params = {
  message: string;
  requested_schema: elicit_requested_schema [@key "requestedSchema"];
  request_params: Request_params.t [@to_yojson Request_params.to_yojson] [@of_yojson Request_params.of_yojson];
} [@@deriving yojson, compare, sexp]

type elicit_request = elicit_request_params request [@@deriving yojson, compare, sexp]

type elicit_result = {
  action: [ `Accept | `Decline | `Cancel ];
  content: json option [@yojson.option];
  result: result [@to_yojson result_to_yojson] [@of_yojson result_of_yojson];
} [@@deriving yojson, compare, sexp]

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
  ] [@@deriving yojson, compare, sexp]

type client_notification =
  [ `Cancelled of cancelled_notification
  | `Progress of progress_notification
  | `Initialized of initialized_notification
  | `RootsListChanged of roots_list_changed_notification
  ] [@@deriving yojson, compare, sexp]

type client_result =
  [ `Empty of empty_result
  | `CreateMessage of create_message_result
  | `ListRoots of list_roots_result
  | `Elicit of elicit_result
  ] [@@deriving yojson, compare, sexp]

type server_request =
  [ `Ping of ping_request
  | `CreateMessage of create_message_request
  | `ListRoots of list_roots_request
  | `Elicit of elicit_request
  ] [@@deriving yojson, compare, sexp]

type server_notification =
  [ `Cancelled of cancelled_notification
  | `Progress of progress_notification
  | `LoggingMessage of logging_message_notification
  | `ResourceUpdated of resource_updated_notification
  | `ResourceListChanged of resource_list_changed_notification
  | `ToolListChanged of tool_list_changed_notification
  | `PromptListChanged of prompt_list_changed_notification
  ] [@@deriving yojson, compare, sexp]

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