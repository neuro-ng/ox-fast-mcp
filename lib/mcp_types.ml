(** Core MCP protocol types - simplified version *)

(** Basic JSON representation *)
type json = 
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Array of json list
  | Object of (string * json) list

(** JSON-RPC 2.0 request *)
type jsonrpc_request = {
  jsonrpc : string;
  id : json option;
  method_ : string;
  params : json option;
}

(** JSON-RPC 2.0 response *)
type jsonrpc_response = {
  jsonrpc : string;
  id : json option;
  result : json option;
  error : jsonrpc_error option;
}

(** JSON-RPC 2.0 error *)
and jsonrpc_error = {
  code : int;
  message : string;
  data : json option;
}

(** MCP tool parameter definition *)
type tool_param = {
  name : string;
  type_ : string;
  description : string option;
  required : bool;
}

(** MCP tool definition *)
type tool_def = {
  name : string;
  description : string;
  input_schema : json option;
}

(** MCP resource definition *)
type resource_def = {
  uri : string;
  name : string option;
  description : string option;
  mime_type : string option;
}

(** MCP prompt definition *)
type prompt_def = {
  name : string;
  description : string;
  arguments : tool_param list option;
}

(** Message content types *)
type content_type = 
  | Text of string
  | Image of { data : string; mime_type : string }
  | Resource of { uri : string; text : string option; mime_type : string option }

(** MCP message *)
type message = {
  role : string;
  content : content_type;
}

(** MCP initialization parameters *)
type init_params = {
  protocol_version : string;
  capabilities : json;
  client_info : json;
}

(** MCP server capabilities *)
type server_capabilities = {
  logging : json option;
  prompts : json option;
  resources : json option;
  tools : json option;
}

(** MCP initialization result *)
type init_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : json;
}

(** Context for tool/resource execution *)
type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
}

(** Transport types *)
type transport_type = 
  | Stdio
  | Http of { host : string; port : int; path : string }
  | Sse of { host : string; port : int } 