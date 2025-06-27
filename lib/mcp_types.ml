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

type jsonrpc_request = {
  jsonrpc : string;
  id : json option;
  method_ : string;
  params : json option;
}
(** JSON-RPC 2.0 request *)

type jsonrpc_response = {
  jsonrpc : string;
  id : json option;
  result : json option;
  error : jsonrpc_error option;
}
(** JSON-RPC 2.0 response *)

and jsonrpc_error = { code : int; message : string; data : json option }
(** JSON-RPC 2.0 error *)

type tool_param = {
  name : string;
  type_ : string;
  description : string option;
  required : bool;
}
(** MCP tool parameter definition *)

type tool_def = {
  name : string;
  description : string;
  input_schema : json option;
}
(** MCP tool definition *)

type resource_def = {
  uri : string;
  name : string option;
  description : string option;
  mime_type : string option;
}
(** MCP resource definition *)

type prompt_def = {
  name : string;
  description : string;
  arguments : tool_param list option;
}
(** MCP prompt definition *)

(** Message content types *)
type content_type =
  | Text of string
  | Image of { data : string; mime_type : string }
  | Resource of {
      uri : string;
      text : string option;
      mime_type : string option;
    }

type message = { role : string; content : content_type }
(** MCP message *)

type init_params = {
  protocol_version : string;
  capabilities : json;
  client_info : json;
}
(** MCP initialization parameters *)

type server_capabilities = {
  logging : json option;
  prompts : json option;
  resources : json option;
  tools : json option;
}
(** MCP server capabilities *)

type init_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : json;
}
(** MCP initialization result *)

type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
}
(** Context for tool/resource execution *)

(** Transport types *)
type transport_type =
  | Stdio
  | Http of { host : string; port : int; path : string }
  | Sse of { host : string; port : int }
