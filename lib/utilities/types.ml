(** Common types used across FastMCP OCaml implementation *)

[@@@ocaml.warning "-32-27"]

open Core
open Async
open Yojson.Safe
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Basic JSON representation with proper OCaml types *)
type json = Yojson.Safe.t

type json_value = [
  | `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of json_value list
  | `Assoc of (string * json_value) list
] [@@deriving yojson]

type parameter = {
  param_name : string [@key "name"];
  param_type : string [@key "type"];
  param_description : string option [@yojson.option] [@key "description"];
  param_default : json_value option [@yojson.option] [@key "default"];
  param_required : bool [@key "required"];
} [@@deriving fields, yojson]

(** Function signature type *)
type function_signature = {
  name : string;
  description : string option;
  parameters : parameter list;
  return_type : string;
  is_async : bool;
  is_static : bool;
  is_method : bool;
  is_class_method : bool;
} [@@deriving fields, yojson]

(** Convert json_value to json *)
let rec json_value_to_json = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `List l -> `List (List.map ~f:json_value_to_json l)
  | `Assoc l -> `Assoc (List.map ~f:(fun (k, v) -> (k, json_value_to_json v)) l)

(** Type adapter for OCaml functions - equivalent to Python's TypeAdapter *)
type 'a type_adapter = {
  signature : function_signature;
  validate_and_call : (string * json) list -> (json, string) result;
  json_schema : unit -> json;
}

(** Create type adapter from function signature and validation logic *)
let create_type_adapter ~name ~parameters ~return_type ?description validate_and_call =
  let sig_val = {
    name;
    parameters;
    return_type;
    description;
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let json_schema () =
    let properties = List.map parameters ~f:(fun param ->
      let base_schema = [("type", `String param.param_type)] in
      let with_description = match param.param_description with
        | Some desc -> ("description", `String desc) :: base_schema
        | None -> base_schema
      in
      let with_default = match param.param_default with
        | Some default -> ("default", json_value_to_json default) :: with_description
        | None -> with_description
      in
      (param.param_name, `Assoc with_default)
    ) in
    
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map parameters ~f:(fun p -> `String p.param_name)))
    ]
  in
  
  { signature = sig_val; validate_and_call; json_schema }

(** Cached type adapters - equivalent to Python's get_cached_typeadapter *)
let type_adapter_cache : (string, json type_adapter) Hashtbl.t =
  Hashtbl.create (module String) ~size:16

let get_cached_typeadapter func_signature =
  let cache_key = func_signature.name in
  match Hashtbl.find type_adapter_cache cache_key with
  | Some adapter -> adapter
  | None ->
    (* Create a basic adapter from signature *)
    let validate_and_call args =
      (* Basic validation that all required parameters are present *)
      let missing_required = List.filter func_signature.parameters ~f:(fun param ->
        param.param_required && not (List.Assoc.mem args ~equal:String.equal param.param_name)
      ) in
      
      if List.length missing_required > 0 then
        let missing_names = List.map missing_required ~f:(fun p -> p.param_name) in
        Result.error ("Missing required parameters: " ^ String.concat ~sep:", " missing_names)
      else
        (* Return a simple success result *)
        Result.ok (`String ("Function " ^ func_signature.name ^ " validated"))
    in
    
    let adapter = create_type_adapter
      ~name:func_signature.name
      ~parameters:func_signature.parameters
      ~return_type:func_signature.return_type
      ?description:func_signature.description
      validate_and_call
    in
    
    Hashtbl.add_exn type_adapter_cache ~key:cache_key ~data:adapter;
    adapter

(** Resource types *)
type resource_type =
  | TextResource of {
      text : string;
      mime_type : string;
    }
  | BinaryResource of {
      data : bytes;
      mime_type : string;
    }
  | FileResource of {
      path : string;
      is_binary : bool;
      mime_type : string;
    }
  | DirectoryResource of {
      path : string;
      recursive : bool;
      pattern : string option;
      mime_type : string;
    }
  | HttpResource of {
      url : string;
      mime_type : string;
    }

(** Resource error types *)
type resource_error =
  | ResourceNotFound of string
  | ResourceAccessDenied of string
  | ResourceInvalidPath of string
  | ResourceIOError of string
  | ResourceParseError of string

(** Content types for MCP messages *)
type content_type =
  | Text of string
  | Image of {
      data : string;
      mime_type : string;
      annotations : (string * json) list option;
      format : string option;
    }
  | Audio of {
      data : string;
      mime_type : string;
      annotations : (string * json) list option;
      format : string option;
    }
  | File of {
      data : string;
      mime_type : string;
      name : string option;
      annotations : (string * json) list option;
      format : string option;
    }

(** Transport types for different connection methods *)
type transport_config =
  | Stdio
  | Http of { host : string; port : int; path : string }
  | Sse of { host : string; port : int; path : string }
  | StreamableHttp of { host : string; port : int; path : string }

(** Authentication configuration *)
type auth_config =
  | Bearer of string
  | OAuth of { client_id : string; client_secret : string }

(** Request/Response ID types *)
type request_id = [ `String of string | `Int of int ]

(** JSON-RPC 2.0 structures *)
type jsonrpc_request = {
  jsonrpc : string;
  id : request_id option;
  method_ : string;
  params : json option;
}

type jsonrpc_error = {
  code : int;
  message : string;
  data : json option;
}

type jsonrpc_response = {
  jsonrpc : string;
  id : request_id option;
  result : json option;
  error : jsonrpc_error option;
}

(** MCP Protocol Types *)

(** Tool parameter definition *)
type tool_param = {
  name : string;
  type_ : string;
  description : string option;
  required : bool;
}

(** Tool definition *)
type tool_def = {
  name : string;
  description : string;
  input_schema : json option;
  annotations : (string * json) list option;
}

(** Resource definition *)
type resource_def = {
  uri : string;
  name : string option;
  description : string option;
  mime_type : string option;
}


(** Resource template definition *)
type resource_template_def = {
  uri_template : string;
  name : string;
  description : string option;
  mime_type : string option;
}


(** Prompt argument definition *)
type prompt_argument = {
  name : string;
  description : string option;
  required : bool;
}


(** Prompt definition *)
type prompt_def = {
  name : string;
  description : string;
  arguments : prompt_argument list option;
}


(** Message for prompts *)
type prompt_message = {
  role : string;
  content : content_type;
}


(** Server capabilities *)
type server_capabilities = {
  logging : json option;
  prompts : json option;
  resources : json option;
  tools : json option;
}


(** Client info *)
type client_info = {
  name : string;
  version : string;
}


(** Initialization parameters *)
type init_params = {
  protocol_version : string;
  capabilities : json;
  client_info : client_info;
}


(** Initialization result *)
type init_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : json;
}


(** Execution context for handlers *)
type execution_context = {
  request_id : string option;
  client_id : string option;
  session_data : (string, json) Hashtbl.t;
  mutable tools_changed : bool;
  mutable resources_changed : bool;
  mutable prompts_changed : bool;
}

(** Progress information *)
type progress_info = {
  progress_token : request_id;
  progress : float;
  total : float option;
  message : string option;
}


(** Log levels *)
type log_level = Debug | Info | Notice | Warning | Error


(** Log entry *)
type log_entry = {
  level : log_level;
  message : string;
  logger : string option;
  data : json option;
}


(** Result types for better error handling *)
type ('a, 'e) result = ('a, 'e) Result.t

(** Common error types *)
type mcp_error =
  | ConnectionError of string
  | AuthenticationError of string
  | NotFoundError of string
  | ValidationError of string
  | TimeoutError of string
  | InternalError of string
  | ToolError of string
  | ResourceError of string
  | PromptError of string

let string_of_mcp_error = function
  | ConnectionError msg -> "Connection error: " ^ msg
  | AuthenticationError msg -> "Authentication error: " ^ msg
  | NotFoundError msg -> "Not found: " ^ msg
  | ValidationError msg -> "Validation error: " ^ msg
  | TimeoutError msg -> "Timeout: " ^ msg
  | InternalError msg -> "Internal error: " ^ msg
  | ToolError msg -> "Tool error: " ^ msg
  | ResourceError msg -> "Resource error: " ^ msg
  | PromptError msg -> "Prompt error: " ^ msg

(** Type adapter functionality - OCaml equivalent of Python's Pydantic TypeAdapter *)

let create_parameter ~name ~type_ ?description ?default ?(required=false) () =
  { param_name = name;
    param_type = type_;
    param_description = description;
    param_default = default;
    param_required = required;
  }

let json_schema params =
  let properties = List.map params ~f:(fun param ->
    let base_schema = [("type", `String param.param_type)] in
    let with_description = match param.param_description with
      | Some desc -> ("description", `String desc) :: base_schema
      | None -> base_schema
    in
    let with_default = match param.param_default with
      | Some default -> ("default", (default :> Yojson.Safe.t)) :: with_description
      | None -> with_description
    in
    (param.param_name, `Assoc with_default)
  ) in
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc properties);
    ("required", `List (List.map params ~f:(fun p -> `String p.param_name)))
  ]

let filter_schema = function
  | `Assoc props -> `Assoc (List.filter props ~f:(fun (k, _) -> not (String.equal k "$schema")))
  | x -> x

(** Type checking utilities inspired by Python version *)

(** Check if a type is a "subclass" (variant case match) *)
let is_content_type_match content_type_variant expected_variant =
  match content_type_variant, expected_variant with
  | Text _, Text _ -> true
  | Image _, Image _ -> true
  | Audio _, Audio _ -> true
  | File _, File _ -> true
  | _ -> false

(** Safe type checking for optional values *)
let is_option_type_safe value_opt expected_check =
  match value_opt with
  | Some value -> expected_check value
  | None -> false

(** Helper to extract mime type from content *)
let get_mime_type_from_content = function
  | Text _ -> "text/plain"
  | Image { mime_type; _ } -> mime_type
  | Audio { mime_type; _ } -> mime_type
  | File { mime_type; _ } -> mime_type

(** Helper to get content data *)
let get_content_data = function
  | Text text -> text
  | Image { data; _ } -> data
  | Audio { data; _ } -> data
  | File { data; _ } -> data

(** Create text content *)
let create_text_content text = Text text

(** Create image content *)
let create_image_content ~data ~mime_type ?annotations ?format () =
  Image { data; mime_type; annotations; format }

(** Create audio content *)
let create_audio_content ~data ~mime_type ?annotations ?format () =
  Audio { data; mime_type; annotations; format }

(** Create file content *)
let create_file_content ~data ~mime_type ?name ?annotations ?format () =
  File { data; mime_type; name; annotations; format }

(** Create image from data with format *)
let create_image_from_data ?(format="png") data =
  let mime_type = "image/" ^ format in
  Image { data; mime_type; annotations = None; format = Some format }

(** Create audio from data with format *)
let create_audio_from_data ?(format="wav") data =
  let mime_type = "audio/" ^ format in
  Audio { data; mime_type; annotations = None; format = Some format }

(** Create file from data with format *)
let create_file_from_data ?(format="octet-stream") ?name data =
  let mime_type = "application/" ^ format in
  File { data; mime_type; name; annotations = None; format = Some format }

(** Utility to encode data to base64 *)
let base64_encode data = 
  match Base64.encode data with
  | Ok encoded -> encoded
  | Error (`Msg msg) -> failwith ("Base64 encoding failed: " ^ msg)

(** Utility to decode base64 data *)
let base64_decode encoded_data =
  match Base64.decode encoded_data with
  | Ok decoded -> Some decoded
  | Error _ -> None

(** Find parameter by type in function signature *)
let find_param_by_type (params : parameter list) (target_type : string) : parameter option =
  List.find_opt (fun param -> param.param_type = target_type) params

(** Find parameter by name in function signature *)
let find_param_by_name (params : parameter list) (target_name : string) : parameter option =
  List.find_opt (fun param -> param.param_name = target_name) params

(** Helper functions *)

let create_execution_context ?request_id ?client_id () =
  {
    request_id;
    client_id;
    session_data = Hashtbl.create 16;
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
  }

(** JSON helpers *)
let json_of_string s = `String s
let json_of_int i = `Int i
let json_of_bool b = `Bool b
let json_of_list l = `List l
let json_of_assoc l = `Assoc l

let string_of_json = function
  | `String s -> Some s
  | _ -> None

let int_of_json = function
  | `Int i -> Some i
  | _ -> None

let bool_of_json = function
  | `Bool b -> Some b
  | _ -> None

(** Protocol version constants *)
let protocol_version = "2024-11-05"
let fastmcp_version = "0.1.0"

(** Manual serialization functions for testing *)

(* Content type serialization *)
let content_type_to_yojson = function
  | Text s -> `Assoc [("type", `String "text"); ("text", `String s)]
  | Image {data; mime_type; annotations; format} ->
    let base = [("type", `String "image"); ("data", `String data); ("mimeType", `String mime_type)] in
    let with_format = match format with
      | Some fmt -> ("format", `String fmt) :: base
      | None -> base
    in
    let with_annot = match annotations with
      | Some annots -> ("annotations", `Assoc annots) :: with_format
      | None -> with_format
    in
    `Assoc with_annot
  | Audio {data; mime_type; annotations; format} ->
    let base = [("type", `String "audio"); ("data", `String data); ("mimeType", `String mime_type)] in
    let with_format = match format with
      | Some fmt -> ("format", `String fmt) :: base
      | None -> base
    in
    let with_annot = match annotations with
      | Some annots -> ("annotations", `Assoc annots) :: with_format
      | None -> with_format
    in
    `Assoc with_annot
  | File {data; mime_type; name; annotations; format} ->
    let base = [("type", `String "file"); ("data", `String data); ("mimeType", `String mime_type)] in
    let with_name = match name with
      | Some n -> ("name", `String n) :: base
      | None -> base
    in
    let with_format = match format with
      | Some fmt -> ("format", `String fmt) :: with_name
      | None -> with_name
    in
    let with_annot = match annotations with
      | Some annots -> ("annotations", `Assoc annots) :: with_format
      | None -> with_format
    in
    `Assoc with_annot

let content_type_of_yojson = function
  | `Assoc l when List.assoc_opt "type" l = Some (`String "text") ->
    (match List.assoc_opt "text" l with
     | Some (`String s) -> Ok (Text s)
     | _ -> Error "Invalid text content")
  | `Assoc l when List.assoc_opt "type" l = Some (`String "image") ->
    (match List.assoc_opt "data" l, List.assoc_opt "mimeType" l with
     | Some (`String data), Some (`String mime_type) ->
       let annotations = match List.assoc_opt "annotations" l with
         | Some (`Assoc annots) -> Some annots
         | _ -> None
       in
       let format = match List.assoc_opt "format" l with
         | Some (`String fmt) -> Some fmt
         | _ -> None
       in
       Ok (Image { data; mime_type; annotations; format })
     | _ -> Error "Invalid image content")
  | `Assoc l when List.assoc_opt "type" l = Some (`String "audio") ->
    (match List.assoc_opt "data" l, List.assoc_opt "mimeType" l with
     | Some (`String data), Some (`String mime_type) ->
       let annotations = match List.assoc_opt "annotations" l with
         | Some (`Assoc annots) -> Some annots
         | _ -> None
       in
       let format = match List.assoc_opt "format" l with
         | Some (`String fmt) -> Some fmt
         | _ -> None
       in
       Ok (Audio { data; mime_type; annotations; format })
     | _ -> Error "Invalid audio content")
  | `Assoc l when List.assoc_opt "type" l = Some (`String "file") ->
    (match List.assoc_opt "data" l, List.assoc_opt "mimeType" l with
     | Some (`String data), Some (`String mime_type) ->
       let name = match List.assoc_opt "name" l with
         | Some (`String n) -> Some n
         | _ -> None
       in
       let annotations = match List.assoc_opt "annotations" l with
         | Some (`Assoc annots) -> Some annots
         | _ -> None
       in
       let format = match List.assoc_opt "format" l with
         | Some (`String fmt) -> Some fmt
         | _ -> None
       in
       Ok (File { data; mime_type; name; annotations; format })
     | _ -> Error "Invalid file content")
  | _ -> Error "Invalid content type"

(* Transport config serialization *)
let transport_config_to_yojson = function
  | Stdio -> `Assoc [("type", `String "stdio")]
  | Http {host; port; path} -> `Assoc [("type", `String "http"); ("host", `String host); ("port", `Int port); ("path", `String path)]
  | Sse {host; port; path} -> `Assoc [("type", `String "sse"); ("host", `String host); ("port", `Int port); ("path", `String path)]
  | StreamableHttp {host; port; path} -> `Assoc [("type", `String "streamable_http"); ("host", `String host); ("port", `Int port); ("path", `String path)]

let transport_config_of_yojson = function
  | `Assoc l when List.assoc_opt "type" l = Some (`String "stdio") -> Ok Stdio
  | `Assoc l when List.assoc_opt "type" l = Some (`String "http") ->
    (match List.assoc_opt "host" l, List.assoc_opt "port" l, List.assoc_opt "path" l with
     | Some (`String host), Some (`Int port), Some (`String path) -> Ok (Http {host; port; path})
     | _ -> Error "Invalid HTTP transport config")
  | `Assoc l when List.assoc_opt "type" l = Some (`String "sse") ->
    (match List.assoc_opt "host" l, List.assoc_opt "port" l, List.assoc_opt "path" l with
     | Some (`String host), Some (`Int port), Some (`String path) -> Ok (Sse {host; port; path})
     | _ -> Error "Invalid SSE transport config")
  | `Assoc l when List.assoc_opt "type" l = Some (`String "streamable_http") ->
    (match List.assoc_opt "host" l, List.assoc_opt "port" l, List.assoc_opt "path" l with
     | Some (`String host), Some (`Int port), Some (`String path) -> Ok (StreamableHttp {host; port; path})
     | _ -> Error "Invalid StreamableHTTP transport config")
  | _ -> Error "Invalid transport config"

(* Auth config serialization *)
let auth_config_to_yojson = function
  | Bearer token -> `Assoc [("type", `String "bearer"); ("token", `String token)]
  | OAuth {client_id; client_secret} -> `Assoc [("type", `String "oauth"); ("client_id", `String client_id); ("client_secret", `String client_secret)]

let auth_config_of_yojson = function
  | `Assoc [("type", `String "bearer"); ("token", `String token)] -> Ok (Bearer token)
  | `Assoc l when List.assoc "type" l = `String "oauth" ->
    (match List.assoc_opt "client_id" l, List.assoc_opt "client_secret" l with
     | Some (`String client_id), Some (`String client_secret) -> Ok (OAuth {client_id; client_secret})
     | _ -> Error "Invalid OAuth config")
  | _ -> Error "Invalid auth config"

(* JSON-RPC serialization *)
let jsonrpc_request_to_yojson (req : jsonrpc_request) =
  let base = [("jsonrpc", `String req.jsonrpc); ("method", `String req.method_)] in
  let with_id = match req.id with
    | Some id -> ("id", (id :> json)) :: base
    | None -> base
  in
  let with_params = match req.params with
    | Some params -> ("params", params) :: with_id
    | None -> with_id
  in
  `Assoc with_params

let jsonrpc_request_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "jsonrpc" l, List.assoc_opt "method" l with
     | Some (`String jsonrpc), Some (`String method_) ->
       let id = match List.assoc_opt "id" l with
         | Some (`String s) -> Some (`String s)
         | Some (`Int i) -> Some (`Int i)
         | _ -> None
       in
       let params = List.assoc_opt "params" l in
       Ok {jsonrpc; id; method_; params}
     | _ -> Error "Invalid JSON-RPC request")
  | _ -> Error "Invalid JSON-RPC request"

(* Tool definition serialization *)
let tool_def_to_yojson (tool : tool_def) =
  let base = [("name", `String tool.name); ("description", `String tool.description)] in
  let with_schema = match tool.input_schema with
    | Some schema -> ("inputSchema", schema) :: base
    | None -> base
  in
  let with_annot = match tool.annotations with
    | Some annots -> ("annotations", `Assoc annots) :: with_schema
    | None -> with_schema
  in
  `Assoc with_annot

let tool_def_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "name" l, List.assoc_opt "description" l with
     | Some (`String name), Some (`String description) ->
       let input_schema = List.assoc_opt "inputSchema" l in
       let annotations = match List.assoc_opt "annotations" l with
         | Some (`Assoc annots) -> Some annots
         | _ -> None
       in
       Ok {name; description; input_schema; annotations}
     | _ -> Error "Invalid tool definition")
  | _ -> Error "Invalid tool definition"

(* Resource definition serialization *)
let resource_def_to_yojson (res : resource_def) =
  let base = [("uri", `String res.uri)] in
  let with_name = match res.name with
    | Some name -> ("name", `String name) :: base
    | None -> base
  in
  let with_desc = match res.description with
    | Some desc -> ("description", `String desc) :: with_name
    | None -> with_name
  in
  let with_mime = match res.mime_type with
    | Some mime -> ("mimeType", `String mime) :: with_desc
    | None -> with_desc
  in
  `Assoc with_mime

let resource_def_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "uri" l with
     | Some (`String uri) ->
       let name = match List.assoc_opt "name" l with Some (`String n) -> Some n | _ -> None in
       let description = match List.assoc_opt "description" l with Some (`String d) -> Some d | _ -> None in
       let mime_type = match List.assoc_opt "mimeType" l with Some (`String m) -> Some m | _ -> None in
       Ok {uri; name; description; mime_type}
     | _ -> Error "Invalid resource definition")
  | _ -> Error "Invalid resource definition"

(* Prompt definition serialization *)
let prompt_def_to_yojson (prompt : prompt_def) =
  let base = [("name", `String prompt.name); ("description", `String prompt.description)] in
  let with_args = match prompt.arguments with
    | Some args ->
      let arg_json = List.map (fun (arg : prompt_argument) ->
        let base = [("name", `String arg.name); ("required", `Bool arg.required)] in
        let with_desc = match arg.description with
          | Some desc -> ("description", `String desc) :: base
          | None -> base
        in
        `Assoc with_desc
      ) args in
      ("arguments", `List arg_json) :: base
    | None -> base
  in
  `Assoc with_args

let prompt_def_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "name" l, List.assoc_opt "description" l with
     | Some (`String name), Some (`String description) ->
       let arguments = match List.assoc_opt "arguments" l with
         | Some (`List arg_list) ->
           let args = List.map (function
             | `Assoc arg_l ->
               (match List.assoc_opt "name" arg_l, List.assoc_opt "required" arg_l with
                | Some (`String arg_name), Some (`Bool required) ->
                  let arg_description = match List.assoc_opt "description" arg_l with
                    | Some (`String d) -> Some d | _ -> None
                  in
                  {name = arg_name; description = arg_description; required}
                | _ -> {name = ""; description = None; required = false})
             | _ -> {name = ""; description = None; required = false}
           ) arg_list in
           Some args
         | _ -> None
       in
       Ok {name; description; arguments}
     | _ -> Error "Invalid prompt definition")
  | _ -> Error "Invalid prompt definition"

(* Progress info serialization *)
let progress_info_to_yojson (progress : progress_info) =
  let base = [
    ("progressToken", (progress.progress_token :> json));
    ("progress", `Float progress.progress)
  ] in
  let with_total = match progress.total with
    | Some total -> ("total", `Float total) :: base
    | None -> base
  in
  let with_message = match progress.message with
    | Some message -> ("message", `String message) :: with_total
    | None -> with_total
  in
  `Assoc with_message

let progress_info_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "progressToken" l, List.assoc_opt "progress" l with
     | Some token, Some (`Float progress) ->
       let progress_token = match token with
         | `String s -> `String s
         | `Int i -> `Int i
         | _ -> `String "unknown"
       in
       let total = match List.assoc_opt "total" l with
         | Some (`Float t) -> Some t
         | _ -> None
       in
       let message = match List.assoc_opt "message" l with
         | Some (`String m) -> Some m
         | _ -> None
       in
       Ok {progress_token; progress; total; message}
     | _ -> Error "Invalid progress info")
  | _ -> Error "Invalid progress info"

(* Log level serialization *)
let log_level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Notice -> "notice"
  | Warning -> "warning"
  | Error -> "error"

let log_level_of_string = function
  | "debug" -> Ok Debug
  | "info" -> Ok Info
  | "notice" -> Ok Notice
  | "warning" -> Ok Warning
  | "error" -> Ok Error
  | _ -> Error "Invalid log level"

(* Log entry serialization *)
let log_entry_to_yojson (entry : log_entry) =
  let base = [
    ("level", `String (log_level_to_string entry.level));
    ("message", `String entry.message)
  ] in
  let with_logger = match entry.logger with
    | Some logger -> ("logger", `String logger) :: base
    | None -> base
  in
  let with_data = match entry.data with
    | Some data -> ("data", data) :: with_logger
    | None -> with_logger
  in
  `Assoc with_data

let log_entry_of_yojson = function
  | `Assoc l ->
    (match List.assoc_opt "level" l, List.assoc_opt "message" l with
     | Some (`String level), Some (`String message) ->
       let logger = match List.assoc_opt "logger" l with
         | Some (`String l) -> Some l
         | _ -> None
       in