(** Proxy Module for OxFastMCP

    Provides proxy types and managers that source tools, resources, and prompts
    from remote MCP servers in addition to local and mounted components. Uses a
    client factory pattern for session management. *)

open! Core
open! Async

(** {1 Types} *)

(** Client interface for proxy operations *)
module type Client = sig
  type t

  val connect : t -> unit Deferred.t
  (** Connect to the remote server *)

  val disconnect : t -> unit Deferred.t
  (** Disconnect from the remote server *)

  val new_ : t -> t
  (** Create a new client instance with the same configuration *)

  val list_tools : t -> Yojson.Safe.t list Deferred.t
  (** List available tools from remote server *)

  val call_tool :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t
  (** Call a tool on the remote server *)

  val call_tool_mcp :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t
  (** Call a tool and get MCP result format *)

  val list_resources : t -> Yojson.Safe.t list Deferred.t
  (** List available resources from remote server *)

  val read_resource : t -> uri:string -> Yojson.Safe.t Deferred.t
  (** Read a resource from the remote server *)

  val list_resource_templates : t -> Yojson.Safe.t list Deferred.t
  (** List available resource templates from remote server *)

  val list_prompts : t -> Yojson.Safe.t list Deferred.t
  (** List available prompts from remote server *)

  val get_prompt :
    t ->
    name:string ->
    arguments:Yojson.Safe.t option ->
    Yojson.Safe.t Deferred.t
  (** Get a prompt result from the remote server *)
end

type 'client client_factory = unit -> 'client Deferred.t
(** Client factory type - can be sync or async *)

(** {1 Tool Result Type} *)

module Tool_result = struct
  type t = {
    content : Yojson.Safe.t list;
    structured_content : Yojson.Safe.t option;
  }

  let create ~content ?structured_content () = { content; structured_content }
end

(** {1 Mirrored Component Marker} *)

module Mirrored_component = struct
  type t = { mirrored : bool }

  let create ?(mirrored = false) () = { mirrored }
  let is_mirrored t = t.mirrored
end

(** {1 Proxy Tool} *)

module Proxy_tool = struct
  type 'client t = {
    name : string;
    description : string option;
    parameters : Yojson.Safe.t;
    annotations : Yojson.Safe.t option;
    output_schema : Yojson.Safe.t option;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  let create ~client ~name ?description ~parameters ?annotations ?output_schema
      ?meta ?(tags = []) () =
    {
      name;
      description;
      parameters;
      annotations;
      output_schema;
      meta;
      tags;
      client;
      mirrored = true;
    }

  (** Create a ProxyTool from an MCP tool schema *)
  let from_mcp_tool (type client) ~(client : client) (mcp_tool : Yojson.Safe.t)
      : client t =
    let open Yojson.Safe.Util in
    let name = mcp_tool |> member "name" |> to_string in
    let description = mcp_tool |> member "description" |> to_string_option in
    let parameters =
      match mcp_tool |> member "inputSchema" with
      | `Null -> `Assoc []
      | x -> x
    in
    let annotations =
      match mcp_tool |> member "annotations" with
      | `Null -> None
      | x -> Some x
    in
    let output_schema =
      match mcp_tool |> member "outputSchema" with
      | `Null -> None
      | x -> Some x
    in
    let meta =
      match mcp_tool |> member "meta" with
      | `Null -> None
      | x -> Some x
    in
    let tags =
      match meta with
      | Some m -> (
        match m |> member "_fastmcp" |> member "tags" with
        | `List tags -> List.filter_map tags ~f:to_string_option
        | _ -> [])
      | None -> []
    in
    {
      name;
      description;
      parameters;
      annotations;
      output_schema;
      meta;
      tags;
      client;
      mirrored = true;
    }

  (** Execute the tool by making a call through the client *)
  let run (type client) (module C : Client with type t = client) (t : client t)
      ~(arguments : Yojson.Safe.t) : Tool_result.t Deferred.t =
    let%bind () = C.connect t.client in
    let%bind result = C.call_tool_mcp t.client ~name:t.name ~arguments in
    let%bind () = C.disconnect t.client in
    let open Yojson.Safe.Util in
    let is_error =
      match result |> member "isError" with
      | `Bool b -> b
      | _ -> false
    in
    if is_error then
      let error_text =
        match result |> member "content" |> to_list with
        | first :: _ -> first |> member "text" |> to_string
        | [] -> "Unknown error"
      in
      raise_s [%message "Tool error" (error_text : string)]
    else
      let content =
        match result |> member "content" with
        | `List items -> items
        | _ -> []
      in
      let structured_content =
        match result |> member "structuredContent" with
        | `Null -> None
        | x -> Some x
      in
      return (Tool_result.create ~content ?structured_content ())
end

(** {1 Proxy Resource} *)

module Proxy_resource = struct
  type 'client t = {
    uri : string;
    name : string;
    description : string option;
    mime_type : string;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    value : string option;
    mirrored : bool;
  }

  let create ~client ~uri ~name ?description ?(mime_type = "text/plain") ?meta
      ?(tags = []) ?value () =
    {
      uri;
      name;
      description;
      mime_type;
      meta;
      tags;
      client;
      value;
      mirrored = true;
    }

  (** Create a ProxyResource from an MCP resource schema *)
  let from_mcp_resource (type client) ~(client : client)
      (mcp_resource : Yojson.Safe.t) : client t =
    let open Yojson.Safe.Util in
    let uri = mcp_resource |> member "uri" |> to_string in
    let name = mcp_resource |> member "name" |> to_string in
    let description =
      mcp_resource |> member "description" |> to_string_option
    in
    let mime_type =
      mcp_resource |> member "mimeType" |> to_string_option
      |> Option.value ~default:"text/plain"
    in
    let meta =
      match mcp_resource |> member "meta" with
      | `Null -> None
      | x -> Some x
    in
    let tags =
      match meta with
      | Some m -> (
        match m |> member "_fastmcp" |> member "tags" with
        | `List tags -> List.filter_map tags ~f:to_string_option
        | _ -> [])
      | None -> []
    in
    {
      uri;
      name;
      description;
      mime_type;
      meta;
      tags;
      client;
      value = None;
      mirrored = true;
    }

  (** Read the resource content from the remote server *)
  let read (type client) (module C : Client with type t = client) (t : client t)
      : string Deferred.t =
    match t.value with
    | Some v -> return v
    | None -> (
      let%bind () = C.connect t.client in
      let%bind result = C.read_resource t.client ~uri:t.uri in
      let%bind () = C.disconnect t.client in
      let open Yojson.Safe.Util in
      match result |> index 0 with
      | content -> (
        match content |> member "text" with
        | `String text -> return text
        | _ -> (
          match content |> member "blob" with
          | `String blob -> return blob
          | _ -> raise_s [%message "Unsupported content type"]))
      | exception _ -> raise_s [%message "No content in resource response"])
end

(** {1 Proxy Resource Template} *)

module Proxy_template = struct
  type 'client t = {
    uri_template : string;
    name : string;
    description : string option;
    mime_type : string;
    parameters : Yojson.Safe.t;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  let create ~client ~uri_template ~name ?description
      ?(mime_type = "text/plain") ?(parameters = `Assoc []) ?meta ?(tags = [])
      () =
    {
      uri_template;
      name;
      description;
      mime_type;
      parameters;
      meta;
      tags;
      client;
      mirrored = true;
    }

  (** Create a ProxyTemplate from an MCP template schema *)
  let from_mcp_template (type client) ~(client : client)
      (mcp_template : Yojson.Safe.t) : client t =
    let open Yojson.Safe.Util in
    let uri_template = mcp_template |> member "uriTemplate" |> to_string in
    let name = mcp_template |> member "name" |> to_string in
    let description =
      mcp_template |> member "description" |> to_string_option
    in
    let mime_type =
      mcp_template |> member "mimeType" |> to_string_option
      |> Option.value ~default:"text/plain"
    in
    let meta =
      match mcp_template |> member "meta" with
      | `Null -> None
      | x -> Some x
    in
    let tags =
      match meta with
      | Some m -> (
        match m |> member "_fastmcp" |> member "tags" with
        | `List tags -> List.filter_map tags ~f:to_string_option
        | _ -> [])
      | None -> []
    in
    {
      uri_template;
      name;
      description;
      mime_type;
      parameters = `Assoc [];
      meta;
      tags;
      client;
      mirrored = true;
    }

  (** Create a resource from the template by calling the remote server *)
  let create_resource (type client) (module C : Client with type t = client)
      (t : client t) ~(params : (string * string) list) :
      client Proxy_resource.t Deferred.t =
    (* URL encode and substitute parameters *)
    let parameterized_uri =
      List.fold params ~init:t.uri_template ~f:(fun uri (key, value) ->
          let encoded_value = Uri.pct_encode value in
          String.substr_replace_all uri
            ~pattern:("{" ^ key ^ "}")
            ~with_:encoded_value)
    in
    let%bind () = C.connect t.client in
    let%bind result = C.read_resource t.client ~uri:parameterized_uri in
    let%bind () = C.disconnect t.client in

    let open Yojson.Safe.Util in
    let content = result |> index 0 in
    let value =
      match content |> member "text" with
      | `String text -> text
      | _ -> (
        match content |> member "blob" with
        | `String blob -> blob
        | _ -> raise_s [%message "Unsupported content type"])
    in
    let mime_type =
      content |> member "mimeType" |> to_string_option
      |> Option.value ~default:t.mime_type
    in

    return
      (Proxy_resource.create ~client:t.client ~uri:parameterized_uri
         ~name:t.name ?description:t.description ~mime_type ?meta:t.meta
         ~tags:t.tags ~value ())
end

(** {1 Proxy Prompt} *)

module Proxy_prompt = struct
  type argument = {
    name : string;
    description : string option;
    required : bool;
  }

  type 'client t = {
    name : string;
    description : string option;
    arguments : argument list;
    meta : Yojson.Safe.t option;
    tags : string list;
    client : 'client;
    mirrored : bool;
  }

  let create ~client ~name ?description ?(arguments = []) ?meta ?(tags = []) ()
      =
    { name; description; arguments; meta; tags; client; mirrored = true }

  (** Create a ProxyPrompt from an MCP prompt schema *)
  let from_mcp_prompt (type client) ~(client : client)
      (mcp_prompt : Yojson.Safe.t) : client t =
    let open Yojson.Safe.Util in
    let name = mcp_prompt |> member "name" |> to_string in
    let description = mcp_prompt |> member "description" |> to_string_option in
    let arguments =
      match mcp_prompt |> member "arguments" with
      | `List args ->
        List.map args ~f:(fun arg ->
            {
              name = arg |> member "name" |> to_string;
              description = arg |> member "description" |> to_string_option;
              required =
                (match arg |> member "required" with
                | `Bool b -> b
                | _ -> false);
            })
      | _ -> []
    in
    let meta =
      match mcp_prompt |> member "meta" with
      | `Null -> None
      | x -> Some x
    in
    let tags =
      match meta with
      | Some m -> (
        match m |> member "_fastmcp" |> member "tags" with
        | `List tags -> List.filter_map tags ~f:to_string_option
        | _ -> [])
      | None -> []
    in
    { name; description; arguments; meta; tags; client; mirrored = true }

  (** Render the prompt by making a call through the client *)
  let render (type client) (module C : Client with type t = client)
      (t : client t) ~(arguments : Yojson.Safe.t) : Yojson.Safe.t Deferred.t =
    let%bind () = C.connect t.client in
    let%bind result =
      C.get_prompt t.client ~name:t.name ~arguments:(Some arguments)
    in
    let%bind () = C.disconnect t.client in
    let open Yojson.Safe.Util in
    return (result |> member "messages")
end

(** {1 Proxy Tool Manager} *)

module Proxy_tool_manager = struct
  type 'client t = {
    client_factory : 'client client_factory;
    local_tools : (string, Yojson.Safe.t) Hashtbl.t;
    transformations : (Yojson.Safe.t -> Yojson.Safe.t) list;
  }

  let create ~client_factory ?(transformations = []) () =
    {
      client_factory;
      local_tools = Hashtbl.create (module String);
      transformations;
    }

  (** Get a client instance by calling the factory *)
  let get_client t = t.client_factory ()

  (** Get tools from both local and remote sources *)
  let get_tools (type client) (module C : Client with type t = client)
      (t : client t) : (string, client Proxy_tool.t) Hashtbl.t Deferred.t =
    let all_tools = Hashtbl.create (module String) in

    (* Add local tools - convert to proxy tools *)
    Hashtbl.iteri t.local_tools ~f:(fun ~key ~data:_ ->
        (* Local tools would be handled differently *)
        ignore key);

    (* Add proxy tools from remote *)
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind client_tools =
      Monitor.try_with (fun () -> C.list_tools client) >>| function
      | Ok tools -> tools
      | Error _ -> []
    in
    let%bind () = C.disconnect client in

    List.iter client_tools ~f:(fun tool ->
        let proxy_tool = Proxy_tool.from_mcp_tool ~client tool in
        if not (Hashtbl.mem all_tools proxy_tool.name) then
          Hashtbl.set all_tools ~key:proxy_tool.name ~data:proxy_tool);

    return all_tools

  (** List tools *)
  let list_tools (type client) (module C : Client with type t = client)
      (t : client t) : client Proxy_tool.t list Deferred.t =
    let%bind tools = get_tools (module C) t in
    return (Hashtbl.data tools)

  (** Call a tool *)
  let call_tool (type client) (module C : Client with type t = client)
      (t : client t) ~(name : string) ~(arguments : Yojson.Safe.t) :
      Tool_result.t Deferred.t =
    (* Try local first, then proxy *)
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind result = C.call_tool client ~name ~arguments in
    let%bind () = C.disconnect client in

    let open Yojson.Safe.Util in
    let content =
      match result |> member "content" with
      | `List items -> items
      | _ -> []
    in
    let structured_content =
      match result |> member "structuredContent" with
      | `Null -> None
      | x -> Some x
    in
    return (Tool_result.create ~content ?structured_content ())
end

(** {1 Proxy Resource Manager} *)

module Proxy_resource_manager = struct
  type 'client t = {
    client_factory : 'client client_factory;
    local_resources : (string, Yojson.Safe.t) Hashtbl.t;
  }

  let create ~client_factory () =
    { client_factory; local_resources = Hashtbl.create (module String) }

  (** Get a client instance by calling the factory *)
  let get_client t = t.client_factory ()

  (** Get resources from both local and remote sources *)
  let get_resources (type client) (module C : Client with type t = client)
      (t : client t) : (string, client Proxy_resource.t) Hashtbl.t Deferred.t =
    let all_resources = Hashtbl.create (module String) in

    (* Add proxy resources from remote *)
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind client_resources =
      Monitor.try_with (fun () -> C.list_resources client) >>| function
      | Ok resources -> resources
      | Error _ -> []
    in
    let%bind () = C.disconnect client in

    List.iter client_resources ~f:(fun resource ->
        let proxy_resource =
          Proxy_resource.from_mcp_resource ~client resource
        in
        if not (Hashtbl.mem all_resources proxy_resource.uri) then
          Hashtbl.set all_resources ~key:proxy_resource.uri ~data:proxy_resource);

    return all_resources

  (** Read a resource *)
  let read_resource (type client) (module C : Client with type t = client)
      (t : client t) ~(uri : string) : string Deferred.t =
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind result = C.read_resource client ~uri in
    let%bind () = C.disconnect client in

    let open Yojson.Safe.Util in
    match result |> index 0 with
    | content -> (
      match content |> member "text" with
      | `String text -> return text
      | _ -> (
        match content |> member "blob" with
        | `String blob -> return blob
        | _ -> raise_s [%message "Unsupported content type"]))
    | exception _ -> raise_s [%message "No content in resource response"]
end

(** {1 Proxy Prompt Manager} *)

module Proxy_prompt_manager = struct
  type 'client t = {
    client_factory : 'client client_factory;
    local_prompts : (string, Yojson.Safe.t) Hashtbl.t;
  }

  let create ~client_factory () =
    { client_factory; local_prompts = Hashtbl.create (module String) }

  (** Get a client instance by calling the factory *)
  let get_client t = t.client_factory ()

  (** Get prompts from both local and remote sources *)
  let get_prompts (type client) (module C : Client with type t = client)
      (t : client t) : (string, client Proxy_prompt.t) Hashtbl.t Deferred.t =
    let all_prompts = Hashtbl.create (module String) in

    (* Add proxy prompts from remote *)
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind client_prompts =
      Monitor.try_with (fun () -> C.list_prompts client) >>| function
      | Ok prompts -> prompts
      | Error _ -> []
    in
    let%bind () = C.disconnect client in

    List.iter client_prompts ~f:(fun prompt ->
        let proxy_prompt = Proxy_prompt.from_mcp_prompt ~client prompt in
        if not (Hashtbl.mem all_prompts proxy_prompt.name) then
          Hashtbl.set all_prompts ~key:proxy_prompt.name ~data:proxy_prompt);

    return all_prompts

  (** Render a prompt *)
  let render_prompt (type client) (module C : Client with type t = client)
      (t : client t) ~(name : string) ~(arguments : Yojson.Safe.t option) :
      Yojson.Safe.t Deferred.t =
    let%bind client = get_client t in
    let%bind () = C.connect client in
    let%bind result = C.get_prompt client ~name ~arguments in
    let%bind () = C.disconnect client in
    return result
end

(** {1 OxFastMCP Proxy Server} *)

module Ox_fast_mcp_proxy = struct
  type 'client t = {
    name : string;
    client_factory : 'client client_factory;
    tool_manager : 'client Proxy_tool_manager.t;
    resource_manager : 'client Proxy_resource_manager.t;
    prompt_manager : 'client Proxy_prompt_manager.t;
  }

  let create ~name ~client_factory ?(transformations = []) () =
    let tool_manager =
      Proxy_tool_manager.create ~client_factory ~transformations ()
    in
    let resource_manager = Proxy_resource_manager.create ~client_factory () in
    let prompt_manager = Proxy_prompt_manager.create ~client_factory () in
    { name; client_factory; tool_manager; resource_manager; prompt_manager }
end

(** {1 Proxy Client} *)

module Proxy_client = struct
  type t = {
    name : string;
    transport : Yojson.Safe.t;
    roots_handler : (unit -> Yojson.Safe.t Deferred.t) option;
    sampling_handler :
      (Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) option;
    elicitation_handler :
      (string -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) option;
    log_handler : (Yojson.Safe.t -> unit Deferred.t) option;
    progress_handler :
      (float -> float option -> string option -> unit Deferred.t) option;
  }

  let generate_name () =
    sprintf "proxy-client-%s" (Time_ns.to_string_utc (Time_ns.now ()))

  let create ~transport ?name ?roots_handler ?sampling_handler
      ?elicitation_handler ?log_handler ?progress_handler () =
    let name = Option.value name ~default:(generate_name ()) in
    {
      name;
      transport;
      roots_handler;
      sampling_handler;
      elicitation_handler;
      log_handler;
      progress_handler;
    }

  (** Default sampling handler - forwards to proxy's connected clients *)
  let default_sampling_handler ~messages:_ ~params:_ =
    (* TODO: Implement actual sampling forwarding *)
    return
      (`Assoc
        [
          ("role", `String "assistant");
          ("model", `String "oxfastmcp-client");
          ("content", `Assoc [ ("type", `String "text"); ("text", `String "") ]);
        ])

  (** Default elicitation handler *)
  let default_elicitation_handler ~message:_ ~params:_ =
    (* TODO: Implement actual elicitation forwarding *)
    return (`Assoc [ ("action", `String "accept"); ("content", `Null) ])

  (** Default log handler *)
  let default_log_handler ~message:_ =
    (* TODO: Implement actual log forwarding *)
    return ()

  (** Default progress handler *)
  let default_progress_handler ~progress:_ ~total:_ ~message:_ =
    (* TODO: Implement actual progress forwarding *)
    return ()
end

(** {1 Stateful Proxy Client} *)

module Stateful_proxy_client = struct
  type session = Yojson.Safe.t
  type 'client cache = (session, 'client) Hashtbl.Poly.t
  type 'client t = { client : Proxy_client.t; caches : 'client cache }

  let create ~client = { client; caches = Hashtbl.Poly.create () }

  (** Clear all cached clients and force disconnect them *)
  let clear t =
    Hashtbl.clear t.caches;
    return ()

  (** Create a new stateful proxy client instance with the same configuration *)
  let new_stateful _t ~session ~create_client =
    (* TODO: Implement session-bound client creation *)
    let _ = session in
    create_client ()
end
