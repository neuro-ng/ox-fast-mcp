(** OxFastMCP Server Module

    The main server module providing the OxFastMCP server implementation. This
    is a more ergonomic interface for MCP servers with tool, resource, and
    prompt management, middleware support, and transport handling. *)

open! Core
open! Async

(** {1 Types} *)

(** Transport protocols for server communication *)
module Transport = struct
  type t = Stdio | Http | Sse | Streamable_http
  [@@deriving sexp, compare, equal, enumerate]

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.of_string s)
end

(** Behavior when encountering duplicate components *)
module Duplicate_behavior = struct
  type t = Warn | Error | Replace | Ignore
  [@@deriving sexp, compare, equal, enumerate]

  let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let of_string s =
    match String.lowercase s with
    | "warn" -> Warn
    | "error" -> Error
    | "replace" -> Replace
    | "ignore" -> Ignore
    | _ -> raise_s [%message "Unknown duplicate behavior" (s : string)]
end

(** Resource prefix format for mounted servers *)
module Resource_prefix_format = struct
  type t = Protocol | Path [@@deriving sexp, compare, equal, enumerate]

  let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let of_string s =
    match String.lowercase s with
    | "protocol" -> Protocol
    | "path" -> Path
    | _ -> raise_s [%message "Unknown resource prefix format" (s : string)]
end

(** {1 Component Types} *)

(** Tool representation *)
module Tool = struct
  type t = {
    name : string;
    key : string;
    description : string option;
    parameters : Yojson.Safe.t;
    annotations : Yojson.Safe.t option;
    output_schema : Yojson.Safe.t option;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    handler : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  let create ~name ?description ?(parameters = `Assoc []) ?annotations
      ?output_schema ?meta ?(tags = String.Set.empty) ~handler () =
    {
      name;
      key = name;
      description;
      parameters;
      annotations;
      output_schema;
      meta;
      tags;
      handler;
    }

  let to_mcp_tool ?(include_fastmcp_meta = false) t =
    let base = [ ("name", `String t.key); ("inputSchema", t.parameters) ] in
    let with_desc =
      match t.description with
      | Some d -> ("description", `String d) :: base
      | None -> base
    in
    let with_annotations =
      match t.annotations with
      | Some a -> ("annotations", a) :: with_desc
      | None -> with_desc
    in
    let with_output =
      match t.output_schema with
      | Some o -> ("outputSchema", o) :: with_annotations
      | None -> with_annotations
    in
    let with_meta =
      if include_fastmcp_meta then
        let tags_list =
          `List (Set.to_list t.tags |> List.map ~f:(fun s -> `String s))
        in
        let fastmcp_meta = `Assoc [ ("tags", tags_list) ] in
        let meta =
          match t.meta with
          | Some (`Assoc fields) -> `Assoc (("_fastmcp", fastmcp_meta) :: fields)
          | _ -> `Assoc [ ("_fastmcp", fastmcp_meta) ]
        in
        ("meta", meta) :: with_output
      else
        match t.meta with
        | Some m -> ("meta", m) :: with_output
        | None -> with_output
    in
    `Assoc with_meta
end

(** Resource representation *)
module Resource = struct
  type t = {
    uri : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    reader : unit -> string Deferred.t;
  }

  let create ~uri ~name ?description ?(mime_type = "text/plain") ?meta
      ?(tags = String.Set.empty) ~reader () =
    { uri; key = uri; name; description; mime_type; meta; tags; reader }

  let to_mcp_resource ?(include_fastmcp_meta = false) t =
    let base =
      [
        ("uri", `String t.key);
        ("name", `String t.name);
        ("mimeType", `String t.mime_type);
      ]
    in
    let with_desc =
      match t.description with
      | Some d -> ("description", `String d) :: base
      | None -> base
    in
    let with_meta =
      if include_fastmcp_meta then
        let tags_list =
          `List (Set.to_list t.tags |> List.map ~f:(fun s -> `String s))
        in
        let fastmcp_meta = `Assoc [ ("tags", tags_list) ] in
        let meta =
          match t.meta with
          | Some (`Assoc fields) -> `Assoc (("_fastmcp", fastmcp_meta) :: fields)
          | _ -> `Assoc [ ("_fastmcp", fastmcp_meta) ]
        in
        ("meta", meta) :: with_desc
      else
        match t.meta with
        | Some m -> ("meta", m) :: with_desc
        | None -> with_desc
    in
    `Assoc with_meta
end

(** Resource template representation *)
module Resource_template = struct
  type t = {
    uri_template : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    parameters : Yojson.Safe.t;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    create_resource : params:(string * string) list -> Resource.t Deferred.t;
  }

  let create ~uri_template ~name ?description ?(mime_type = "text/plain")
      ?(parameters = `Assoc []) ?meta ?(tags = String.Set.empty)
      ~create_resource () =
    {
      uri_template;
      key = uri_template;
      name;
      description;
      mime_type;
      parameters;
      meta;
      tags;
      create_resource;
    }
end

(** Prompt representation *)
module Prompt = struct
  type argument = {
    name : string;
    description : string option;
    required : bool;
  }

  type t = {
    name : string;
    key : string;
    description : string option;
    arguments : argument list;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    render : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  let create ~name ?description ?(arguments = []) ?meta
      ?(tags = String.Set.empty) ~render () =
    { name; key = name; description; arguments; meta; tags; render }
end

(** {1 Middleware} *)

module Middleware = struct
  type context = {
    message : Yojson.Safe.t;
    source : string;
    type_ : string;
    method_ : string;
  }

  type next = context -> Yojson.Safe.t Deferred.t
  type t = context -> next:next -> Yojson.Safe.t Deferred.t

  let identity ctx ~next = next ctx

  let compose middlewares =
    List.fold_right middlewares ~init:identity ~f:(fun mw acc ctx ~next ->
        mw ctx ~next:(fun ctx' -> acc ctx' ~next))
end

(** {1 Main Server} *)

module Ox_fast_mcp = struct
  type t = {
    name : string;
    version : string option;
    instructions : string option;
    website_url : string option;
    icons : Yojson.Safe.t list;
    resource_prefix_format : Resource_prefix_format.t;
    include_tags : String.Set.t option;
    exclude_tags : String.Set.t option;
    strict_input_validation : bool;
    include_fastmcp_meta : bool;
    mutable middleware : Middleware.t list;
    mutable tools : (string, Tool.t) Hashtbl.t;
    mutable resources : (string, Resource.t) Hashtbl.t;
    mutable templates : (string, Resource_template.t) Hashtbl.t;
    mutable prompts : (string, Prompt.t) Hashtbl.t;
    on_duplicate_tools : Duplicate_behavior.t;
    on_duplicate_resources : Duplicate_behavior.t;
    on_duplicate_prompts : Duplicate_behavior.t;
  }

  let generate_name () =
    let now = Time_ns.now () in
    let hash = Time_ns.to_int_ns_since_epoch now |> Int.to_string in
    sprintf "OxFastMCP-%s" (String.prefix hash 8)

  let create ?name ?version ?instructions ?website_url ?(icons = [])
      ?(resource_prefix_format = Resource_prefix_format.Protocol) ?include_tags
      ?exclude_tags ?(strict_input_validation = false)
      ?(include_fastmcp_meta = true) ?(middleware = [])
      ?(on_duplicate_tools = Duplicate_behavior.Warn)
      ?(on_duplicate_resources = Duplicate_behavior.Warn)
      ?(on_duplicate_prompts = Duplicate_behavior.Warn) () =
    let name = Option.value name ~default:(generate_name ()) in
    {
      name;
      version;
      instructions;
      website_url;
      icons;
      resource_prefix_format;
      include_tags;
      exclude_tags;
      strict_input_validation;
      include_fastmcp_meta;
      middleware;
      tools = Hashtbl.create (module String);
      resources = Hashtbl.create (module String);
      templates = Hashtbl.create (module String);
      prompts = Hashtbl.create (module String);
      on_duplicate_tools;
      on_duplicate_resources;
      on_duplicate_prompts;
    }

  (* Accessors *)
  let name t = t.name
  let version t = t.version
  let instructions t = t.instructions

  let should_enable_component t ~(tags : String.Set.t) =
    let include_ok =
      match t.include_tags with
      | None -> true
      | Some include_set -> not (Set.is_empty (Set.inter tags include_set))
    in
    let exclude_ok =
      match t.exclude_tags with
      | None -> true
      | Some exclude_set -> Set.is_empty (Set.inter tags exclude_set)
    in
    include_ok && exclude_ok

  let handle_duplicate ~behavior ~key ~component_type =
    match behavior with
    | Duplicate_behavior.Error ->
      raise_s
        [%message
          "Duplicate component" (component_type : string) (key : string)]
    | Duplicate_behavior.Warn ->
      Logging.Global.warning
        (sprintf "Duplicate %s: %s (replacing)" component_type key)
    | Duplicate_behavior.Replace -> ()
    | Duplicate_behavior.Ignore -> ()

  let add_tool t (tool : Tool.t) =
    let key = tool.key in
    if Hashtbl.mem t.tools key then
      handle_duplicate ~behavior:t.on_duplicate_tools ~key
        ~component_type:"tool";
    match t.on_duplicate_tools with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.tools key -> ()
    | _ -> Hashtbl.set t.tools ~key ~data:tool

  let remove_tool t ~name = Hashtbl.remove t.tools name

  let get_tools t =
    Hashtbl.to_alist t.tools
    |> List.filter_map ~f:(fun (key, tool) ->
           if should_enable_component t ~tags:tool.tags then Some (key, tool)
           else None)
    |> Hashtbl.of_alist_exn (module String)

  let list_tools_mcp t =
    get_tools t |> Hashtbl.data
    |> List.map
         ~f:(Tool.to_mcp_tool ~include_fastmcp_meta:t.include_fastmcp_meta)

  let add_resource t (resource : Resource.t) =
    let key = resource.key in
    if Hashtbl.mem t.resources key then
      handle_duplicate ~behavior:t.on_duplicate_resources ~key
        ~component_type:"resource";
    match t.on_duplicate_resources with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.resources key -> ()
    | _ -> Hashtbl.set t.resources ~key ~data:resource

  let get_resources t =
    Hashtbl.to_alist t.resources
    |> List.filter_map ~f:(fun (key, resource) ->
           if should_enable_component t ~tags:resource.tags then
             Some (key, resource)
           else None)
    |> Hashtbl.of_alist_exn (module String)

  let list_resources_mcp t =
    get_resources t |> Hashtbl.data
    |> List.map
         ~f:
           (Resource.to_mcp_resource
              ~include_fastmcp_meta:t.include_fastmcp_meta)

  let add_template t (template : Resource_template.t) =
    let key = template.key in
    if Hashtbl.mem t.templates key then
      handle_duplicate ~behavior:t.on_duplicate_resources ~key
        ~component_type:"resource_template";
    match t.on_duplicate_resources with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.templates key -> ()
    | _ -> Hashtbl.set t.templates ~key ~data:template

  let get_templates t =
    Hashtbl.to_alist t.templates
    |> List.filter_map ~f:(fun (key, template) ->
           if should_enable_component t ~tags:template.tags then
             Some (key, template)
           else None)
    |> Hashtbl.of_alist_exn (module String)

  let add_prompt t (prompt : Prompt.t) =
    let key = prompt.key in
    if Hashtbl.mem t.prompts key then
      handle_duplicate ~behavior:t.on_duplicate_prompts ~key
        ~component_type:"prompt";
    match t.on_duplicate_prompts with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.prompts key -> ()
    | _ -> Hashtbl.set t.prompts ~key ~data:prompt

  let get_prompts t =
    Hashtbl.to_alist t.prompts
    |> List.filter_map ~f:(fun (key, prompt) ->
           if should_enable_component t ~tags:prompt.tags then Some (key, prompt)
           else None)
    |> Hashtbl.of_alist_exn (module String)

  let call_tool t ~name ~arguments =
    match Hashtbl.find t.tools name with
    | None -> raise_s [%message "Unknown tool" (name : string)]
    | Some tool ->
      if not (should_enable_component t ~tags:tool.tags) then
        raise_s [%message "Tool is disabled" (name : string)]
      else tool.handler arguments

  let read_resource t ~uri =
    match Hashtbl.find t.resources uri with
    | None -> raise_s [%message "Unknown resource" (uri : string)]
    | Some resource ->
      if not (should_enable_component t ~tags:resource.tags) then
        raise_s [%message "Resource is disabled" (uri : string)]
      else resource.reader ()

  let get_prompt t ~name ~arguments =
    match Hashtbl.find t.prompts name with
    | None -> raise_s [%message "Unknown prompt" (name : string)]
    | Some prompt ->
      if not (should_enable_component t ~tags:prompt.tags) then
        raise_s [%message "Prompt is disabled" (name : string)]
      else prompt.render arguments

  let add_middleware t middleware =
    t.middleware <- t.middleware @ [ middleware ]

  let run_stdio_async _t =
    Logging.Global.info "Starting OxFastMCP server with stdio transport";
    return ()

  let run_http_async _t ~transport:_ ~host:_ ~port:_ =
    Logging.Global.info "Starting OxFastMCP server with HTTP transport";
    return ()

  let run_async t ?(transport = Transport.Stdio) ?host ?port () =
    match transport with
    | Transport.Stdio -> run_stdio_async t
    | Transport.Http | Transport.Sse | Transport.Streamable_http ->
      let host = Option.value host ~default:"127.0.0.1" in
      let port = Option.value port ~default:8000 in
      run_http_async t ~transport ~host ~port
end

(** {1 Helper Functions} *)

let add_resource_prefix ~uri ~prefix ~format =
  match format with
  | Resource_prefix_format.Protocol -> (
    (* Split on "://" to get protocol and path *)
    match String.substr_index uri ~pattern:"://" with
    | Some idx ->
      let proto = String.prefix uri (idx + 3) in
      (* include :// *)
      let rest = String.drop_prefix uri (idx + 3) in
      sprintf "%s%s/%s" proto prefix rest
    | None -> sprintf "%s/%s" prefix uri)
  | Resource_prefix_format.Path -> sprintf "%s/%s" prefix uri

let has_resource_prefix ~uri ~prefix ~format =
  match format with
  | Resource_prefix_format.Protocol ->
    String.is_substring uri ~substring:(sprintf "/%s/" prefix)
  | Resource_prefix_format.Path -> String.is_prefix uri ~prefix:(prefix ^ "/")

let remove_resource_prefix ~uri ~prefix ~format =
  match format with
  | Resource_prefix_format.Protocol ->
    String.substr_replace_first uri ~pattern:(sprintf "/%s" prefix) ~with_:""
  | Resource_prefix_format.Path ->
    String.chop_prefix_if_exists uri ~prefix:(prefix ^ "/")
