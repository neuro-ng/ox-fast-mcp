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

(** {1 Protocol Handlers} *)

module Protocol = struct
  type handler = Context.t -> Yojson.Safe.t Deferred.t
  (** Handler function type - takes context, returns JSON response *)

  type method_map = (string, handler) Hashtbl.t
  (** Method name to handler mapping *)

  let create_method_map () : method_map = Hashtbl.create (module String)
  [@@warning "-32"]
  (* Unused until protocol handlers are implemented *)
end

(** {1 Main Server} *)

module Ox_fast_mcp = struct
  type mounted_server = {
    server : t;
    prefix : string option;
    mounted_resource_prefix_format : Resource_prefix_format.t;
  }
  (** Inline recursive type for mounted servers *)

  and t = {
    name : string;
    version : string option;
    mutable instructions : string option;
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
    mutable mounted_servers : mounted_server list;
    on_duplicate_tools : Duplicate_behavior.t;
    on_duplicate_resources : Duplicate_behavior.t;
    on_duplicate_prompts : Duplicate_behavior.t;
    (* Statistics tracking *)
    mutable tool_call_counts : (string, int) Hashtbl.t;
    mutable resource_access_counts : (string, int) Hashtbl.t;
  }

  let generate_name () =
    let now = Time_ns.now () in
    let hash = Time_ns.to_int_ns_since_epoch now |> Int.to_string in
    sprintf "OxFastMCP-%s" (String.prefix hash 8)

  (** {2 Validation Helpers} *)

  let validate_tool_name name =
    if String.is_empty name then Error "Tool name cannot be empty"
    else if String.contains name ' ' then
      Error "Tool name cannot contain spaces"
    else if not (Char.is_alphanum name.[0] || Char.equal name.[0] '_') then
      Error "Tool name must start with a letter, number, or underscore"
    else Ok ()

  let validate_resource_uri uri =
    if String.is_empty uri then Error "Resource URI cannot be empty"
    else if not (String.contains uri ':') then
      Error "Resource URI must contain a scheme (e.g., 'file://', 'http://')"
    else
      match String.substr_index uri ~pattern:"://" with
      | Some idx when idx > 0 -> Ok ()
      | _ -> Error "Resource URI must have a valid scheme format (scheme://...)"

  let validate_prompt_name name =
    if String.is_empty name then Error "Prompt name cannot be empty"
    else if String.contains name ' ' then
      Error "Prompt name cannot contain spaces"
    else Ok ()

  let validate_template_uri uri_template =
    if String.is_empty uri_template then Error "Template URI cannot be empty"
    else if not (String.contains uri_template '{') then
      Error
        "Template URI must contain at least one parameter placeholder (e.g., \
         {param})"
    else if not (String.contains uri_template ':') then
      Error "Template URI must contain a scheme (e.g., 'file://', 'http://')"
    else Ok ()

  (** {2 Name Normalization} *)

  (** Normalize a tool name to valid format.
      Converts to lowercase, replaces spaces and invalid chars with underscores,
      ensures starts with letter or underscore. *)
  let normalize_tool_name name =
    let normalized =
      name
      |> String.lowercase
      |> String.map ~f:(fun c ->
             if Char.is_alphanum c || Char.equal c '_' then c else '_')
    in
    (* Ensure it starts with letter or underscore *)
    if String.is_empty normalized then "_"
    else if Char.is_digit normalized.[0] then
      "_" ^ normalized
    else normalized

  (** {2 Public Validation Helpers} *)

  (** Check if a tool name is valid *)
  let is_valid_tool_name name =
    Result.is_ok (validate_tool_name name)

  (** Check if a URI is valid *)
  let is_valid_uri uri =
    Result.is_ok (validate_resource_uri uri)

  (** Check if a prompt name is valid *)
  let is_valid_prompt_name name =
    Result.is_ok (validate_prompt_name name)

  (** Check if a template URI is valid *)
  let is_valid_template_uri uri =
    Result.is_ok (validate_template_uri uri)

  (** {2 Similar Name Suggestions} *)

  (** Suggest similar component names using Levenshtein-like similarity *)
  let suggest_similar_names target available =
    let distance s1 s2 =
      let len1 = String.length s1 in
      let len2 = String.length s2 in
      let matrix = Array.make_matrix ~dimx:(len1 + 1) ~dimy:(len2 + 1) 0 in
      for i = 0 to len1 do
        matrix.(i).(0) <- i
      done;
      for j = 0 to len2 do
        matrix.(0).(j) <- j
      done;
      for i = 1 to len1 do
        for j = 1 to len2 do
          let cost = if Char.equal s1.[i - 1] s2.[j - 1] then 0 else 1 in
          matrix.(i).(j) <-
            Int.min
              (Int.min (matrix.(i - 1).(j) + 1) (matrix.(i).(j - 1) + 1))
              (matrix.(i - 1).(j - 1) + cost)
        done
      done;
      matrix.(len1).(len2)
    in
    available
    |> List.map ~f:(fun name -> name, distance target name)
    |> List.sort ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)
    |> List.take_while ~f:(fun (_, dist) -> dist <= 3)
    |> List.map ~f:fst
    |> List.take_while ~f:(fun _ -> true)
    |> (fun l -> List.take l 5)

  let create ?name ?version ?instructions ?website_url ?(icons = [])
      ?(resource_prefix_format = Resource_prefix_format.Protocol) ?include_tags
      ?exclude_tags ?(strict_input_validation = false)
      ?(include_fastmcp_meta = true) ?(middleware = [])
      ?(on_duplicate_tools = Duplicate_behavior.Warn)
      ?(on_duplicate_resources = Duplicate_behavior.Warn)
      ?(on_duplicate_prompts = Duplicate_behavior.Warn) ?(tools = [])
      ?(resources = []) ?(prompts = []) () =
    let name = Option.value name ~default:(generate_name ()) in
    let tools_tbl = Hashtbl.create (module String) in
    let resources_tbl = Hashtbl.create (module String) in
    let prompts_tbl = Hashtbl.create (module String) in
    (* Add bulk tools *)
    List.iter tools ~f:(fun tool ->
        Hashtbl.set tools_tbl ~key:tool.Tool.key ~data:tool);
    (* Add bulk resources *)
    List.iter resources ~f:(fun resource ->
        Hashtbl.set resources_tbl ~key:resource.Resource.key ~data:resource);
    (* Add bulk prompts *)
    List.iter prompts ~f:(fun prompt ->
        Hashtbl.set prompts_tbl ~key:prompt.Prompt.key ~data:prompt);
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
      tools = tools_tbl;
      resources = resources_tbl;
      templates = Hashtbl.create (module String);
      prompts = prompts_tbl;
      mounted_servers = [];
      on_duplicate_tools;
      on_duplicate_resources;
      on_duplicate_prompts;
      (* Initialize statistics *)
      tool_call_counts = Hashtbl.create (module String);
      resource_access_counts = Hashtbl.create (module String);
    }

  (** Apply middleware chain to a handler *)
  let apply_middleware_chain (middlewares : Middleware.t list)
      (handler : Context.t -> Yojson.Safe.t Deferred.t) (ctx : Context.t) :
      Yojson.Safe.t Deferred.t =
    (* Convert Context.t to Middleware.context *)
    let mw_ctx =
      {
        Middleware.message = Option.value ctx.params ~default:(`Assoc []);
        source = "server";
        type_ = "request";
        method_ = Option.value ctx.method_name ~default:"unknown";
      }
    in
    (* Apply middleware chain *)
    let rec apply_chain remaining_middlewares =
      match remaining_middlewares with
      | [] -> handler ctx
      | mw :: rest ->
        let next _mw_ctx = apply_chain rest in
        mw mw_ctx ~next
    in
    apply_chain middlewares
  [@@warning "-32"]
  (* Unused until setup_handlers uses it *)

  (* Accessors *)
  let name t = t.name
  let version t = t.version
  let instructions t = t.instructions
  let website_url t = t.website_url
  let icons t = t.icons

  (** Set server instructions (mutable) *)
  let set_instructions t instructions = t.instructions <- Some instructions

  (** Get server info for MCP protocol *)
  let server_info t =
    `Assoc
      [
        ("name", `String t.name);
        ( "version",
          match t.version with
          | Some v -> `String v
          | None -> `String "0.1.0" );
      ]

  (** Get server capabilities for MCP protocol *)
  let capabilities _t =
    `Assoc
      [
        ( "tools",
          `Assoc
            [ ("listChanged", `Bool true); ("supportsProgress", `Bool false) ]
        );
        ( "resources",
          `Assoc [ ("subscribe", `Bool false); ("listChanged", `Bool true) ] );
        ("prompts", `Assoc [ ("listChanged", `Bool true) ]);
      ]

  (** Get server statistics *)
  let get_stats t =
    `Assoc
      [
        ("tools_count", `Int (Hashtbl.length t.tools));
        ("resources_count", `Int (Hashtbl.length t.resources));
        ("prompts_count", `Int (Hashtbl.length t.prompts));
        ("templates_count", `Int (Hashtbl.length t.templates));
        ("mounted_servers_count", `Int (List.length t.mounted_servers));
      ]

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

  let rec get_tools t =
    (* Get local tools and filter them *)
    let local_tools =
      Hashtbl.to_alist t.tools
      |> List.filter_map ~f:(fun (key, tool) ->
             if should_enable_component t ~tags:tool.tags then Some (key, tool)
             else None)
    in
    (* Get tools from mounted servers with prefix *)
    let mounted_tools =
      List.concat_map t.mounted_servers ~f:(fun mounted ->
          let child_tools = get_tools mounted.server |> Hashtbl.to_alist in
          List.filter_map child_tools ~f:(fun (key, child_tool) ->
              (* Apply parent server's filtering *)
              if not (should_enable_component t ~tags:child_tool.Tool.tags) then
                None
              else
                let new_key =
                  match mounted.prefix with
                  | Some prefix -> sprintf "%s_%s" prefix key
                  | None -> key
                in
                Some (new_key, child_tool)))
    in
    (* Merge mounted first, then local (so local wins, but among mounted, later
       wins) *)
    List.append mounted_tools local_tools
    |> Hashtbl.of_alist_multi (module String)
    |> Hashtbl.map ~f:(fun tools -> List.last_exn tools)

  let get_tool t ~key =
    match Hashtbl.find (get_tools t) key with
    | Some tool -> return tool
    | None -> raise_s [%message "Unknown tool" (key : string)]

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

  let rec get_resources t =
    (* Get local resources and filter them *)
    let local_resources =
      Hashtbl.to_alist t.resources
      |> List.filter_map ~f:(fun (key, resource) ->
             if should_enable_component t ~tags:resource.tags then
               Some (key, resource)
             else None)
    in
    (* Get resources from mounted servers with resource prefix *)
    let mounted_resources =
      List.concat_map t.mounted_servers ~f:(fun mounted ->
          let child_resources =
            get_resources mounted.server |> Hashtbl.to_alist
          in
          List.filter_map child_resources ~f:(fun (key, child_resource) ->
              if
                not
                  (should_enable_component t ~tags:child_resource.Resource.tags)
              then None
              else
                let new_key =
                  match mounted.prefix with
                  | Some prefix -> (
                    (* Inline resource prefix logic *)
                    match mounted.mounted_resource_prefix_format with
                    | Resource_prefix_format.Protocol -> (
                      match String.substr_index key ~pattern:"://" with
                      | Some idx ->
                        let proto = String.prefix key (idx + 3) in
                        let rest = String.drop_prefix key (idx + 3) in
                        sprintf "%s%s/%s" proto prefix rest
                      | None -> sprintf "%s/%s" prefix key)
                    | Resource_prefix_format.Path -> sprintf "%s/%s" prefix key)
                  | None -> key
                in
                Some (new_key, child_resource)))
    in
    (* Merge local and mounted *)
    List.append local_resources mounted_resources
    |> Hashtbl.of_alist_multi (module String)
    |> Hashtbl.map ~f:(fun resources -> List.last_exn resources)

  let get_resource t ~key =
    match Hashtbl.find (get_resources t) key with
    | Some resource -> return resource
    | None ->
      let available = get_resources t |> Hashtbl.keys |> List.of_list in
      let suggestions = suggest_similar_names key available in
      (match suggestions with
      | [] -> raise_s [%message "Unknown resource" (key : string)]
      | sugg ->
        let suggestion_str = String.concat ~sep:", " sugg in
        raise_s
          [%message
            "Unknown resource"
              (key : string)
              ~did_you_mean:(suggestion_str : string)])

  let list_resources_mcp t =
    get_resources t |> Hashtbl.data
    |> List.map
         ~f:
           (Resource.to_mcp_resource
              ~include_fastmcp_meta:t.include_fastmcp_meta)

  let remove_resource t ~uri = Hashtbl.remove t.resources uri

  let add_template t (template : Resource_template.t) =
    let key = template.key in
    if Hashtbl.mem t.templates key then
      handle_duplicate ~behavior:t.on_duplicate_resources ~key
        ~component_type:"resource_template";
    match t.on_duplicate_resources with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.templates key -> ()
    | _ -> Hashtbl.set t.templates ~key ~data:template

  let remove_template t ~uri_template = Hashtbl.remove t.templates uri_template

  let rec get_templates t =
    (* Get local templates and filter them *)
    let local_templates =
      Hashtbl.to_alist t.templates
      |> List.filter_map ~f:(fun (key, template) ->
             if should_enable_component t ~tags:template.tags then
               Some (key, template)
             else None)
    in
    (* Get templates from mounted servers with resource prefix *)
    let mounted_templates =
      List.concat_map t.mounted_servers ~f:(fun mounted ->
          let child_templates =
            get_templates mounted.server |> Hashtbl.to_alist
          in
          List.filter_map child_templates ~f:(fun (key, child_template) ->
              if
                not
                  (should_enable_component t
                     ~tags:child_template.Resource_template.tags)
              then None
              else
                let new_key =
                  match mounted.prefix with
                  | Some prefix -> (
                    (* Inline resource prefix logic *)
                    match mounted.mounted_resource_prefix_format with
                    | Resource_prefix_format.Protocol -> (
                      match String.substr_index key ~pattern:"://" with
                      | Some idx ->
                        let proto = String.prefix key (idx + 3) in
                        let rest = String.drop_prefix key (idx + 3) in
                        sprintf "%s%s/%s" proto prefix rest
                      | None -> sprintf "%s/%s" prefix key)
                    | Resource_prefix_format.Path -> sprintf "%s/%s" prefix key)
                  | None -> key
                in
                Some (new_key, child_template)))
    in
    (* Merge local and mounted *)
    List.append local_templates mounted_templates
    |> Hashtbl.of_alist_multi (module String)
    |> Hashtbl.map ~f:(fun templates -> List.last_exn templates)

  let get_template t ~key =
    match Hashtbl.find (get_templates t) key with
    | Some template -> return template
    | None ->
      let available = get_templates t |> Hashtbl.keys |> List.of_list in
      let suggestions = suggest_similar_names key available in
      (match suggestions with
      | [] -> raise_s [%message "Unknown resource template" (key : string)]
      | sugg ->
        let suggestion_str = String.concat ~sep:", " sugg in
        raise_s
          [%message
            "Unknown resource template"
              (key : string)
              ~did_you_mean:(suggestion_str : string)])

  let list_templates_mcp t =
    get_templates t |> Hashtbl.data
    |> List.map ~f:(fun template ->
           `Assoc
             [
               ("uriTemplate", `String template.Resource_template.uri_template);
               ("name", `String template.Resource_template.name);
               ( "description",
                 match template.Resource_template.description with
                 | Some d -> `String d
                 | None -> `Null );
               ("mimeType", `String template.Resource_template.mime_type);
             ])

  let add_prompt t (prompt : Prompt.t) =
    let key = prompt.key in
    if Hashtbl.mem t.prompts key then
      handle_duplicate ~behavior:t.on_duplicate_prompts ~key
        ~component_type:"prompt";
    match t.on_duplicate_prompts with
    | Duplicate_behavior.Ignore when Hashtbl.mem t.prompts key -> ()
    | _ -> Hashtbl.set t.prompts ~key ~data:prompt

  let remove_prompt t ~name = Hashtbl.remove t.prompts name

  let rec get_prompts t =
    (* Get local prompts and filter them *)
    let local_prompts =
      Hashtbl.to_alist t.prompts
      |> List.filter_map ~f:(fun (key, prompt) ->
             if should_enable_component t ~tags:prompt.tags then
               Some (key, prompt)
             else None)
    in
    (* Get prompts from mounted servers with prefix *)
    let mounted_prompts =
      List.concat_map t.mounted_servers ~f:(fun mounted ->
          let child_prompts = get_prompts mounted.server |> Hashtbl.to_alist in
          List.filter_map child_prompts ~f:(fun (key, child_prompt) ->
              if not (should_enable_component t ~tags:child_prompt.Prompt.tags)
              then None
              else
                let new_key =
                  match mounted.prefix with
                  | Some prefix -> sprintf "%s_%s" prefix key
                  | None -> key
                in
                Some (new_key, child_prompt)))
    in
    (* Merge local and mounted *)
    List.append local_prompts mounted_prompts
    |> Hashtbl.of_alist_multi (module String)
    |> Hashtbl.map ~f:(fun prompts -> List.last_exn prompts)

  let get_prompt_component t ~key =
    match Hashtbl.find (get_prompts t) key with
    | Some prompt -> return prompt
    | None ->
      let available = get_prompts t |> Hashtbl.keys |> List.of_list in
      let suggestions = suggest_similar_names key available in
      (match suggestions with
      | [] -> raise_s [%message "Unknown prompt" (key : string)]
      | sugg ->
        let suggestion_str = String.concat ~sep:", " sugg in
        raise_s
          [%message
            "Unknown prompt"
              (key : string)
              ~did_you_mean:(suggestion_str : string)])

  let list_prompts_mcp t =
    get_prompts t |> Hashtbl.data
    |> List.map ~f:(fun prompt ->
           `Assoc
             [
               ("name", `String prompt.Prompt.name);
               ( "description",
                 match prompt.Prompt.description with
                 | Some d -> `String d
                 | None -> `Null );
               ("arguments", `List []);
             ])

  (** Simple helper to add a resource with just uri and handler *)
  let add_simple_resource ?description ?(mime_type = "text/plain") ?meta
      ?(tags = String.Set.empty) ~uri ~name ~reader t =
    let resource =
      Resource.create ~uri ~name ?description ~mime_type ?meta ~tags ~reader ()
    in
    add_resource t resource

  (** Simple helper to add a tool with just name and handler *)
  let add_simple_tool ?description ?(parameters = `Assoc []) ?annotations
      ?output_schema ?meta ?(tags = String.Set.empty) ~name ~handler t =
    let tool =
      Tool.create ~name ?description ~parameters ?annotations ?output_schema
        ?meta ~tags ~handler ()
    in
    add_tool t tool

  (** Simple helper to add a prompt with just name and handler *)
  let add_simple_prompt ?description ?(arguments = []) ?meta
      ?(tags = String.Set.empty) ~name ~render t =
    let prompt =
      Prompt.create ~name ?description ~arguments ?meta ~tags ~render ()
    in
    add_prompt t prompt

  (** {2 Batch Operation Helpers} *)

  let add_tools t tools = List.iter tools ~f:(fun tool -> add_tool t tool)

  let add_resources t resources =
    List.iter resources ~f:(fun resource -> add_resource t resource)

  let add_prompts t prompts =
    List.iter prompts ~f:(fun prompt -> add_prompt t prompt)

  let add_templates t templates =
    List.iter templates ~f:(fun template -> add_template t template)

  (** {2 Server Inspection & Discovery} *)

  (** Get comprehensive server description *)
  let describe_server t =
    `Assoc
      [
        ("name", `String t.name);
        ( "version",
          match t.version with
          | Some v -> `String v
          | None -> `String "0.1.0" );
        ( "instructions",
          match t.instructions with
          | Some i -> `String i
          | None -> `Null );
        ("tool_count", `Int (Hashtbl.length (get_tools t)));
        ("resource_count", `Int (Hashtbl.length (get_resources t)));
        ("template_count", `Int (Hashtbl.length (get_templates t)));
        ("prompt_count", `Int (Hashtbl.length (get_prompts t)));
        ("mounted_server_count", `Int (List.length t.mounted_servers));
        ("middleware_count", `Int (List.length t.middleware));
        ("strict_input_validation", `Bool t.strict_input_validation);
        ("include_fastmcp_meta", `Bool t.include_fastmcp_meta);
      ]

  (** Find tools with a specific tag *)
  let find_tools_by_tag t ~tag =
    get_tools t |> Hashtbl.data
    |> List.filter ~f:(fun tool -> Set.mem tool.Tool.tags tag)

  (** Find resources by URI scheme *)
  let find_resources_by_scheme t ~scheme =
    get_resources t |> Hashtbl.data
    |> List.filter ~f:(fun resource ->
           String.is_prefix resource.Resource.uri ~prefix:(scheme ^ "://"))

  (** Find prompts with a specific tag *)
  let find_prompts_by_tag t ~tag =
    get_prompts t |> Hashtbl.data
    |> List.filter ~f:(fun prompt -> Set.mem prompt.Prompt.tags tag)

  (** Validate server configuration *)
  let validate_server t =
    let errors = ref [] in
    (* Check for empty name *)
    if String.is_empty t.name then
      errors := "Server name cannot be empty" :: !errors;
    (* Check for duplicate tool names *)
    let tool_names = get_tools t |> Hashtbl.keys |> Hash_set.of_list (module String) in
    if Hash_set.length tool_names < Hashtbl.length (get_tools t) then
      errors := "Duplicate tool names detected" :: !errors;
    (* Check for invalid URIs *)
    get_resources t |> Hashtbl.iter ~f:(fun resource ->
        match validate_resource_uri resource.Resource.uri with
        | Error msg -> errors := sprintf "Invalid resource URI: %s" msg :: !errors
        | Ok () -> ());
    (* Return result *)
    if List.is_empty !errors then Ok ()
    else Error (List.rev !errors)

  (** {2 Statistics & Debug Utilities} *)

  (** Get all component names organized by type *)
  let list_all_component_names t =
    `Assoc
      [
        ("tools", `List (Hashtbl.keys (get_tools t) |> List.of_list |> List.map ~f:(fun s -> `String s)));
        ("resources", `List (Hashtbl.keys (get_resources t) |> List.of_list |> List.map ~f:(fun s -> `String s)));
        ("prompts", `List (Hashtbl.keys (get_prompts t) |> List.of_list |> List.map ~f:(fun s -> `String s)));
        ("templates", `List (Hashtbl.keys (get_templates t) |> List.of_list |> List.map ~f:(fun s -> `String s)));
      ]

  (** Count components grouped by tag *)
  let component_count_by_tag t =
    let tag_counts = Hashtbl.create (module String) in
    let increment_tag tag  =
      Hashtbl.update tag_counts tag ~f:(function
        | None -> 1
        | Some count -> count + 1)
    in
    (* Count tool tags *)
    get_tools t |> Hashtbl.iter ~f:(fun tool ->
        Set.iter tool.Tool.tags ~f:increment_tag);
    (* Count resource tags *)
    get_resources t |> Hashtbl.iter ~f:(fun resource ->
        Set.iter resource.Resource.tags ~f:increment_tag);
    (* Count prompt tags *)
    get_prompts t |> Hashtbl.iter ~f:(fun prompt ->
        Set.iter prompt.Prompt.tags ~f:increment_tag);
    (* Convert to sorted list *)
    Hashtbl.to_alist tag_counts
    |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)

  (** Get tool call statistics *)
  let get_tool_stats t =
    Hashtbl.to_alist t.tool_call_counts
    |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)

  (** Get resource access statistics *)
  let get_resource_stats t =
    Hashtbl.to_alist t.resource_access_counts
    |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)

  (** Reset all statistics *)
  let reset_stats t =
    Hashtbl.clear t.tool_call_counts;
    Hashtbl.clear t.resource_access_counts

  (** Comprehensive health check *)
  let health_check t =
    try
      let validation = validate_server t in
      let tool_count = Hashtbl.length (get_tools t) in
      let resource_count = Hashtbl.length (get_resources t) in
      let status = match validation with
        | Ok () -> "healthy"
        | Error _ -> "degraded"
      in
      Ok (`Assoc [
        ("status", `String status);
        ("server_name", `String t.name);
        ("tool_count", `Int tool_count);
        ("resource_count", `Int resource_count);
        ("validation", match validation with
          | Ok () -> `String "passed"
          | Error errs -> `Assoc [("errors", `List (List.map errs ~f:(fun e -> `String e)))]);
      ])
    with exn ->
      Error (Exn.to_string exn)

  let rec call_tool t ~name ~arguments =
    match Hashtbl.find t.tools name with
    | Some tool ->
      if not (should_enable_component t ~tags:tool.tags) then
        raise_s [%message "Tool is disabled" (name : string)]
      else
        (* Apply input validation if enabled *)
        let validated_args =
          if t.strict_input_validation then
            (* Strict mode: require exact type matches *)
            match
              Input_validation.validate_tool_input ~mode:Strict
                ~schema:tool.parameters ~input:arguments
            with
            | Ok args -> args
            | Error errors ->
              let error_msg = Input_validation.format_errors errors in
              raise_s
                [%message
                  "Input validation failed (strict mode)"
                    (name : string)
                    (error_msg : string)]
          else
            (* Lenient mode (default): attempt type coercion *)
            match
              Input_validation.validate_tool_input ~mode:Lenient
                ~schema:tool.parameters ~input:arguments
            with
            | Ok args -> args
            | Error _errors ->
              (* In lenient mode, fall back to original arguments if coercion fails *)
              arguments
        in
        tool.handler validated_args
    | None ->
      (* Not found locally, try mounted servers *)
      let rec try_mounted servers =
        match servers with
        | [] ->
          (* Not found anywhere *)
          let available = get_tools t |> Hashtbl.keys |> List.of_list in
          let suggestions = suggest_similar_names name available in
          (match suggestions with
          | [] ->
            raise_s
              [%message
                "Unknown tool"
                  (name : string)
                  ~available:(List.take available 10 : string list)]
          | sugg ->
            let suggestion_str = String.concat ~sep:", " sugg in
            raise_s
              [%message
                "Unknown tool"
                  (name : string)
                  ~did_you_mean:(suggestion_str : string)])
        | mounted :: rest -> (
          match mounted.prefix with
          | Some prefix ->
            let expected_prefix = prefix ^ "_" in
            if String.is_prefix name ~prefix:expected_prefix then
              let stripped_name =
                String.drop_prefix name (String.length expected_prefix)
              in
              try call_tool mounted.server ~name:stripped_name ~arguments
              with _ -> try_mounted rest
            else try_mounted rest
          | None -> (
            try call_tool mounted.server ~name ~arguments
            with _ -> try_mounted rest))
      in
      try_mounted t.mounted_servers

  let rec read_resource t ~uri =
    match Hashtbl.find t.resources uri with
    | Some resource ->
      if not (should_enable_component t ~tags:resource.tags) then
        raise_s [%message "Resource is disabled" (uri : string)]
      else resource.reader ()
    | None ->
      (* Not found locally, try mounted servers *)
      let rec try_mounted servers =
        match servers with
        | [] ->
          let available = get_resources t |> Hashtbl.keys |> List.of_list in
          raise_s
            [%message
              "Unknown resource"
                (uri : string)
                ~available:(List.take available 10 : string list)]
        | mounted :: rest -> (
          match mounted.prefix with
          | Some prefix -> (
            match mounted.mounted_resource_prefix_format with
            | Resource_prefix_format.Protocol -> (
              match String.substr_index uri ~pattern:"://" with
              | Some idx ->
                let proto = String.prefix uri (idx + 3) in
                let path = String.drop_prefix uri (idx + 3) in
                let expected_prefix = prefix ^ "/" in
                if String.is_prefix path ~prefix:expected_prefix then
                  let stripped_path =
                    String.drop_prefix path (String.length expected_prefix)
                  in
                  let stripped_uri = proto ^ stripped_path in
                  try read_resource mounted.server ~uri:stripped_uri
                  with _ -> try_mounted rest
                else try_mounted rest
              | None ->
                let expected_prefix = prefix ^ "/" in
                if String.is_prefix uri ~prefix:expected_prefix then
                  let stripped_uri =
                    String.drop_prefix uri (String.length expected_prefix)
                  in
                  try read_resource mounted.server ~uri:stripped_uri
                  with _ -> try_mounted rest
                else try_mounted rest)
            | Resource_prefix_format.Path ->
              let expected_prefix = prefix ^ "/" in
              if String.is_prefix uri ~prefix:expected_prefix then
                let stripped_uri =
                  String.drop_prefix uri (String.length expected_prefix)
                in
                try read_resource mounted.server ~uri:stripped_uri
                with _ -> try_mounted rest
              else try_mounted rest)
          | None -> (
            try read_resource mounted.server ~uri with _ -> try_mounted rest))
      in
      try_mounted t.mounted_servers

  let rec get_prompt t ~name ~arguments =
    match Hashtbl.find t.prompts name with
    | Some prompt ->
      if not (should_enable_component t ~tags:prompt.tags) then
        raise_s [%message "Prompt is disabled" (name : string)]
      else prompt.render arguments
    | None ->
      (* Not found locally, try mounted servers *)
      let rec try_mounted servers =
        match servers with
        | [] ->
          let available = get_prompts t |> Hashtbl.keys |> List.of_list in
          raise_s
            [%message
              "Unknown prompt"
                (name : string)
                ~available:(List.take available 10 : string list)]
        | mounted :: rest -> (
          match mounted.prefix with
          | Some prefix ->
            let expected_prefix = prefix ^ "_" in
            if String.is_prefix name ~prefix:expected_prefix then
              let stripped_name =
                String.drop_prefix name (String.length expected_prefix)
              in
              try get_prompt mounted.server ~name:stripped_name ~arguments
              with _ -> try_mounted rest
            else try_mounted rest
          | None -> (
            try get_prompt mounted.server ~name ~arguments
            with _ -> try_mounted rest))
      in
      try_mounted t.mounted_servers

  let add_middleware t middleware =
    t.middleware <- t.middleware @ [ middleware ]

  let import_server t ~server ?prefix
      ?(resource_prefix_format = Resource_prefix_format.Protocol) () =
    let mounted =
      {
        server;
        prefix;
        mounted_resource_prefix_format = resource_prefix_format;
      }
    in
    t.mounted_servers <- t.mounted_servers @ [ mounted ]

  (** {1 Protocol Handlers} *)

  (** Protocol handler: List tools through middleware *)
  let _list_tools_mcp t ctx =
    let handler _ctx =
      let tools = list_tools_mcp t in
      return (`Assoc [ ("tools", `List tools) ])
    in
    apply_middleware_chain t.middleware handler ctx

  (** Protocol handler: Call tool through middleware *)
  let _call_tool_mcp t ctx =
    let handler ctx =
      match ctx.Context.params with
      | Some (`Assoc fields) -> (
        match
          ( List.Assoc.find fields ~equal:String.equal "name",
            List.Assoc.find fields ~equal:String.equal "arguments" )
        with
        | Some (`String name), Some arguments ->
          let%bind result = call_tool t ~name ~arguments in
          return (`Assoc [ ("content", result) ])
        | _ -> raise_s [%message "Invalid call_tool params"])
      | _ -> raise_s [%message "Expected object for call_tool params"]
    in
    apply_middleware_chain t.middleware handler ctx

  (** Protocol handler: List resources through middleware *)
  let _list_resources_mcp t ctx =
    let handler _ctx =
      let resources = list_resources_mcp t in
      return (`Assoc [ ("resources", `List resources) ])
    in
    apply_middleware_chain t.middleware handler ctx

  (** Protocol handler: Read resource through middleware *)
  let _read_resource_mcp t ctx =
    let handler ctx =
      match ctx.Context.params with
      | Some (`Assoc fields) -> (
        match List.Assoc.find fields ~equal:String.equal "uri" with
        | Some (`String uri) ->
          let%bind content = read_resource t ~uri in
          return (`Assoc [ ("contents", `List [ `String content ]) ])
        | _ -> raise_s [%message "Invalid read_resource params"])
      | _ -> raise_s [%message "Expected object for read_resource params"]
    in
    apply_middleware_chain t.middleware handler ctx

  (** Protocol handler: List prompts through middleware *)
  let _list_prompts_mcp t ctx =
    let handler _ctx =
      let prompts = list_prompts_mcp t in
      return (`Assoc [ ("prompts", `List prompts) ])
    in
    apply_middleware_chain t.middleware handler ctx

  (** Protocol handler: Get prompt through middleware *)
  let _get_prompt_mcp t ctx =
    let handler ctx =
      match ctx.Context.params with
      | Some (`Assoc fields) -> (
        match
          ( List.Assoc.find fields ~equal:String.equal "name",
            List.Assoc.find fields ~equal:String.equal "arguments" )
        with
        | Some (`String name), arguments_opt ->
          let arguments = Option.value arguments_opt ~default:(`Assoc []) in
          let%bind result = get_prompt t ~name ~arguments in
          return result
        | _ -> raise_s [%message "Invalid get_prompt params"])
      | _ -> raise_s [%message "Expected object for get_prompt params"]
    in
    apply_middleware_chain t.middleware handler ctx

  (** Setup protocol handlers mapping *)
  let setup_handlers t : Protocol.method_map =
    let handlers = Protocol.create_method_map () in
    Hashtbl.set handlers ~key:"tools/list" ~data:(fun ctx ->
        _list_tools_mcp t ctx);
    Hashtbl.set handlers ~key:"tools/call" ~data:(fun ctx ->
        _call_tool_mcp t ctx);
    Hashtbl.set handlers ~key:"resources/list" ~data:(fun ctx ->
        _list_resources_mcp t ctx);
    Hashtbl.set handlers ~key:"resources/read" ~data:(fun ctx ->
        _read_resource_mcp t ctx);
    Hashtbl.set handlers ~key:"prompts/list" ~data:(fun ctx ->
        _list_prompts_mcp t ctx);
    Hashtbl.set handlers ~key:"prompts/get" ~data:(fun ctx ->
        _get_prompt_mcp t ctx);
    handlers

  (** {1 STDIO Transport} *)

  (** Handle a single MCP message and return response *)
  let handle_stdio_message (handlers : Protocol.method_map)
      (message : Yojson.Safe.t) : Yojson.Safe.t Deferred.t =
    (* Extract method and params from JSON-RPC message *)
    match message with
    | `Assoc fields -> (
      match
        ( List.Assoc.find fields ~equal:String.equal "method",
          List.Assoc.find fields ~equal:String.equal "params",
          List.Assoc.find fields ~equal:String.equal "id" )
      with
      | Some (`String method_name), params_opt, id_opt -> (
        (* Look up handler for this method *)
        match Hashtbl.find handlers method_name with
        | Some handler ->
          (* Create context for this request *)
          let ctx =
            Context.create ?method_name:(Some method_name) ?params:params_opt ()
          in
          (* Call handler and wrap response in JSON-RPC format *)
          let%bind result = handler ctx in
          let response_fields =
            [ ("jsonrpc", `String "2.0"); ("result", result) ]
          in
          let response_fields =
            match id_opt with
            | Some id -> ("id", id) :: response_fields
            | None -> response_fields
          in
          return (`Assoc response_fields)
        | None ->
          (* Method not found *)
          let error =
            `Assoc
              [
                ("code", `Int (-32601));
                ("message", `String "Method not found");
                ("data", `Assoc [ ("method", `String method_name) ]);
              ]
          in
          let response_fields =
            [ ("jsonrpc", `String "2.0"); ("error", error) ]
          in
          let response_fields =
            match id_opt with
            | Some id -> ("id", id) :: response_fields
            | None -> response_fields
          in
          return (`Assoc response_fields))
      | _ ->
        (* Invalid request format *)
        let error =
          `Assoc
            [
              ("code", `Int (-32600));
              ("message", `String "Invalid request");
              ("data", message);
            ]
        in
        return (`Assoc [ ("jsonrpc", `String "2.0"); ("error", error) ]))
    | _ ->
      (* Not a JSON object *)
      let error =
        `Assoc
          [
            ("code", `Int (-32600));
            ("message", `String "Invalid request - expected object");
          ]
      in
      return (`Assoc [ ("jsonrpc", `String "2.0"); ("error", error) ])

  (** Handle MCP initialization request *)
  let handle_initialize_message t (id : Yojson.Safe.t) : Yojson.Safe.t =
    let server_info =
      `Assoc
        [
          ("name", `String t.name);
          ( "version",
            match t.version with
            | Some v -> `String v
            | None -> `String "0.1.0" );
        ]
    in
    let capabilities =
      `Assoc
        [
          ( "tools",
            `Assoc
              [ ("listChanged", `Bool true); ("supportsProgress", `Bool false) ]
          );
          ( "resources",
            `Assoc [ ("subscribe", `Bool false); ("listChanged", `Bool true) ]
          );
          ("prompts", `Assoc [ ("listChanged", `Bool true) ]);
        ]
    in
    let result =
      `Assoc
        [
          ("protocolVersion", `String "2024-11-05");
          ("serverInfo", server_info);
          ("capabilities", capabilities);
        ]
    in
    `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

  (** Read a single JSON-RPC message from stdin *)
  let read_stdio_message (reader : Reader.t) : Yojson.Safe.t option Deferred.t =
    match%bind Reader.read_line reader with
    | `Ok line -> (
      try
        let json = Yojson.Safe.from_string line in
        return (Some json)
      with exn ->
        Logging.Global.error
          (sprintf "Failed to parse JSON: %s - %s" line (Exn.to_string exn));
        return None)
    | `Eof -> return None

  (** Write a JSON-RPC message to stdout *)
  let write_stdio_message (writer : Writer.t) (message : Yojson.Safe.t) :
      unit Deferred.t =
    let json_str = Yojson.Safe.to_string message in
    Writer.write_line writer json_str;
    Writer.flushed writer

  let run_stdio_async ?log_level t =
    Option.iter log_level ~f:(fun level ->
        Logging.Global.info (sprintf "Setting log level to %s" level));
    Logging.Global.info "Starting OxFastMCP server with stdio transport";

    (* Setup protocol handlers *)
    let handlers = setup_handlers t in

    (* Get stdin/stdout *)
    let reader = Lazy.force Reader.stdin in
    let writer = Lazy.force Writer.stdout in

    (* Message processing loop *)
    let rec process_loop () =
      match%bind read_stdio_message reader with
      | None ->
        Logging.Global.info "EOF on stdin, shutting down";
        return ()
      | Some message -> (
        (* Check if this is initialize *)
        match message with
        | `Assoc fields -> (
          match
            ( List.Assoc.find fields ~equal:String.equal "method",
              List.Assoc.find fields ~equal:String.equal "id" )
          with
          | Some (`String "initialize"), Some id ->
            (* Handle initialization *)
            let response = handle_initialize_message t id in
            let%bind () = write_stdio_message writer response in
            process_loop ()
          | Some (`String "initialized"), _ ->
            (* Initialized notification - just acknowledge *)
            process_loop ()
          | Some (`String _method_name), _id_opt ->
            (* Handle regular message *)
            let%bind response = handle_stdio_message handlers message in
            let%bind () = write_stdio_message writer response in
            process_loop ()
          | _ ->
            (* Invalid message format *)
            Logging.Global.warning "Received message without method field";
            process_loop ())
        | _ ->
          (* Not an object *)
          Logging.Global.warning "Received non-object message";
          process_loop ())
    in

    process_loop ()

  let run_http_async ?log_level t ~transport ~host ~port =
    let%bind () =
      (match log_level with
       | Some level -> 
         (* Capitalize for Async's Log.Level.of_string - expects "Info", "Debug", "Error" *)
         Log.Global.set_level (Log.Level.of_string (String.capitalize level))
       | None -> ());
      Log.Global.info "Starting HTTP server on %s:%d with %s transport" host port
        (Transport.to_string transport);
      Deferred.unit
    in
    
    (* Create HTTP app based on transport *)
    let app =
      match transport with
      | Transport.Sse ->
        Http.create_sse_app
          ~server:(`Assoc [("name", `String t.name)])
          ~message_path:"/messages"
          ~sse_path:"/sse"
          ()
      | Transport.Streamable_http ->
        Http.create_streamable_http_app
          ~server:(`Assoc [("name", `String t.name)])
          ~streamable_http_path:"/mcp/v1"
          ()
      | _ ->
        raise_s [%message "Invalid HTTP transport" (transport : Transport.t)]
    in
    
    (* Configure server *)
    let config = Http.Server_config.{ host; port; backlog = 10 } in
    
    (* Start HTTP server *)
    Http.start_http_server ~config ~app ()

  let run_async t ?(transport = Transport.Stdio) ?host ?port ?log_level () =
    match transport with
    | Transport.Stdio -> run_stdio_async ?log_level t
    | Transport.Http | Transport.Sse | Transport.Streamable_http ->
      let host = Option.value host ~default:"127.0.0.1" in
      let port = Option.value port ~default:8000 in
      let%bind _server = run_http_async ?log_level t ~transport ~host ~port in
      Deferred.never ()  (* HTTP server runs indefinitely *)
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
