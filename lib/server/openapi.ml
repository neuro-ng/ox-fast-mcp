open Core
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe.Util

(* Logging functions *)
let log_debug f = Printf.printf "[DEBUG] %s\n%!" (f (sprintf))
let log_info f = Printf.printf "[INFO] %s\n%!" (f (sprintf))  
let log_warning f = Printf.printf "[WARNING] %s\n%!" (f (sprintf))
let log_error f = Printf.printf "[ERROR] %s\n%!" (f (sprintf))

module MCPType = struct
  type t =
    | Tool
    | Resource
    | ResourceTemplate
    | Exclude
  [@@deriving show, eq]

  let to_string = function
    | Tool -> "TOOL"
    | Resource -> "RESOURCE"
    | ResourceTemplate -> "RESOURCE_TEMPLATE"
    | Exclude -> "EXCLUDE"

  let of_string = function
    | "TOOL" -> Some Tool
    | "RESOURCE" -> Some Resource
    | "RESOURCE_TEMPLATE" -> Some ResourceTemplate
    | "EXCLUDE" -> Some Exclude
    | _ -> None
end

(* Deprecated RouteType for backward compatibility *)
module RouteType = struct
  type t =
    | Tool
    | Resource
    | ResourceTemplate
    | Ignore  (* Deprecated, use Exclude instead *)
  [@@deriving show, eq]

  let to_string = function
    | Tool -> "TOOL"
    | Resource -> "RESOURCE"
    | ResourceTemplate -> "RESOURCE_TEMPLATE"
    | Ignore -> "IGNORE"

  let of_string = function
    | "TOOL" -> Some Tool
    | "RESOURCE" -> Some Resource
    | "RESOURCE_TEMPLATE" -> Some ResourceTemplate
    | "IGNORE" -> Some Ignore
    | _ -> None

  (* Convert RouteType to MCPType *)
  let to_mcp_type = function
    | Tool -> MCPType.Tool
    | Resource -> MCPType.Resource
    | ResourceTemplate -> MCPType.ResourceTemplate
    | Ignore -> MCPType.Exclude
end

type route_map = {
  methods: string list;  (* "*" for all methods *)
  pattern: string;  (* regex pattern *)
  mcp_type: MCPType.t;
  tags: string list;
  mcp_tags: string list;
} [@@deriving fields]

let default_route_mappings = [
  { methods = ["*"];
    pattern = ".*";
    mcp_type = MCPType.Tool;
    tags = [];
    mcp_tags = [] }
]

let slugify text =
  let open Re.Str in
  if String.is_empty text then ""
  else
    text
    |> global_replace (regexp "[ \\-\\.]+") "_"  (* Replace spaces and separators with underscore *)
    |> global_replace (regexp "[^a-zA-Z0-9_]") ""  (* Remove non-alphanumeric except underscore *)
    |> global_replace (regexp "_+") "_"  (* Remove multiple consecutive underscores *)
    |> String.strip ~drop:(fun c -> c = '_')  (* Remove leading/trailing underscores *)

module OpenAPITool = struct
  type t = {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    name: string;
    description: string;
    parameters: Yojson.Safe.t;
    tags: string list;
    timeout: float option;
  }

  let create ~client ~route ~name ~description ~parameters ?(tags=[]) ?timeout () =
    let enhanced_description = format_description_with_responses
      ~parameters:route.parameters
      ?request_body:route.request_body
      ~responses:route.responses
      description
    in
    let cleaned_parameters = clean_schema_for_display parameters in
    { client; route; name; description=enhanced_description; parameters=cleaned_parameters; tags; timeout }

  let run t args =
    let open Lwt.Syntax in
    let open Utilities.Openapi in
    let path = t.route.path in
    
    (* Validate and extract path parameters *)
    let path_params = List.filter_map t.route.parameters ~f:(fun p ->
      match p.location with
      | `Path -> 
          Option.bind (Map.find args p.name) ~f:(fun v -> 
            match validate_parameter_value p v with
            | Ok validated -> Some (p.name, handle_array_parameter p [validated])
            | Error msg -> None  (* Log error and skip invalid parameter *)
          )
      | _ -> None
    ) in

    (* Validate required path params *)
    let required_path_params = List.filter_map t.route.parameters ~f:(fun p ->
      match p.location, p.required with
      | `Path, true -> Some p.name
      | _ -> None
    ) in
    
    let missing_params = List.filter required_path_params ~f:(fun name ->
      not (List.exists path_params ~f:(fun (n,_) -> String.equal n name))
    ) in

    if not (List.is_empty missing_params) then
      Lwt.fail_with (sprintf "Missing required path parameters: %s" 
        (String.concat ~sep:", " missing_params))
    else
      (* Replace path parameters *)
      let path = List.fold path_params ~init:path ~f:(fun path (name, values) ->
        let pattern = sprintf "{%s}" name in
        match values with
        | [value] -> String.substr_replace_all path ~pattern ~with_:value
        | _ -> path  (* Should not happen for path parameters *)
      ) in

      (* Build query parameters with validation *)
      let query_params = List.filter_map t.route.parameters ~f:(fun p ->
        match p.location with
        | `Query ->
            Option.bind (Map.find args p.name) ~f:(fun v ->
              if String.is_empty v then None
              else 
                match validate_parameter_value p v with
                | Ok validated -> 
                    let values = handle_array_parameter p [validated] in
                    Some (p.name, values)
                | Error msg -> None  (* Log error and skip invalid parameter *)
            )
        | _ -> None
      ) |> List.concat_map ~f:(fun (name, values) ->
        List.map values ~f:(fun v -> (name, v))
      ) in

      (* Build headers *)
      let headers = List.filter_map t.route.parameters ~f:(fun p ->
        match p.location with
        | `Header ->
            Option.map (Map.find args p.name) ~f:(fun v ->
              (p.name, v))
        | _ -> None
      ) in

      (* Build request body *)
      let body = match t.route.request_body with
        | Some rb when rb.required ->
            let body_params = Map.filter args ~f:(fun ~key ~data ->
              not (List.exists t.route.parameters ~f:(fun p ->
                String.equal p.name key))
            ) in
            if Map.is_empty body_params then
              None
            else
              Some (Yojson.Safe.to_string (`Assoc (Map.to_alist body_params)))
        | _ -> None
      in

      (* Make request *)
      let uri = Uri.of_string path in
      let uri = Uri.add_query_params uri query_params in
      let headers = Header.of_list headers in

      let* response, body = 
        match t.route.http_method with
        | `GET -> Client.get ~headers uri
        | `POST -> Client.post ~headers ?body uri  
        | `PUT -> Client.put ~headers ?body uri
        | `DELETE -> Client.delete ~headers uri
        | `PATCH -> Client.patch ~headers ?body uri
        | _ -> Lwt.fail_with "Unsupported HTTP method"
      in

      match Response.status response with
      | `OK | `Created | `Accepted ->
          let* body_str = Cohttp_lwt.Body.to_string body in
          Lwt.return [Mcp.Types.ContentBlock.create ~content:body_str ()]
      | status ->
          let status_code = Code.code_of_status status in
          let* error_body = Cohttp_lwt.Body.to_string body in
          Lwt.fail_with (sprintf "HTTP error %d: %s" status_code error_body)
end

module OpenAPIResource = struct
  type t = {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    uri: Uri.t;
    name: string;
    description: string;
    mime_type: string;
    tags: string list;
    timeout: float option;
  }

  let create ~client ~route ~uri ~name ~description ?(mime_type="application/json") ?(tags=[]) ?timeout () =
    let enhanced_description = format_description_with_responses
      ~parameters:route.parameters
      ?request_body:route.request_body
      ~responses:route.responses
      description
    in
    { client; route; uri; name; description=enhanced_description; mime_type; tags; timeout }

  let read t =
    let open Lwt.Syntax in
    let open Utilities.Openapi in
    
    let path = t.route.path in
    let uri_str = Uri.to_string t.uri in

    (* Extract path parameters from URI if present *)
    let path = 
      if String.contains path '{' && String.contains path '}' then
        let parts = String.split uri_str ~on:'/' in
        let param_matches = Re.Str.all_matches (Re.Str.regexp "{\\([^}]+\\)}") path in
        let param_names = List.map param_matches ~f:(fun m ->
          Re.Str.matched_group 1 m
        ) in
        List.foldi param_names ~init:path ~f:(fun i path param_name ->
          let param_value = List.nth_exn parts (List.length parts - 1 - i) in
          let param_info = List.find t.route.parameters ~f:(fun p -> 
            String.equal p.name param_name && p.location = `Path
          ) in
          match param_info with
          | Some p -> 
              let values = handle_array_parameter p [param_value] in
              String.substr_replace_all path ~pattern:(sprintf "{%s}" param_name) 
                ~with_:(String.concat ~sep:"," values)
          | None ->
              String.substr_replace_all path ~pattern:(sprintf "{%s}" param_name) 
                ~with_:param_value
        )
      else
        path
    in

    (* Make request *)
    let uri = Uri.of_string path in
    let* response, body = Client.get uri in

    match Response.status response with
    | `OK ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let content_type = Header.get (Response.headers response) "content-type" in
        
        Lwt.return (
          match content_type with
          | Some ct when String.is_substring ct ~substring:"application/json" ->
              body_str  (* Already JSON string *)
          | Some ct when String.is_substring ct ~substring:"text/" 
                     || String.is_substring ct ~substring:"application/xml" ->
              body_str
          | _ ->
              body_str  (* Return as-is *)
        )
    | status ->
        let status_code = Code.code_of_status status in
        let* error_body = Cohttp_lwt.Body.to_string body in
        Lwt.fail_with (sprintf "HTTP error %d: %s" status_code error_body)
end

module OpenAPIResourceTemplate = struct
  type t = {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    uri_template: string;
    name: string;
    description: string;
    parameters: Yojson.Safe.t;
    tags: string list;
    timeout: float option;
  }

  let create ~client ~route ~uri_template ~name ~description ~parameters ?(tags=[]) ?timeout () =
    let enhanced_description = format_description_with_responses
      ~parameters:route.parameters
      ?request_body:route.request_body
      ~responses:route.responses
      description
    in
    let cleaned_parameters = clean_schema_for_display parameters in
    { client; route; uri_template; name; description=enhanced_description; parameters=cleaned_parameters; tags; timeout }

  let create_resource t ~uri ~params =
    let uri_parts = Map.to_alist params |> List.map ~f:(fun (k,v) ->
      sprintf "%s=%s" k v
    ) in
    
    let description = Option.value t.route.description 
      ~default:(sprintf "Resource for %s" t.route.path)
    in
    
    OpenAPIResource.create
      ~client:t.client
      ~route:t.route
      ~uri:(Uri.of_string uri)
      ~name:(sprintf "%s-%s" t.name (String.concat ~sep:"-" uri_parts))
      ~description
      ~tags:t.tags
      ?timeout:t.timeout
      ()
end

let determine_route_type route mappings =
  let open Utilities.Openapi in
  
  let matches_mapping route mapping =
    (* Check HTTP method *)
    let method_matches = 
      List.mem mapping.methods "*" ~equal:String.equal ||
      List.exists mapping.methods ~f:(fun m -> 
        String.equal m (HttpRoute.get_method route))
    in

    (* Check path pattern *)
    let pattern_matches =
      Re.Str.string_match (Re.Str.regexp mapping.pattern) route.path 0
    in

    (* Check tags *)
    let tags_match =
      List.is_empty mapping.tags ||
      List.for_all mapping.tags ~f:(fun t ->
        List.exists route.tags ~f:(String.equal t))
    in

    method_matches && pattern_matches && tags_match
  in

  (* Find first matching mapping *)
  match List.find mappings ~f:(matches_mapping route) with
  | Some mapping -> mapping
  | None -> List.hd_exn default_route_mappings

let generate_default_name route mcp_names =
  let open Utilities.Openapi in
  
  let name =
    match route.operation_id with
    | Some id ->
        Option.value (Map.find mcp_names id) ~default:(
          match String.split id ~on:'_' with
          | [name; _] -> name  (* Take first part before double underscore *)
          | _ -> id
        )
    | None ->
        Option.value route.summary ~default:(sprintf "%s_%s" 
          (HttpRoute.get_method route) route.path)
  in

  let name = slugify name in
  
  (* Truncate to 56 chars *)
  if String.length name > 56 then
    String.sub name ~pos:0 ~len:56
  else
    name

type component_type = [ `Tool | `Resource | `Template ]

type component_counts = {
  tools: int String.Map.t;
  resources: int String.Map.t;
  templates: int String.Map.t;
} [@@deriving sexp]

let get_unique_name name component_type counts =
  let map = match component_type with
    | `Tool -> counts.tools
    | `Resource -> counts.resources
    | `Template -> counts.templates
  in
  
  let count = Map.find_or_add map name ~default:(fun () -> 0) in
  let new_name = if count = 0 then name else sprintf "%s_%d" name count in
  
  (* Update the count for future collisions *)
  let updated_counts = match component_type with
    | `Tool -> { counts with tools = Map.set counts.tools ~key:name ~data:(count + 1) }
    | `Resource -> { counts with resources = Map.set counts.resources ~key:name ~data:(count + 1) }
    | `Template -> { counts with templates = Map.set counts.templates ~key:name ~data:(count + 1) }
  in
  
  (new_name, updated_counts)

class fast_mcp_openapi
  ~openapi_spec
  ~client
  ?(name="OpenAPI FastMCP")
  ?(route_maps=default_route_mappings)
  ?route_map_fn
  ?mcp_component_fn
  ?(mcp_names=String.Map.empty)
  ?(tags=String.Set.empty)
  ?timeout
  () =
object(self)
  inherit Mcp.Server.t name

  val mutable component_counts = {
    tools = String.Map.empty;
    resources = String.Map.empty;
    templates = String.Map.empty;
  }
  
  method private create_openapi_tool route name tags =
    let open Utilities.Openapi in
    
    let tool_name, updated_counts = get_unique_name name `Tool component_counts in
    component_counts <- updated_counts;
    
    let description = 
      match route.description, route.summary with
      | Some d, _ -> d
      | None, Some s -> s
      | None, None -> sprintf "Executes %s %s" (HttpRoute.get_method route) route.path
    in

    let parameters = combine_schemas route in

    let tool = OpenAPITool.create
      ~client
      ~route
      ~name:tool_name
      ~description
      ~parameters
      ~tags:(Set.to_list tags)
      ?timeout
      ()
    in

    (* Call component customization if provided *)
    Option.iter mcp_component_fn ~f:(fun f -> 
      try
        f route tool;
        log_debug (fun m -> m "Tool %s customized by component_fn" tool_name)
      with e ->
        log_warning (fun m -> m "Error in component_fn for tool %s: %s. Using component as-is." 
          tool_name (Exn.to_string e))
    );

    tool_name, tool

  method private create_openapi_resource route name tags =
    let open Utilities.Openapi in
    
    let resource_name, updated_counts = get_unique_name name `Resource component_counts in
    component_counts <- updated_counts;
    
    let resource_uri = Uri.of_string (sprintf "resource://%s" resource_name) in
    
    let description =
      match route.description, route.summary with 
      | Some d, _ -> d
      | None, Some s -> s
      | None, None -> sprintf "Resource for %s" route.path
    in

    let resource = OpenAPIResource.create
      ~client
      ~route
      ~uri:resource_uri
      ~name:resource_name
      ~description
      ~tags:(Set.to_list tags)
      ?timeout
      ()
    in

    (* Call component customization if provided *)
    Option.iter mcp_component_fn ~f:(fun f -> 
      try
        f route resource;
        log_debug (fun m -> m "Resource %s customized by component_fn" resource_name)
      with e ->
        log_warning (fun m -> m "Error in component_fn for resource %s: %s. Using component as-is." 
          resource_name (Exn.to_string e))
    );

    resource_uri, resource

  method private create_openapi_template route name tags =
    let open Utilities.Openapi in
    
    let template_name, updated_counts = get_unique_name name `Template component_counts in
    component_counts <- updated_counts;
    
    let path_params = List.filter route.parameters ~f:(fun p ->
      match p.location with `Path -> true | _ -> false
    ) in
    
    let uri_template = sprintf "resource://%s/%s" template_name
      (String.concat ~sep:"/" (List.map path_params ~f:(fun p ->
        sprintf "{%s}" p.name))) 
    in

    let description =
      match route.description, route.summary with
      | Some d, _ -> d  
      | None, Some s -> s
      | None, None -> sprintf "Template for %s" route.path
    in

    let parameters = `Assoc [
      "type", `String "object";
      "properties", `Assoc (List.map path_params ~f:(fun p ->
        p.name, Option.value_map p.schema ~default:(`Assoc []) ~f:(fun schema ->
          `Assoc (("description", `String (Option.value p.description ~default:"")) :: 
                  to_assoc schema))));
      "required", `List (List.filter_map path_params ~f:(fun p ->
        if p.required then Some (`String p.name) else None))
    ] in

    let template = OpenAPIResourceTemplate.create
      ~client
      ~route
      ~uri_template
      ~name:template_name
      ~description
      ~parameters
      ~tags:(Set.to_list tags)
      ?timeout
      ()
    in

    (* Call component customization if provided *)
    Option.iter mcp_component_fn ~f:(fun f -> 
      try
        f route template;
        log_debug (fun m -> m "Template %s customized by component_fn" template_name)
      with e ->
        log_warning (fun m -> m "Error in component_fn for template %s: %s. Using component as-is." 
          template_name (Exn.to_string e))
    );

    uri_template, template

  initializer
    let open Utilities.Openapi in
    
    let routes = parse_openapi_to_http_routes openapi_spec in
    
    List.iter routes ~f:(fun route ->
      let route_map = determine_route_type route route_maps in
      
      (* Apply custom route mapping if provided *)
      let mcp_type = match route_map_fn with
        | Some f -> Option.value (f route route_map.mcp_type) ~default:route_map.mcp_type
        | None -> route_map.mcp_type
      in

      let name = generate_default_name route mcp_names in
      let route_tags = Set.union
        (Set.of_list (module String) route.tags)
        (Set.of_list (module String) route_map.mcp_tags)
        |> Set.union tags
      in

      match mcp_type with
      | MCPType.Tool ->
          let tool_name, tool = self#create_openapi_tool route name route_tags in
          (* Register tool directly with the MCP server framework *)
          log_debug (fun m -> m "Registered TOOL: %s (%s %s) with tags: %s" 
            tool_name (HttpRoute.get_method route) route.path 
            (String.concat ~sep:", " route.tags))
          
      | MCPType.Resource ->
          let uri, resource = self#create_openapi_resource route name route_tags in
          (* Register resource directly with the MCP server framework *)
          log_debug (fun m -> m "Registered RESOURCE: %s (%s %s) with tags: %s" 
            (Uri.to_string uri) (HttpRoute.get_method route) route.path 
            (String.concat ~sep:", " route.tags))
          
      | MCPType.ResourceTemplate ->
          let uri_template, template = self#create_openapi_template route name route_tags in
          (* Register resource template directly with the MCP server framework *)
          log_debug (fun m -> m "Registered TEMPLATE: %s (%s %s) with tags: %s" 
            uri_template (HttpRoute.get_method route) route.path 
            (String.concat ~sep:", " route.tags))
          
      | MCPType.Exclude ->
          log_info (fun m -> m "Excluding route: %s %s" (HttpRoute.get_method route) route.path)
    )
end

let format_json_for_description data =
  try
    let json_str = Yojson.Safe.pretty_to_string ~std:true data in
    "```json\n" ^ json_str ^ "\n```"
  with _ ->
    "```\nCould not serialize to JSON\n```"

let format_description_with_responses ?(parameters=[]) ?request_body ~responses base_description =
  let buf = Buffer.create 256 in
  Buffer.add_string buf base_description;

  (* Add parameter information *)
  let path_params = List.filter parameters ~f:(fun p -> p.location = `Path) in
  let query_params = List.filter parameters ~f:(fun p -> p.location = `Query) in

  if not (List.is_empty path_params) then (
    Buffer.add_string buf "\n\n**Path Parameters:**";
    List.iter path_params ~f:(fun param ->
      let required_marker = if param.required then " (Required)" else "" in
      let desc = Option.value param.description ~default:"No description." in
      Buffer.add_string buf (sprintf "\n- **%s**%s: %s" 
        param.name required_marker desc)
    )
  );

  if not (List.is_empty query_params) then (
    Buffer.add_string buf "\n\n**Query Parameters:**";
    List.iter query_params ~f:(fun param ->
      let required_marker = if param.required then " (Required)" else "" in
      let desc = Option.value param.description ~default:"No description." in
      Buffer.add_string buf (sprintf "\n- **%s**%s: %s" 
        param.name required_marker desc)
    )
  );

  (* Add request body information *)
  Option.iter request_body ~f:(fun rb ->
    Option.iter rb.description ~f:(fun desc ->
      Buffer.add_string buf "\n\n**Request Body:**";
      let required_marker = if rb.required then " (Required)" else "" in
      Buffer.add_string buf (sprintf "\n%s%s" desc required_marker);

      (* Add request body property descriptions *)
      match List.Assoc.find rb.content_schema ~equal:String.equal "application/json" with
      | Some (`Assoc fields) -> (
          match List.Assoc.find fields ~equal:String.equal "properties" with
          | Some (`Assoc props) ->
              Buffer.add_string buf "\n\n**Request Properties:**";
              List.iter props ~f:(fun (prop_name, prop_schema) ->
                match prop_schema with
                | `Assoc fields -> (
                    match List.Assoc.find fields ~equal:String.equal "description" with
                    | Some (`String desc) ->
                        let required = match List.Assoc.find fields ~equal:String.equal "required" with
                          | Some (`List reqs) -> 
                              List.exists reqs ~f:(function 
                                | `String s -> String.equal s prop_name 
                                | _ -> false)
                          | _ -> false
                        in
                        let req_mark = if required then " (Required)" else "" in
                        Buffer.add_string buf (sprintf "\n- **%s**%s: %s" 
                          prop_name req_mark desc)
                    | _ -> ()
                  )
                | _ -> ()
              )
          | _ -> ()
        )
      | _ -> ()
    )
  );

  (* Add response information *)
  if not (List.is_empty responses) then (
    Buffer.add_string buf "\n\n**Responses:**";
    List.iter responses ~f:(fun (status, resp) ->
      let desc = Option.value resp.description ~default:"No description." in
      Buffer.add_string buf (sprintf "\n- **%s**: %s" status desc);

      (* Add example response if available *)
      match List.Assoc.find resp.content_schema ~equal:String.equal "application/json" with
      | Some schema ->
          let example = generate_example_from_schema schema in
          Buffer.add_string buf "\n\n  Example response:\n";
          Buffer.add_string buf (format_json_for_description example)
      | None -> ()
    )
  );

  Buffer.contents buf

let handle_array_parameter param value =
  let open Utilities.Openapi in
  match param.schema with
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "type" with
      | Some (`String "array") ->
          (* Check if explode is specified (default is true for query, false for path) *)
          let default_explode = match param.location with
            | `Query -> true
            | _ -> false
          in
          let explode = match List.Assoc.find fields ~equal:String.equal "explode" with
            | Some (`Bool b) -> b
            | _ -> default_explode
          in
          
          if explode then
            (* Return as separate values *)
            value
          else
            (* Join with commas *)
            [String.concat ~sep:"," value]
      | _ -> value
    )
  | _ -> value

let clean_schema_for_display schema =
  let rec clean = function
    | `Assoc fields ->
        let fields_to_remove = [
          "allOf"; "anyOf"; "oneOf"; "not"; "nullable"; "discriminator";
          "readOnly"; "writeOnly"; "deprecated"; "xml"; "externalDocs"
        ] in
        let cleaned_fields = List.filter fields ~f:(fun (k, _) -> 
          not (List.mem fields_to_remove k ~equal:String.equal)
        ) in
        let process_field (key, value) = match key with
          | "properties" -> (key, clean value)
          | "items" -> (key, clean value)
          | "additionalProperties" when value <> `Bool true -> 
              (key, clean value)
          | _ -> (key, value)
        in
        `Assoc (List.map cleaned_fields ~f:process_field)
    | `List items -> `List (List.map items ~f:clean)
    | other -> other
  in
  clean schema 

let validate_schema_format schema =
  match member "format" schema with
  | `String format -> (
    match format with
    | "date-time" | "date" | "time" | "duration"
    | "email" | "idn-email"
    | "hostname" | "idn-hostname"
    | "ipv4" | "ipv6"
    | "uri" | "uri-reference" | "iri" | "iri-reference"
    | "uuid"
    | "json-pointer" | "relative-json-pointer"
    | "regex"
    | "byte" | "binary"
    | "password" -> true
    | _ -> false
  )
  | `Null -> true
  | _ -> false

let validate_schema_enum schema =
  match member "enum" schema with
  | `List values -> 
      let validate_enum_value = function
        | `String _ | `Int _ | `Float _ | `Bool _ -> true
        | _ -> false
      in
      List.for_all values ~f:validate_enum_value
  | `Null -> true
  | _ -> false

let validate_schema schema =
  try
    let version = member "openapi" schema |> to_string in
    let paths = member "paths" schema in
    match paths with
    | `Assoc _ -> 
        if String.is_prefix version ~prefix:"3.0" || String.is_prefix version ~prefix:"3.1" then
          let rec validate_schema_node = function
            | `Assoc fields ->
                List.for_all fields ~f:(fun (_, value) -> validate_schema_node value) &&
                (match member "enum" schema with
                 | `List _ -> validate_schema_enum schema
                 | _ -> true) &&
                (match member "format" schema with
                 | `String _ -> validate_schema_format schema
                 | _ -> true)
            | `List items -> List.for_all items ~f:validate_schema_node
            | _ -> true
          in
          if validate_schema_node schema then
            Ok version
          else
            Error "Invalid schema: invalid enum values or format"
        else
          Error (sprintf "Unsupported OpenAPI version: %s. Only 3.0.x and 3.1.x are supported." version)
    | _ -> Error "Invalid OpenAPI schema: missing or invalid 'paths' object"
  with e ->
    Error (sprintf "Invalid OpenAPI schema: %s" (Printexc.to_string e))

let validate_parameter_value param value =
  let open Utilities.Openapi in
  match param.schema with
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "type" with
      | Some (`String "string") -> 
          (match List.Assoc.find fields ~equal:String.equal "format" with
          | Some (`String format) -> (
              match format with
              | "date-time" -> 
                  (try
                    ignore (Core_unix.strptime value "%Y-%m-%dT%H:%M:%SZ");
                    Ok value
                  with _ -> Error "Invalid date-time format")
              | "date" ->
                  (try
                    ignore (Core_unix.strptime value "%Y-%m-%d");
                    Ok value
                  with _ -> Error "Invalid date format")
              | "email" ->
                  if String.contains value '@' && String.contains value '.' then
                    Ok value
                  else
                    Error "Invalid email format"
              | "uri" ->
                  (try
                    ignore (Uri.of_string value);
                    Ok value
                  with _ -> Error "Invalid URI format")
              | "uuid" ->
                  let uuid_regex = Re.Str.regexp "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" in
                  if Re.Str.string_match uuid_regex (String.lowercase value) 0 then
                    Ok value
                  else
                    Error "Invalid UUID format"
              | _ -> Ok value
            )
          | _ -> Ok value)
      | Some (`String "integer") ->
          (try
            ignore (int_of_string value);
            Ok value
          with _ -> Error "Invalid integer format")
      | Some (`String "number") ->
          (try
            ignore (float_of_string value);
            Ok value
          with _ -> Error "Invalid number format")
      | Some (`String "boolean") ->
          (match String.lowercase value with
          | "true" | "false" -> Ok value
          | _ -> Error "Invalid boolean format")
      | Some (`String "array") ->
          Ok value  (* Array validation is handled by handle_array_parameter *)
      | Some (`String type_) ->
          Error (sprintf "Unsupported parameter type: %s" type_)
      | _ -> Ok value
    )
  | _ -> Ok value 

let rec generate_example_from_schema schema =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "type" with
    | Some (`String "object") ->
        let properties = match List.Assoc.find fields ~equal:String.equal "properties" with
          | Some (`Assoc props) -> props
          | _ -> []
        in
        `Assoc (List.map properties ~f:(fun (name, prop_schema) ->
          (name, generate_example_from_schema prop_schema)
        ))
    | Some (`String "array") ->
        let items = match List.Assoc.find fields ~equal:String.equal "items" with
          | Some items_schema -> generate_example_from_schema items_schema
          | None -> `Null
        in
        `List [items]
    | Some (`String "string") ->
        (match List.Assoc.find fields ~equal:String.equal "format" with
        | Some (`String "date-time") -> `String "2024-03-21T13:45:30Z"
        | Some (`String "date") -> `String "2024-03-21"
        | Some (`String "email") -> `String "user@example.com"
        | Some (`String "uri") -> `String "https://example.com"
        | Some (`String "uuid") -> `String "123e4567-e89b-12d3-a456-426614174000"
        | _ -> 
            match List.Assoc.find fields ~equal:String.equal "example" with
            | Some ex -> ex
            | None -> `String "string")
    | Some (`String "integer") ->
        (match List.Assoc.find fields ~equal:String.equal "example" with
        | Some ex -> ex
        | None -> `Int 42)
    | Some (`String "number") ->
        (match List.Assoc.find fields ~equal:String.equal "example" with
        | Some ex -> ex
        | None -> `Float 3.14)
    | Some (`String "boolean") ->
        (match List.Assoc.find fields ~equal:String.equal "example" with
        | Some ex -> ex
        | None -> `Bool true)
    | _ -> `String "unknown_type")
  | _ -> `Null

let rec replace_ref_with_defs root_schema schema =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "$ref" with
    | Some (`String ref) when String.is_prefix ref ~prefix:"#/" -> (
        let parts = String.split ref ~on:'/' |> List.tl_exn in
        let rec find_def schema parts =
          match parts, schema with
          | [], s -> Some s
          | part :: rest, `Assoc fields ->
              Option.bind (List.Assoc.find fields ~equal:String.equal part) ~f:(fun s ->
                find_def s rest)
          | _, _ -> None
        in
        match find_def root_schema parts with
        | Some def -> replace_ref_with_defs root_schema def
        | None -> schema
      )
    | Some (`String ref) ->
        (* External reference - replace with warning *)
        `Assoc [
          ("type", `String "string");
          ("description", `String (sprintf "External reference: %s" ref))
        ]
    | _ ->
        `Assoc (List.map fields ~f:(fun (k, v) ->
          (k, replace_ref_with_defs root_schema v)))
  )
  | `List items ->
      `List (List.map items ~f:(replace_ref_with_defs root_schema))
  | other -> other 

let combine_schemas route =
  let open Utilities.Openapi in
  combine_schemas route 