open Core
open Lwt.Syntax
open Types

(** Resource template type *)
type t = {
  uri_template : string;
  name : string;
  mime_type : string;
  description : string option;
  tags : string list;
  enabled : bool;
  parameters : Yojson.Safe.t;  (* JSON schema for parameters *)
} [@@deriving sexp, yojson]

(** Parameter validation error *)
exception Parameter_validation_error of string

(** Function signature error *)
exception Template_function_signature_error of string

(** Lambda function error *)
exception Lambda_function_error of string

(** Function parameter type *)
type parameter_type =
  | Required of string  (* Required named parameter *)
  | Optional of string  (* Optional named parameter *)
  | Context            (* Context parameter *)
  | VarArgs            (* Variable arguments *)
  | KwArgs             (* Keyword arguments *)
[@@deriving sexp, compare]

(** Function signature *)
type template_function_signature = {
  parameters : parameter_type list;
  docstring : string option;
  is_async : bool;
} [@@deriving sexp]

(** Function type *)
type ('ctx, 'params, 'result) function_type =
  | Normal of (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Static of (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Method of (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Class_method of (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature

(** Function metadata *)
type function_metadata = {
  name : string;
  description : string option;
  is_static : bool;
  is_method : bool;
  is_class_method : bool;
  signature : template_function_signature;
} [@@deriving sexp]

(** Get function metadata *)
let get_function_metadata = function
  | Normal (fn, sig_) -> 
    let name = Stdlib.Obj.extension_constructor fn |> Stdlib.Obj.extension_name in
    if String.equal name "<fun>" then
      raise (Lambda_function_error "Lambda functions must be provided a name")
    else
      { name; 
        description = sig_.docstring; 
        is_static = false; 
        is_method = false; 
        is_class_method = false;
        signature = sig_ }
  | Static (fn, sig_) ->
    let name = Stdlib.Obj.extension_constructor fn |> Stdlib.Obj.extension_name in
    { name; 
      description = sig_.docstring; 
      is_static = true; 
      is_method = false; 
      is_class_method = false;
      signature = sig_ }
  | Method (fn, sig_) ->
    let name = Stdlib.Obj.extension_constructor fn |> Stdlib.Obj.extension_name in
    { name; 
      description = sig_.docstring; 
      is_static = false; 
      is_method = true; 
      is_class_method = false;
      signature = sig_ }
  | Class_method (fn, sig_) ->
    let name = Stdlib.Obj.extension_constructor fn |> Stdlib.Obj.extension_name in
    { name; 
      description = sig_.docstring; 
      is_static = false; 
      is_method = false; 
      is_class_method = true;
      signature = sig_ }

(** Extract required and optional parameters from function signature *)
let extract_parameters signature =
  List.fold signature.parameters ~init:(String.Set.empty, String.Set.empty)
    ~f:(fun (required, optional) -> function
      | Required name -> (Set.add required name, optional)
      | Optional name -> (required, Set.add optional name)
      | _ -> (required, optional))

(** Extract parameter names from URI template *)
let extract_uri_parameters template =
  let pattern = Str.regexp "{\\([^}*]+\\)\\*?}" in
  let rec find_all pos acc =
    try
      let _ = Str.search_forward pattern template pos in
      let param = Str.matched_group 1 template in
      find_all (Str.match_end ()) (param :: acc)
    with Not_found -> acc
  in
  find_all 0 []
  |> List.rev
  |> String.Set.of_list

(** Validate function parameters against URI template *)
let validate_function_parameters ~uri_template ~required_params ~optional_params ~signature =
  let uri_params = extract_uri_parameters uri_template in
  
  (* Check if required parameters are a subset of URI parameters *)
  if not (Set.is_subset required_params ~of_:uri_params) then
    let missing = Set.diff required_params uri_params in
    raise (Template_function_signature_error (
      sprintf "Required parameters %s must be present in URI template parameters %s"
        (Set.to_list missing |> String.concat ~sep:", ")
        (Set.to_list uri_params |> String.concat ~sep:", ")
    ));

  (* Check if URI parameters are a subset of all parameters unless we have kwargs *)
  let has_kwargs = List.exists signature.parameters ~f:(function KwArgs -> true | _ -> false) in
  if not has_kwargs then
    let all_params = Set.union required_params optional_params in
    if not (Set.is_subset uri_params ~of_:all_params) then
      let extra = Set.diff uri_params all_params in
      raise (Template_function_signature_error (
        sprintf "URI parameters %s must be a subset of function parameters %s"
          (Set.to_list extra |> String.concat ~sep:", ")
          (Set.to_list all_params |> String.concat ~sep:", ")
      ))

(** Build regex pattern from URI template *)
let build_regex template =
  let parts = Str.split (Str.regexp "\\({[^}]+}\\)") template in
  let pattern = List.fold parts ~init:"" ~f:(fun acc part ->
    if String.is_prefix part ~prefix:"{" && String.is_suffix part ~suffix:"}" then
      let name = String.sub part ~pos:1 ~len:(String.length part - 2) in
      if String.is_suffix name ~suffix:"*" then
        let name = String.sub name ~pos:0 ~len:(String.length name - 1) in
        acc ^ "(?P<" ^ name ^ ">.+)"
      else
        acc ^ "(?P<" ^ name ^ ">[^/]+)"
    else
      acc ^ (Str.quote part)
  ) in
  Str.regexp ("^" ^ pattern ^ "$")

(** Match URI against template and extract parameters *)
let match_uri_template uri uri_template =
  let regex = build_regex uri_template in
  try
    let groups = Str.string_match regex uri 0 in
    if groups then
      let params = Hashtbl.create (module String) in
      let rec extract_groups n =
        try
          let name = Str.group_by_name n in
          let value = Uri.pct_decode (Str.matched_group n uri) in
          Hashtbl.add_exn params ~key:name ~data:value;
          extract_groups (n + 1)
        with Not_found -> ()
      in
      extract_groups 1;
      Some (Hashtbl.to_alist params)
    else
      None
  with _ -> None

(** Convert template to MCP template *)
let to_mcp_template ?overrides t =
  let base = [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
    ("description", match t.description with 
      | Some desc -> `String desc 
      | None -> `Null);
    ("mimeType", `String t.mime_type);
    ("parameters", t.parameters);
  ] in
  let json = match overrides with
    | Some overrides -> 
      List.fold overrides ~init:base ~f:(fun acc (k, v) ->
        (k, v) :: List.filter acc ~f:(fun (k', _) -> String.(k' <> k)))
    | None -> base
  in
  Mcp.Types.Resource_template.of_yojson (`Assoc json)
  |> function
    | Ok r -> r
    | Error msg -> failwith msg

(** Create template from MCP template *)
let of_mcp_template (mcp_template : Mcp.Types.Resource_template.t) =
  {
    uri_template = mcp_template.uri_template;
    name = mcp_template.name;
    mime_type = Option.value mcp_template.mime_type ~default:"text/plain";
    description = mcp_template.description;
    tags = [];
    enabled = true;
    parameters = `Assoc [];  (* Remote templates don't have local parameters *)
  }

(** Get template key *)
let key t = t.uri_template

(** Notify that resource list has changed *)
let notify_resource_list_changed () =
  match%lwt Mcp.Server.Context.get () with
  | None -> Lwt.return_unit
  | Some ctx -> Mcp.Server.Context.queue_resource_list_changed ctx

(** Enable template *)
let enable t =
  let* () = notify_resource_list_changed () in
  Lwt.return { t with enabled = true }

(** Disable template *)
let disable t =
  let* () = notify_resource_list_changed () in
  Lwt.return { t with enabled = false }

(** Function template type *)
type ('ctx, 'a) function_template = {
  base : t;
  fn : ?ctx:'ctx -> (string * string) list -> 'a Lwt.t;
  required_params : String.Set.t;
  optional_params : String.Set.t;
  metadata : function_metadata;
} [@@deriving sexp]

(** Create function template *)
let create_function_template
    ?name ?description ?mime_type ?tags ?(enabled=true)
    ~uri_template
    fn_type =
  (* Get function metadata *)
  let metadata = get_function_metadata fn_type in
  
  (* Extract parameters from function signature *)
  let required_params, optional_params = extract_parameters metadata.signature in

  (* Use provided name or function name *)
  let name = match name with
    | Some n -> n
    | None -> metadata.name
  in

  (* Use provided description or function description *)
  let description = match description with
    | Some d -> Some d
    | None -> metadata.description
  in

  (* Validate function parameters against URI template *)
  validate_function_parameters ~uri_template ~required_params ~optional_params ~signature:metadata.signature;
  
  (* Create parameter schema *)
  let param_schema = 
    let required = Set.to_list required_params in
    let properties = 
      Set.union required_params optional_params
      |> Set.to_list
      |> List.map ~f:(fun param -> 
        (param, `Assoc [
          ("type", `String "string");
          ("description", `String (sprintf "Parameter %s" param));
        ]))
    in
    `Assoc [
      ("type", `String "object");
      ("required", `List (List.map required ~f:(fun p -> `String p)));
      ("properties", `Assoc properties);
    ]
  in

  let base = {
    uri_template;
    name;
    mime_type = Option.value mime_type ~default:"text/plain";
    description;
    tags = Option.value tags ~default:[];
    enabled;
    parameters = param_schema;
  } in

  let fn = match fn_type with
    | Normal (f, _) | Static (f, _) | Method (f, _) | Class_method (f, _) -> f
  in

  { base; fn; required_params; optional_params; metadata }

(** Read function template *)
let read_function_template ?ctx t params =
  if not t.base.enabled then
    Lwt.fail_with "Template is disabled"
  else
    (* Validate required parameters are present *)
    let param_keys = List.map params ~f:fst |> String.Set.of_list in
    if not (Set.is_subset t.required_params ~of_:param_keys) then
      let missing = Set.diff t.required_params param_keys in
      Lwt.fail (Parameter_validation_error (
        sprintf "Missing required parameters: %s"
          (Set.to_list missing |> String.concat ~sep:", ")
      ))
    else
      t.fn ?ctx params

(** Create resource from template *)
let create_resource ?ctx t uri params =
  let* content = read_function_template ?ctx t params in
  let resource = Resource.make_function_resource
    ~uri:(Uri.of_string uri)
    ~name:t.base.name
    ?description:t.base.description
    ~mime_type:t.base.mime_type
    ~tags:t.base.tags
    ~enabled:t.base.enabled
    (fun () -> Lwt.return content)
  in
  Lwt.return resource 