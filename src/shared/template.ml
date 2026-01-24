open Core
open Async

(** URI template matching for dynamic resources
    
    Supports RFC 6570 URI templates:
    - Path parameters: {var}, {var*} (wildcard)
    - Query parameters: {?var1,var2}
*)

(** Extract query parameter names from RFC 6570 [{?param1,param2}] syntax *)
let extract_query_params uri_template =
  let re = Re.Perl.compile_pat "\\{\\?([^}]+)\\}" in
  match Re.exec_opt re uri_template with
  | None -> String.Set.empty
  | Some group -> (
    match Re.Group.get_opt group 1 with
    | None -> String.Set.empty
    | Some params_str ->
      params_str |> String.split ~on:',' |> List.map ~f:String.strip
      |> String.Set.of_list)

(** Build regex pattern for URI template and return parameter names in order *)
let build_regex_with_names template =
  (* Remove query parameter syntax for path matching *)
  let template_without_query =
    Re.replace_string (Re.Perl.compile_pat "\\{\\?[^}]+\\}") ~by:"" template
  in

  (* Extract parameter names *)
  let param_names = ref [] in

  (* Split on parameter placeholders *)
  let re_split = Re.Perl.compile_pat "(\\{[^}]+\\})" in
  let parts = Re.split_full re_split template_without_query in

  let pattern_parts =
    List.map parts ~f:(fun part ->
        match part with
        | `Delim group ->
          let param_str = Re.Group.get group 0 in
          (* Remove { and } *)
          let name =
            String.sub param_str ~pos:1 ~len:(String.length param_str - 2)
          in
          let clean_name, is_wildcard =
            if String.is_suffix name ~suffix:"*" then
              (String.drop_suffix name 1, true)
            else (name, false)
          in
          param_names := clean_name :: !param_names;
          if is_wildcard then (* Wildcard: match multiple segments *)
            "(.+)"
          else (* Simple: match single segment *)
            "([^/]+)"
        | `Text text ->
          (* Escape special regex characters *)
          Str.quote text)
  in

  let pattern = String.concat pattern_parts in
  let regex = Re.Perl.compile_pat (sprintf "^%s$" pattern) in
  (regex, List.rev !param_names)

(** Match URI against template and extract both path and query parameters *)
let match_uri_template uri uri_template =
  (* Split URI into path and query parts *)
  let uri_path, query_string =
    match String.lsplit2 uri ~on:'?' with
    | Some (path, query) -> (path, Some query)
    | None -> (uri, None)
  in

  (* Match path parameters *)
  let regex, param_names = build_regex_with_names uri_template in
  match Re.exec_opt regex uri_path with
  | None -> None
  | Some groups ->
    (* Extract parameters by index and match to names *)
    let params =
      List.foldi param_names ~init:String.Map.empty ~f:(fun idx acc name ->
          try
            (* Group 0 is the whole match, groups 1+ are captures *)
            let value = Re.Group.get groups (idx + 1) in
            let decoded = Uri.pct_decode value in
            Map.set acc ~key:name ~data:decoded
          with _ -> acc)
    in

    (* Extract query parameters if present *)
    let params_with_query =
      match query_string with
      | None -> params
      | Some query ->
        let query_param_names = extract_query_params uri_template in
        let parsed_query = Uri.query_of_encoded query in

        Set.fold query_param_names ~init:params ~f:(fun acc name ->
            match List.Assoc.find parsed_query ~equal:String.equal name with
            | Some (first_value :: _) ->
              (* Take first value if multiple provided *)
              Map.set acc ~key:name ~data:first_value
            | _ -> acc)
    in

    Some params_with_query

type template_config = {
  uri_template : string;
  name : string;
  description : string option;
  mime_type : string; [@default "text/plain"]
}
[@@deriving sexp_of]
(** Resource template configuration *)

(** Resource template for dynamic resource creation *)
module ResourceTemplate = struct
  type 'a t = {
    config : template_config;
    read_fn : string String.Map.t -> 'a Deferred.t;
    list_fn : (string String.Map.t -> Mcp.Types.resource list Deferred.t) option;
  }

  let create ~uri_template ~name ?description ?(mime_type = "text/plain")
      ~read_fn ?list_fn () =
    {
      config = { uri_template; name; description; mime_type };
      read_fn;
      list_fn;
    }

  (** Check if URI matches template and extract parameters *)
  let matches t uri = match_uri_template uri t.config.uri_template

  (** Read resource content using the template's read function *)
  let read t uri =
    match matches t uri with
    | None ->
      Deferred.Or_error.error_string
        (sprintf "URI %s does not match template %s" uri t.config.uri_template)
    | Some params -> t.read_fn params >>| Result.return

  (** List resources using the template's list function *)
  let list t uri =
    match t.list_fn with
    | None -> Deferred.Or_error.error_string "Template does not support listing"
    | Some list_fn -> (
      match matches t uri with
      | None ->
        Deferred.Or_error.error_string
          (sprintf "URI %s does not match template %s" uri t.config.uri_template)
      | Some params -> list_fn params >>| Result.return)

  (** Convert to MCP resource template type *)
  let to_mcp t =
    Mcp.Types.
      {
        uri_template = t.config.uri_template;
        description = t.config.description;
        mime_type = Some t.config.mime_type;
        annotations = None;
        icons = None;
        meta = None;
        base_metadata = { name = t.config.name; title = None };
      }
end

(** Create a text resource template *)
let text_resource_template ~uri_template ~name ?description
    ~(read_fn : string String.Map.t -> string Deferred.t) ?list_fn () =
  let read_fn params =
    let%map content = read_fn params in
    Mcp.Types.
      {
        text = content;
        resource_contents =
          { uri = uri_template; mime_type = Some "text/plain"; meta = None };
      }
  in
  ResourceTemplate.create ~uri_template ~name ?description
    ~mime_type:"text/plain" ~read_fn ?list_fn ()

(** Create a binary resource template *)
let binary_resource_template ~uri_template ~name ?description
    ?(mime_type = "application/octet-stream")
    ~(read_fn : string String.Map.t -> string Deferred.t) ?list_fn () =
  let read_fn params =
    let%map content = read_fn params in
    Mcp.Types.
      {
        blob = content;
        resource_contents =
          { uri = uri_template; mime_type = Some mime_type; meta = None };
      }
  in
  ResourceTemplate.create ~uri_template ~name ?description ~mime_type ~read_fn
    ?list_fn ()
