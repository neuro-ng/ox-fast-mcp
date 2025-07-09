open Core
open Tool_types

(* Context variable to store current transformed tool *)
let current_tool : transformed option ref = ref None

(** JSON manipulation helpers *)
let update_json_field json field value =
  match json with
  | `Assoc fields ->
    let updated_fields =
      List.map fields ~f:(fun (k, v) ->
          if String.equal k field then (k, value) else (k, v))
    in
    if List.exists fields ~f:(fun (k, _) -> String.equal k field) then
      `Assoc updated_fields
    else `Assoc ((field, value) :: fields)
  | _ -> `Assoc [ (field, value) ]

let remove_json_field json field =
  match json with
  | `Assoc fields ->
    `Assoc (List.filter fields ~f:(fun (k, _) -> not (String.equal k field)))
  | _ -> json

let merge_json_objects json1 json2 =
  match (json1, json2) with
  | `Assoc fields1, `Assoc fields2 ->
    let merged =
      List.fold fields1 ~init:fields2 ~f:(fun acc (k, v) ->
          if List.exists fields2 ~f:(fun (k2, _) -> String.equal k2 k) then acc
          else (k, v) :: acc)
    in
    `Assoc merged
  | _, `Assoc _ -> json2
  | `Assoc _, _ -> json1
  | _ -> json2

(** Helper functions for JSON schema manipulation *)
let extract_properties json_schema =
  match json_schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "properties" with
    | Some (`Assoc props) ->
      Map.of_alist_exn (module String)
        (List.map props ~f:(fun (k, v) -> (k, v)))
    | _ -> String.Map.empty)
  | _ -> String.Map.empty

let extract_required json_schema =
  match json_schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "required" with
    | Some (`List req_list) ->
      List.filter_map req_list ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> [])
  | _ -> []

let create_schema ?(type_ = "object") ?(properties = []) ?(required = [])
    ?(additional_properties = false) () =
  let props_json = `Assoc (List.map properties ~f:(fun (k, v) -> (k, v))) in
  let required_json = `List (List.map required ~f:(fun s -> `String s)) in
  let additional_props_json = `Bool additional_properties in

  `Assoc
    [
      ("type", `String type_);
      ("properties", props_json);
      ("required", required_json);
      ("additionalProperties", additional_props_json);
    ]

let forward ctx arguments =
  match !current_tool with
  | None ->
    Lwt.fail
      (Invalid_argument "forward() can only be called within a transformed tool")
  | Some tool -> tool.forwarding_fn ctx arguments

let forward_raw ctx arguments =
  match !current_tool with
  | None ->
    Lwt.fail
      (Invalid_argument
         "forward_raw() can only be called within a transformed tool")
  | Some tool -> tool.parent_tool.handler ctx arguments

(** Check if a function accepts kwargs *)
let function_has_kwargs _fn =
  false (* Placeholder - Template module not available *)

(** Apply transformation to a single parameter *)
let apply_single_transform ~old_name ~old_schema
    ~(transform : Tool_types.Arg_transform.t) ~is_required =
  if transform.hide then None
  else
    let new_name =
      match transform.name with
      | Some name -> name
      | None -> old_name
    in

    let new_schema = ref (old_schema : Yojson.Safe.t) in

    (* Handle description *)
    (match transform.description with
    | Some desc ->
      new_schema := update_json_field !new_schema "description" (`String desc)
    | None -> new_schema := remove_json_field !new_schema "description");

    (* Handle required transformation first *)
    let is_required =
      match transform.required with
      | Some true ->
        new_schema := remove_json_field !new_schema "default";
        true
      | Some false -> false
      | None -> is_required
    in

    (* Handle default value *)
    let is_required =
      match transform.default with
      | Some default when Option.is_none transform.required ->
        new_schema := update_json_field !new_schema "default" default;
        false (* Not required if it has a default *)
      | _ -> is_required
    in

    (* Handle type transformation *)
    (match (transform.type_, transform.type_schema) with
    | Some type_name, None ->
      new_schema := update_json_field !new_schema "type" (`String type_name)
    | None, Some type_schema ->
      new_schema := merge_json_objects !new_schema type_schema
    | Some type_name, Some _ ->
      (* Both specified - type_ takes precedence for simplicity *)
      new_schema := update_json_field !new_schema "type" (`String type_name)
    | None, None -> ());

    (* Handle examples *)
    (match transform.examples with
    | Some examples ->
      new_schema := update_json_field !new_schema "examples" examples
    | None -> ());

    Some (new_name, !new_schema, is_required)

(** Create schema and forwarding function for transformed tool *)
let create_forwarding_transform ~(parent_tool : Tool_types.function_tool)
    ~transform_args =
  let parent_props = parent_tool.base.parameters |> extract_properties in
  let parent_required =
    parent_tool.base.parameters |> extract_required
    |> Set.of_list (module String)
  in

  let new_props = ref String.Map.empty in
  let new_required = ref (Set.empty (module String)) in
  let new_to_old = ref String.Map.empty in
  let hidden_defaults = ref String.Map.empty in

  (* Process each parameter *)
  Map.iteri parent_props ~f:(fun ~key:old_name ~data:old_schema ->
      let transform =
        match Map.find transform_args old_name with
        | Some t -> t
        | None -> Tool_types.Arg_transform.create ()
      in

      (* Handle hidden parameters *)
      if transform.hide then
        match (transform.default, transform.default_factory) with
        | Some default, None ->
          hidden_defaults :=
            Map.set !hidden_defaults ~key:old_name ~data:(`Default default)
        | None, Some factory ->
          hidden_defaults :=
            Map.set !hidden_defaults ~key:old_name ~data:(`Factory factory)
        | None, None ->
          if Set.mem parent_required old_name then
            invalid_arg
              (sprintf "Hidden parameter '%s' has no default value" old_name)
        | Some _, Some _ -> assert false (* prevented by Arg_transform.create *)
      else
        (* Process visible parameter *)
        match
          apply_single_transform ~old_name ~old_schema ~transform
            ~is_required:(Set.mem parent_required old_name)
        with
        | Some (new_name, new_schema, is_required) ->
          new_props := Map.set !new_props ~key:new_name ~data:new_schema;
          new_to_old := Map.set !new_to_old ~key:new_name ~data:old_name;
          if is_required then new_required := Set.add !new_required new_name
        | None -> ());

  let schema =
    create_schema ~type_:"object" ~properties:(Map.to_alist !new_props)
      ~required:(Set.to_list !new_required)
      ~additional_properties:false ()
  in

  (* Create forwarding function *)
  let forwarding_fn ctx arguments =
    (* Convert arguments from JSON to map *)
    let arguments_map =
      match arguments with
      | `Assoc args -> Map.of_alist_exn (module String) args
      | _ -> String.Map.empty
    in

    (* Validate arguments *)
    let valid_args = Map.keys !new_props |> Set.of_list (module String) in
    let provided_args = Map.keys arguments_map |> Set.of_list (module String) in
    let unknown_args = Set.diff provided_args valid_args in

    if not (Set.is_empty unknown_args) then
      invalid_arg
        (sprintf "Got unexpected keyword argument(s): %s"
           (Set.to_list unknown_args |> String.concat ~sep:", "));

    (* Check required arguments *)
    let missing_args = Set.diff !new_required provided_args in
    if not (Set.is_empty missing_args) then
      invalid_arg
        (sprintf "Missing required argument(s): %s"
           (Set.to_list missing_args |> String.concat ~sep:", "));

    (* Map arguments to parent names *)
    let parent_args = ref String.Map.empty in
    Map.iteri arguments_map ~f:(fun ~key:new_name ~data:value ->
        let old_name =
          match Map.find !new_to_old new_name with
          | Some name -> name
          | None -> new_name
        in
        parent_args := Map.set !parent_args ~key:old_name ~data:value);

    (* Add hidden defaults *)
    Map.iteri !hidden_defaults ~f:(fun ~key:old_name ~data ->
        let value =
          match data with
          | `Default d -> d
          | `Factory f -> f ()
        in
        parent_args := Map.set !parent_args ~key:old_name ~data:value);

    parent_tool.handler ctx (`Assoc (Map.to_alist !parent_args))
  in

  (schema, forwarding_fn)

let merge_schema_with_precedence ~base_schema ~override_schema ~fn =
  let merged_props = ref (extract_properties base_schema) in
  let merged_required =
    ref (Set.of_list (module String) (extract_required base_schema))
  in
  let override_props = extract_properties override_schema in
  let override_required =
    Set.of_list (module String) (extract_required override_schema)
  in

  (* Override properties *)
  Map.iteri override_props ~f:(fun ~key ~data ->
      match Map.find !merged_props key with
      | Some base_param ->
        let merged_param = merge_json_objects base_param data in
        merged_props := Map.set !merged_props ~key ~data:merged_param
      | None -> merged_props := Map.set !merged_props ~key ~data);

  (* Handle required parameters *)
  let final_required = ref override_required in
  Set.iter !merged_required ~f:(fun param_name ->
      if not (Map.mem override_props param_name) then
        final_required := Set.add !final_required param_name
      else if
        Map.mem override_props param_name
        && not
             (match Map.find_exn !merged_props param_name with
             | `Assoc fields ->
               List.exists fields ~f:(fun (k, _) -> String.equal k "default")
             | _ -> false)
      then
        if not (Set.mem override_required param_name) then
          final_required := Set.add !final_required param_name);

  (* Remove parameters with defaults *)
  Map.iteri !merged_props ~f:(fun ~key ~data ->
      if
        match data with
        | `Assoc fields ->
          List.exists fields ~f:(fun (k, _) -> String.equal k "default")
        | _ -> false
      then final_required := Set.remove !final_required key);

  (* If function has kwargs, allow extra parameters *)
  let has_kwargs = function_has_kwargs fn in
  if has_kwargs then
    create_schema ~type_:"object"
      ~properties:(Map.to_alist !merged_props)
      ~required:(Set.to_list !final_required)
      ~additional_properties:true ()
  else
    create_schema ~type_:"object"
      ~properties:(Map.to_alist !merged_props)
      ~required:(Set.to_list !final_required)
      ~additional_properties:false ()

let create_from_tool ?name ?description ?tags ?transform_fn
    ?(transform_args : Tool_types.Arg_transform.t String.Map.t =
      String.Map.empty) ?_annotations ?_serializer ?enabled
    (parent_tool : Tool_types.function_tool) =
  (* Validate transform_args *)
  let parent_params =
    parent_tool.base.parameters |> extract_properties |> Map.keys
    |> Set.of_list (module String)
  in
  let transform_keys = Map.keys transform_args |> Set.of_list (module String) in
  let unknown_args = Set.diff transform_keys parent_params in
  if not (Set.is_empty unknown_args) then
    invalid_arg
      (sprintf "Unknown arguments in transform_args: %s. Parent tool has: %s"
         (Set.to_list unknown_args |> String.concat ~sep:", ")
         (Set.to_list parent_params |> String.concat ~sep:", "));

  (* Validate for duplicate names after transformation *)
  let new_names = ref [] in
  Map.iteri transform_args ~f:(fun ~key:old_name ~data:transform ->
      if not transform.hide then
        let new_name =
          match transform.name with
          | Some name -> name
          | None -> old_name
        in
        new_names := new_name :: !new_names);

  (* Check for duplicates *)
  let name_counts =
    List.fold !new_names ~init:String.Map.empty ~f:(fun acc name ->
        Map.update acc name ~f:(function
          | Some count -> count + 1
          | None -> 1))
  in
  let duplicates =
    Map.fold name_counts ~init:[] ~f:(fun ~key ~data acc ->
        if data > 1 then key :: acc else acc)
  in
  if not (List.is_empty duplicates) then
    invalid_arg
      (sprintf "Multiple arguments would be mapped to the same names: %s"
         (String.concat duplicates ~sep:", "));

  (* Create forwarding transform *)
  let schema, forwarding_fn =
    create_forwarding_transform ~parent_tool ~transform_args
  in

  match transform_fn with
  | None ->
    (* Pure transformation - use forwarding_fn *)
    {
      base =
        {
          name = Option.value name ~default:parent_tool.base.name;
          description =
            Option.value description ~default:parent_tool.base.description;
          parameters = schema;
          enabled = Option.value enabled ~default:parent_tool.base.enabled;
          tags = Option.value tags ~default:parent_tool.base.tags;
          annotations = parent_tool.base.annotations;
        };
      parent_tool;
      fn = forwarding_fn;
      forwarding_fn;
      transform_args;
    }
  | Some custom_fn ->
    (* Custom function with schema merging *)
    let fn_schema = `Assoc [] in
    (* Placeholder - Template module not available *)
    let has_kwargs = false in

    (* Placeholder - function_has_kwargs not available *)

    (* Validate function parameters *)
    (if not has_kwargs then
       let fn_params =
         extract_properties fn_schema |> Map.keys |> Set.of_list (module String)
       in
       let transformed_params =
         extract_properties schema |> Map.keys |> Set.of_list (module String)
       in
       let missing_params = Set.diff transformed_params fn_params in
       if not (Set.is_empty missing_params) then
         invalid_arg
           (sprintf
              "Function missing parameters required after transformation: %s. \
               Function declares: %s"
              (Set.to_list missing_params |> String.concat ~sep:", ")
              (Set.to_list fn_params |> String.concat ~sep:", ")));

    (* Merge schemas with precedence *)
    let final_schema =
      merge_schema_with_precedence ~base_schema:fn_schema
        ~override_schema:schema ~fn:custom_fn
    in

    {
      base =
        {
          name = Option.value name ~default:parent_tool.base.name;
          description =
            Option.value description ~default:parent_tool.base.description;
          parameters = final_schema;
          enabled = Option.value enabled ~default:parent_tool.base.enabled;
          tags = Option.value tags ~default:parent_tool.base.tags;
          annotations = parent_tool.base.annotations;
        };
      parent_tool;
      fn = custom_fn;
      forwarding_fn;
      transform_args;
    }
