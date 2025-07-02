open Core
open Async
open Tool_types

(* Context variable to store current transformed tool *)
let current_tool : transformed option ref = ref None

module Arg_transform = struct
  type t = {
    name : string option;
    description : string option;
    default : Yojson.Safe.t option;
    default_factory : (unit -> Yojson.Safe.t) option;
    type_schema : Yojson.Safe.t option;
    hide : bool;
    required : bool option;
    examples : Yojson.Safe.t option;
  }
  [@@deriving sexp]

  let create
      ?name
      ?description
      ?default
      ?default_factory
      ?type_schema
      ?(hide = false)
      ?required
      ?examples
      () =
    (* Validation *)
    if Option.is_some default && Option.is_some default_factory then
      failwith "Cannot specify both 'default' and 'default_factory' in ArgTransform";
    if Option.is_some default_factory && not hide then
      failwith "default_factory can only be used with hide=True";
    if Option.equal Bool.equal (Some true) required &&
       (Option.is_some default || Option.is_some default_factory) then
      failwith "Required parameters cannot have defaults";
    if hide && Option.equal Bool.equal (Some true) required then
      failwith "Hidden parameters cannot be required";
    if Option.equal Bool.equal (Some false) required then
      failwith "Cannot specify 'required=false'. Set a default value instead";

    { name
    ; description
    ; default
    ; default_factory
    ; type_schema
    ; hide
    ; required
    ; examples
    }
end

let forward arguments =
  match !current_tool with
  | None -> failwith "forward() can only be called within a transformed tool"
  | Some tool -> Tool.run_transformed_forwarding tool arguments

let forward_raw arguments =
  match !current_tool with
  | None -> failwith "forward_raw() can only be called within a transformed tool"
  | Some tool -> Tool.run tool.parent_tool arguments

(** Check if a function accepts kwargs *)
let function_has_kwargs fn =
  let open Template in
  let signature = fn.metadata.signature in
  List.exists signature.parameters ~f:(function KwArgs -> true | _ -> false)

(** Apply transformation to a single parameter *)
let apply_single_transform ~old_name ~old_schema ~transform ~is_required =
  if transform.Arg_transform.hide then None
  else
    let new_name =
      match transform.name with
      | Some name -> name
      | None -> old_name
    in

    let new_schema = Map.copy old_schema in
    
    (* Handle description *)
    (match transform.description with
    | Some desc -> Map.set new_schema ~key:"description" ~data:(`String desc)
    | None -> Map.remove new_schema "description");

    (* Handle required transformation first *)
    let is_required =
      match transform.required with
      | Some true ->
        Map.remove new_schema "default";
        true
      | Some false -> false
      | None -> is_required
    in

    (* Handle default value *)
    (match transform.default with
    | Some default when Option.is_none transform.required ->
      Map.set new_schema ~key:"default" ~data:default;
      false (* Not required if it has a default *)
    | _ -> is_required);

    (* Handle type schema *)
    Option.iter transform.type_schema ~f:(fun type_schema ->
      Map.merge_into ~src:type_schema ~dst:new_schema
        ~f:(fun ~key:_ -> function
          | `Left v | `Right v -> Some v
          | `Both (_, v2) -> Some v2));

    (* Handle examples *)
    Option.iter transform.examples ~f:(fun examples ->
      Map.set new_schema ~key:"examples" ~data:examples);

    Some (new_name, new_schema, is_required)

(** Create schema and forwarding function for transformed tool *)
let create_forwarding_transform ~parent_tool ~transform_args =
  let parent_props = Tool.parameters parent_tool |> Schema.properties in
  let parent_required = Tool.parameters parent_tool |> Schema.required |> Set.of_list (module String) in

  let new_props = String.Map.empty in
  let new_required = Set.empty (module String) in
  let new_to_old = String.Map.empty in
  let hidden_defaults = String.Map.empty in

  (* Process each parameter *)
  Map.iteri parent_props ~f:(fun ~key:old_name ~data:old_schema ->
    let transform =
      match Map.find transform_args old_name with
      | Some t -> t
      | None -> Arg_transform.create ()
    in

    (* Handle hidden parameters *)
    if transform.hide then
      match (transform.default, transform.default_factory) with
      | Some default, None ->
        Map.set hidden_defaults ~key:old_name ~data:(`Default default)
      | None, Some factory ->
        Map.set hidden_defaults ~key:old_name ~data:(`Factory factory)
      | None, None ->
        if Set.mem parent_required old_name then
          failwith (sprintf "Hidden parameter '%s' has no default value" old_name)
      | Some _, Some _ -> assert false (* prevented by Arg_transform.create *)
    else
      (* Process visible parameter *)
      match apply_single_transform ~old_name ~old_schema ~transform
              ~is_required:(Set.mem parent_required old_name) with
      | Some (new_name, new_schema, is_required) ->
        Map.set new_props ~key:new_name ~data:new_schema;
        Map.set new_to_old ~key:new_name ~data:old_name;
        if is_required then Set.add new_required new_name
      | None -> ());

  let schema = Schema.create
    ~type_:"object"
    ~properties:(Map.to_alist new_props)
    ~required:(Set.to_list new_required)
    ~additional_properties:false
    ()
  in

  (* Create forwarding function *)
  let forwarding_fn arguments =
    (* Validate arguments *)
    let valid_args = Map.keys new_props |> Set.of_list (module String) in
    let provided_args = Map.keys arguments |> Set.of_list (module String) in
    let unknown_args = Set.diff provided_args valid_args in
    
    if not (Set.is_empty unknown_args) then
      failwith (sprintf "Got unexpected keyword argument(s): %s"
        (Set.to_list unknown_args |> String.concat ~sep:", "));

    (* Check required arguments *)
    let missing_args = Set.diff new_required provided_args in
    if not (Set.is_empty missing_args) then
      failwith (sprintf "Missing required argument(s): %s"
        (Set.to_list missing_args |> String.concat ~sep:", "));

    (* Map arguments to parent names *)
    let parent_args = String.Map.empty in
    Map.iteri arguments ~f:(fun ~key:new_name ~data:value ->
      let old_name =
        match Map.find new_to_old new_name with
        | Some name -> name
        | None -> new_name
      in
      Map.set parent_args ~key:old_name ~data:value);

    (* Add hidden defaults *)
    Map.iteri hidden_defaults ~f:(fun ~key:old_name ~data ->
      let value = match data with
        | `Default d -> d
        | `Factory f -> f ()
      in
      Map.set parent_args ~key:old_name ~data:value);

    Tool.run parent_tool parent_args
  in

  (schema, forwarding_fn)

let merge_schema_with_precedence ~base_schema ~override_schema ~fn =
  let merged_props = Map.copy (Map.of_alist_exn (module String) base_schema.properties) in
  let merged_required = Set.of_list (module String) base_schema.required in
  let override_props = Map.of_alist_exn (module String) override_schema.properties in
  let override_required = Set.of_list (module String) override_schema.required in

  (* Override properties *)
  Map.iteri override_props ~f:(fun ~key ~data ->
    match Map.find merged_props key with
    | Some base_param ->
      let merged_param = Map.merge_skewed base_param data
        ~combine:(fun ~key:_ _v1 v2 -> v2)
      in
      Map.set merged_props ~key ~data:merged_param
    | None ->
      Map.set merged_props ~key ~data);

  (* Handle required parameters *)
  let final_required = Set.copy override_required in
  Set.iter merged_required ~f:(fun param_name ->
    if not (Map.mem override_props param_name) then
      Set.add final_required param_name
    else if Map.mem override_props param_name &&
            not (Map.mem (Map.find_exn merged_props param_name) "default") then
      if not (Set.mem override_required param_name) then
        Set.add final_required param_name);

  (* Remove parameters with defaults *)
  Map.iteri merged_props ~f:(fun ~key ~data ->
    if Map.mem data "default" then
      Set.remove final_required key);

  (* If function has kwargs, allow extra parameters *)
  let has_kwargs = function_has_kwargs fn in
  if has_kwargs then
    Schema.create
      ~type_:"object"
      ~properties:(Map.to_alist merged_props)
      ~required:(Set.to_list final_required)
      ~additional_properties:true
      ()
  else
    Schema.create
      ~type_:"object"
      ~properties:(Map.to_alist merged_props)
      ~required:(Set.to_list final_required)
      ~additional_properties:false
      ()

let from_tool
    ?name
    ?description
    ?tags
    ?transform_fn
    ?transform_args
    ?annotations
    ?serializer
    ?enabled
    parent_tool =
  let transform_args = Option.value transform_args ~default:String.Map.empty in

  (* Validate transform_args *)
  let parent_params = Tool.parameters parent_tool |> Schema.properties |> Map.keys |> Set.of_list (module String) in
  let unknown_args = Map.keys transform_args |> Set.of_list (module String) |> Set.diff parent_params in
  if not (Set.is_empty unknown_args) then
    failwith (sprintf "Unknown arguments in transform_args: %s. Parent tool has: %s"
      (Set.to_list unknown_args |> String.concat ~sep:", ")
      (Set.to_list parent_params |> String.concat ~sep:", "));

  (* Create forwarding transform *)
  let schema, forwarding_fn = create_forwarding_transform ~parent_tool ~transform_args in

  match transform_fn with
  | None ->
    (* Pure transformation - use forwarding_fn *)
    Tool.create_transformed
      ?name
      ?description
      ?tags
      ?annotations
      ?serializer
      ?enabled
      ~parent_tool
      ~fn:forwarding_fn
      ~forwarding_fn
      ~parameters:schema
      ~transform_args
      ()

  | Some custom_fn ->
    (* Custom function with schema merging *)
    let fn_schema = Template.parameters custom_fn in
    let has_kwargs = function_has_kwargs custom_fn in

    (* Validate function parameters *)
    if not has_kwargs then
      let fn_params = Schema.properties fn_schema |> Map.keys |> Set.of_list (module String) in
      let transformed_params = Schema.properties schema |> Map.keys |> Set.of_list (module String) in
      let missing_params = Set.diff transformed_params fn_params in
      if not (Set.is_empty missing_params) then
        failwith (sprintf "Function missing parameters required after transformation: %s. Function declares: %s"
          (Set.to_list missing_params |> String.concat ~sep:", ")
          (Set.to_list fn_params |> String.concat ~sep:", "));

    (* Merge schemas with precedence *)
    let final_schema = merge_schema_with_precedence ~base_schema:fn_schema ~override_schema:schema ~fn:custom_fn in

    Tool.create_transformed
      ?name
      ?description
      ?tags
      ?annotations
      ?serializer
      ?enabled
      ~parent_tool
      ~fn:custom_fn
      ~forwarding_fn
      ~parameters:final_schema
      ~transform_args
      () 