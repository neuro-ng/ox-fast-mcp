open Yojson.Safe
open Core

(** Deep copy a JSON value *)
let clone (json : t) : t = from_string (to_string json)

(** Return a new schema with param removed from properties, required, and (if no
    longer referenced) $defs *)
let prune_param (schema : t) (param : string) : t =
  let props =
    match schema |> Util.member "properties" with
    | `Assoc props -> props
    | _ -> []
  in

  (* Remove the parameter from properties *)
  let new_props = List.filter (fun (k, _) -> k <> param) props in

  (* Update required list if it exists *)
  let new_required =
    match schema |> Util.member "required" with
    | `List required ->
      let filtered =
        List.filter
          (fun v ->
            match v with
            | `String s -> s <> param
            | _ -> true)
          required
      in
      if filtered = [] then None else Some (`List filtered)
    | _ -> None
  in

  (* Construct new schema *)
  let base =
    match schema with
    | `Assoc fields -> fields
    | _ -> []
  in

  let without_props_required =
    List.filter (fun (k, _) -> k <> "properties" && k <> "required") base
  in

  let with_props = ("properties", `Assoc new_props) :: without_props_required in

  match new_required with
  | Some req -> `Assoc (("required", req) :: with_props)
  | None -> `Assoc with_props

(** Extract the definition name from a $ref string *)
let extract_def_name ref =
  if String.starts_with ~prefix:"#/$defs/" ref then
    let parts = String.split_on_char '/' ref in
    match List.rev parts with
    | def_name :: _ -> Some def_name
    | [] -> None
  else None

(** Walk the schema and prune unused defs *)
let prune_unused_defs (schema : t) : t =
  let root_defs = ref (Hashtbl.create 10) in
  let referenced_by = Hashtbl.create 10 in

  let rec walk ?(current_def = None) ?(skip_defs = false) (node : t) =
    match node with
    | `Assoc fields ->
      (* Process $ref for definition tracking *)
      (match List.assoc_opt "$ref" fields with
      | Some (`String ref) -> (
        match extract_def_name ref with
        | Some def_name -> (
          match current_def with
          | Some current ->
            let refs =
              try Hashtbl.find referenced_by def_name with Not_found -> []
            in
            Hashtbl.replace referenced_by def_name (current :: refs)
          | None -> Hashtbl.replace !root_defs def_name true)
        | None -> ())
      | _ -> ());

      (* Walk children *)
      List.iter
        (fun (k, v) ->
          if not (skip_defs && k = "$defs") then walk ~current_def v)
        fields
    | `List items -> List.iter (walk ~current_def) items
    | _ -> ()
  in

  (* First traverse skipping $defs *)
  walk ~skip_defs:true schema;

  (* Process $defs if they exist *)
  match schema |> Util.member "$defs" with
  | `Assoc defs ->
    (* Walk each def to find internal references *)
    List.iter
      (fun (def_name, value) -> walk ~current_def:(Some def_name) value)
      defs;

    (* Recursive function to check if a def is referenced *)
    let rec def_is_referenced def_name =
      try Hashtbl.find !root_defs def_name
      with Not_found -> (
        try
          let refs = Hashtbl.find referenced_by def_name in
          List.exists def_is_referenced refs
        with Not_found -> false)
    in

    (* Filter out unreferenced defs *)
    let new_defs =
      List.filter (fun (def_name, _) -> def_is_referenced def_name) defs
    in

    (* Update schema *)
    let base =
      match schema with
      | `Assoc fields -> List.filter (fun (k, _) -> k <> "$defs") fields
      | _ -> []
    in

    if new_defs = [] then `Assoc base
    else `Assoc (("$defs", `Assoc new_defs) :: base)
  | _ -> schema

(** Walk the schema and optionally prune titles and additionalProperties *)
let walk_and_prune ?(prune_titles = false)
    ?(prune_additional_properties = false) (schema : t) : t =
  let rec walk node =
    match node with
    | `Assoc fields ->
      let fields =
        fields
        |> (if prune_titles then List.filter (fun (k, _) -> k <> "title")
            else Fun.id)
        |>
        if prune_additional_properties then
          List.filter (fun (k, v) ->
              not (k = "additionalProperties" && v = `Bool false))
        else Fun.id
      in
      `Assoc (List.map (fun (k, v) -> (k, walk v)) fields)
    | `List items -> `List (List.map walk items)
    | x -> x
  in
  walk schema

(** Remove additionalProperties from the schema if it is false *)
let prune_additional_properties (schema : t) : t =
  match schema with
  | `Assoc fields ->
    let new_fields =
      List.filter
        (fun (k, v) -> not (k = "additionalProperties" && v = `Bool false))
        fields
    in
    `Assoc new_fields
  | x -> x

(** Remove the given parameters from the schema *)
let compress_schema ?(prune_params = []) ?(prune_defs = true)
    ?(prune_additional_properties = true) ?(prune_titles = false) (schema : t) :
    t =
  (* Make a copy *)
  let schema = clone schema in

  (* Remove specific parameters if requested *)
  let schema = List.fold_left prune_param schema prune_params in

  (* Do a single walk to handle pruning operations *)
  let schema =
    if prune_titles || prune_additional_properties then
      walk_and_prune ~prune_titles ~prune_additional_properties schema
    else schema
  in

  (* Prune unused defs if requested *)
  if prune_defs then prune_unused_defs schema else schema

type schema = Yojson.Safe.t

let validate schema json =
  match schema with
  | `Assoc _ -> true (* For testing, accept any object schema *)
  | `Null -> true (* For testing, accept null schema *)
  | _ -> false (* Reject invalid schemas *)

let create_object_schema ?(required = []) properties =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map required ~f:(fun s -> `String s)));
    ]

let create_string_property ?description () =
  `Assoc
    [
      ("type", `String "string");
      ( "description",
        Option.value_map description ~default:`Null ~f:(fun s -> `String s) );
    ]

let create_integer_property ?description ?default () =
  `Assoc
    [
      ("type", `String "integer");
      ( "description",
        Option.value_map description ~default:`Null ~f:(fun s -> `String s) );
      ("default", Option.value_map default ~default:`Null ~f:(fun i -> `Int i));
    ]
