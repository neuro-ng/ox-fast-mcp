open! Core
open! Async
open Yojson.Safe
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type any_json = Yojson.Safe.t
(** Type for an arbitrary JSON value within a schema *)

(* Custom conversion functions for any_json to bypass deriving yojson *)
let any_json_of_yojson (json : Yojson.Safe.t) : any_json = json
let yojson_of_any_json (any : any_json) : Yojson.Safe.t = any

type property = { name : string; value : any_json } [@@deriving yojson]
(** Property type for JSON schema *)

type json_schema = {
  properties : property list option; [@yojson.option]
  required : string list option; [@yojson.option]
  defs : property list option; [@yojson.option] [@key "$defs"]
  additional_properties : bool option; [@yojson.option]
  title : string option; [@yojson.option]
}
[@@deriving yojson]
(** JSON schema type *)

(** Prune a parameter from a schema *)
let prune_param (schema : Yojson.Safe.t) (param : string) : Yojson.Safe.t =
  match schema with
  | `Assoc fields ->
    let props =
      match List.Assoc.find fields ~equal:String.equal "properties" with
      | Some (`Assoc props) ->
        let new_props =
          List.filter props ~f:(fun (k, _) -> not (String.equal k param))
        in
        (* Keep empty properties object rather than removing it entirely *)
        [ ("properties", `Assoc new_props) ]
      | _ -> []
    in
    let required =
      match List.Assoc.find fields ~equal:String.equal "required" with
      | Some (`List reqs) ->
        let new_reqs =
          List.filter reqs ~f:(function
            | `String s -> not (String.equal s param)
            | _ -> true)
        in
        if List.is_empty new_reqs then [] else [ ("required", `List new_reqs) ]
      | _ -> []
    in
    let other_fields =
      List.filter fields ~f:(fun (k, _) ->
          not (List.mem [ "properties"; "required" ] k ~equal:String.equal))
    in
    `Assoc (other_fields @ props @ required)
  | other -> other

(** Walk schema and track def references *)
let find_referenced_defs (schema : Yojson.Safe.t) : string list =
  let root_defs = Hash_set.create (module String) in
  let referenced_by = Hashtbl.create (module String) in

  let rec walk ?current_def ?(skip_defs = false) (node : Yojson.Safe.t) =
    match node with
    | `Assoc fields ->
      (* Process $ref for definition tracking *)
      (match List.Assoc.find fields ~equal:String.equal "$ref" with
      | Some (`String ref) when String.is_prefix ref ~prefix:"#/$defs/" -> (
        let def_name = List.last_exn (String.split ref ~on:'/') in
        match current_def with
        | Some curr -> Hashtbl.add_multi referenced_by ~key:def_name ~data:curr
        | None -> Hash_set.add root_defs def_name)
      | _ -> ());

      (* Walk children *)
      List.iter fields ~f:(fun (k, v) ->
          if not (skip_defs && String.equal k "$defs") then walk ?current_def v)
    | `List items -> List.iter items ~f:(walk ?current_def)
    | _ -> ()
  in

  (* Traverse schema once, skipping defs *)
  walk ~skip_defs:true schema;

  (* Process defs references *)
  (match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "$defs" with
    | Some (`Assoc defs) ->
      List.iter defs ~f:(fun (name, value) -> walk ~current_def:name value)
    | _ -> ())
  | _ -> ());

  (* Return list of referenced defs *)
  let rec is_referenced def_name =
    Hash_set.mem root_defs def_name
    ||
    match Hashtbl.find referenced_by def_name with
    | Some refs -> List.exists refs ~f:is_referenced
    | None -> false
  in

  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "$defs" with
    | Some (`Assoc defs) ->
      List.filter_map defs ~f:(fun (name, _) ->
          if is_referenced name then Some name else None)
    | _ -> [])
  | _ -> []

(** Prune unused definitions from schema *)
let prune_unused_defs (schema : Yojson.Safe.t) : Yojson.Safe.t =
  let referenced = find_referenced_defs schema in
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "$defs" with
    | Some (`Assoc defs) ->
      let new_defs =
        List.filter defs ~f:(fun (name, _) ->
            List.mem referenced name ~equal:String.equal)
      in
      if List.is_empty new_defs then
        `Assoc
          (List.filter fields ~f:(fun (k, _) -> not (String.equal k "$defs")))
      else
        `Assoc
          (List.filter fields ~f:(fun (k, _) -> not (String.equal k "$defs"))
          @ [ ("$defs", `Assoc new_defs) ])
    | _ -> schema)
  | other -> other

(** Walk schema and optionally prune titles and additionalProperties *)
let walk_and_prune ?(prune_titles = false)
    ?(prune_additional_properties = false) (schema : Yojson.Safe.t) :
    Yojson.Safe.t =
  let rec walk (node : Yojson.Safe.t) =
    match node with
    | `Assoc fields ->
      let fields =
        fields
        |> (if prune_titles then
              List.filter ~f:(fun (k, _) -> not (String.equal k "title"))
            else Fn.id)
        |>
        if prune_additional_properties then
          List.filter ~f:(fun (k, v) ->
              not
                (String.equal k "additionalProperties" && equal v (`Bool false)))
        else Fn.id
      in
      `Assoc (List.map fields ~f:(fun (k, v) -> (k, walk v)))
    | `List items -> `List (List.map items ~f:walk)
    | other -> other
  in
  walk schema

(** Compress schema by pruning specified elements *)
let compress_schema ?(prune_params = []) ?(prune_defs = true)
    ?(prune_additional_properties = true) ?(prune_titles = false)
    (schema : Yojson.Safe.t) : Yojson.Safe.t =
  let schema = schema in

  (* Make copy in real implementation *)

  (* Remove specific parameters *)
  let schema = List.fold prune_params ~init:schema ~f:prune_param in

  (* Do single walk for pruning operations *)
  let schema =
    if prune_titles || prune_additional_properties then
      walk_and_prune ~prune_titles ~prune_additional_properties schema
    else schema
  in

  (* Prune unused definitions *)
  let schema = if prune_defs then prune_unused_defs schema else schema in

  schema
