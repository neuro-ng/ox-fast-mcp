(** Elicitation Module for OxFastMCP

    Provides types and utilities for MCP elicitation responses, including schema
    validation for MCP elicitation requirements. *)

open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 Elicitation Result Types} *)

(** Action type for elicitation responses *)
module Action = struct
  type t = Accept | Decline | Cancel [@@deriving sexp, compare, equal]

  let to_string = function
    | Accept -> "accept"
    | Decline -> "decline"
    | Cancel -> "cancel"

  let of_string = function
    | "accept" -> Accept
    | "decline" -> Decline
    | "cancel" -> Cancel
    | s -> raise_s [%message "Unknown elicitation action" (s : string)]

  let yojson_of_t t = `String (to_string t)

  let t_of_yojson = function
    | `String s -> of_string s
    | json ->
      let json_str = Yojson.Safe.to_string json in
      raise_s [%message "Invalid elicitation action" (json_str : string)]
end

(** Result when user accepts the elicitation with data *)
module Accepted_elicitation = struct
  type 'a t = { action : Action.t; [@default Action.Accept] data : 'a }
  [@@deriving sexp]

  let create ~data = { action = Action.Accept; data }

  let yojson_of_t yojson_of_data t =
    `Assoc
      [
        ("action", Action.yojson_of_t t.action); ("data", yojson_of_data t.data);
      ]

  let t_of_yojson data_of_yojson = function
    | `Assoc fields ->
      let action =
        match List.Assoc.find fields ~equal:String.equal "action" with
        | Some json -> Action.t_of_yojson json
        | None -> Action.Accept
      in
      let data =
        match List.Assoc.find fields ~equal:String.equal "data" with
        | Some json -> data_of_yojson json
        | None ->
          raise_s [%message "Missing 'data' field in AcceptedElicitation"]
      in
      { action; data }
    | json ->
      let json_str = Yojson.Safe.to_string json in
      raise_s [%message "Invalid AcceptedElicitation" (json_str : string)]
end

(** Result when user declines the elicitation *)
module Declined_elicitation = struct
  type t = {
    action : Action.t; [@default Action.Decline]
    message : string option;
  }
  [@@deriving sexp]

  let create ?message () = { action = Action.Decline; message }

  let yojson_of_t t =
    let fields = [ ("action", Action.yojson_of_t t.action) ] in
    let fields =
      match t.message with
      | Some msg -> fields @ [ ("message", `String msg) ]
      | None -> fields
    in
    `Assoc fields

  let t_of_yojson = function
    | `Assoc fields ->
      let action =
        match List.Assoc.find fields ~equal:String.equal "action" with
        | Some json -> Action.t_of_yojson json
        | None -> Action.Decline
      in
      let message =
        match List.Assoc.find fields ~equal:String.equal "message" with
        | Some (`String s) -> Some s
        | _ -> None
      in
      { action; message }
    | json ->
      let json_str = Yojson.Safe.to_string json in
      raise_s [%message "Invalid DeclinedElicitation" (json_str : string)]
end

(** Result when user cancels the elicitation *)
module Cancelled_elicitation = struct
  type t = {
    action : Action.t; [@default Action.Cancel]
    reason : string option;
  }
  [@@deriving sexp]

  let create ?reason () = { action = Action.Cancel; reason }

  let yojson_of_t t =
    let fields = [ ("action", Action.yojson_of_t t.action) ] in
    let fields =
      match t.reason with
      | Some r -> fields @ [ ("reason", `String r) ]
      | None -> fields
    in
    `Assoc fields

  let t_of_yojson = function
    | `Assoc fields ->
      let action =
        match List.Assoc.find fields ~equal:String.equal "action" with
        | Some json -> Action.t_of_yojson json
        | None -> Action.Cancel
      in
      let reason =
        match List.Assoc.find fields ~equal:String.equal "reason" with
        | Some (`String s) -> Some s
        | _ -> None
      in
      { action; reason }
    | json ->
      let json_str = Yojson.Safe.to_string json in
      raise_s [%message "Invalid CancelledElicitation" (json_str : string)]
end

(** Container for scalar elicitation types *)
module Scalar_elicitation_type = struct
  type 'a t = { value : 'a } [@@deriving sexp]

  let create ~value = { value }
end

(** Elicitation result combining all possible outcomes *)
type 'a elicitation_result =
  | Accepted of 'a Accepted_elicitation.t
  | Declined of Declined_elicitation.t
  | Cancelled of Cancelled_elicitation.t
[@@deriving sexp]

(** {1 Allowed Types} *)

(** Set of allowed primitive types for elicitation schemas *)
let allowed_types =
  Set.of_list (module String) [ "string"; "number"; "integer"; "boolean" ]

(** {1 Schema Validation} *)

exception Elicitation_schema_error of string
(** Error for invalid elicitation schemas *)

(** Validate that a JSON schema follows MCP elicitation requirements.

    This ensures the schema is compatible with MCP elicitation requirements:
    - Must be an object schema
    - Must only contain primitive field types (string, number, integer, boolean)
    - Must be flat (no nested objects or arrays of objects)
    - Allows const fields (for Literal types) and enum fields (for Enum types)
    - Only primitive types and their nullable variants are allowed

    @param schema The JSON schema to validate
    @raise Elicitation_schema_error
      if the schema doesn't meet MCP elicitation requirements *)
let validate_elicitation_json_schema (schema : Yojson.Safe.t) : unit =
  (* Check that the schema is an object *)
  let schema_type =
    match schema with
    | `Assoc fields -> List.Assoc.find fields ~equal:String.equal "type"
    | _ -> None
  in
  (match schema_type with
  | Some (`String "object") -> ()
  | Some (`String t) ->
    raise
      (Elicitation_schema_error
         (sprintf
            "Elicitation schema must be an object schema, got type '%s'. \
             Elicitation schemas are limited to flat objects with primitive \
             properties only."
            t))
  | _ ->
    raise
      (Elicitation_schema_error
         "Elicitation schema must be an object schema. Elicitation schemas are \
          limited to flat objects with primitive properties only."));

  let properties =
    match schema with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "properties" with
      | Some (`Assoc props) -> props
      | _ -> [])
    | _ -> []
  in

  let defs =
    match schema with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "$defs" with
      | Some (`Assoc d) -> d
      | _ -> [])
    | _ -> []
  in

  List.iter properties ~f:(fun (prop_name, prop_schema) ->
      let prop_fields =
        match prop_schema with
        | `Assoc fields -> fields
        | _ -> []
      in

      (* Extract type from property schema *)
      let prop_type =
        match List.Assoc.find prop_fields ~equal:String.equal "type" with
        | Some (`String t) -> Some t
        | Some (`List types) -> (
          (* Handle nullable types - filter out "null" and get remaining type *)
          let non_null_types =
            List.filter_map types ~f:(function
              | `String "null" -> None
              | `String t -> Some t
              | _ -> None)
          in
          match non_null_types with
          | [ t ] -> Some t
          | _ -> None)
        | _ -> None
      in

      (* Handle const fields (Literal types) *)
      if List.Assoc.mem prop_fields ~equal:String.equal "const" then ()
        (* Handle enum fields (Enum types) *)
      else if List.Assoc.mem prop_fields ~equal:String.equal "enum" then ()
        (* Handle references to definitions *)
      else if List.Assoc.mem prop_fields ~equal:String.equal "$ref" then
        let ref_path =
          match List.Assoc.find prop_fields ~equal:String.equal "$ref" with
          | Some (`String s) -> s
          | _ -> ""
        in
        if String.is_prefix ref_path ~prefix:"#/$defs/" then
          let def_name = String.chop_prefix_exn ref_path ~prefix:"#/$defs/" in
          let ref_def =
            match List.Assoc.find defs ~equal:String.equal def_name with
            | Some (`Assoc d) -> d
            | _ -> []
          in
          (* If the referenced definition has an enum, it's allowed *)
          if List.Assoc.mem ref_def ~equal:String.equal "enum" then ()
          else
            (* If the referenced definition has a type that's allowed, it's
               allowed *)
            let ref_type =
              match List.Assoc.find ref_def ~equal:String.equal "type" with
              | Some (`String t) -> Some t
              | _ -> None
            in
            match ref_type with
            | Some t when Set.mem allowed_types t -> ()
            | _ ->
              raise
                (Elicitation_schema_error
                   (sprintf
                      "Elicitation schema field '%s' contains a reference '%s' \
                       that could not be validated. Only references to enum \
                       types or primitive types are allowed."
                      prop_name ref_path))
        else
          raise
            (Elicitation_schema_error
               (sprintf
                  "Elicitation schema field '%s' contains a reference '%s' \
                   that could not be validated. Only references to enum types \
                   or primitive types are allowed."
                  prop_name ref_path)) (* Handle union types (oneOf/anyOf) *)
      else if
        List.Assoc.mem prop_fields ~equal:String.equal "oneOf"
        || List.Assoc.mem prop_fields ~equal:String.equal "anyOf"
      then
        let one_of =
          match List.Assoc.find prop_fields ~equal:String.equal "oneOf" with
          | Some (`List schemas) -> schemas
          | _ -> []
        in
        let any_of =
          match List.Assoc.find prop_fields ~equal:String.equal "anyOf" with
          | Some (`List schemas) -> schemas
          | _ -> []
        in
        let union_schemas = one_of @ any_of in
        List.iter union_schemas ~f:(fun union_schema ->
            let union_fields =
              match union_schema with
              | `Assoc fields -> fields
              | _ -> []
            in
            (* Allow const and enum in unions *)
            if
              List.Assoc.mem union_fields ~equal:String.equal "const"
              || List.Assoc.mem union_fields ~equal:String.equal "enum"
            then ()
            else
              let union_type =
                match
                  List.Assoc.find union_fields ~equal:String.equal "type"
                with
                | Some (`String t) -> Some t
                | _ -> None
              in
              match union_type with
              | Some t when Set.mem allowed_types t -> ()
              | Some t ->
                raise
                  (Elicitation_schema_error
                     (sprintf
                        "Elicitation schema field '%s' has union type '%s' \
                         which is not a primitive type. Only %s are allowed in \
                         elicitation schemas."
                        prop_name t
                        (String.concat ~sep:", " (Set.to_list allowed_types))))
              | None -> ())
      else
        (* Check if it's a primitive type *)
        match prop_type with
        | Some "object" ->
          raise
            (Elicitation_schema_error
               (sprintf
                  "Elicitation schema field '%s' is an object, but nested \
                   objects are not allowed. Elicitation schemas must be flat \
                   objects with primitive properties only."
                  prop_name))
        | Some "array" ->
          let items_type =
            match List.Assoc.find prop_fields ~equal:String.equal "items" with
            | Some (`Assoc items_fields) -> (
              match List.Assoc.find items_fields ~equal:String.equal "type" with
              | Some (`String t) -> Some t
              | _ -> None)
            | _ -> None
          in
          if Option.equal String.equal items_type (Some "object") then
            raise
              (Elicitation_schema_error
                 (sprintf
                    "Elicitation schema field '%s' is an array of objects, but \
                     arrays of objects are not allowed. Elicitation schemas \
                     must be flat objects with primitive properties only."
                    prop_name))
        | Some t when not (Set.mem allowed_types t) ->
          raise
            (Elicitation_schema_error
               (sprintf
                  "Elicitation schema field '%s' has type '%s' which is not a \
                   primitive type. Only %s are allowed in elicitation schemas."
                  prop_name t
                  (String.concat ~sep:", " (Set.to_list allowed_types))))
        | _ -> ())

(** Get the schema for an elicitation response.

    This applies schema compression and validates the result against MCP
    elicitation requirements.

    @param schema The JSON schema to process
    @return The compressed and validated schema *)
let get_elicitation_schema (schema : Yojson.Safe.t) : Yojson.Safe.t =
  let schema = Json_schema.compress_schema schema in
  validate_elicitation_json_schema schema;
  schema
