open Core

(** Input validation module for tool parameters
    
    Supports two modes:
    - Strict: Exact type matching required
    - Lenient: Attempts type coercion for compatible types
*)

(** Validation mode *)
type mode =
  | Strict  (** Exact type match required *)
  | Lenient (** Attempt type coercion *)

(** Coercion error detail *)
type coercion_error = {
  field : string;  (** Field path, e.g., "user.age" *)
  expected_type : string;  (** Expected type name *)
  actual_value : Yojson.Safe.t;  (** Actual value received *)
  message : string;  (** Human-readable error message *)
}

(** Coerce a single value to the expected type
    
    @param expected_type The expected JSON type ("integer", "number", "string", "boolean")
    @param value The value to coerce
    @return Ok (coerced value) or Error (error message)
*)
 let coerce_value ~expected_type ~(value : Yojson.Safe.t) : (Yojson.Safe.t, string) Result.t =
  match (expected_type, value) with
  (* Identity coercions - always succeed *)
  | "integer", (`Int _ | `Intlit _) -> Ok value
  | "number", (`Int _ | `Intlit _ | `Float _) -> Ok value
  | "string", `String _ -> Ok value
  | "boolean", `Bool _ -> Ok value
  | "null", `Null -> Ok value
  | "array", `List _ -> Ok value
  | "object", `Assoc _ -> Ok value
  (* String to integer *)
  | "integer", `String s -> (
    match Int.of_string s with
    | i -> Ok (`Int i)
    | exception _ ->
      Error
        (sprintf "Cannot convert string '%s' to integer" s))
  (* String to number/float *)
  | "number", `String s -> (
    match Float.of_string s with
    | f -> Ok (`Float f)
    | exception _ ->
      Error
        (sprintf "Cannot convert string '%s' to number" s))
  (* String to boolean *)
  | "boolean", `String s -> (
    match String.lowercase s with
    | "true" -> Ok (`Bool true)
    | "false" -> Ok (`Bool false)
    | _ ->
      Error
        (sprintf "Cannot convert string '%s' to boolean (use 'true' or 'false')" s))
  (* Type mismatch *)
  | expected, actual ->
    let actual_type =
      match actual with
      | `Int _ | `Intlit _ -> "integer"
      | `Float _ -> "number"
      | `String _ -> "string"
      | `Bool _ -> "boolean"
      | `Null -> "null"
      | `List _ -> "array"
      | `Assoc _ -> "object"
      | `Tuple _ -> "tuple"
      | `Variant _ -> "variant"
    in
    Error
      (sprintf "Type mismatch: expected %s, got %s" expected actual_type)

(** Helper: Extract type from JSON schema property *)
let get_schema_type (schema : Yojson.Safe.t) : string option =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "type" with
    | Some (`String t) -> Some t
    | _ -> None)
  | _ -> None

(** Helper: Get properties from schema *)
let get_schema_properties (schema : Yojson.Safe.t) : (string * Yojson.Safe.t) list =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "properties" with
    | Some (`Assoc props) -> props
    | _ -> [])
  | _ -> []

(** Helper: Get required fields from schema *)
let get_required_fields (schema : Yojson.Safe.t) : String.Set.t =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "required" with
    | Some (`List items) ->
      items
      |> List.filter_map ~f:(fun item ->
          match item with `String s -> Some s | _ -> None)
      |> String.Set.of_list
    | _ -> String.Set.empty)
  | _ -> String.Set.empty

(** Helper: Get enum values from schema property *)
let get_enum_values (schema : Yojson.Safe.t) : Yojson.Safe.t list option =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "enum" with
    | Some (`List values) -> Some values
    | _ -> None)
  | _ -> None

(** Validate value against enum constraint *)
let validate_enum ~field_path ~enum_values ~value
    : (Yojson.Safe.t, coercion_error) Result.t =
  if List.exists enum_values ~f:(fun v -> Yojson.Safe.equal v value) then
    Ok value
  else
    let valid_values =
      enum_values
      |> List.map ~f:Yojson.Safe.to_string
      |> String.concat ~sep:", "
    in
    Error
      {
        field = field_path;
        expected_type = "enum";
        actual_value = value;
        message = sprintf "Value must be one of: %s" valid_values;
      }

(** Validate and coerce a value according to schema in lenient mode *)
let rec validate_value_lenient ~field_path ~schema ~value
    : (Yojson.Safe.t, coercion_error) Result.t =
  (* First check enum constraint if present *)
  match get_enum_values schema with
  | Some enum_vals -> validate_enum ~field_path ~enum_values:enum_vals ~value
  | None -> (
    (* No enum, proceed with type coercion *)
    match get_schema_type schema with
    | Some expected_type -> (
      match coerce_value ~expected_type ~value with
      | Ok v -> Ok v
      | Error msg ->
        Error
          {
            field = field_path;
            expected_type;
            actual_value = value;
            message = msg;
          })
    | None ->
      (* No type specified in schema, accept as-is *)
      Ok value)

(** Validate and coerce an object's fields according to schema *)
and validate_object_lenient ~field_path ~schema ~(obj : (string * Yojson.Safe.t) list)
    : (Yojson.Safe.t, coercion_error list) Result.t =
  let properties = get_schema_properties schema in
  let required_fields = get_required_fields schema in
  
  (* Check for missing required fields *)
  let obj_keys = List.map obj ~f:fst |> String.Set.of_list in
  let missing_required = Set.diff required_fields obj_keys |> Set.to_list in
  
  if not (List.is_empty missing_required) then
    (* Report missing required fields as errors *)
    let errors =
      List.map missing_required ~f:(fun field_name ->
        let full_path =
          if String.is_empty field_path then field_name
          else field_path ^ "." ^ field_name
        in
        {
          field = full_path;
          expected_type = "required";
          actual_value = `Null;
          message = sprintf "Required field '%s' is missing" field_name;
        })
    in
    Error errors
  else if List.is_empty properties then
    (* No properties defined, return object as-is *)
    Ok (`Assoc obj)
  else
    (* Validate each field according to property schema *)
    let results =
      List.map obj ~f:(fun (key, value) ->
          match List.Assoc.find properties ~equal:String.equal key with
          | Some prop_schema ->
            let new_path =
              if String.is_empty field_path then key
              else field_path ^ "." ^ key
            in
            (match validate_value_lenient ~field_path:new_path ~schema:prop_schema ~value with
            | Ok v -> Ok (key, v)
            | Error e -> Error e)
          | None ->
            (* Property not in schema, keep as-is *)
            Ok (key, value))
    in
    (* Collect errors and successes *)
    let errors = List.filter_map results ~f:(fun r -> match r with Error e -> Some e | Ok _ -> None) in
    if List.is_empty errors then
      let validated = List.filter_map results ~f:(fun r -> match r with Ok v -> Some v | Error _ -> None) in
      Ok (`Assoc validated)
    else Error errors

(** Validate and coerce a list's elements according to schema *)
and validate_list_lenient ~field_path ~schema ~(items : Yojson.Safe.t list)
    : (Yojson.Safe.t, coercion_error list) Result.t =
  (* Get items schema if specified *)
  let item_schema =
    match schema with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "items" with
      | Some s -> Some s
      | None -> None)
    | _ -> None
  in
  match item_schema with
  | None ->
    (* No item schema, return list as-is *)
    Ok (`List items)
  | Some item_sch ->
    (* Validate each item *)
    let results =
      List.mapi items ~f:(fun i item ->
          let item_path = sprintf "%s[%d]" field_path i in
          validate_value_lenient ~field_path:item_path ~schema:item_sch ~value:item)
    in
    let errors = List.filter_map results ~f:(fun r -> match r with Error e -> Some e | Ok _ -> None) in
    if List.is_empty errors then
      let validated = List.filter_map results ~f:(fun r -> match r with Ok v -> Some v | Error _ -> None) in
      Ok (`List validated)
    else Error errors

(** Validate and potentially coerce tool input against a JSON schema
    
    @param mode Validation mode (Strict or Lenient)
    @param schema JSON schema for the input (simplified format)
    @param input The input to validate
    @return Ok (validated/coerced input) or Error (list of errors)
*)
let validate_tool_input ~mode ~(schema : Yojson.Safe.t) ~(input : Yojson.Safe.t)
    : (Yojson.Safe.t, coercion_error list) Result.t =
  match mode with
  | Strict ->
    (* In strict mode, we don't coerce types, only validate structure *)
    (* For now, just return input - full strict validation can be added later *)
    let _ = schema in
    Ok input
  | Lenient ->
    (* In lenient mode, attempt to coerce values according to schema *)
    match input with
    | `Assoc fields ->
      (* Input is an object, validate its fields *)
      validate_object_lenient ~field_path:"" ~schema ~obj:fields
    | `List items -> (
      (* Input is a list, validate items *)
      match get_schema_type schema with
      | Some "array" ->
        validate_list_lenient ~field_path:"" ~schema ~items
      | _ ->
        (* Schema doesn't specify array, return as-is *)
        Ok input)
     | other ->
      (* Single value, validate against schema type *)
      (match validate_value_lenient ~field_path:"" ~schema ~value:other with
      | Ok v -> Ok v
      | Error e -> Error [e])

(** Get helpful hint for common errors *)
let get_error_hint (err : coercion_error) : string option =
  match err.expected_type with
  | "integer" -> (
    match err.actual_value with
    | `String s when String.contains s '.' ->
      Some "Hint: Did you mean to use a number (float) instead of integer?"
    | `String _ ->
      Some "Hint: Provide a valid integer like 42, not a string"
    | _ -> None)
  | "boolean" -> (
    match err.actual_value with
    | `String s when String.equal (String.lowercase s) "yes" || String.equal (String.lowercase s) "no" ->
      Some "Hint: Use 'true' or 'false' instead of 'yes'/'no'"
    | `Int 0 | `Int 1 ->
      Some "Hint: Use true or false instead of 0/1"
    | _ -> None)
  | "enum" ->
    Some "Hint: Check the allowed values in the error message above"
  | "required" ->
    Some "Hint: This field must be provided in the request"
  | _ -> None

(** Format a single error with enhanced details *)
let format_single_error (err : coercion_error) : string =
  let field_display =
    if String.is_empty err.field then "input"
    else sprintf "'%s'" err.field
  in
  let base_msg = sprintf "  • Field %s: %s" field_display err.message in
  let hint_msg =
    match get_error_hint err with
    | Some hint -> sprintf "\n    %s" hint
    | None -> ""
  in
  base_msg ^ hint_msg

(** Format a list of coercion errors into a readable message *)
let format_errors errors =
  let header =
    if List.length errors = 1 then
      "Validation error:"
    else
      sprintf "Found %d validation errors:" (List.length errors)
  in
  let formatted_errors =
    errors
    |> List.map ~f:format_single_error
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s" header formatted_errors
