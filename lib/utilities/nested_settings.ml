open Core

module Field_error = struct
  type t =
    | Invalid_type of { field : string; expected : string; got : string }
    | Missing_required of string
    | Invalid_value of { field : string; message : string }
    | Nested_error of { field : string; error : t }
  [@@deriving sexp]

  let rec to_string = function
    | Invalid_type { field; expected; got } ->
      sprintf "Field '%s' expected type %s but got %s" field expected got
    | Missing_required field -> sprintf "Missing required field '%s'" field
    | Invalid_value { field; message } ->
      sprintf "Invalid value for field '%s': %s" field message
    | Nested_error { field; error } ->
      sprintf "In field '%s': %s" field (to_string error)
end

module Field_metadata = struct
  type validator = string -> (unit, string) Result.t

  type t = {
    name : string;
    description : string option;
    required : bool;
    validators : validator list;
  }

  let create ?description ?(required = true) ?(validators = []) name =
    { name; description; required; validators }

  let validate t value =
    List.fold_result t.validators ~init:() ~f:(fun () validator ->
      validator value)
end

module Field_value = struct
  type t =
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | List of t list
    | Dict of (string * t) list
    | Null
  [@@deriving sexp, yojson_of, yojson]

  let type_name = function
    | String _ -> "string"
    | Int _ -> "int"
    | Float _ -> "float"
    | Bool _ -> "bool"
    | List _ -> "list"
    | Dict _ -> "dict"
    | Null -> "null"

  let to_string = function
    | String s -> s
    | Int i -> Int.to_string i
    | Float f -> Float.to_string f
    | Bool b -> Bool.to_string b
    | List _ -> "<list>"
    | Dict _ -> "<dict>"
    | Null -> "null"

  let of_string s = String s
  let of_int i = Int i
  let of_float f = Float f
  let of_bool b = Bool b
  let of_list l = List l
  let of_dict d = Dict d

  let get_string = function
    | String s -> Ok s
    | v -> Error (sprintf "Expected string, got %s" (type_name v))

  let get_int = function
    | Int i -> Ok i
    | v -> Error (sprintf "Expected int, got %s" (type_name v))

  let get_float = function
    | Float f -> Ok f
    | v -> Error (sprintf "Expected float, got %s" (type_name v))

  let get_bool = function
    | Bool b -> Ok b
    | v -> Error (sprintf "Expected bool, got %s" (type_name v))

  let get_list = function
    | List l -> Ok l
    | v -> Error (sprintf "Expected list, got %s" (type_name v))

  let get_dict = function
    | Dict d -> Ok d
    | v -> Error (sprintf "Expected dict, got %s" (type_name v))
end

module Nested_settings = struct
  type t = {
    fields : (string * Field_metadata.t) list;
    values : (string * Field_value.t) list;
  }
  [@@deriving sexp]

  let create fields = { fields; values = [] }

  let add_field t name metadata =
    { t with fields = (name, metadata) :: t.fields }

  let set_value t name value =
    { t with values = (name, value) :: List.filter t.values ~f:(fun (n, _) -> n <> name) }

  let get_value t name =
    List.Assoc.find t.values name ~equal:String.equal

  let get_field_metadata t name =
    List.Assoc.find t.fields name ~equal:String.equal

  let validate_field t name value =
    match get_field_metadata t name with
    | None -> Ok () (* Unknown fields are allowed *)
    | Some metadata ->
      Field_metadata.validate metadata (Field_value.to_string value)

  let validate t =
    let validate_required_fields () =
      List.fold_result t.fields ~init:() ~f:(fun () (name, metadata) ->
        if metadata.required && not (List.Assoc.mem t.values name ~equal:String.equal)
        then Error (Field_error.Missing_required name)
        else Ok ())
    in
    let validate_field_values () =
      List.fold_result t.values ~init:() ~f:(fun () (name, value) ->
        match validate_field t name value with
        | Ok () -> Ok ()
        | Error msg ->
          Error (Field_error.Invalid_value { field = name; message = msg }))
    in
    Result.combine_errors_unit
      [ validate_required_fields (); validate_field_values () ]

  let to_yojson t =
    `Assoc
      (List.map t.values ~f:(fun (name, value) ->
         (name, Field_value.yojson_of_t value)))

  let of_yojson json =
    match json with
    | `Assoc fields ->
      Ok
        (List.fold fields ~init:(create []) ~f:(fun acc (name, value) ->
           match Field_value.t_of_yojson value with
           | Ok v -> set_value acc name v
           | Error msg ->
             raise
               (Exceptions.Tool_error.Tool_error
                  (Exceptions.Tool_error.create ~tool_name:"nested_settings"
                     (sprintf "Failed to parse field '%s': %s" name msg)))))
    | _ -> Error "Expected object"
end

let ( |> ) x f = f x

let validate_min_length min s =
  if String.length s >= min then Ok ()
  else Error (sprintf "Length must be at least %d" min)

let validate_max_length max s =
  if String.length s <= max then Ok ()
  else Error (sprintf "Length must be at most %d" max)

let validate_pattern pattern s =
  match Re.Str.string_match (Re.Str.regexp pattern) s 0 with
  | true -> Ok ()
  | false -> Error (sprintf "Must match pattern %s" pattern)

let validate_min min s =
  try
    let n = Float.of_string s in
    if Float.(n >= min) then Ok ()
    else Error (sprintf "Must be at least %f" min)
  with _ -> Error "Not a number"

let validate_max max s =
  try
    let n = Float.of_string s in
    if Float.(n <= max) then Ok ()
    else Error (sprintf "Must be at most %f" max)
  with _ -> Error "Not a number"

let validate_enum values s =
  if List.mem values s ~equal:String.equal then Ok ()
  else
    Error
      (sprintf "Must be one of: %s"
         (String.concat ~sep:", " values)) 