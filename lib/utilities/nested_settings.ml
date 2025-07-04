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
    | String of string [@to_yojson fun s -> `String s] [@of_yojson fun j -> match j with `String s -> Ok s | _ -> Error "Expected string"]
    | Int of int [@to_yojson fun i -> `Int i] [@of_yojson fun j -> match j with `Int i -> Ok i | _ -> Error "Expected int"]
    | Float of float [@to_yojson fun f -> `Float f] [@of_yojson fun j -> match j with `Float f -> Ok f | _ -> Error "Expected float"]
    | Bool of bool [@to_yojson fun b -> `Bool b] [@of_yojson fun j -> match j with `Bool b -> Ok b | _ -> Error "Expected bool"]
    | List of t list [@to_yojson fun l -> `List (List.map l ~f:yojson_of_t)] [@of_yojson fun j -> match j with `List l -> List.map_result l ~f:t_of_yojson | _ -> Error "Expected list"]
    | Dict of (string * t) list [@to_yojson fun d -> `Assoc (List.map d ~f:(fun (k, v) -> (k, yojson_of_t v)))] [@of_yojson fun j -> match j with `Assoc d -> List.map_result d ~f:(fun (k, v) -> t_of_yojson v |> Result.map ~f:(fun v -> (k, v))) | _ -> Error "Expected dict"]
    | Null [@to_yojson fun _ -> `Null] [@of_yojson fun j -> match j with `Null -> Ok Null | _ -> Error "Expected null"]
  [@@deriving sexp, yojson]

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

type value =
  | String of string [@key "string"]
  | Int of int [@key "int"]
  | Float of float [@key "float"]
  | Bool of bool [@key "bool"]
  | List of value list [@key "list"]
  | Dict of (string * value) list [@key "dict"]
[@@deriving sexp, compare, yojson]

type t = (string * value) list [@@deriving sexp, compare, yojson]

let _ = fun (_ : value) -> ()
[@@@end]

let _ = fun (_ : t) -> ()
[@@@end]

let rec merge_value v1 v2 =
  match v1, v2 with
  | Dict d1, Dict d2 ->
    Dict (merge_dicts d1 d2)
  | List l1, List l2 ->
    List (l1 @ l2)
  | _, v -> v

and merge_dicts d1 d2 =
  let combined = d1 @ d2 in
  List.fold combined ~init:[] ~f:(fun acc (k, v) ->
    match List.Assoc.find acc k ~equal:String.equal with
    | None -> (k, v) :: acc
    | Some existing ->
      let merged = merge_value existing v in
      List.Assoc.add acc k merged ~equal:String.equal
  )
  |> List.rev

let merge s1 s2 = merge_dicts s1 s2 