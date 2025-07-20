open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Component validation errors *)
module Error = struct
  type t =
    | Invalid_name of string
    | Invalid_tags of string list
    | Invalid_json of string
    | Invalid_version of string
    | Invalid_extra of string
    | Invalid_type of string
    | Validation_error of string
  [@@deriving sexp, compare, equal, hash]

  exception Component_error of t [@@deriving sexp]

  let to_string = function
    | Invalid_name msg -> sprintf "Invalid name: %s" msg
    | Invalid_tags tags ->
      sprintf "Invalid tags: [%s]" (String.concat ~sep:", " tags)
    | Invalid_json msg -> sprintf "Invalid JSON: %s" msg
    | Invalid_version msg -> sprintf "Invalid version: %s" msg
    | Invalid_extra msg -> sprintf "Invalid extra data: %s" msg
    | Invalid_type msg -> sprintf "Invalid type: %s" msg
    | Validation_error msg -> sprintf "Validation error: %s" msg

  let to_error t = Error.of_string (to_string t)
end

type 'a t = {
  name : string; [@key "name"]  (** The name of the component *)
  description : string option; [@key "description"]
      (** The description of the component *)
  tags : string list; [@key "tags"]  (** Tags for the component *)
  enabled : bool; [@key "enabled"] [@default true]
      (** Whether the component is enabled *)
  key : string option; [@key "_key"] [@default None]
      (** Internal key for bookkeeping *)
  extra : 'a; [@key "extra"]  (** Extra data specific to each component type *)
  version : int option; [@key "version"] [@default None]
      (** Component version for migrations *)
}
[@@deriving fields, sexp, compare, equal, hash]
(** Base type for FastMCP components *)

let t_of_yojson yojson_of_extra json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = json |> member "description" |> to_string_option in
    let tags = json |> member "tags" |> to_list |> List.map ~f:to_string in
    let enabled =
      json |> member "enabled" |> to_bool_option |> Option.value ~default:true
    in
    let key = json |> member "_key" |> to_string_option in
    let extra = json |> member "extra" |> yojson_of_extra in
    let version = json |> member "version" |> to_int_option in
    Ok { name; description; tags; enabled; key; extra; version }
  with
  | Type_error (msg, _) -> Error msg
  | Failure msg -> Error msg
  | _ -> Error "Invalid JSON format"

let yojson_of_t yojson_to_extra t =
  `Assoc
    [
      ("name", `String t.name);
      ( "description",
        match t.description with
        | None -> `Null
        | Some s -> `String s );
      ("tags", `List (List.map t.tags ~f:(fun s -> `String s)));
      ("enabled", `Bool t.enabled);
      ( "_key",
        match t.key with
        | None -> `Null
        | Some s -> `String s );
      ("extra", yojson_to_extra t.extra);
      ( "version",
        match t.version with
        | None -> `Null
        | Some i -> `Int i );
    ]

(** Component validation rules *)
module Validation = struct
  let validate_name name =
    if String.is_empty name then
      Error (Error.Invalid_name "Name cannot be empty")
    else if String.exists name ~f:Char.is_whitespace then
      Error (Error.Invalid_name "Name cannot contain whitespace")
    else Ok ()

  let validate_tags tags =
    let invalid_tags =
      List.filter tags ~f:(fun tag ->
          String.is_empty tag || String.exists tag ~f:Char.is_whitespace)
    in
    if List.is_empty invalid_tags then Ok ()
    else Error (Error.Invalid_tags invalid_tags)

  let validate_version = function
    | None -> Ok ()
    | Some v when v < 0 ->
      Error (Error.Invalid_version "Version cannot be negative")
    | Some _ -> Ok ()

  let validate_extra = function
    | None -> Error (Error.Invalid_extra "Extra data cannot be None")
    | Some _ -> Ok ()

  let validate_all t =
    Result.all_unit
      [
        validate_name t.name;
        validate_tags t.tags;
        validate_version t.version;
        validate_extra (Some t.extra);
      ]
end

(** Convert a list to a set, defaulting to an empty list if None *)
let convert_set_default_none (maybe_set : 'a list option) : 'a list =
  Option.value maybe_set ~default:[]
  |> List.dedup_and_sort ~compare:Poly.compare

(** Convert Result to Or_error *)
let to_or_error = function
  | Ok x -> Ok x
  | Error e -> Error (Error.to_error e)

(** Create a new FastMCP component with validation *)
let create ?(description = None) ?(tags = []) ?(enabled = true) ?(key = None)
    ?(version = None) ~name ~extra () =
  let t =
    {
      name;
      description;
      tags = convert_set_default_none (Some tags);
      enabled;
      key;
      extra;
      version;
    }
  in
  match Validation.validate_all t with
  | Ok () -> Ok t
  | Error e -> Error e

(** Get the component's key, falling back to name if not set *)
let get_key t = Option.value t.key ~default:t.name

(** Create a copy of the component with a new key *)
let with_key t new_key = { t with key = Some new_key }

(** Enable the component *)
let enable t = { t with enabled = true }

(** Disable the component *)
let disable t = { t with enabled = false }

(** Compare two components for equality *)
let equal ?(compare_extra = Poly.equal) a b =
  [%compare.equal: string] a.name b.name
  && [%compare.equal: string option] a.description b.description
  && List.equal String.equal
       (List.sort ~compare:String.compare a.tags)
       (List.sort ~compare:String.compare b.tags)
  && [%compare.equal: bool] a.enabled b.enabled
  && compare_extra a.extra b.extra

(** Convert component to string representation *)
let to_string t =
  let tags_str = String.concat ~sep:", " t.tags in
  let desc_str = Option.value t.description ~default:"None" in
  sprintf "FastMCPComponent(name=%s, description=%s, tags=[%s], enabled=%b)"
    t.name desc_str tags_str t.enabled

(** Create a copy of the component with updated fields *)
let copy ?name ?description ?tags ?enabled ?key ?extra ?version t =
  let t =
    {
      name = Option.value name ~default:t.name;
      description = Option.value description ~default:t.description;
      tags =
        Option.value tags ~default:t.tags
        |> Option.some |> convert_set_default_none;
      enabled = Option.value enabled ~default:t.enabled;
      key = Option.value key ~default:t.key;
      extra = Option.value extra ~default:t.extra;
      version = Option.value version ~default:t.version;
    }
  in
  match Validation.validate_all t with
  | Ok () -> Ok t
  | Error e -> Error e

(** JSON serialization with validation *)
let to_yojson yojson_of_extra t =
  match Validation.validate_all t with
  | Ok () -> Ok (yojson_of_t yojson_of_extra t)
  | Error e -> Error e

(** JSON deserialization with validation *)
let of_yojson yojson_to_extra json =
  match
    t_of_yojson
      (fun j ->
        match yojson_to_extra j with
        | Ok x -> x
        | Error e -> raise_s [%message e])
      json
  with
  | Error msg -> Error msg
  | Ok t -> (
    match Validation.validate_all t with
    | Ok () -> Ok t
    | Error e -> Error (Error.to_string e))

(** Version-related functions *)
let get_version t = t.version

let set_version t v =
  let t = { t with version = Some v } in
  match Validation.validate_all t with
  | Ok () -> Ok t
  | Error e -> Error e

let clear_version t =
  let t = { t with version = None } in
  match Validation.validate_all t with
  | Ok () -> Ok t
  | Error e -> Error e

(** Comparison functions *)
let compare_by_name a b = String.compare a.name b.name

let compare_by_version a b = Option.compare Int.compare a.version b.version
let compare_by_key a b = Option.compare String.compare a.key b.key

(** Hashing functions *)
let hash ?(hash_extra = Hashtbl.hash) t =
  [%hash:
    string * string option * string list * bool * string option * int option]
    (t.name, t.description, t.tags, t.enabled, t.key, t.version)
  + hash_extra t.extra

(** Type conversion functions *)
let to_list ts = List.sort ts ~compare:compare_by_name

let of_list ts =
  List.sort ts ~compare:compare_by_name
  |> List.dedup_and_sort ~compare:compare_by_name

module For_testing = struct
  let%test_unit "create with defaults" =
    let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
    [%test_result: string] t.name ~expect:"test";
    [%test_result: string option] t.description ~expect:None;
    [%test_result: string list] t.tags ~expect:[];
    [%test_result: bool] t.enabled ~expect:true;
    [%test_result: string option] t.key ~expect:None

  let%test_unit "get_key fallback" =
    let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
    [%test_result: string] (get_key t) ~expect:"test";
    let t_with_key = with_key t "custom_key" in
    [%test_result: string] (get_key t_with_key) ~expect:"custom_key"

  let%test_unit "enable/disable" =
    let t =
      create ~name:"test" ~enabled:false ~extra:() () |> to_or_error |> ok_exn
    in
    [%test_result: bool] t.enabled ~expect:false;
    let t = enable t in
    [%test_result: bool] t.enabled ~expect:true;
    let t = disable t in
    [%test_result: bool] t.enabled ~expect:false

  let%test_unit "equal comparison" =
    let t1 =
      create ~name:"test" ~description:(Some "desc") ~tags:[ "a"; "b" ] ~extra:1
        ()
      |> to_or_error |> ok_exn
    in
    let t2 =
      create ~name:"test" ~description:(Some "desc") ~tags:[ "b"; "a" ] ~extra:1
        ()
      |> to_or_error |> ok_exn
    in
    let t3 =
      create ~name:"test2" ~description:(Some "desc") ~tags:[ "a"; "b" ]
        ~extra:1 ()
      |> to_or_error |> ok_exn
    in
    assert (equal t1 t2);
    assert (not (equal t1 t3))

  let%test_unit "validation - invalid name" =
    let assert_raises_invalid_name f =
      match f () with
      | Ok _ -> failwith "Expected Invalid_name error"
      | Error (Error.Invalid_name _) -> ()
      | Error _ -> failwith "Expected Invalid_name error"
    in
    assert_raises_invalid_name (fun () -> create ~name:"" ~extra:() ());
    assert_raises_invalid_name (fun () ->
        create ~name:"invalid name" ~extra:() ())

  let%test_unit "validation - invalid tags" =
    let assert_raises_invalid_tags f =
      match f () with
      | Ok _ -> failwith "Expected Invalid_tags error"
      | Error (Error.Invalid_tags _) -> ()
      | Error _ -> failwith "Expected Invalid_tags error"
    in
    assert_raises_invalid_tags (fun () ->
        create ~name:"test" ~tags:[ "" ] ~extra:() ());
    assert_raises_invalid_tags (fun () ->
        create ~name:"test" ~tags:[ "invalid tag" ] ~extra:() ())

  let%test_unit "validation - invalid version" =
    let assert_raises_invalid_version f =
      match f () with
      | Ok _ -> failwith "Expected Invalid_version error"
      | Error (Error.Invalid_version _) -> ()
      | Error _ -> failwith "Expected Invalid_version error"
    in
    assert_raises_invalid_version (fun () ->
        create ~name:"test" ~version:(Some (-1)) ~extra:() ())

  let%test_unit "copy with validation" =
    let t = create ~name:"test" ~extra:1 () |> to_or_error |> ok_exn in
    let t2 = copy ~name:"test2" t |> to_or_error |> ok_exn in
    [%test_result: string] t2.name ~expect:"test2";
    [%test_result: int] t2.extra ~expect:1;
    let assert_raises_invalid_name f =
      match f () with
      | Ok _ -> failwith "Expected Invalid_name error"
      | Error (Error.Invalid_name _) -> ()
      | Error _ -> failwith "Expected Invalid_name error"
    in
    assert_raises_invalid_name (fun () -> copy ~name:"" t)

  let%test_unit "version operations" =
    let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
    [%test_result: int option] (get_version t) ~expect:None;
    let t = set_version t 1 |> to_or_error |> ok_exn in
    [%test_result: int option] (get_version t) ~expect:(Some 1);
    let t = clear_version t |> to_or_error |> ok_exn in
    [%test_result: int option] (get_version t) ~expect:None

  let%test_unit "comparison functions" =
    let t1 = create ~name:"a" ~extra:1 () |> to_or_error |> ok_exn in
    let t2 = create ~name:"b" ~extra:1 () |> to_or_error |> ok_exn in
    let t3 =
      create ~name:"a" ~version:(Some 1) ~extra:1 () |> to_or_error |> ok_exn
    in
    let t4 =
      create ~name:"a" ~version:(Some 2) ~extra:1 () |> to_or_error |> ok_exn
    in
    assert (compare_by_name t1 t2 < 0);
    assert (compare_by_version t3 t4 < 0);
    assert (compare_by_key t1 t2 = 0)

  let%test_unit "list operations" =
    let ts =
      [
        create ~name:"b" ~extra:1 () |> to_or_error |> ok_exn;
        create ~name:"a" ~extra:1 () |> to_or_error |> ok_exn;
        create ~name:"c" ~extra:1 () |> to_or_error |> ok_exn;
      ]
    in
    let sorted = to_list ts in
    assert (List.length sorted = 3);
    assert (String.equal (List.hd_exn sorted).name "a");
    assert (String.equal (List.last_exn sorted).name "c")
end
