(** Base component types for OxFastMCP

    Implements the polymorphic component pattern for tools, prompts, resources.
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 2 (lines 85-164) See:
    COMPLIANCE_ACTION_PLAN.md Task 2.1 *)

open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Helper functions for String.Set.t JSON conversion *)
let string_set_of_yojson json =
  match json with
  | `List lst -> (
    try
      Ok
        (String.Set.of_list
           (List.map lst ~f:(function
             | `String s -> s
             | _ -> failwith "Expected string in list")))
    with _ -> Error "Invalid string in list")
  | _ -> Error "Expected JSON list"

let string_set_to_yojson set =
  `List (List.map ~f:(fun s -> `String s) (Set.to_list set))

module String_set = struct
  type t = String.Set.t [@@deriving compare, sexp]

  let t_of_yojson json =
    match string_set_of_yojson json with
    | Ok set -> set
    | Error msg -> failwith msg

  let yojson_of_t = string_set_to_yojson
end

(** Custom meta type that supports both sexp and yojson *)
module Meta = struct
  type t = (string * Yojson.Safe.t) list

  (* Custom yojson converters *)
  let yojson_of_t meta = `Assoc meta

  let t_of_yojson = function
    | `Assoc assoc -> assoc
    | _ -> []

  (* For sexp, we just store as a string representation *)
  let sexp_of_t meta =
    let json = `Assoc meta in
    Sexp.Atom (Yojson.Safe.to_string json)

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> (
      try
        match Yojson.Safe.from_string s with
        | `Assoc assoc -> assoc
        | _ -> []
      with _ -> [])
    | _ -> []
end

type 'a component = {
  name : string;
  title : string option; [@default None] [@yojson_drop_if Option.is_none]
  description : string option; [@default None] [@yojson_drop_if Option.is_none]
  tags : String_set.t;
  meta : Meta.t option; [@default None] [@yojson_drop_if Option.is_none]
  enabled : bool;
  key : string option; [@default None] [@yojson_drop_if Option.is_none]
  data : 'a;
}
[@@deriving sexp, yojson]
(** Base component record type - polymorphic over component-specific data *)

(* Custom compare function for components *)
let compare_component compare_a a b =
  let open Stdlib in
  match String.compare a.name b.name with
  | 0 -> (
    match Option.compare String.compare a.title b.title with
    | 0 -> (
      match Option.compare String.compare a.description b.description with
      | 0 -> (
        match String_set.compare a.tags b.tags with
        | 0 -> (
          (* Skip meta comparison - Yojson.Safe.t doesn't have compare *)
          match Bool.compare a.enabled b.enabled with
          | 0 -> (
            match Option.compare String.compare a.key b.key with
            | 0 -> compare_a a.data b.data
            | c -> c)
          | c -> c)
        | c -> c)
      | c -> c)
    | c -> c)
  | c -> c

type fastmcp_meta = { tags : string list } [@@deriving sexp, yojson]
(** Metadata type for FastMCP components *)

(** Mirrored component flag *)
type mirrored_flag = Local | Mirrored [@@deriving sexp, compare, yojson]

type 'a mirrored_component = {
  component : 'a component;
  mirrored : mirrored_flag;
}
[@@deriving sexp, yojson]
(** Component with mirrored status *)

let create ?key ?title ?description ?(tags = String.Set.empty) ?meta
    ?(enabled = true) ~name ~data () =
  { name; title; description; tags; meta; enabled; key; data }

let key t = Option.value t.key ~default:t.name
let with_key t new_key = { t with key = Some new_key }
let enable t = { t with enabled = true }
let disable t = { t with enabled = false }

let get_meta ~include_fastmcp_meta t =
  let base_meta = Option.value t.meta ~default:[] in
  match include_fastmcp_meta with
  | Some true | None ->
    (* Include FastMCP metadata *)
    let fastmcp_meta = { tags = Set.to_list t.tags } in
    let fastmcp_json = yojson_of_fastmcp_meta fastmcp_meta in
    Some (("_fastmcp", fastmcp_json) :: base_meta)
  | Some false ->
    (* Don't include FastMCP metadata *)
    if List.is_empty base_meta then None else Some base_meta
