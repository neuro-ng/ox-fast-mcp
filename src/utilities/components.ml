open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

type t = {
  name : string;
  description : string option;
  tags : String_set.t;
  enabled : bool;
  key : string option; [@default None] [@yojson_drop_if Option.is_none]
}
[@@deriving compare, sexp, yojson]

let create ?key ?description ?(tags = String.Set.empty) ?(enabled = true) ~name
    () =
  { name; description; tags; enabled; key }

let key t = Option.value t.key ~default:t.name
let with_key t new_key = { t with key = Some new_key }
let enable t = { t with enabled = true }
let disable t = { t with enabled = false }
