open Core
open Lwt.Syntax
open Types

type content = 
  | Text of string
  | Binary of bytes
[@@deriving sexp, yojson]

type t = {
  uri : Uri.t;
  name : string;
  mime_type : string;
  description : string option;
  tags : string list;
  enabled : bool;
  read_fn : (unit -> content Lwt.t) option;
} [@@deriving sexp, yojson_of]

let text s = Text s

let binary b = 
  Binary b

let validate_mime_type mime_type =
  let pattern = Str.regexp "^[a-zA-Z0-9]+/[a-zA-Z0-9\\-+.]+$" in
  try
    ignore (Str.search_forward pattern mime_type 0);
    true
  with Not_found -> false

let default_mime_type = "text/plain"

let set_default_mime_type = function
  | Some mime_type when validate_mime_type mime_type -> mime_type
  | Some mime_type -> 
    failwith (Printf.sprintf "Invalid MIME type format: %s" mime_type)
  | None -> default_mime_type

let set_default_name uri name =
  match name with
  | Some name -> name
  | None -> Uri.to_string uri

let from_function ?name ?description ?mime_type ?tags ?(enabled=true) ~uri fn =
  let mime_type = set_default_mime_type mime_type in
  let name = set_default_name uri name in
  let tags = Option.value tags ~default:[] in
  { uri; name; mime_type; description; tags; enabled; read_fn = Some fn }

let read t =
  match t.enabled, t.read_fn with
  | false, _ -> Lwt.fail_with "Resource is disabled"
  | true, None -> Lwt.fail_with "Resource has no read function"
  | true, Some fn -> fn ()

let notify_resource_list_changed () =
  match%lwt Mcp.Server.Context.get () with
  | None -> Lwt.return_unit
  | Some ctx -> Mcp.Server.Context.queue_resource_list_changed ctx

let enable t =
  let* () = notify_resource_list_changed () in
  Lwt.return { t with enabled = true }

let disable t =
  let* () = notify_resource_list_changed () in
  Lwt.return { t with enabled = false }

let key t = Uri.to_string t.uri

let to_mcp_resource ?overrides t =
  let base = [
    ("uri", `String (Uri.to_string t.uri));
    ("name", `String t.name);
    ("mimeType", `String t.mime_type);
    ("description", match t.description with 
      | Some desc -> `String desc 
      | None -> `Null);
    ("tags", `List (List.map t.tags ~f:(fun t -> `String t)));
    ("enabled", `Bool t.enabled);
  ] in
  let json = match overrides with
    | Some overrides -> 
      List.fold overrides ~init:base ~f:(fun acc (k, v) ->
        (k, v) :: List.filter acc ~f:(fun (k', _) -> String.(k' <> k)))
    | None -> base
  in
  Mcp.Types.Resource.of_yojson (`Assoc json)
  |> function
    | Ok r -> r
    | Error msg -> failwith msg 