(** Base module for all MCP resources
    
    Converted to use Async instead of Lwt for consistency with rest of codebase.
    See: COMPLIANCE_ACTION_PLAN.md Task 1.2
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 4 (lines 330-390)
*)

open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type content = Text of string | Binary of bytes [@@deriving sexp, yojson]

type t = {
  uri : string;
  name : string;
  mime_type : string;
  description : string option;
  tags : string list;
  enabled : bool;
  read_fn : (unit -> content Deferred.t) option;
}
[@@deriving yojson_of]

let text s = Text s
let binary b = Binary b

let validate_mime_type mime_type =
  let pattern = Str.regexp "^[a-zA-Z0-9]+/[a-zA-Z0-9\\-+.]+$" in
  try
    ignore (Str.search_forward pattern mime_type 0);
    true
  with _ -> false

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

let from_function ?name ?description ?mime_type ?tags ?(enabled = true) ~uri fn
    =
  let mime_type = set_default_mime_type mime_type in
  let name = set_default_name uri name in
  let tags = Option.value tags ~default:[] in
  {
    uri = Uri.to_string uri;
    name;
    mime_type;
    description;
    tags;
    enabled;
    read_fn = Some fn;
  }

let read t =
  match (t.enabled, t.read_fn) with
  | false, _ -> Deferred.Or_error.error_string "Resource is disabled" |> Deferred.Or_error.ok_exn
  | true, None -> Deferred.Or_error.error_string "Resource has no read function" |> Deferred.Or_error.ok_exn
  | true, Some fn -> fn ()

(* let notify_resource_list_changed () = 
   match%bind Mcp.Server.Context.get () with 
   | None -> return ()
   | Some ctx -> Mcp.Server.Context.queue_resource_list_changed ctx *)

let enable t =
  (* let%bind () = notify_resource_list_changed () in *)
  return { t with enabled = true }

let disable t =
  (* let%bind () = notify_resource_list_changed () in *)
  return { t with enabled = false }

let key t = t.uri

let to_mcp_resource ?overrides t =
  let base =
    [
      ("uri", `String t.uri);
      ("name", `String t.name);
      ("mimeType", `String t.mime_type);
      ( "description",
        match t.description with
        | Some desc -> `String desc
        | None -> `Null );
      ("tags", `List (List.map t.tags ~f:(fun t -> `String t)));
      ("enabled", `Bool t.enabled);
    ]
  in
  let json =
    match overrides with
    | Some overrides ->
      List.fold overrides ~init:base ~f:(fun acc (k, v) ->
          (k, v) :: List.filter acc ~f:(fun (k', _) -> String.(k' <> k)))
    | None -> base
  in
  Mcp.Types.resource_of_yojson (`Assoc json)
