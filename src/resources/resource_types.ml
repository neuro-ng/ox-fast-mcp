(** Resource types for OxFastMCP

    Implements resource type system per PYTHON_TO_OCAML_TYPE_MAP.md Section 4
    Follows the same pattern as tool_types.ml and prompt_types.ml See: Task
    4.1 - Resource Integration with Component Pattern *)

open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 Resource Content Types} *)

(** Resource content type *)
type content = Text of string | Binary of bytes
[@@deriving sexp, compare, yojson]

(** {1 Resource Handler} *)

type reader =
  unit -> (content, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Resource reader function - returns Result.t for error handling *)

(** {1 Resource Data} *)

(** Annotations module for handling JSON annotations *)
module Annotations = struct
  type t = (string * Yojson.Safe.t) list

  let yojson_of_t annotations = `Assoc annotations

  let t_of_yojson = function
    | `Assoc assoc -> assoc
    | _ -> []
end

type resource_data = {
  uri : string;  (** Resource URI *)
  mime_type : string;  (** MIME type *)
  annotations : Annotations.t option;
      [@default None] [@yojson_drop_if Option.is_none]
      (** Optional annotations *)
}
[@@deriving yojson]
(** Resource-specific metadata *)

(** {1 Resource Kinds} *)

(** Resource variants - different ways to provide resources *)
type resource_kind =
  | Function_resource of { fn : reader  (** Dynamic resource via function *) }
  | Text_resource of { content : string  (** Static text content *) }
  | Binary_resource of { content : bytes  (** Static binary content *) }
  | File_resource of { path : string  (** File path *) }
(* Note: No sexp derivation due to function field in Function_resource *)

(** {1 Unified Resource Type} *)

type resource_component_data = { data : resource_data; kind : resource_kind }
(** Component-specific data for resources *)
(* Note: No sexp derivation due to function field *)

type t = resource_component_data Components.component
(** Main resource type - uses polymorphic component pattern! *)
(* Note: No sexp derivation due to function field *)

(** {1 Resource Operations} *)

(** Get resource URI *)
let get_uri (resource : t) : string = resource.data.data.uri

(** Get resource MIME type *)
let get_mime_type (resource : t) : string = resource.data.data.mime_type

(** Get resource name *)
let get_name (resource : t) = resource.name

(** Get resource description *)
let get_description (resource : t) = resource.description

(** Check if resource is enabled *)
let is_enabled (resource : t) = resource.enabled

(** Get resource tags *)
let get_tags (resource : t) = resource.tags |> Set.to_list

(** Get resource annotations *)
let get_annotations (resource : t) = resource.data.data.annotations

(** {1 Resource Modification} *)

(** Enable a resource *)
let enable (resource : t) : t = Components.enable resource

(** Disable a resource *)
let disable (resource : t) : t = Components.disable resource

(** Get or generate resource key *)
let key (resource : t) : string = Components.key resource

(** Set resource key *)
let with_key (resource : t) (new_key : string) : t =
  Components.with_key resource new_key

(** {1 Resource Reading} *)

(** Read resource content *)
let read (resource : t) :
    (content, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t =
  if not resource.enabled then
    Deferred.return
      (Error
         {
           Ox_fast_mcp.Exceptions.message =
             Printf.sprintf "Resource '%s' is disabled" resource.name;
           code = Some 403;
           data = None;
         })
  else
    match resource.data.kind with
    | Function_resource { fn } -> fn ()
    | Text_resource { content } -> Deferred.return (Ok (Text content))
    | Binary_resource { content } -> Deferred.return (Ok (Binary content))
    | File_resource { path } -> (
      (* Read file asynchronously *)
      Monitor.try_with (fun () ->
          Reader.file_contents path >>| fun contents -> Ok (Text contents))
      >>| function
      | Ok result -> result
      | Error exn ->
        Error
          {
            Ox_fast_mcp.Exceptions.message =
              Printf.sprintf "Failed to read file '%s': %s" path
                (Exn.to_string exn);
            code = Some 500;
            data = None;
          })

(** {1 Resource Creation} *)

(** Create a function resource *)
let create_function_resource ~uri ~name ?description ?(mime_type = "text/plain")
    ?(tags = []) ?key ?annotations ?(enabled = true) (fn : reader) : t =
  let resource_data = { uri; mime_type; annotations } in
  let resource_component_data =
    { data = resource_data; kind = Function_resource { fn } }
  in
  Components.create ~name ?description ~tags:(String.Set.of_list tags) ?key
    ~enabled ~data:resource_component_data ()

(** Create a text resource *)
let create_text_resource ~uri ~name ~content ?description
    ?(mime_type = "text/plain") ?(tags = []) ?key ?annotations ?(enabled = true)
    () : t =
  let resource_data = { uri; mime_type; annotations } in
  let resource_component_data =
    { data = resource_data; kind = Text_resource { content } }
  in
  Components.create ~name ?description ~tags:(String.Set.of_list tags) ?key
    ~enabled ~data:resource_component_data ()

(** Create a binary resource *)
let create_binary_resource ~uri ~name ~content ?description ~mime_type
    ?(tags = []) ?key ?annotations ?(enabled = true) () : t =
  let resource_data = { uri; mime_type; annotations } in
  let resource_component_data =
    { data = resource_data; kind = Binary_resource { content } }
  in
  Components.create ~name ?description ~tags:(String.Set.of_list tags) ?key
    ~enabled ~data:resource_component_data ()

(** Create a file resource *)
let create_file_resource ~uri ~name ~path ?description
    ?(mime_type = "text/plain") ?(tags = []) ?key ?annotations ?(enabled = true)
    () : t =
  let resource_data = { uri; mime_type; annotations } in
  let resource_component_data =
    { data = resource_data; kind = File_resource { path } }
  in
  Components.create ~name ?description ~tags:(String.Set.of_list tags) ?key
    ~enabled ~data:resource_component_data ()

(** {1 MCP Integration} *)

(** Convert to MCP resource *)
let to_mcp_resource ?(_include_fastmcp_meta = false) (resource : t) :
    Mcp.Types.resource =
  let base_metadata : Mcp.Types.base_metadata =
    { name = resource.name; title = resource.title }
  in
  {
    Mcp.Types.uri = resource.data.data.uri;
    description = resource.description;
    mime_type = Some resource.data.data.mime_type;
    size = None;
    (* TODO: Compute size if available *)
    annotations = None;
    (* TODO: Add annotations if needed *)
    meta = None;
    (* TODO: Add fastmcp metadata if requested *)
    base_metadata;
  }

(** {1 Component Integration} *)

(** Resources are now directly components - no conversion needed! *)
let to_component (resource : t) : t = resource

(** Create resource from component *)
let from_component (component : resource_component_data Components.component) :
    t =
  component

(** {1 Helper Functions} *)

(** Validate MIME type format *)
let validate_mime_type mime_type =
  let pattern = Str.regexp "^[a-zA-Z0-9]+/[a-zA-Z0-9\\-+.]+$" in
  try
    ignore (Str.search_forward pattern mime_type 0);
    true
  with _ -> false

(** Create text content helper *)
let text s = Text s

(** Create binary content helper *)
let binary b = Binary b
