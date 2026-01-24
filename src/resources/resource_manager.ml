(** Resource Manager for OxFastMCP

    Manages a collection of resources with support for registration, retrieval,
    and reading. Enhanced to align with Manager.S pattern and use new
    resource_types. See: specs/resource_manager.todo *)

open! Core
open! Async
open! Async.Let_syntax

(** {1 Duplicate Behavior} *)

module DuplicateBehavior = struct
  type t = Warn | Replace | Error | Ignore [@@deriving sexp, yojson]

  let of_string = function
    | "warn" -> Ok Warn
    | "replace" -> Ok Replace
    | "error" -> Ok Error
    | "ignore" -> Ok Ignore
    | s ->
      Or_error.error_string (Printf.sprintf "Invalid duplicate_behavior: %s" s)

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error error -> failwith (Error.to_string_hum error)
end

(** {1 Resource Manager Type} *)

type t = {
  resources : (string, Resource_types.t) Hashtbl.t;
  mutable duplicate_behavior : DuplicateBehavior.t;
  mutable mask_error_details : bool;
  mutable enabled : bool;
}

(** {1 Manager Creation} *)

let create ?(duplicate_behavior = DuplicateBehavior.Warn)
    ?(mask_error_details = false) () : t =
  {
    resources = Hashtbl.create (module String);
    duplicate_behavior;
    mask_error_details;
    enabled = true;
  }

(** {1 Resource Operations} *)

let add (manager : t) (resource : Resource_types.t) : unit Deferred.t =
  let key = Resource_types.key resource in
  match Hashtbl.mem manager.resources key with
  | false ->
    Hashtbl.set manager.resources ~key ~data:resource;
    return ()
  | true -> (
    match manager.duplicate_behavior with
    | DuplicateBehavior.Error ->
      failwith (Printf.sprintf "Resource already exists: %s" key)
    | DuplicateBehavior.Warn ->
      Logs.warn (fun m -> m "Resource already exists, replacing: %s" key);
      Hashtbl.set manager.resources ~key ~data:resource;
      return ()
    | DuplicateBehavior.Replace ->
      Hashtbl.set manager.resources ~key ~data:resource;
      return ()
    | DuplicateBehavior.Ignore -> return ())

let remove (manager : t) (key : string) : unit Deferred.t =
  Hashtbl.remove manager.resources key;
  return ()

let get (manager : t) (key : string) : Resource_types.t option =
  Hashtbl.find manager.resources key

let has_resource (manager : t) (key : string) : bool =
  Hashtbl.mem manager.resources key

let list (manager : t) : Resource_types.t list = Hashtbl.data manager.resources

let list_enabled (manager : t) : Resource_types.t list =
  Hashtbl.data manager.resources |> List.filter ~f:Resource_types.is_enabled

let enable_manager (manager : t) : unit Deferred.t =
  manager.enabled <- true;
  return ()

let disable_manager (manager : t) : unit Deferred.t =
  manager.enabled <- false;
  return ()

let enable (manager : t) (key : string) : bool =
  match Hashtbl.find manager.resources key with
  | Some resource ->
    let enabled_resource = Resource_types.enable resource in
    Hashtbl.set manager.resources ~key ~data:enabled_resource;
    true
  | None -> false

let disable (manager : t) (key : string) : bool =
  match Hashtbl.find manager.resources key with
  | Some resource ->
    let disabled_resource = Resource_types.disable resource in
    Hashtbl.set manager.resources ~key ~data:disabled_resource;
    true
  | None -> false

let is_enabled (manager : t) (key : string) : bool =
  match Hashtbl.find manager.resources key with
  | Some resource -> Resource_types.is_enabled resource
  | None -> false

let is_manager_enabled (manager : t) : bool = manager.enabled

let clear (manager : t) : unit Deferred.t =
  Hashtbl.clear manager.resources;
  return ()

let count (manager : t) : int = Hashtbl.length manager.resources

let find_by_tags (manager : t) (tags : string list) : Resource_types.t list =
  Hashtbl.data manager.resources
  |> List.filter ~f:(fun resource ->
         let resource_tags = Resource_types.get_tags resource in
         List.exists tags ~f:(fun tag ->
             List.mem resource_tags tag ~equal:String.equal))

let find_by_name (manager : t) (name : string) : Resource_types.t option =
  Hashtbl.data manager.resources
  |> List.find ~f:(fun resource ->
         String.equal (Resource_types.get_name resource) name)

(** {1 Resource Reading} *)

let read_resource (manager : t) (key : string) :
    ( Resource_types.content,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t =
  if not manager.enabled then
    let error_data =
      {
        Ox_fast_mcp.Exceptions.message = "Resource manager is disabled";
        code = Some 403;
        data = None;
      }
    in
    return (Error error_data)
  else
    match get manager key with
    | None ->
      let error_data =
        {
          Ox_fast_mcp.Exceptions.message =
            Printf.sprintf "Resource not found: %s" key;
          code = Some 404;
          data = None;
        }
      in
      return (Error error_data)
    | Some resource -> (
      if not (Resource_types.is_enabled resource) then
        let error_data =
          {
            Ox_fast_mcp.Exceptions.message =
              Printf.sprintf "Resource is disabled: %s" key;
            code = Some 403;
            data = None;
          }
        in
        return (Error error_data)
      else
        Monitor.try_with ~extract_exn:true (fun () ->
            Resource_types.read resource)
        >>= function
        | Ok result -> return result
        | Error exn ->
          let error_msg =
            if manager.mask_error_details then "Resource reading failed"
            else
              Printf.sprintf "Resource reading failed: %s" (Exn.to_string exn)
          in
          let error_data =
            {
              Ox_fast_mcp.Exceptions.message = error_msg;
              code = Some 500;
              data = None;
            }
          in
          return (Error error_data))

(** {1 Utility Functions} *)

let get_resources (manager : t) : Resource_types.t list = list manager

let set_duplicate_behavior (manager : t) (behavior : DuplicateBehavior.t) : unit
    =
  manager.duplicate_behavior <- behavior

let set_mask_error_details (manager : t) (mask : bool) : unit =
  manager.mask_error_details <- mask
