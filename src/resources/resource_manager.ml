open Core
open Lwt.Syntax
open Types

type t = { resources : (string, resource) Hashtbl.t; mutable enabled : bool }

let create () = { resources = Hashtbl.create (module String); enabled = true }

let add_resource t resource =
  let key = Uri.to_string resource.uri in
  if Hashtbl.mem t.resources key then
    failwith (Printf.sprintf "Resource with URI %s already exists" key)
  else Hashtbl.add_exn t.resources ~key ~data:resource;
  let* () = Resource.notify_resource_list_changed () in
  Lwt.return_unit

let remove_resource t uri =
  let key = Uri.to_string uri in
  Hashtbl.remove t.resources key;
  let* () = Resource.notify_resource_list_changed () in
  Lwt.return_unit

let get_resource t uri =
  let key = Uri.to_string uri in
  Hashtbl.find t.resources key

let list_resources t =
  Hashtbl.data t.resources |> List.filter ~f:(fun r -> r.enabled)

let enable t =
  t.enabled <- true;
  let* () = Resource.notify_resource_list_changed () in
  Lwt.return_unit

let disable t =
  t.enabled <- false;
  let* () = Resource.notify_resource_list_changed () in
  Lwt.return_unit

let is_enabled t = t.enabled

let clear t =
  Hashtbl.clear t.resources;
  let* () = Resource.notify_resource_list_changed () in
  Lwt.return_unit
