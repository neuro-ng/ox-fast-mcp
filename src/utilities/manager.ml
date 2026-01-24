(** Polymorphic Manager signature for OxFastMCP

    Defines a unified interface for managing components (tools, resources,
    prompts). See: PYTHON_TO_OCAML_TYPE_MAP.md Section 6 (lines 531-577) See:
    Task 6.1 - Polymorphic Manager Signature *)

open! Core
open! Async

(** {1 Manager Signature} *)

(** Polymorphic manager interface *)
module type S = sig
  type t
  type item

  val create : unit -> t
  val add : t -> item -> unit Deferred.t
  val remove : t -> string -> unit Deferred.t
  val get : t -> string -> item option
  val list : t -> item list
  val list_enabled : t -> item list
  val enable : t -> string -> bool
  val disable : t -> string -> bool
  val is_enabled : t -> string -> bool
  val clear : t -> unit Deferred.t
  val count : t -> int
  val find_by_tags : t -> string list -> item list
  val find_by_name : t -> string -> item option
end

(** {1 Helper Functions} *)

let key_of_item key_fn item = key_fn item
let is_item_enabled is_enabled_fn item = is_enabled_fn item
let get_item_tags get_tags_fn item = get_tags_fn item

(** {1 Default Manager Implementation Helper} *)

(** Functor to create a manager implementation from item operations *)
module Make (Item : sig
  type t

  val key : t -> string
  val is_enabled : t -> bool
  val get_tags : t -> string list
  val get_name : t -> string
end) : S with type item := Item.t = struct
  type t = { items : (string, Item.t) Hashtbl.t; mutable enabled : bool }

  let create () = { items = Hashtbl.create (module String); enabled = true }

  let add t item =
    let key = Item.key item in
    Hashtbl.set t.items ~key ~data:item;
    return ()

  let remove t key =
    Hashtbl.remove t.items key;
    return ()

  let get t key = Hashtbl.find t.items key
  let list t = Hashtbl.data t.items
  let list_enabled t = Hashtbl.data t.items |> List.filter ~f:Item.is_enabled

  let enable t key =
    match Hashtbl.find t.items key with
    | Some _item ->
      (* Note: This is simplified. In practice, you'd need to update the item *)
      (* For now, just return success if found *)
      true
    | None -> false

  let disable t key =
    match Hashtbl.find t.items key with
    | Some _item -> true
    | None -> false

  let is_enabled t key =
    match Hashtbl.find t.items key with
    | Some item -> Item.is_enabled item
    | None -> false

  let clear t =
    Hashtbl.clear t.items;
    return ()

  let count t = Hashtbl.length t.items

  let find_by_tags t tags =
    Hashtbl.data t.items
    |> List.filter ~f:(fun item ->
           let item_tags = Item.get_tags item in
           List.exists tags ~f:(fun tag ->
               List.mem item_tags tag ~equal:String.equal))

  let find_by_name t name =
    Hashtbl.data t.items
    |> List.find ~f:(fun item -> String.equal (Item.get_name item) name)
end
