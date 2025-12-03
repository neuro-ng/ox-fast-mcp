(** Polymorphic Manager signature for OxFastMCP
    
    Defines a unified interface for managing components (tools, resources, prompts).
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 6 (lines 531-577)
    See: Task 6.1 - Polymorphic Manager Signature
*)

open! Core
open! Async

(** {1 Manager Signature} *)

(** Polymorphic manager interface for managing collections of components *)
module type S = sig
  (** The manager type *)
  type t
  
  (** The type of items managed *)
  type item
  
  (** {2 Creation} *)
  
  val create : unit -> t
  (** Create a new manager *)
  
  (** {2 Item Management} *)
  
  val add : t -> item -> unit Deferred.t
  (** Add an item to the manager *)
  
  val remove : t -> string -> unit Deferred.t
  (** Remove an item by key *)
  
  val get : t -> string -> item option
  (** Get an item by key *)
  
  val list : t -> item list
  (** List all items *)
  
  val list_enabled : t -> item list
  (** List only enabled items *)
  
  (** {2 Item Operations} *)
  
  val enable : t -> string -> bool
  (** Enable an item by key. Returns true if successful *)
  
  val disable : t -> string -> bool
  (** Disable an item by key. Returns true if successful *)
  
  val is_enabled : t -> string -> bool
  (** Check if an item is enabled *)
  
  val clear : t -> unit Deferred.t
  (** Clear all items *)
  
  val count : t -> int
  (** Get the number of items *)
  
  (** {2 Search and Filter} *)
  
  val find_by_tags : t -> string list -> item list
  (** Find items by tags *)
  
  val find_by_name : t -> string -> item option
  (** Find item by name (may differ from key) *)
end

(** {1 Helper Functions} *)

(** Extract key from an item that has a `key` function *)
val key_of_item : ('a -> string) -> 'a -> string
(** Helper to extract key from items *)

(** Check if item is enabled *)
val is_item_enabled : ('a -> bool) -> 'a -> bool
(** Helper to check if item is enabled *)

(** Get item tags *)
val get_item_tags : ('a -> string list) -> 'a -> string list
(** Helper to extract tags from items *)

