open! Core
open! Async
module Time = Time_float_unix

type cache_stats = { hits : int; misses : int; sets : int } [@@deriving sexp]
(** Cache statistics type *)

type 'a t
(** A cache that automatically expires entries after a specified duration *)

val create : expiration:Time.Span.t -> 'a t
(** Create a new timed cache with the specified expiration duration *)

val set : 'a t -> key:string -> value:'a -> unit
(** Set a value in the cache *)

val get : 'a t -> key:string -> 'a option
(** Get a value from the cache. Returns None if not found or expired *)

val clear : 'a t -> unit
(** Clear all entries from the cache *)

val get_stats : 'a t -> cache_stats
(** Get the current cache statistics *)

val get_ttl : 'a t -> string -> Time.Span.t option
(** Get the time-to-live (TTL) for a key in seconds *)

val size : 'a t -> int
(** Get the current size of the cache *)

val get_expiration : 'a t -> Time.Span.t
(** Get the expiration time of the cache *)

(** Module interface matching the Python TimedCache class *)
module TimedCache : sig
  type nonrec 'a t = 'a t

  val create : expiration:Time.Span.t -> 'a t
  val set : 'a t -> key:string -> value:'a -> unit
  val get : 'a t -> key:string -> 'a option
  val clear : 'a t -> unit
  val get_stats : 'a t -> cache_stats
  val get_ttl : 'a t -> string -> Time.Span.t option
  val size : 'a t -> int
  val get_expiration : 'a t -> Time.Span.t
  val now : unit -> Time.t
end
