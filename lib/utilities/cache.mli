open! Core
open! Async

module Time = Time_float_unix

(** Cache statistics type *)
type cache_stats = {
  hits: int;
  misses: int;
  sets: int;
} [@@deriving sexp]

(** A cache that automatically expires entries after a specified duration *)
type 'a t

(** Create a new timed cache with the specified expiration duration *)
val create : expiration:Time.Span.t -> 'a t

(** Set a value in the cache *)
val set : 'a t -> key:string -> value:'a -> unit

(** Get a value from the cache. Returns None if not found or expired *)
val get : 'a t -> key:string -> 'a option

(** Clear all entries from the cache *)
val clear : 'a t -> unit

(** Get the current cache statistics *)
val get_stats : 'a t -> cache_stats

(** Get the time-to-live (TTL) for a key in seconds *)
val get_ttl : 'a t -> string -> Time.Span.t option

(** Get the current size of the cache *)
val size : 'a t -> int

(** Get the expiration time of the cache *)
val get_expiration : 'a t -> Time.Span.t

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