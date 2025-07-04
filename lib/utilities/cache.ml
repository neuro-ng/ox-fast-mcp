(** Cache module for storing values with expiration times *)

open! Core
open! Async

module Time = Time_float_unix

(** Cache statistics type *)
type cache_stats = {
  hits: int;
  misses: int;
  sets: int;
} [@@deriving sexp]

(** A cache entry that expires after a certain time *)
type 'a cache_entry = {
  value : 'a;
  expires : Time_float_unix.t;
} [@@deriving sexp, compare]

(** A cache that automatically expires entries after a specified duration *)
type 'a t = {
  expiration : Time.Span.t;
  mutable cache : 'a cache_entry Map.M(String).t;
  mutable stats: cache_stats;
} [@@deriving sexp]

(** Create a new timed cache with the specified expiration duration *)
let create ~expiration = 
  { expiration
  ; cache = Map.empty (module String)
  ; stats = { hits = 0; misses = 0; sets = 0 }
  }

(** Get the expiration time of the cache *)
let get_expiration t = t.expiration

(** Get the current size of the cache *)
let size t = Map.length t.cache

(** Get the current timestamp in seconds since epoch *)
let now () = Time.now ()

(** Set a value in the cache *)
let set t ~key ~value =
  let expires = Time.add (Time.now ()) t.expiration in
  t.stats <- { t.stats with sets = t.stats.sets + 1 };
  t.cache <- Map.set t.cache ~key ~data:{ value; expires }

(** Get a value from the cache. Returns None if not found or expired *)
let get t ~key =
  match Map.find t.cache key with
  | None ->
    t.stats <- { t.stats with misses = t.stats.misses + 1 };
    None
  | Some entry ->
    if Time.(entry.expires > now ()) then (
      t.stats <- { t.stats with hits = t.stats.hits + 1 };
      Some entry.value
    ) else (
      t.stats <- { t.stats with misses = t.stats.misses + 1 };
      t.cache <- Map.remove t.cache key;
      None
    )

(** Clear all entries from the cache *)
let clear t =
  t.cache <- Map.empty (module String);
  t.stats <- { hits = 0; misses = 0; sets = 0 }

(** Get the time-to-live (TTL) for a key in seconds *)
let get_ttl t key =
  match Map.find t.cache key with
  | None -> None
  | Some entry ->
    let now = Time_float_unix.now () in
    let span = Time_float_unix.diff entry.expires now in
    if Time_float_unix.Span.(span > zero) 
    then Some span 
    else None

(** Get the current cache statistics *)
let get_stats t = t.stats

module TimedCache = struct
  type nonrec 'a t = 'a t

  let create = create

  let get_expiration = get_expiration

  let size = size

  let now = now

  let set = set

  let get = get

  let clear = clear

  let get_ttl = get_ttl

  let get_stats = get_stats
end 