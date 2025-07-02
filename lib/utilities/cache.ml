(** Cache module for storing values with expiration times *)

(** Cache statistics type *)
type cache_stats = {
  hits: int;
  misses: int;
  sets: int;
}

(** TimedCache module implementation *)
module TimedCache = struct
  type 'a t = {
    expiration: float;  (* Expiration time in seconds *)
    mutable cache: (string, ('a * float)) Hashtbl.t;  (* Value and expiration timestamp *)
    mutable stats: cache_stats;
  }

  (** Create a new TimedCache with the given expiration time in seconds *)
  let create expiration =
    {
      expiration;
      cache = Hashtbl.create 16;
      stats = { hits = 0; misses = 0; sets = 0 };
    }

  (** Get the expiration time of the cache *)
  let get_expiration t = t.expiration

  (** Get the current size of the cache *)
  let size t = Hashtbl.length t.cache

  (** Get the current timestamp in seconds since epoch *)
  let now () = Unix.gettimeofday ()

  (** Set a value in the cache *)
  let set t key value =
    let expires = now () +. t.expiration in
    t.stats <- { t.stats with sets = t.stats.sets + 1 };
    Hashtbl.replace t.cache key (value, expires)

  (** Get a value from the cache *)
  let get t key =
    match Hashtbl.find_opt t.cache key with
    | Some (value, expires) when expires > now () ->
      t.stats <- { t.stats with hits = t.stats.hits + 1 };
      Some value
    | Some _ ->
      (* Value exists but has expired *)
      t.stats <- { t.stats with misses = t.stats.misses + 1 };
      Hashtbl.remove t.cache key;
      None
    | None ->
      t.stats <- { t.stats with misses = t.stats.misses + 1 };
      None

  (** Clear all entries from the cache *)
  let clear t = Hashtbl.clear t.cache

  (** Get the time-to-live (TTL) for a key in seconds *)
  let get_ttl t key =
    match Hashtbl.find_opt t.cache key with
    | Some (_, expires) ->
      let ttl = expires -. now () in
      if ttl > 0.0 then Some ttl else None
    | None -> None

  (** Get the current cache statistics *)
  let get_stats t = t.stats
end 