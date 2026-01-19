(** Server Resource Adapter - Resource_manager-like interface over server storage

    Provides an adapter that wraps the server's inline resource storage (Hashtbl)
    with an interface compatible with Resource_manager. This enables using manager
    patterns without changing the underlying server storage mechanism.
    
    Uses a generic type parameter to work with any resource record type. *)

open! Core
open! Async

(** Generic adapter type working with any resource type that has a 'key' field *)
type 'resource t = {
  get_resources : unit -> (string, 'resource) Hashtbl.t;
  set_resource : key:string -> data:'resource -> unit;
  remove_resource : string -> unit;
  get_key : 'resource -> string;
  get_uri : 'resource -> string;
  get_name : 'resource -> string;
  get_tags : 'resource -> String.Set.t;
  get_description : 'resource -> string option;
  get_mime_type : 'resource -> string;
  read_handler : 'resource -> string Deferred.t;
  on_duplicate : [ `Warn | `Error | `Replace | `Ignore ];
}

let create ~get_resources ~set_resource ~remove_resource ~get_key ~get_uri 
    ~get_name ~get_tags ~get_description ~get_mime_type ~read_handler ~on_duplicate =
  { get_resources; set_resource; remove_resource; get_key; get_uri; get_name; 
    get_tags; get_description; get_mime_type; read_handler; on_duplicate }

(** {1 Manager-like Interface} *)

let has_resource t key =
  Hashtbl.mem (t.get_resources ()) key

let get_resource t key =
  Hashtbl.find (t.get_resources ()) key

let list_resources t =
  Hashtbl.data (t.get_resources ())

let count t =
  Hashtbl.length (t.get_resources ())

let add_resource t resource =
  let key = t.get_key resource in
  if Hashtbl.mem (t.get_resources ()) key then
    match t.on_duplicate with
    | `Error -> Error (sprintf "Resource already exists: %s" key)
    | `Warn ->
      Logs.warn (fun m -> m "Resource already exists, replacing: %s" key);
      t.set_resource ~key ~data:resource;
      Ok ()
    | `Replace ->
      t.set_resource ~key ~data:resource;
      Ok ()
    | `Ignore -> Ok ()
  else begin
    t.set_resource ~key ~data:resource;
    Ok ()
  end

let remove_resource t key =
  if Hashtbl.mem (t.get_resources ()) key then begin
    t.remove_resource key;
    Ok ()
  end else
    Error (sprintf "Resource not found: %s" key)

let find_by_tags t tags =
  Hashtbl.data (t.get_resources ())
  |> List.filter ~f:(fun resource ->
         let resource_tags = t.get_tags resource in
         List.exists tags ~f:(fun tag -> Set.mem resource_tags tag))

let find_by_name t name =
  Hashtbl.data (t.get_resources ())
  |> List.find ~f:(fun resource -> String.equal (t.get_name resource) name)

let find_by_scheme t scheme =
  Hashtbl.data (t.get_resources ())
  |> List.filter ~f:(fun resource ->
         let uri = t.get_uri resource in
         String.is_prefix uri ~prefix:(scheme ^ "://"))

let read_resource t key =
  match get_resource t key with
  | None -> 
    return (Error (sprintf "Resource not found: %s" key))
  | Some resource ->
    Monitor.try_with (fun () -> t.read_handler resource)
    >>| function
    | Ok result -> Ok result
    | Error exn ->
      Error (sprintf "Error reading resource %s: %s" key (Exn.to_string exn))
