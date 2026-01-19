(** Server Tool Adapter - Tool_manager-like interface over server storage

    Provides an adapter that wraps the server's inline tool storage (Hashtbl)
    with an interface compatible with Tool_manager. This enables using manager
    patterns without changing the underlying server storage mechanism.
    
    Uses a generic type parameter to work with any tool record type. *)

open! Core
open! Async

(** Generic adapter type working with any tool type that has a 'key' field *)
type 'tool t = {
  get_tools : unit -> (string, 'tool) Hashtbl.t;
  set_tool : key:string -> data:'tool -> unit;
  remove_tool : string -> unit;
  get_key : 'tool -> string;
  get_name : 'tool -> string;
  get_tags : 'tool -> String.Set.t;
  call_handler : 'tool -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  on_duplicate : [ `Warn | `Error | `Replace | `Ignore ];
  mask_error_details : bool;
}

let create ~get_tools ~set_tool ~remove_tool ~get_key ~get_name ~get_tags
    ~call_handler ~on_duplicate ~mask_error_details =
  { get_tools; set_tool; remove_tool; get_key; get_name; get_tags; 
    call_handler; on_duplicate; mask_error_details }

(** {1 Manager-like Interface} *)

let has_tool t key =
  Hashtbl.mem (t.get_tools ()) key

let get_tool t key =
  Hashtbl.find (t.get_tools ()) key

let list_tools t =
  Hashtbl.data (t.get_tools ())

let count t =
  Hashtbl.length (t.get_tools ())

let add_tool t tool =
  let key = t.get_key tool in
  if Hashtbl.mem (t.get_tools ()) key then
    match t.on_duplicate with
    | `Error -> Error (sprintf "Tool already exists: %s" key)
    | `Warn ->
      Logs.warn (fun m -> m "Tool already exists, replacing: %s" key);
      t.set_tool ~key ~data:tool;
      Ok ()
    | `Replace ->
      t.set_tool ~key ~data:tool;
      Ok ()
    | `Ignore -> Ok ()
  else begin
    t.set_tool ~key ~data:tool;
    Ok ()
  end

let remove_tool t key =
  if Hashtbl.mem (t.get_tools ()) key then begin
    t.remove_tool key;
    Ok ()
  end else
    Error (sprintf "Tool not found: %s" key)

let find_by_tags t tags =
  Hashtbl.data (t.get_tools ())
  |> List.filter ~f:(fun tool ->
         let tool_tags = t.get_tags tool in
         List.exists tags ~f:(fun tag -> Set.mem tool_tags tag))

let find_by_name t name =
  Hashtbl.data (t.get_tools ())
  |> List.find ~f:(fun tool -> String.equal (t.get_name tool) name)

let call_tool t key arguments =
  match get_tool t key with
  | None -> 
    return (Error (sprintf "Tool not found: %s" key))
  | Some tool ->
    Monitor.try_with (fun () -> t.call_handler tool arguments)
    >>| function
    | Ok result -> Ok result
    | Error exn ->
      let msg = 
        if t.mask_error_details then sprintf "Error calling tool %s" key
        else sprintf "Error calling tool %s: %s" key (Exn.to_string exn)
      in
      Error msg
