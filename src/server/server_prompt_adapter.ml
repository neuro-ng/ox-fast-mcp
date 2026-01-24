(** Server Prompt Adapter - Prompt_manager-like interface over server storage

    Provides an adapter that wraps the server's inline prompt storage (Hashtbl)
    with an interface compatible with Prompt_manager. This enables using manager
    patterns without changing the underlying server storage mechanism.

    Uses a generic type parameter to work with any prompt record type. *)

open! Core
open! Async

type 'prompt t = {
  get_prompts : unit -> (string, 'prompt) Hashtbl.t;
  set_prompt : key:string -> data:'prompt -> unit;
  remove_prompt : string -> unit;
  get_key : 'prompt -> string;
  get_name : 'prompt -> string;
  get_tags : 'prompt -> String.Set.t;
  get_description : 'prompt -> string option;
  render_handler : 'prompt -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  on_duplicate : [ `Warn | `Error | `Replace | `Ignore ];
}
(** Generic adapter type working with any prompt type that has a 'key' field *)

let create ~get_prompts ~set_prompt ~remove_prompt ~get_key ~get_name ~get_tags
    ~get_description ~render_handler ~on_duplicate =
  {
    get_prompts;
    set_prompt;
    remove_prompt;
    get_key;
    get_name;
    get_tags;
    get_description;
    render_handler;
    on_duplicate;
  }

(** {1 Manager-like Interface} *)

let has_prompt t key = Hashtbl.mem (t.get_prompts ()) key
let get_prompt t key = Hashtbl.find (t.get_prompts ()) key
let list_prompts t = Hashtbl.data (t.get_prompts ())
let count t = Hashtbl.length (t.get_prompts ())

let add_prompt t prompt =
  let key = t.get_key prompt in
  if Hashtbl.mem (t.get_prompts ()) key then
    match t.on_duplicate with
    | `Error -> Error (sprintf "Prompt already exists: %s" key)
    | `Warn ->
      Logs.warn (fun m -> m "Prompt already exists, replacing: %s" key);
      t.set_prompt ~key ~data:prompt;
      Ok ()
    | `Replace ->
      t.set_prompt ~key ~data:prompt;
      Ok ()
    | `Ignore -> Ok ()
  else (
    t.set_prompt ~key ~data:prompt;
    Ok ())

let remove_prompt t key =
  if Hashtbl.mem (t.get_prompts ()) key then (
    t.remove_prompt key;
    Ok ())
  else Error (sprintf "Prompt not found: %s" key)

let find_by_tags t tags =
  Hashtbl.data (t.get_prompts ())
  |> List.filter ~f:(fun prompt ->
         let prompt_tags = t.get_tags prompt in
         List.exists tags ~f:(fun tag -> Set.mem prompt_tags tag))

let find_by_name t name =
  Hashtbl.data (t.get_prompts ())
  |> List.find ~f:(fun prompt -> String.equal (t.get_name prompt) name)

let render_prompt t key arguments =
  match get_prompt t key with
  | None -> return (Error (sprintf "Prompt not found: %s" key))
  | Some prompt -> (
    Monitor.try_with (fun () -> t.render_handler prompt arguments) >>| function
    | Ok result -> Ok result
    | Error exn ->
      Error (sprintf "Error rendering prompt %s: %s" key (Exn.to_string exn)))
