(** Prompt Manager for OxFastMCP

    Manages a collection of prompts with support for registration, retrieval,
    and rendering. Implements the Manager.S pattern for consistency. See:
    specs/prompt_manager.todo *)

open! Core
open! Async
open! Async.Let_syntax

(** {1 Duplicate Behavior} *)

module DuplicateBehavior = struct
  type t = Warn | Replace | Error | Ignore [@@deriving sexp, yojson]

  let of_string = function
    | "warn" -> Ok Warn
    | "replace" -> Ok Replace
    | "error" -> Ok Error
    | "ignore" -> Ok Ignore
    | s ->
      Or_error.error_string (Printf.sprintf "Invalid duplicate_behavior: %s" s)

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error error -> failwith (Error.to_string_hum error)
end

(** {1 Prompt Manager Type} *)

type t = {
  prompts : (string, Prompt_types.t) Hashtbl.t;
  mutable duplicate_behavior : DuplicateBehavior.t;
  mutable mask_error_details : bool;
}

(** {1 Manager Creation} *)

let create ?(duplicate_behavior = DuplicateBehavior.Warn)
    ?(mask_error_details = false) () : t =
  {
    prompts = Hashtbl.create (module String);
    duplicate_behavior;
    mask_error_details;
  }

(** {1 Prompt Operations} *)

let add (manager : t) (prompt : Prompt_types.t) : unit Deferred.t =
  let key = Prompt_types.key prompt in
  match Hashtbl.mem manager.prompts key with
  | false ->
    Hashtbl.set manager.prompts ~key ~data:prompt;
    return ()
  | true -> (
    match manager.duplicate_behavior with
    | DuplicateBehavior.Error ->
      failwith (Printf.sprintf "Prompt already exists: %s" key)
    | DuplicateBehavior.Warn ->
      Logs.warn (fun m -> m "Prompt already exists, replacing: %s" key);
      Hashtbl.set manager.prompts ~key ~data:prompt;
      return ()
    | DuplicateBehavior.Replace ->
      Hashtbl.set manager.prompts ~key ~data:prompt;
      return ()
    | DuplicateBehavior.Ignore -> return ())

let remove (manager : t) (key : string) : unit Deferred.t =
  Hashtbl.remove manager.prompts key;
  return ()

let get (manager : t) (key : string) : Prompt_types.t option =
  Hashtbl.find manager.prompts key

let has_prompt (manager : t) (key : string) : bool =
  Hashtbl.mem manager.prompts key

let list (manager : t) : Prompt_types.t list = Hashtbl.data manager.prompts

let list_enabled (manager : t) : Prompt_types.t list =
  Hashtbl.data manager.prompts |> List.filter ~f:Prompt_types.is_enabled

let enable (manager : t) (key : string) : bool =
  match Hashtbl.find manager.prompts key with
  | Some prompt ->
    let enabled_prompt = Prompt_types.enable prompt in
    Hashtbl.set manager.prompts ~key ~data:enabled_prompt;
    true
  | None -> false

let disable (manager : t) (key : string) : bool =
  match Hashtbl.find manager.prompts key with
  | Some prompt ->
    let disabled_prompt = Prompt_types.disable prompt in
    Hashtbl.set manager.prompts ~key ~data:disabled_prompt;
    true
  | None -> false

let is_enabled (manager : t) (key : string) : bool =
  match Hashtbl.find manager.prompts key with
  | Some prompt -> Prompt_types.is_enabled prompt
  | None -> false

let clear (manager : t) : unit Deferred.t =
  Hashtbl.clear manager.prompts;
  return ()

let count (manager : t) : int = Hashtbl.length manager.prompts

let find_by_tags (manager : t) (tags : string list) : Prompt_types.t list =
  Hashtbl.data manager.prompts
  |> List.filter ~f:(fun prompt ->
         let prompt_tags = Prompt_types.get_tags prompt in
         List.exists tags ~f:(fun tag ->
             List.mem prompt_tags tag ~equal:String.equal))

let find_by_name (manager : t) (name : string) : Prompt_types.t option =
  Hashtbl.data manager.prompts
  |> List.find ~f:(fun prompt ->
         String.equal (Prompt_types.get_name prompt) name)

(** {1 Prompt Rendering} *)

let render_prompt (manager : t) (key : string)
    ~(arguments : (string * Yojson.Safe.t) list option) :
    ( Prompt_types.prompt_message list,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t =
  match get manager key with
  | None ->
    let error_data =
      {
        Ox_fast_mcp.Exceptions.message =
          Printf.sprintf "Prompt not found: %s" key;
        code = Some 404;
        data = None;
      }
    in
    return (Error error_data)
  | Some prompt -> (
    if not (Prompt_types.is_enabled prompt) then
      let error_data =
        {
          Ox_fast_mcp.Exceptions.message =
            Printf.sprintf "Prompt is disabled: %s" key;
          code = Some 403;
          data = None;
        }
      in
      return (Error error_data)
    else
      Monitor.try_with ~extract_exn:true (fun () ->
          Prompt_types.render prompt ~arguments)
      >>= function
      | Ok result -> return result
      | Error exn ->
        let error_msg =
          if manager.mask_error_details then "Prompt rendering failed"
          else Printf.sprintf "Prompt rendering failed: %s" (Exn.to_string exn)
        in
        let error_data =
          {
            Ox_fast_mcp.Exceptions.message = error_msg;
            code = Some 500;
            data = None;
          }
        in
        return (Error error_data))

(** {1 Utility Functions} *)

let get_prompts (manager : t) : Prompt_types.t list = list manager

let set_duplicate_behavior (manager : t) (behavior : DuplicateBehavior.t) : unit
    =
  manager.duplicate_behavior <- behavior

let set_mask_error_details (manager : t) (mask : bool) : unit =
  manager.mask_error_details <- mask
