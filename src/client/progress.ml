open Core
open Async

type handler = float -> float option -> string option -> unit Deferred.t

let default_handler progress total message =
  let progress_str =
    match total with
    | Some total ->
      (* We have both progress and total *)
      let percent = progress /. total *. 100.0 in
      sprintf "%f/%f (%.1f%%)" progress total percent
    | None ->
      (* We only have progress *)
      sprintf "%f" progress
  in

  (* Include message if available *)
  let log_msg =
    match message with
    | Some msg -> sprintf "Progress: %s - %s" progress_str msg
    | None -> sprintf "Progress: %s" progress_str
  in

  Logs.debug (fun m -> m "%s" log_msg);
  return ()
