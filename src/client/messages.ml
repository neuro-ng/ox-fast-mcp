(** Client Messages Module - Simplified Async Version

    Provides message handling for MCP client callbacks. *)

open Async

type message =
  | Request of { method_ : string; params : Yojson.Safe.t option }
  | Notification of { method_ : string; params : Yojson.Safe.t option }
  | Error of { message : string; code : int }  (** Simplified message type *)

type t = { mutable handler : t -> message -> unit Deferred.t }
(** Message handler type *)

let create () = { handler = (fun _ _ -> return ()) }

let dispatch t message =
  match message with
  | Request { method_; params = _ } ->
    Logs.debug (fun m -> m "Request: %s" method_);
    t.handler t message
  | Notification { method_; params = _ } ->
    Logs.debug (fun m -> m "Notification: %s" method_);
    t.handler t message
  | Error { message = msg; code } ->
    Logs.err (fun m -> m "Error %d: %s" code msg);
    t.handler t message
