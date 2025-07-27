open Core
open Lwt.Syntax
open Mcp.Types

type progress_fn = float -> float option -> string option -> unit Lwt.t
(** Progress notification callback type *)

(** Request responder module *)
module Request_responder : sig
  type ('req, 'res) t = {
    request_id : Mcp.Types.request_id;
    request_meta : Mcp.Types.meta option;
    request : 'req;
    message_metadata : Message.message_metadata option;
    mutable completed : bool;
    mutable cancel_scope : unit -> unit; (* Simplified cancel scope *)
  }

  val create :
    request_id:Mcp.Types.request_id ->
    ?request_meta:Mcp.Types.meta ->
    request:'req ->
    ?message_metadata:Message.message_metadata ->
    on_complete:(('req, 'res) t -> unit Lwt.t) ->
    ('req, 'res) t

  val with_responder :
    ('req, 'res) t -> (('req, 'res) t -> 'a Lwt.t) -> 'a Lwt.t

  val respond : ('req, 'res) t -> 'res -> unit Lwt.t
  val cancel : ('req, 'res) t -> unit Lwt.t
  val in_flight : ('req, 'res) t -> bool
  val is_cancelled : ('req, 'res) t -> bool
end

(** Base session module *)
module Base_session : sig
  type ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t = {
    read_stream :
      (Message.session_message, [> `Msg of string ]) result Lwt_stream.t;
    write_stream : Message.session_message -> unit Lwt.t;
    mutable request_id : int;
    receive_request_type : 'recv_req; (* Type info for request validation *)
    receive_notification_type : 'recv_notif;
        (* Type info for notification validation *)
    read_timeout : float option;
    mutable in_flight : ('recv_req, 'send_res) Request_responder.t list;
    mutable progress_callbacks : (Mcp.Types.request_id, progress_fn) Hashtbl.t;
  }

  val create :
    read_stream:
      (Message.session_message, [> `Msg of string ]) result Lwt_stream.t ->
    write_stream:(Message.session_message -> unit Lwt.t) ->
    receive_request_type:'recv_req ->
    receive_notification_type:'recv_notif ->
    ?read_timeout:float ->
    unit ->
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t

  val with_session :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    (('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t -> 'a Lwt.t) ->
    'a Lwt.t

  val send_request :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    'send_req ->
    ?request_read_timeout:float ->
    ?metadata:Message.message_metadata ->
    ?progress_callback:progress_fn ->
    unit ->
    'recv_req Lwt.t

  val send_notification :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    'send_notif ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    unit Lwt.t

  val send_progress_notification :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    progress_token:Mcp.Types.request_id ->
    progress:float ->
    ?total:float ->
    ?message:string ->
    unit ->
    unit Lwt.t

  val received_request :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    ('recv_req, 'send_res) Request_responder.t ->
    unit Lwt.t
  (** Protected methods that can be overridden by inheritors *)

  val received_notification :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    'recv_notif ->
    unit Lwt.t

  val handle_incoming :
    ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t ->
    [> `Request of ('recv_req, 'send_res) Request_responder.t
    | `Notification of 'recv_notif
    | `Error of exn ] ->
    unit Lwt.t
end
