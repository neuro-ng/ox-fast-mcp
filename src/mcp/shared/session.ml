open Core
open Async
open Mcp.Types
module Types = Mcp.Types

type progress_fn = float -> float option -> string option -> unit Deferred.t

module Request_responder = struct
  type ('req, 'res) t = {
    request_id : request_id;
    request_meta : meta option;
    request : 'req;
    message_metadata : Message.message_metadata option;
    mutable completed : bool;
    mutable cancel_scope : unit -> unit;
  }

  let[@ocaml.warning "-16"] create ~request_id ?request_meta ~request
      ?message_metadata ~on_complete:_ =
    {
      request_id;
      request_meta;
      request;
      message_metadata;
      completed = false;
      cancel_scope = (fun () -> ());
    }

  let with_responder t f =
    let old_cancel_scope = t.cancel_scope in
    let cancelled = ref false in
    t.cancel_scope <- (fun () -> cancelled := true);

    let%bind result =
      Monitor.try_with (fun () -> f t) >>| function
      | Ok r -> r
      | Error exn ->
        if not !cancelled then t.completed <- true;
        raise exn
    in

    t.cancel_scope <- old_cancel_scope;
    return result

  let respond t _response =
    if t.completed then raise (Failure "Request already responded to")
    else (
      t.completed <- true;
      return ())

  let cancel t =
    t.cancel_scope ();
    t.completed <- true;
    return ()

  let in_flight t = not t.completed
  let is_cancelled _t = false
end

module Base_session = struct
  let equal_request_id (a : request_id) (b : request_id) : bool =
    match (a, b) with
    | `Int x, `Int y -> Int.equal x y
    | `String x, `String y -> String.equal x y
    | _ -> false

  type ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t = {
    read_stream : Message.session_message Pipe.Reader.t;
    write_stream : Message.session_message -> unit Deferred.t;
    mutable request_id : int;
    receive_request_type : 'recv_req;
    receive_notification_type : 'recv_notif;
    read_timeout : float option;
    mutable in_flight : ('recv_req, 'send_res) Request_responder.t list;
    mutable progress_callbacks : (string, progress_fn) Hashtbl.t;
  }

  let create ~read_stream ~write_stream ~receive_request_type
      ~receive_notification_type ?read_timeout () =
    {
      read_stream;
      write_stream;
      request_id = 0;
      receive_request_type;
      receive_notification_type;
      read_timeout;
      in_flight = [];
      progress_callbacks = Hashtbl.create (module String);
    }

  let with_session t f =
    let%bind result = f t in
    (* Cleanup *)
    List.iter t.in_flight ~f:(fun resp ->
        if Request_responder.in_flight resp then
          don't_wait_for (Request_responder.cancel resp));
    Hashtbl.clear t.progress_callbacks;
    return result

  let[@ocaml.warning "-32"] send_error t request_id error =
    let jsonrpc_error = { jsonrpc = "2.0"; id = `Int request_id; error } in
    let message = Message.{ message = `Error jsonrpc_error; metadata = None } in
    t.write_stream message

  let[@ocaml.warning "-32"] send_response t request_id response =
    let jsonrpc_response =
      { jsonrpc = "2.0"; id = `Int request_id; result = response }
    in
    let message =
      Message.{ message = `Response jsonrpc_response; metadata = None }
    in
    t.write_stream message

  let[@ocaml.warning "-27"] send_request
      (type send_req send_notif send_res recv_req recv_notif)
      (t : (send_req, send_notif, send_res, recv_req, recv_notif) t)
      (request : send_req) ?request_read_timeout ?metadata ?progress_callback ()
      : recv_req Deferred.t =
    let request_id = t.request_id in
    t.request_id <- request_id + 1;

    (* Create JSONRPC request *)
    let jsonrpc_request =
      {
        jsonrpc = "2.0";
        id = `Int request_id;
        method_ = "request";
        params = None;
      }
    in

    (* Add progress token if callback provided *)
    let%bind () =
      match progress_callback with
      | Some cb ->
        Hashtbl.add_exn t.progress_callbacks ~key:(Int.to_string request_id)
          ~data:cb;
        return ()
      | None -> return ()
    in

    (* Send request *)
    let message = Message.{ message = `Request jsonrpc_request; metadata } in
    let%bind () = t.write_stream message in

    (* We don't have a concrete way to decode 'recv_req here. Fail explicitly to
       match interface. *)
    raise (Failure "send_request: not implemented for return type")

  let send_notification t _notification ?related_request_id () =
    let jsonrpc_notification =
      {
        jsonrpc = "2.0";
        method_ = "notification";
        (* This would need to be properly set *)
        params = None;
        (* This would need proper conversion *)
      }
    in

    let metadata =
      Option.map related_request_id ~f:(fun id ->
          Message.Server
            { related_request_id = Some id; request_context = None })
    in

    let message =
      Message.{ message = `Notification jsonrpc_notification; metadata }
    in
    t.write_stream message

  let send_progress_notification t ~progress_token ~progress ?total ?message ()
      =
    let notification =
      {
        jsonrpc = "2.0";
        method_ = "$/progress";
        params =
          Some
            (`Assoc
              [
                ( "progressToken",
                  match progress_token with
                  | `Int i -> `Int i
                  | `String s -> `String s );
                ("progress", `Float progress);
                ( "total",
                  match total with
                  | Some t -> `Float t
                  | None -> `Null );
                ( "message",
                  match message with
                  | Some m -> `String m
                  | None -> `Null );
              ]);
      }
    in

    let message =
      Message.{ message = `Notification notification; metadata = None }
    in
    t.write_stream message

  let received_request _t _responder = return ()
  let received_notification _t _notification = return ()

  let handle_incoming (type send_req send_notif send_res recv_req recv_notif)
      (_t : (send_req, send_notif, send_res, recv_req, recv_notif) t)
      (_evt :
        [> `Request of (recv_req, send_res) Request_responder.t
        | `Notification of recv_notif
        | `Error of exn ]) : unit Deferred.t =
    return ()

  let[@ocaml.warning "-32"] rec receive_loop t =
    Monitor.try_with (fun () -> Pipe.read t.read_stream) >>= function
    | Error _exn ->
      let%bind () = return () in
      receive_loop t
    | Ok message -> (
      match message with
      | `Eof -> return () (* Stream closed *)
      | `Ok message -> (
        match message.Message.message with
        | `Request req ->
          (* Treat incoming request as validated (no type witness function) *)
          let responder =
            Request_responder.create ~request_id:req.id ?request_meta:None
              ~request:req ?message_metadata:message.metadata
              ~on_complete:(fun _r ->
                t.in_flight <-
                  List.filter t.in_flight ~f:(fun r' ->
                      not (equal_request_id r'.request_id req.id));
                return ())
          in
          t.in_flight <- responder :: t.in_flight;
          let%bind () = received_request t responder in
          if not responder.completed then
            let%bind () = handle_incoming t (`Request responder) in
            receive_loop t
          else receive_loop t
        | `Notification notif -> (
          (* Handle cancellation notifications *)
          match notif.method_ with
          | "$/cancelled" -> (
            match notif.params with
            | Some (`Assoc [ ("requestId", `String id) ]) -> (
              match
                List.find t.in_flight ~f:(fun r ->
                    match r.request_id with
                    | `String sid -> String.equal sid id
                    | `Int _ -> false)
              with
              | Some req ->
                let%bind () = Request_responder.cancel req in
                receive_loop t
              | None -> receive_loop t)
            | _ -> receive_loop t)
          | "$/progress" -> (
            match notif.params with
            | Some (`Assoc params) -> (
              match
                List.Assoc.find params ~equal:String.equal "progressToken"
              with
              | Some (`String token) -> (
                match Hashtbl.find t.progress_callbacks token with
                | Some callback ->
                  let progress =
                    match
                      List.Assoc.find params ~equal:String.equal "progress"
                    with
                    | Some (`Float p) -> p
                    | _ -> 0.0
                  in
                  let total =
                    match
                      List.Assoc.find params ~equal:String.equal "total"
                    with
                    | Some (`Float t) -> Some t
                    | _ -> None
                  in
                  let message =
                    match
                      List.Assoc.find params ~equal:String.equal "message"
                    with
                    | Some (`String m) -> Some m
                    | _ -> None
                  in
                  let%bind () = callback progress total message in
                  receive_loop t
                | None -> receive_loop t)
              | _ -> receive_loop t)
            | _ -> receive_loop t)
          | _ -> (
            (* Handle other notifications *)
            Monitor.try_with (fun () -> return notif)
            >>= function
            | Ok notification ->
              let%bind () = received_notification t notification in
              let%bind () = handle_incoming t (`Notification notification) in
              receive_loop t
            | Error _exn -> receive_loop t))
        | `Response _resp ->
          (* In this simplified implementation, just continue *)
          receive_loop t
        | `Error _err ->
          (* In this simplified implementation, just continue *)
          receive_loop t))
end
