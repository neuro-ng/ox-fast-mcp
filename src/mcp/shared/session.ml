open Core
open Lwt.Syntax
open Mcp.Types

type progress_fn = float -> float option -> string option -> unit Lwt.t

module Request_responder = struct
  type ('req, 'res) t = {
    request_id : request_id;
    request_meta : meta option;
    request : 'req;
    message_metadata : Message.message_metadata option;
    mutable completed : bool;
    mutable cancel_scope : unit -> unit;
  }

  let[@ocaml.warning "-16"] create ~request_id ?request_meta ~request ?message_metadata ~on_complete =
    { request_id; request_meta; request; message_metadata; completed = false; cancel_scope = (fun () -> ()) }

  let with_responder t f =
    let old_cancel_scope = t.cancel_scope in
    let cancelled = ref false in
    t.cancel_scope <- (fun () -> cancelled := true);

    let* result =
      Lwt.catch
        (fun () -> f t)
        (fun exn ->
          if not !cancelled then t.completed <- true;
          Lwt.fail exn)
    in

    t.cancel_scope <- old_cancel_scope;
    Lwt.return result

  let respond t response =
    if t.completed then Lwt.fail_with "Request already responded to"
    else (
      t.completed <- true;
      Lwt.return_unit)

  let cancel t =
    t.cancel_scope ();
    t.completed <- true;
    Lwt.return_unit

  let in_flight t = (not t.completed)
  let is_cancelled t = false
end

module Base_session = struct
  type response_stream = {
    send : jsonrpc_message -> unit Lwt.t;
    close : unit -> unit Lwt.t;
  }

  type ('send_req, 'send_notif, 'send_res, 'recv_req, 'recv_notif) t = {
    read_stream : Message.session_message Lwt_stream.t;
    write_stream : Message.session_message -> unit Lwt.t;
    mutable request_id : int;
    receive_request_type : 'recv_req;
    receive_notification_type : 'recv_notif;
    read_timeout : float option;
    mutable in_flight : ('recv_req, 'send_res) Request_responder.t list;
    mutable progress_callbacks : (string, progress_fn) Hashtbl.t;
    mutable response_streams : (request_id, response_stream) Hashtbl.t;
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
      response_streams = Hashtbl.create (module struct
        type t = request_id
        let compare (a : t) (b : t) = Poly.compare a b
        let hash = Hashtbl.hash
        let sexp_of_t _ = Sexp.Atom "request_id"
        let t_of_sexp _ = `Int 0
      end);
    }

  let with_session t f =
    let* result = f t in
    (* Cleanup *)
    List.iter t.in_flight ~f:(fun resp ->
        if Request_responder.in_flight resp then
          Lwt.async (fun () -> Request_responder.cancel resp));
    Hashtbl.clear t.progress_callbacks;
    (* Close all response streams *)
    Hashtbl.iter t.response_streams ~f:(fun stream -> Lwt.async stream.close);
    Hashtbl.clear t.response_streams;
    Lwt.return result

  let send_error t request_id error =
    let jsonrpc_error = { jsonrpc = "2.0"; id = `Int request_id; error } in
    let message = Message.{ message = (`Error jsonrpc_error); metadata = None } in
    t.write_stream message

  let send_response t request_id response =
    let jsonrpc_response =
      { jsonrpc = "2.0"; id = `Int request_id; result = response }
    in
    let message = Message.{ message = (`Response jsonrpc_response); metadata = None } in
    t.write_stream message

  let send_request t request ?request_read_timeout ?metadata ?progress_callback
      () =
    let request_id = t.request_id in
    t.request_id <- request_id + 1;

    (* Create response stream *)
    let response_promise, resolver = Lwt.wait () in
    let stream =
      {
        send =
          (fun msg ->
            Lwt.wakeup_later resolver msg;
            Lwt.return_unit);
        close =
          (fun () ->
            if not (Lwt.is_sleeping response_promise) then
              Lwt.wakeup_later_exn resolver (Failure "Stream closed");
            Lwt.return_unit);
      }
    in
    Hashtbl.add_exn t.response_streams ~key:(`Int request_id) ~data:stream;

    (* Create JSONRPC request *)
    let jsonrpc_request =
      {
        jsonrpc = "2.0";
        id = `Int request_id;
        method_ = "request";
        (* This would need to be properly set based on request type *)
        params = None;
        (* This would need proper conversion *)
      }
    in

    (* Add progress token if callback provided *)
    let* () =
      match progress_callback with
      | Some cb ->
         Hashtbl.add_exn t.progress_callbacks ~key:(Int.to_string request_id) ~data:cb;
        Lwt.return_unit
      | None -> Lwt.return_unit
    in

    (* Send request *)
    let message = Message.{ message = (`Request jsonrpc_request); metadata } in
    let* () = t.write_stream message in

    (* Wait for response with timeout *)
    let timeout = Option.value request_read_timeout ~default:t.read_timeout in
    match timeout with
    | Some t ->
      let* result = Lwt_unix.with_timeout t (fun () -> response_promise) in
      Lwt.return result
    | None -> response_promise

  let send_notification t notification ?related_request_id () =
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

    let message = Message.{ message = (`Notification jsonrpc_notification); metadata } in
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
                ("progressToken", `String progress_token);
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

    let message = Message.{ message = (`Notification notification); metadata = None } in
    t.write_stream message

  let received_request _t _responder = Lwt.return_unit
  let received_notification _t _notification = Lwt.return_unit

  let handle_incoming _t = function
    | `Request _responder -> Lwt.return_unit
    | `Notification _notification -> Lwt.return_unit
    | `Error _exn -> Lwt.return_unit

  let rec receive_loop t =
    try%lwt
      let* message = Lwt_stream.get t.read_stream in
      match message with
      | None -> Lwt.return_unit (* Stream closed *)
      | Some message -> (
        match message.Message.message with
        | Request req -> (
          (* Validate and handle request *)
          try%lwt
            let validated_request = t.receive_request_type req in
            let responder =
              Request_responder.create ~request_id:req.id
                ~request_meta:(Option.bind req.params (fun p -> p.meta))
                ~request:validated_request ~message_metadata:message.metadata
                ~on_complete:(fun r ->
                  t.in_flight <-
                    List.filter t.in_flight ~f:(fun r' ->
                        r'.request_id <> r.request_id);
                  Lwt.return_unit)
            in
            t.in_flight <- responder :: t.in_flight;
            let* () = received_request t responder in
            if not responder.completed then
              let* () = handle_incoming t (`Request responder) in
              receive_loop t
            else receive_loop t
          with exn ->
            let* () =
              send_error t req.id
                {
                  code = invalid_params;
                  message = "Invalid request parameters";
                  data = None;
                }
            in
            receive_loop t)
        | Notification notif -> (
          (* Handle cancellation notifications *)
          match notif.method_ with
          | "$/cancelled" -> (
            match notif.params with
            | Some (`Assoc [ ("requestId", `String id) ]) -> (
              match
                List.find t.in_flight ~f:(fun r -> String.equal r.request_id id)
              with
              | Some req ->
                let* () = Request_responder.cancel req in
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
                  let* () = callback progress total message in
                  receive_loop t
                | None -> receive_loop t)
              | _ -> receive_loop t)
            | _ -> receive_loop t)
          | _ -> (
            (* Handle other notifications *)
            try%lwt
              let notification = t.receive_notification_type notif in
              let* () = received_notification t notification in
              let* () = handle_incoming t (`Notification notification) in
              receive_loop t
            with _ -> receive_loop t))
        | Response resp -> (
          (* Handle response *)
          match Hashtbl.find t.response_streams resp.id with
          | Some stream ->
            let* () = stream.send resp in
            Hashtbl.remove t.response_streams resp.id;
            receive_loop t
          | None ->
            let* () =
              handle_incoming t
                (`Error
                  (Failure
                     (sprintf "Received response with unknown request ID: %s"
                        (Int.to_string resp.id))))
            in
            receive_loop t)
        | Error err -> (
          (* Handle error response *)
           match Hashtbl.find t.response_streams err.id with
          | Some stream ->
            let* () = stream.send err in
            Hashtbl.remove t.response_streams err.id;
            receive_loop t
          | None -> receive_loop t))
    with
    | End_of_file -> Lwt.return_unit (* Stream closed normally *)
    | exn ->
      (* Log error and continue *)
      let* () =
        Logs_lwt.err (fun m ->
            m "Error in receive loop: %s" (Exn.to_string exn))
      in
      receive_loop t
end
