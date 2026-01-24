(** AT Protocol social operations - follow, like, repost *)

open! Core
open! Async
open Atproto_types

(** Follow a user by handle *)
let follow_user_by_handle handle : Types.follow_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* First resolve handle to DID *)
        let resolve_endpoint =
          sprintf "com.atproto.identity.resolveHandle?handle=%s" handle
        in
        let%bind did_json = Client.api_get client ~endpoint:resolve_endpoint in
        let json = Yojson.Safe.from_string did_json in
        let did = Yojson.Safe.Util.(member "did" json |> to_string) in

        (* Get current timestamp *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create follow record *)
        let record =
          `Assoc
            [
              ("$type", `String "app.bsky.graph.follow");
              ("subject", `String did);
              ("createdAt", `String created_at);
            ]
        in

        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.graph.follow");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          (Types.
             {
               success = true;
               handle = Some handle;
               did = Some did;
               uri = Some Yojson.Safe.Util.(member "uri" json |> to_string);
               error = None;
             }
            : Types.follow_result))
  >>| function
  | Ok result -> result
  | Error exn ->
    (Types.
       {
         success = false;
         handle = None;
         did = None;
         uri = None;
         error = Some (Exn.to_string exn);
       }
      : Types.follow_result)

(** Like a post *)
let like_post ~uri ~cid : Types.like_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Get current timestamp *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create like record *)
        let record =
          `Assoc
            [
              ("$type", `String "app.bsky.feed.like");
              ("subject", `Assoc [ ("uri", `String uri); ("cid", `String cid) ]);
              ("createdAt", `String created_at);
            ]
        in

        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.feed.like");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          Types.
            {
              success = true;
              liked_uri = Some uri;
              like_uri = Some Yojson.Safe.Util.(member "uri" json |> to_string);
              error = None;
            })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        liked_uri = None;
        like_uri = None;
        error = Some (Exn.to_string exn);
      }

(** Repost a post *)
let repost ~uri ~cid : Types.repost_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Get current timestamp *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create repost record *)
        let record =
          `Assoc
            [
              ("$type", `String "app.bsky.feed.repost");
              ("subject", `Assoc [ ("uri", `String uri); ("cid", `String cid) ]);
              ("createdAt", `String created_at);
            ]
        in

        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.feed.repost");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          Types.
            {
              success = true;
              reposted_uri = Some uri;
              repost_uri =
                Some Yojson.Safe.Util.(member "uri" json |> to_string);
              error = None;
            })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        reposted_uri = None;
        repost_uri = None;
        error = Some (Exn.to_string exn);
      }

(** Unfollow a user by DID **)
let unfollow_user (did : string) : Types.delete_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session -> (
        (* List follows to find the record *)
        let list_endpoint =
          sprintf "com.atproto.repo.listRecords?repo=%s&collection=%s"
            session.did "app.bsky.graph.follow"
        in
        let%bind json_str = Client.api_get client ~endpoint:list_endpoint in
        let json = Yojson.Safe.from_string json_str in

        (* Find the follow record for this DID *)
        let records = Yojson.Safe.Util.(member "records" json |> to_list) in
        let matching_record =
          List.find records ~f:(fun record ->
              let subject =
                Yojson.Safe.Util.(
                  member "value" record |> member "subject" |> to_string)
              in
              String.equal subject did)
        in

        match matching_record with
        | None ->
          return
            Types.
              {
                success = false;
                record_key = None;
                error = Some "Follow record not found";
              }
        | Some record ->
          (* Extract rkey from URI *)
          let uri = Yojson.Safe.Util.(member "uri" record |> to_string) in
          let rkey =
            match String.rsplit2 uri ~on:'/' with
            | Some (_, key) -> key
            | None -> failwith "Invalid URI format"
          in

          (* Delete the follow record *)
          let endpoint = "com.atproto.repo.deleteRecord" in
          let body =
            `Assoc
              [
                ("repo", `String session.did);
                ("collection", `String "app.bsky.graph.follow");
                ("rkey", `String rkey);
              ]
          in

          let%bind _response = Client.api_post client ~endpoint ~body in
          return Types.{ success = true; record_key = Some rkey; error = None }))
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; record_key = None; error = Some (Exn.to_string exn) }

(** Unlike a post **)
let unlike_post ~uri ~cid : Types.delete_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session -> (
        (* List likes to find the record *)
        let list_endpoint =
          sprintf "com.atproto.repo.listRecords?repo=%s&collection=%s"
            session.did "app.bsky.feed.like"
        in
        let%bind json_str = Client.api_get client ~endpoint:list_endpoint in
        let json = Yojson.Safe.from_string json_str in

        (* Find the like record for this post *)
        let records = Yojson.Safe.Util.(member "records" json |> to_list) in
        let matching_record =
          List.find records ~f:(fun record ->
              let subject =
                Yojson.Safe.Util.(member "value" record |> member "subject")
              in
              let subject_uri =
                Yojson.Safe.Util.(member "uri" subject |> to_string)
              in
              let subject_cid =
                Yojson.Safe.Util.(member "cid" subject |> to_string)
              in
              String.equal subject_uri uri && String.equal subject_cid cid)
        in

        match matching_record with
        | None ->
          return
            Types.
              {
                success = false;
                record_key = None;
                error = Some "Like record not found";
              }
        | Some record ->
          (* Extract rkey from URI *)
          let record_uri =
            Yojson.Safe.Util.(member "uri" record |> to_string)
          in
          let rkey =
            match String.rsplit2 record_uri ~on:'/' with
            | Some (_, key) -> key
            | None -> failwith "Invalid URI format"
          in

          (* Delete the like record *)
          let endpoint = "com.atproto.repo.deleteRecord" in
          let body =
            `Assoc
              [
                ("repo", `String session.did);
                ("collection", `String "app.bsky.feed.like");
                ("rkey", `String rkey);
              ]
          in

          let%bind _response = Client.api_post client ~endpoint ~body in
          return Types.{ success = true; record_key = Some rkey; error = None }))
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; record_key = None; error = Some (Exn.to_string exn) }

(** Unrepost a post **)
let unrepost ~uri ~cid : Types.delete_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session -> (
        (* List reposts to find the record *)
        let list_endpoint =
          sprintf "com.atproto.repo.listRecords?repo=%s&collection=%s"
            session.did "app.bsky.feed.repost"
        in
        let%bind json_str = Client.api_get client ~endpoint:list_endpoint in
        let json = Yojson.Safe.from_string json_str in

        (* Find the repost record for this post *)
        let records = Yojson.Safe.Util.(member "records" json |> to_list) in
        let matching_record =
          List.find records ~f:(fun record ->
              let subject =
                Yojson.Safe.Util.(member "value" record |> member "subject")
              in
              let subject_uri =
                Yojson.Safe.Util.(member "uri" subject |> to_string)
              in
              let subject_cid =
                Yojson.Safe.Util.(member "cid" subject |> to_string)
              in
              String.equal subject_uri uri && String.equal subject_cid cid)
        in

        match matching_record with
        | None ->
          return
            Types.
              {
                success = false;
                record_key = None;
                error = Some "Repost record not found";
              }
        | Some record ->
          (* Extract rkey from URI *)
          let record_uri =
            Yojson.Safe.Util.(member "uri" record |> to_string)
          in
          let rkey =
            match String.rsplit2 record_uri ~on:'/' with
            | Some (_, key) -> key
            | None -> failwith "Invalid URI format"
          in

          (* Delete the repost record *)
          let endpoint = "com.atproto.repo.deleteRecord" in
          let body =
            `Assoc
              [
                ("repo", `String session.did);
                ("collection", `String "app.bsky.feed.repost");
                ("rkey", `String rkey);
              ]
          in

          let%bind _response = Client.api_post client ~endpoint ~body in
          return Types.{ success = true; record_key = Some rkey; error = None }))
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; record_key = None; error = Some (Exn.to_string exn) }

(** Mute a user by handle *)
let mute_user handle : Types.mute_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Resolve handle to DID *)
        let resolve_endpoint =
          sprintf "com.atproto.identity.resolveHandle?handle=%s" handle
        in
        let%bind did_json = Client.api_get client ~endpoint:resolve_endpoint in
        let json = Yojson.Safe.from_string did_json in
        let did = Yojson.Safe.Util.(member "did" json |> to_string) in

        (* Get current timestamp *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create mute record *)
        let record =
          `Assoc
            [
              ("$type", `String "app.bsky.graph.mute");
              ("subject", `String did);
              ("createdAt", `String created_at);
            ]
        in

        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.graph.mute");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          (Types.
             {
               success = true;
               handle = Some handle;
               did = Some did;
               uri = Some Yojson.Safe.Util.(member "uri" json |> to_string);
               error = None;
             }
            : Types.mute_result))
  >>| function
  | Ok result -> result
  | Error exn ->
    (Types.
       {
         success = false;
         handle = None;
         did = None;
         uri = None;
         error = Some (Exn.to_string exn);
       }
      : Types.mute_result)

(** Unmute a user by DID **)
let unmute_user (did : string) : Types.delete_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session -> (
        (* List mutes to find the record *)
        let list_endpoint =
          sprintf "com.atproto.repo.listRecords?repo=%s&collection=%s"
            session.did "app.bsky.graph.mute"
        in
        let%bind json_str = Client.api_get client ~endpoint:list_endpoint in
        let json = Yojson.Safe.from_string json_str in

        (* Find the mute record for this DID *)
        let records = Yojson.Safe.Util.(member "records" json |> to_list) in
        let matching_record =
          List.find records ~f:(fun record ->
              let subject =
                Yojson.Safe.Util.(
                  member "value" record |> member "subject" |> to_string)
              in
              String.equal subject did)
        in

        match matching_record with
        | None ->
          return
            Types.
              {
                success = false;
                record_key = None;
                error = Some "Mute record not found";
              }
        | Some record ->
          (* Extract rkey from URI *)
          let uri = Yojson.Safe.Util.(member "uri" record |> to_string) in
          let rkey =
            match String.rsplit2 uri ~on:'/' with
            | Some (_, key) -> key
            | None -> failwith "Invalid URI format"
          in

          (* Delete the mute record *)
          let endpoint = "com.atproto.repo.deleteRecord" in
          let body =
            `Assoc
              [
                ("repo", `String session.did);
                ("collection", `String "app.bsky.graph.mute");
                ("rkey", `String rkey);
              ]
          in

          let%bind _response = Client.api_post client ~endpoint ~body in
          return Types.{ success = true; record_key = Some rkey; error = None }))
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; record_key = None; error = Some (Exn.to_string exn) }

(** Block a user by handle *)
let block_user handle : Types.block_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Resolve handle to DID *)
        let resolve_endpoint =
          sprintf "com.atproto.identity.resolveHandle?handle=%s" handle
        in
        let%bind did_json = Client.api_get client ~endpoint:resolve_endpoint in
        let json = Yojson.Safe.from_string did_json in
        let did = Yojson.Safe.Util.(member "did" json |> to_string) in

        (* Get current timestamp *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create block record *)
        let record =
          `Assoc
            [
              ("$type", `String "app.bsky.graph.block");
              ("subject", `String did);
              ("createdAt", `String created_at);
            ]
        in

        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.graph.block");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          Types.
            {
              success = true;
              handle = Some handle;
              did = Some did;
              uri = Some Yojson.Safe.Util.(member "uri" json |> to_string);
              error = None;
            })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        handle = None;
        did = None;
        uri = None;
        error = Some (Exn.to_string exn);
      }

(** Unblock a user by DID **)
let unblock_user (did : string) : Types.delete_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session -> (
        (* List blocks to find the record *)
        let list_endpoint =
          sprintf "com.atproto.repo.listRecords?repo=%s&collection=%s"
            session.did "app.bsky.graph.block"
        in
        let%bind json_str = Client.api_get client ~endpoint:list_endpoint in
        let json = Yojson.Safe.from_string json_str in

        (* Find the block record for this DID *)
        let records = Yojson.Safe.Util.(member "records" json |> to_list) in
        let matching_record =
          List.find records ~f:(fun record ->
              let subject =
                Yojson.Safe.Util.(
                  member "value" record |> member "subject" |> to_string)
              in
              String.equal subject did)
        in

        match matching_record with
        | None ->
          return
            Types.
              {
                success = false;
                record_key = None;
                error = Some "Block record not found";
              }
        | Some record ->
          (* Extract rkey from URI *)
          let uri = Yojson.Safe.Util.(member "uri" record |> to_string) in
          let rkey =
            match String.rsplit2 uri ~on:'/' with
            | Some (_, key) -> key
            | None -> failwith "Invalid URI format"
          in

          (* Delete the block record *)
          let endpoint = "com.atproto.repo.deleteRecord" in
          let body =
            `Assoc
              [
                ("repo", `String session.did);
                ("collection", `String "app.bsky.graph.block");
                ("rkey", `String rkey);
              ]
          in

          let%bind _response = Client.api_post client ~endpoint ~body in
          return Types.{ success = true; record_key = Some rkey; error = None }))
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; record_key = None; error = Some (Exn.to_string exn) }
