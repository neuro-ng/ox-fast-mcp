(** AT Protocol profile operations *)

open! Core
open! Async
open Atproto_types

(** Get current user profile information *)
let get_profile_info () : Types.profile_info Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Call app.bsky.actor.getProfile *)
        let endpoint =
          sprintf "app.bsky.actor.getProfile?actor=%s" session.did
        in
        let%bind json_str = Client.api_get client ~endpoint in

        let json = Yojson.Safe.from_string json_str in
        let open Yojson.Safe.Util in
        return
          Types.
            {
              connected = true;
              handle = Some (member "handle" json |> to_string);
              display_name = member "displayName" json |> to_string_option;
              did = Some session.did;
              followers = member "followersCount" json |> to_int_option;
              following = member "followsCount" json |> to_int_option;
              posts = member "postsCount" json |> to_int_option;
              error = None;
            })
  >>| function
  | Ok profile -> profile
  | Error exn ->
    Types.
      {
        connected = false;
        handle = None;
        display_name = None;
        did = None;
        followers = None;
        following = None;
        posts = None;
        error = Some (Exn.to_string exn);
      }

(** Update current user's profile **)
let update_profile (params : Types.profile_update_param) :
    Types.profile_update_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Upload avatar if provided *)
        let%bind avatar_blob =
          match params.avatar_path with
          | None -> return None
          | Some path ->
            let%bind blob_ref = Images.upload_image_file ~path in
            return (Some blob_ref)
        in

        (* Build profile record fields *)
        let fields = ref [] in
        fields := ("$type", `String "app.bsky.actor.profile") :: !fields;

        let updated_fields = ref [] in

        (match params.display_name with
        | Some name ->
          fields := ("displayName", `String name) :: !fields;
          updated_fields := "display_name" :: !updated_fields
        | None -> ());

        (match params.description with
        | Some desc ->
          fields := ("description", `String desc) :: !fields;
          updated_fields := "description" :: !updated_fields
        | None -> ());

        (match avatar_blob with
        | Some blob ->
          let avatar_json =
            `Assoc
              [
                ("$type", `String "blob");
                ("ref", `Assoc [ ("$link", `String blob.ref_link) ]);
                ("mimeType", `String blob.mime_type);
                ("size", `Int blob.size);
              ]
          in
          fields := ("avatar", avatar_json) :: !fields;
          updated_fields := "avatar" :: !updated_fields
        | None -> ());

        (* Create profile update request *)
        let record = `Assoc !fields in
        let endpoint = "com.atproto.repo.putRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.actor.profile");
              ("rkey", `String "self");
              ("record", record);
            ]
        in

        let%bind _response = Client.api_post client ~endpoint ~body in
        return
          Types.
            { success = true; updated_fields = !updated_fields; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      { success = false; updated_fields = []; error = Some (Exn.to_string exn) }

(** Get profile by handle **)
let get_profile_by_handle (handle : string) :
    Types.profile_query_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      let endpoint = sprintf "app.bsky.actor.getProfile?actor=%s" handle in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      let profile =
        Types.
          {
            did = member "did" json |> to_string;
            handle = member "handle" json |> to_string;
            display_name = member "displayName" json |> to_string_option;
            description = member "description" json |> to_string_option;
            avatar = member "avatar" json |> to_string_option;
            banner = member "banner" json |> to_string_option;
            followers_count =
              member "followersCount" json
              |> to_int_option |> Option.value ~default:0;
            follows_count =
              member "followsCount" json |> to_int_option
              |> Option.value ~default:0;
            posts_count =
              member "postsCount" json |> to_int_option
              |> Option.value ~default:0;
          }
      in
      return Types.{ success = true; profile = Some profile; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.{ success = false; profile = None; error = Some (Exn.to_string exn) }

(** Get profile by DID **)
let get_profile_by_did (did : string) : Types.profile_query_result Deferred.t =
  get_profile_by_handle did
(* DID works the same as handle in getProfile *)
