(** AT Protocol post creation operations *)

open! Core
open! Async
open Atproto_types

(** Detect URLs in text and create facets *)
let detect_url_facets text =
  (* Simple URL regex pattern *)
  let url_pattern = "https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(/[^\\s]*)?" in
  let url_regex = Re.Pcre.regexp url_pattern in

  (* Find all matches *)
  let matches = Re.all url_regex text in

  List.map matches ~f:(fun group ->
      let url = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in

      (* Calculate UTF-8 byte positions *)
      let byte_start = String.sub text ~pos:0 ~len:start_pos |> String.length in
      let byte_end = String.sub text ~pos:0 ~len:end_pos |> String.length in

      `Assoc
        [
          ( "index",
            `Assoc
              [ ("byteStart", `Int byte_start); ("byteEnd", `Int byte_end) ] );
          ( "features",
            `List
              [
                `Assoc
                  [
                    ("$type", `String "app.bsky.richtext.facet#link");
                    ("uri", `String url);
                  ];
              ] );
        ])

(** Detect hashtags in text and create facets *)
let detect_hashtag_facets text =
  (* Hashtag regex pattern - match leading space/start but capture only the
     tag *)
  let hashtag_pattern = "(?:^|\\s)(#[a-zA-Z0-9]+)" in
  let hashtag_regex = Re.Pcre.regexp hashtag_pattern in

  (* Find all matches *)
  let matches = Re.all hashtag_regex text in

  List.map matches ~f:(fun group ->
      let tag_with_hash = Re.Group.get group 1 in
      let tag = String.drop_prefix tag_with_hash 1 in
      (* Remove '#' *)
      let start_pos = Re.Group.start group 1 in
      let end_pos = Re.Group.stop group 1 in

      (* Calculate UTF-8 byte positions *)
      let byte_start = String.sub text ~pos:0 ~len:start_pos |> String.length in
      let byte_end = String.sub text ~pos:0 ~len:end_pos |> String.length in

      `Assoc
        [
          ( "index",
            `Assoc
              [ ("byteStart", `Int byte_start); ("byteEnd", `Int byte_end) ] );
          ( "features",
            `List
              [
                `Assoc
                  [
                    ("$type", `String "app.bsky.richtext.facet#tag");
                    ("tag", `String tag);
                  ];
              ] );
        ])

(** Build facets for links, mentions, and hashtags *)
let build_facets ~text ~links ~mentions =
  let url_facets = detect_url_facets text in
  let hashtag_facets = detect_hashtag_facets text in

  (* Add manual links *)
  let link_facets =
    match links with
    | None -> []
    | Some link_list ->
      List.filter_map link_list ~f:(fun (link : Types.rich_text_link) ->
          (* Find the link text in the original text *)
          match String.substr_index text ~pattern:link.text with
          | None -> None
          | Some pos ->
            let byte_start = String.sub text ~pos:0 ~len:pos |> String.length in
            let byte_end = byte_start + String.length link.text in
            Some
              (`Assoc
                [
                  ( "index",
                    `Assoc
                      [
                        ("byteStart", `Int byte_start);
                        ("byteEnd", `Int byte_end);
                      ] );
                  ( "features",
                    `List
                      [
                        `Assoc
                          [
                            ("$type", `String "app.bsky.richtext.facet#link");
                            ("uri", `String link.url);
                          ];
                      ] );
                ]))
  in

  (* Add mentions *)
  let mention_facets =
    match mentions with
    | None -> []
    | Some mention_list ->
      List.filter_map mention_list
        ~f:(fun (mention : Types.rich_text_mention) ->
          let display =
            Option.value mention.display_text ~default:("@" ^ mention.handle)
          in
          match String.substr_index text ~pattern:display with
          | None -> None
          | Some pos ->
            let byte_start = String.sub text ~pos:0 ~len:pos |> String.length in
            let byte_end = byte_start + String.length display in
            Some
              (`Assoc
                [
                  ( "index",
                    `Assoc
                      [
                        ("byteStart", `Int byte_start);
                        ("byteEnd", `Int byte_end);
                      ] );
                  ( "features",
                    `List
                      [
                        `Assoc
                          [
                            ("$type", `String "app.bsky.richtext.facet#mention");
                            ("did", `String mention.handle);
                            (* In production, resolve handle to DID *)
                          ];
                      ] );
                ]))
  in

  let all_facets = url_facets @ hashtag_facets @ link_facets @ mention_facets in
  if List.is_empty all_facets then None else Some (`List all_facets)

(** Create a basic text post **)
let create_post ~text ?images ?links ?mentions ?reply_to ?quote () :
    Types.post_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      match Client.get_session client with
      | None -> failwith "Not authenticated"
      | Some session ->
        (* Build facets for rich text *)
        let facets = build_facets ~text ~links ~mentions in

        (* Upload images if provided *)
        let%bind images_embed =
          match images with
          | None -> return None
          | Some img_params -> Images.create_images_embed img_params
        in

        (* Build embed field (images, quote, or both) *)
        let%bind embed_field =
          match (images_embed, quote) with
          | None, None -> return []
          | Some imgs, None ->
            return [ ("embed", Types.embed_to_yojson (Types.Images imgs)) ]
          | None, Some q ->
            let quote_embed = Images.create_quote_embed q in
            return
              [ ("embed", Types.embed_to_yojson (Types.Record quote_embed)) ]
          | Some imgs, Some q ->
            let quote_embed = Images.create_quote_embed q in
            return
              [
                ( "embed",
                  Types.embed_to_yojson
                    (Types.RecordWithMedia
                       { record = quote_embed; media = imgs }) );
              ]
        in

        (* Build reply field if replying *)
        let reply_field =
          match reply_to with
          | None -> []
          | Some r ->
            let reply_ref = Images.create_reply_ref r in
            let parent_uri, parent_cid = reply_ref.parent in
            let root_uri, root_cid = reply_ref.root in
            [
              ( "reply",
                `Assoc
                  [
                    ( "parent",
                      `Assoc
                        [
                          ("uri", `String parent_uri);
                          ("cid", `String parent_cid);
                        ] );
                    ( "root",
                      `Assoc
                        [ ("uri", `String root_uri); ("cid", `String root_cid) ]
                    );
                  ] );
            ]
        in

        (* Get current timestamp in ISO 8601 format *)
        let created_at =
          Time_ns.now ()
          |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
        in

        (* Create post record *)
        let record_fields =
          [
            ("$type", `String "app.bsky.feed.post");
            ("text", `String text);
            ("createdAt", `String created_at);
          ]
        in

        let record_fields =
          match facets with
          | None -> record_fields
          | Some f -> record_fields @ [ ("facets", f) ]
        in

        let record_fields = record_fields @ embed_field @ reply_field in
        let record = `Assoc record_fields in

        (* Call com.atproto.repo.createRecord *)
        let endpoint = "com.atproto.repo.createRecord" in
        let body =
          `Assoc
            [
              ("repo", `String session.did);
              ("collection", `String "app.bsky.feed.post");
              ("record", record);
            ]
        in

        let%bind json_str = Client.api_post client ~endpoint ~body in
        let json = Yojson.Safe.from_string json_str in

        return
          Types.
            {
              success = true;
              uri = Some Yojson.Safe.Util.(member "uri" json |> to_string);
              cid = Some Yojson.Safe.Util.(member "cid" json |> to_string);
              text = Some text;
              created_at = Some created_at;
              error = None;
            })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        uri = None;
        cid = None;
        text = None;
        created_at = None;
        error = Some (Exn.to_string exn);
      }

(** Create a thread of posts *)
let create_thread (posts : Types.thread_post list) :
    Types.thread_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      (* Create posts sequentially, each replying to the previous *)
      let%bind post_uris =
        Deferred.List.fold posts ~init:(None, [])
          ~f:(fun (_prev_uri, uris) thread_post ->
            (* For now, just create simple posts - reply_to would need full
               implementation *)
            let%bind result =
              create_post ~text:thread_post.text ?links:thread_post.links
                ?mentions:thread_post.mentions ()
            in
            match result.uri with
            | Some uri -> return (Some uri, uri :: uris)
            | None -> failwith "Failed to create post in thread")
      in

      let _, post_uris = post_uris in
      let post_uris = List.rev post_uris in

      return
        Types.
          {
            success = true;
            thread_uri = List.hd post_uris;
            post_uris;
            post_count = List.length post_uris;
            error = None;
          })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        thread_uri = None;
        post_uris = [];
        post_count = 0;
        error = Some (Exn.to_string exn);
      }
