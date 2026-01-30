(** AT Protocol read operations - timeline, notifications, search *)

open! Core
open! Async
open Atproto_types

(** Parse a post from API response *)
let parse_post json =
  let open Yojson.Safe.Util in
  Types.
    {
      author = member "author" json |> member "handle" |> to_string;
      text = member "record" json |> member "text" |> to_string_option;
      created_at =
        member "record" json |> member "createdAt" |> to_string_option;
      likes =
        member "likeCount" json |> to_int_option |> Option.value ~default:0;
      reposts =
        member "repostCount" json |> to_int_option |> Option.value ~default:0;
      replies =
        member "replyCount" json |> to_int_option |> Option.value ~default:0;
      uri = member "uri" json |> to_string;
      cid = member "cid" json |> to_string;
    }

(** Fetch user timeline *)
let fetch_timeline limit : Types.timeline_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      let endpoint = sprintf "app.bsky.feed.getTimeline?limit=%d" limit in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let feed = Yojson.Safe.Util.(member "feed" json |> to_list) in

      let posts =
        List.map feed ~f:(fun item ->
            let post = Yojson.Safe.Util.member "post" item in
            parse_post post)
      in

      return
        Types.{ success = true; count = List.length posts; posts; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        count = 0;
        posts = [];
        error = Some (Exn.to_string exn);
      }

(** Parse a notification from API response *)
let parse_notification json =
  let open Yojson.Safe.Util in
  Types.
    {
      reason = member "reason" json |> to_string;
      author = member "author" json |> member "handle" |> to_string_option;
      is_read = member "isRead" json |> to_bool;
      indexed_at = member "indexedAt" json |> to_string;
      uri = member "uri" json |> to_string;
      cid = member "cid" json |> to_string;
    }

(** Fetch notifications *)
let fetch_notifications limit : Types.notifications_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      let endpoint =
        sprintf "app.bsky.notification.listNotifications?limit=%d" limit
      in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let notifications_json =
        Yojson.Safe.Util.(member "notifications" json |> to_list)
      in

      let notifications = List.map notifications_json ~f:parse_notification in

      return
        Types.
          {
            success = true;
            count = List.length notifications;
            notifications;
            error = None;
          })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        count = 0;
        notifications = [];
        error = Some (Exn.to_string exn);
      }

(** Search posts *)
let search_posts ~query ~limit : Types.search_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      (* URL encode the query *)
      let encoded_query = Uri.pct_encode query in
      let endpoint =
        sprintf "app.bsky.feed.searchPosts?q=%s&limit=%d" encoded_query limit
      in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let posts_json = Yojson.Safe.Util.(member "posts" json |> to_list) in

      let posts = List.map posts_json ~f:parse_post in

      return
        Types.
          {
            success = true;
            query;
            count = List.length posts;
            posts;
            error = None;
          })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        query;
        count = 0;
        posts = [];
        error = Some (Exn.to_string exn);
      }

(** Fetch author feed *)
let fetch_author_feed ~actor ~limit : Types.author_feed_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      let endpoint =
        sprintf "app.bsky.feed.getAuthorFeed?actor=%s&limit=%d" actor limit
      in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let feed = Yojson.Safe.Util.(member "feed" json |> to_list) in
      let posts =
        List.map feed ~f:(fun item ->
            let post = Yojson.Safe.Util.member "post" item in
            parse_post post)
      in
      return
        Types.{ success = true; count = List.length posts; posts; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        count = 0;
        posts = [];
        error = Some (Exn.to_string exn);
      }

(** Fetch custom feed (app.bsky.feed.getFeed) *)
let fetch_custom_feed ~feed_uri ~limit : Types.timeline_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      let encoded_feed_uri = Uri.pct_encode feed_uri in
      let endpoint =
        sprintf "app.bsky.feed.getFeed?feed=%s&limit=%d" encoded_feed_uri limit
      in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let feed = Yojson.Safe.Util.(member "feed" json |> to_list) in

      let posts =
        List.map feed ~f:(fun item ->
            let post = Yojson.Safe.Util.member "post" item in
            parse_post post)
      in

      return
        Types.{ success = true; count = List.length posts; posts; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.
      {
        success = false;
        count = 0;
        posts = [];
        error = Some (Exn.to_string exn);
      }

(** Parse thread view recursively *)
let rec parse_thread_view json : Types.thread_view_post option =
  let open Yojson.Safe.Util in
  try
    let type_ = member "$type" json |> to_string_option in
    match type_ with
    | Some "app.bsky.feed.defs#threadViewPost" ->
      let post_json = member "post" json in
      let post = parse_post post_json in

      let parent =
        match member "parent" json with
        | `Null -> None
        | parent_json -> parse_thread_view parent_json
      in

      let replies =
        match member "replies" json with
        | `List replies_json ->
          Some (List.filter_map replies_json ~f:parse_thread_view)
        | _ -> None
      in

      Some Types.{ post; parent; replies }
    | _ -> None (* Skip blocked/not-found posts which have different types *)
  with _ -> None

(** Fetch post thread *)
let fetch_post_thread ~uri : Types.post_thread_result Deferred.t =
  let open Deferred.Let_syntax in
  Monitor.try_with (fun () ->
      let settings = Settings.get_settings () in
      let%bind client = Client.get_client settings in

      (* Encoding URI for query param *)
      let encoded_uri = Uri.pct_encode uri in
      let endpoint = sprintf "app.bsky.feed.getPostThread?uri=%s" encoded_uri in
      let%bind json_str = Client.api_get client ~endpoint in

      let json = Yojson.Safe.from_string json_str in
      let thread_json = Yojson.Safe.Util.member "thread" json in
      let thread = parse_thread_view thread_json in

      return Types.{ success = true; thread; error = None })
  >>| function
  | Ok result -> result
  | Error exn ->
    Types.{ success = false; thread = None; error = Some (Exn.to_string exn) }

(** Get a single post (wrapper around fetch_post_thread) *)
let get_post ~uri : Types.post_result Deferred.t =
  let open Deferred.Let_syntax in
  match%bind fetch_post_thread ~uri with
  | { success = true; thread = Some thread_view; _ } ->
    return
      Types.
        {
          success = true;
          uri = Some thread_view.post.uri;
          cid = Some thread_view.post.cid;
          text = thread_view.post.text;
          created_at = thread_view.post.created_at;
          error = None;
        }
  | { error = Some err; _ } ->
    return
      Types.
        {
          success = false;
          uri = None;
          cid = None;
          text = None;
          created_at = None;
          error = Some err;
        }
  | _ ->
    return
      Types.
        {
          success = false;
          uri = None;
          cid = None;
          text = None;
          created_at = None;
          error = Some "Post not found";
        }
