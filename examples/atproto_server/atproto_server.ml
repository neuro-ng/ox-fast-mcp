(** ATProto MCP Server - Real Implementation

    This is a functional MCP server for Bluesky/AT Protocol integration. Uses
    the atproto library to make real API calls.

    Run with: dune exec atproto-server

    Required environment variables:
    - ATPROTO_HANDLE: Your Bluesky handle (e.g., "user.bsky.social")
    - ATPROTO_PASSWORD: Your Bluesky app password
    - ATPROTO_PDS_URL: PDS URL (optional, defaults to "https://bsky.social") *)

open! Core
open! Async
open Atproto_types
module Types = Atproto_types.Types

let main () =
  let open Deferred.Let_syntax in
  (* Load settings from environment *)
  let settings = Settings.get_settings () in

  (* Create MCP server *)
  let server =
    Server.Ox_fast_mcp.create ~name:"atproto-mcp-server" ~version:"0.2.0"
      ~instructions:"ATProto MCP Server for Bluesky interactions" ()
  in

  (* Add resources *)
  Server.Ox_fast_mcp.add_simple_resource ~uri:"atproto://profile/status"
    ~name:"Profile Status"
    ~description:"Check ATProto connection and current user profile"
    ~mime_type:"application/json"
    ~reader:(fun () ->
      let%bind profile = Atproto.Profile.get_profile_info () in
      return (Types.profile_info_to_yojson profile |> Yojson.Safe.to_string))
    server;

  Server.Ox_fast_mcp.add_simple_resource ~uri:"atproto://timeline"
    ~name:"Timeline Feed" ~description:"Get authenticated user's timeline feed"
    ~mime_type:"application/json"
    ~reader:(fun () ->
      let%bind timeline =
        Atproto.Read.fetch_timeline settings.timeline_default_limit
      in
      return (Types.timeline_result_to_yojson timeline |> Yojson.Safe.to_string))
    server;

  Server.Ox_fast_mcp.add_simple_resource ~uri:"atproto://notifications"
    ~name:"Notifications" ~description:"Get recent notifications"
    ~mime_type:"application/json"
    ~reader:(fun () ->
      let%bind notifs =
        Atproto.Read.fetch_notifications settings.notifications_default_limit
      in
      return
        (Types.notifications_result_to_yojson notifs |> Yojson.Safe.to_string))
    server;

  (* Add tools *)

  (* Profile management tools *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"update_profile"
    ~description:"Update your Bluesky profile (display name, bio, avatar)"
    ~handler:(fun params ->
      let display_name =
        Yojson.Safe.Util.member "display_name" params
        |> Yojson.Safe.Util.to_string_option
      in
      let description =
        Yojson.Safe.Util.member "description" params
        |> Yojson.Safe.Util.to_string_option
      in
      let avatar_path =
        Yojson.Safe.Util.member "avatar_path" params
        |> Yojson.Safe.Util.to_string_option
      in
      let update_params = Types.{ display_name; description; avatar_path } in
      let%bind result = Atproto.Profile.update_profile update_params in
      return (Types.profile_update_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"get_profile"
    ~description:"Get detailed profile information by handle or DID"
    ~handler:(fun params ->
      let actor =
        Yojson.Safe.Util.member "actor" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Profile.get_profile_by_handle actor in
      return (Types.profile_query_result_to_yojson result))
    server;

  (* Add tools *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"post"
    ~description:
      "Create a post on Bluesky with optional rich text features, images, \
       replies, and quotes"
    ~handler:(fun params ->
      let text =
        Yojson.Safe.Util.member "text" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let links =
        try
          let links_json =
            Yojson.Safe.Util.member "links" params |> Yojson.Safe.Util.to_list
          in
          Some
            (List.filter_map links_json ~f:(fun link_json ->
                 try
                   Some
                     Types.
                       {
                         text =
                           Yojson.Safe.Util.member "text" link_json
                           |> Yojson.Safe.Util.to_string;
                         url =
                           Yojson.Safe.Util.member "url" link_json
                           |> Yojson.Safe.Util.to_string;
                       }
                 with _ -> None))
        with _ -> None
      in
      (* Parse images *)
      let images =
        try
          let images_json =
            Yojson.Safe.Util.member "images" params |> Yojson.Safe.Util.to_list
          in
          Some
            (List.filter_map images_json ~f:(fun img_json ->
                 try
                   Some
                     Types.
                       {
                         path =
                           Yojson.Safe.Util.member "path" img_json
                           |> Yojson.Safe.Util.to_string;
                         alt_text =
                           Yojson.Safe.Util.member "alt_text" img_json
                           |> Yojson.Safe.Util.to_string_option;
                       }
                 with _ -> None))
        with _ -> None
      in
      (* Parse reply_to *)
      let reply_to =
        try
          let reply_json = Yojson.Safe.Util.member "reply_to" params in
          Some
            Types.
              {
                parent_uri =
                  Yojson.Safe.Util.member "parent_uri" reply_json
                  |> Yojson.Safe.Util.to_string;
                parent_cid =
                  Yojson.Safe.Util.member "parent_cid" reply_json
                  |> Yojson.Safe.Util.to_string;
                root_uri =
                  Yojson.Safe.Util.member "root_uri" reply_json
                  |> Yojson.Safe.Util.to_string_option;
                root_cid =
                  Yojson.Safe.Util.member "root_cid" reply_json
                  |> Yojson.Safe.Util.to_string_option;
              }
        with _ -> None
      in
      (* Parse quote *)
      let quote =
        try
          let quote_json = Yojson.Safe.Util.member "quote" params in
          Some
            Types.
              {
                uri =
                  Yojson.Safe.Util.member "uri" quote_json
                  |> Yojson.Safe.Util.to_string;
                cid =
                  Yojson.Safe.Util.member "cid" quote_json
                  |> Yojson.Safe.Util.to_string;
              }
        with _ -> None
      in
      let%bind result =
        Atproto.Posts.create_post ~text ?links ?images ?reply_to ?quote ()
      in
      return (Types.post_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"search"
    ~description:"Search for posts on Bluesky"
    ~handler:(fun params ->
      let query =
        Yojson.Safe.Util.member "query" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let limit =
        Yojson.Safe.Util.member "limit" params
        |> Yojson.Safe.Util.to_int_option
        |> Option.value ~default:settings.search_default_limit
      in
      let%bind result = Atproto.Read.search_posts ~query ~limit in
      return (Types.search_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"follow"
    ~description:"Follow a user by their handle"
    ~handler:(fun params ->
      let handle =
        Yojson.Safe.Util.member "handle" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Social.follow_user_by_handle handle in
      return (Types.follow_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"like" ~description:"Like a post"
    ~handler:(fun params ->
      let uri =
        Yojson.Safe.Util.member "uri" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let cid =
        Yojson.Safe.Util.member "cid" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Social.like_post ~uri ~cid in
      return (Types.like_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"repost" ~description:"Repost a post"
    ~handler:(fun params ->
      let uri =
        Yojson.Safe.Util.member "uri" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let cid =
        Yojson.Safe.Util.member "cid" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Social.repost ~uri ~cid in
      return (Types.repost_result_to_yojson result))
    server;

  (* Reverse social actions *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"unfollow"
    ~description:"Unfollow a user by handle or DID"
    ~handler:(fun params ->
      let actor =
        Yojson.Safe.Util.member "actor" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      (* Resolve handle to DID if needed *)
      let%bind did =
        if String.is_prefix actor ~prefix:"did:" then return actor
        else
          let settings = Settings.get_settings () in
          let%bind client = Atproto.Client.get_client settings in
          let resolve_endpoint =
            sprintf "com.atproto.identity.resolveHandle?handle=%s" actor
          in
          let%bind did_json =
            Atproto.Client.api_get client ~endpoint:resolve_endpoint
          in
          let json = Yojson.Safe.from_string did_json in
          return Yojson.Safe.Util.(member "did" json |> to_string)
      in
      let%bind result = Atproto.Social.unfollow_user did in
      return (Types.delete_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"unlike"
    ~description:"Remove a like from a post"
    ~handler:(fun params ->
      let uri =
        Yojson.Safe.Util.member "uri" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let cid =
        Yojson.Safe.Util.member "cid" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Social.unlike_post ~uri ~cid in
      return (Types.delete_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"unrepost"
    ~description:"Remove a repost"
    ~handler:(fun params ->
      let uri =
        Yojson.Safe.Util.member "uri" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let cid =
        Yojson.Safe.Util.member "cid" params
        |> Yojson.Safe.Util.to_string_option |> Option.value ~default:""
      in
      let%bind result = Atproto.Social.unrepost ~uri ~cid in
      return (Types.delete_result_to_yojson result))
    server;

  Server.Ox_fast_mcp.add_simple_tool ~name:"create_thread"
    ~description:"Create a thread of posts"
    ~handler:(fun params ->
      let posts_json =
        try Yojson.Safe.Util.member "posts" params |> Yojson.Safe.Util.to_list
        with _ -> []
      in
      let posts =
        List.filter_map posts_json ~f:(fun post_json ->
            try Some (Types.thread_post_of_yojson post_json) with _ -> None)
      in
      let%bind result = Atproto.Posts.create_thread posts in
      return (Types.thread_result_to_yojson result))
    server;

  Log.Global.info "🦋 ATProto MCP Server starting...";
  Log.Global.info "Authenticating with Bluesky as %s" settings.atproto_handle;

  (* Run server *)
  Server.Ox_fast_mcp.run_async server ~transport:Stdio ()

let () =
  Command.async ~summary:"ATProto MCP server for Bluesky interactions"
    (Command.Param.return main)
  |> Command_unix.run
