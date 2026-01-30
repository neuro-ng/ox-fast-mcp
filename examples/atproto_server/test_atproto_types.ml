open Core
open Atproto_types.Types

let%expect_test "profile_info serialization" =
  let profile =
    {
      connected = true;
      handle = Some "user.bsky.social";
      display_name = Some "User Name";
      did = Some "did:plc:12345";
      followers = Some 100;
      following = Some 50;
      posts = Some 10;
      error = None;
    }
  in
  let json = profile_info_to_yojson profile in
  print_endline (Yojson.Safe.pretty_to_string json);
  [%expect
    {|
    {
      "connected": true,
      "handle": "user.bsky.social",
      "display_name": "User Name",
      "did": "did:plc:12345",
      "followers": 100,
      "following": 50,
      "posts": 10,
      "error": null
    } |}]

let%expect_test "post_result serialization" =
  let result =
    {
      success = true;
      uri = Some "at://did:plc:123/app.bsky.feed.post/456";
      cid = Some "bafyreib7";
      text = None;
      created_at = None;
      error = None;
    }
  in
  let json = post_result_to_yojson result in
  print_endline (Yojson.Safe.pretty_to_string json);
  [%expect
    {|
    {
      "success": true,
      "uri": "at://did:plc:123/app.bsky.feed.post/456",
      "cid": "bafyreib7",
      "text": null,
      "created_at": null,
      "error": null
    } |}]
