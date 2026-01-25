open Core

type t = { hue_bridge_ip : string; hue_bridge_username : string }

let create () =
  let hue_bridge_ip =
    match Sys.getenv "HUE_BRIDGE_IP" with
    | Some ip -> ip
    | None -> "127.0.0.1" (* Default mock IP *)
  in
  let hue_bridge_username =
    match Sys.getenv "HUE_BRIDGE_USERNAME" with
    | Some user -> user
    | None -> "mock_user" (* Default mock user *)
  in
  { hue_bridge_ip; hue_bridge_username }

let config = create ()
