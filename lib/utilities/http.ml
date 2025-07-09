open Core

let find_available_port () =
  let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, 0) in
  let sock =
    Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM
      ~protocol:0 ()
  in
  Core_unix.bind sock ~addr;
  let port =
    Core_unix.getsockname sock |> function
    | Core_unix.ADDR_INET (_, port) -> port
    | _ -> failwith "Expected INET address"
  in
  Core_unix.close sock;
  port
