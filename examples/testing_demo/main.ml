(* open Core - unused *)
open Async

let () =
  Command.async ~summary:"Testing Demo Server"
    (Command.Param.return (fun () -> Testing_demo_lib.Demo_server.main ()))
  |> Command_unix.run
