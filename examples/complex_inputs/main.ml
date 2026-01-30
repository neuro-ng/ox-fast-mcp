(* open Core - unused *)
open Async

let () =
  Command.async ~summary:"Complex Inputs Server"
    (Command.Param.return (fun () -> Complex_inputs_lib.Shrimp_server.main ()))
  |> Command_unix.run
