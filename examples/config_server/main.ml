(* open Core - unused *)
open Async

let () =
  let command =
    Command.async
      ~summary:
        "Simple example showing FastMCP server with command line argument \
         support"
      (let%map_open.Command name =
         flag "--name"
           (optional_with_default "ConfigurableServer" string)
           ~doc:"NAME Server name"
       and debug = flag "--debug" no_arg ~doc:"Enable debug mode" in
       fun () -> Config_server_lib.Config_server.main ~name ~debug ())
  in
  Command_unix.run command
