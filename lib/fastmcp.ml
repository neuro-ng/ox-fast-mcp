open Core

module Settings = struct
  type t = {
    deprecation_warnings : bool;
  }

  let create ?(deprecation_warnings=true) () = {
    deprecation_warnings;
  }

  let default = create ()

  let get_deprecation_warnings t = t.deprecation_warnings

  let set_deprecation_warnings _t value = { deprecation_warnings = value }
end

let settings = Settings.default

module Server = struct
  module Context = struct
    type t = {
      method_name : string;
      params : Yojson.Safe.t;
      id : string option;
      resource : string option;
    }
  end

  type t = {
    name : string;
  }

  let create name = { name }

  let start server ~port =
    Logs.info (fun m -> m "Server %s starting on port %d" server.name port);
    Lwt.return_unit
end

module Client = struct
  type t = {
    server : Server.t;
  }

  let create server = { server }
end

let version = Version.version

(* Deprecated Image module *)
module Image = struct
  type t [@@deprecated "Use Utilities.Types.Image instead"]
end

let () =
  (* Initialize logging *)
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);

  (* Parse command line arguments *)
  let open Command.Let_syntax in
  Command.group ~summary:"FastMCP CLI"
    [ "server",
      Command.basic
        ~summary:"Start FastMCP server"
        [%map_open
          let port =
            flag "-p" (optional_with_default 8080 int)
              ~doc:"PORT Port to listen on (default: 8080)"
          and name =
            flag "-n" (optional_with_default "FastMCP" string)
              ~doc:"NAME Server name (default: FastMCP)"
          in
          fun () ->
            let server = Server.create name in
            ignore (Server.start server ~port);
            Lwt_main.run (Lwt.return_unit)
        ]
    ]
  |> Command_unix.run

(* Suppress some warnings *)
[@@@warning "-3"] 