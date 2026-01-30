open Core
open Async

(* -------------------------------------------------------------------------- *)
(* Hub MCP                                                                    *)
(* -------------------------------------------------------------------------- *)

module FastMCP = Ox_fast_mcp_server.Server.Ox_fast_mcp

let hub_mcp = FastMCP.create ~name:"Smart Home Hub (phue2)" ()

(* Mount the lights service under the 'hue' prefix *)
let () =
  FastMCP.import_server hub_mcp ~server:Smart_home_lib.Lights.lights_mcp
    ~prefix:"hue" ()

(* Mount the thermostat service under the 'thermo' prefix *)
let () =
  FastMCP.import_server hub_mcp ~server:Smart_home_lib.Thermostat.thermostat_mcp
    ~prefix:"thermo" ()

(* Tool: hub_status *)
let () =
  FastMCP.add_simple_tool hub_mcp ~name:"hub_status"
    ~description:"Checks the status of the main hub and connections."
    ~handler:(fun _ ->
      try
        let client =
          Smart_home_lib.Phue.create
            ~ip:Smart_home_lib.Config.config.hue_bridge_ip
            ~username:Smart_home_lib.Config.config.hue_bridge_username
        in
        (* Try to get simple resource to verify connection *)
        let%map _ = Smart_home_lib.Phue.get_lights client in
        `String "Hub OK. Hue Bridge Connected (via Phue)."
      with e ->
        return
          (`String
            (Printf.sprintf
               "Hub Warning: Hue Bridge connection failed or not attempted: %s"
               (Exn.to_string e))))

(* -------------------------------------------------------------------------- *)
(* Main Entrypoint                                                            *)
(* -------------------------------------------------------------------------- *)

let () =
  let transport =
    match Sys.getenv "FASTMCP_TRANSPORT" with
    | Some "http" -> Ox_fast_mcp_server.Server.Transport.Http
    | Some "sse" -> Ox_fast_mcp_server.Server.Transport.Sse
    | _ -> Ox_fast_mcp_server.Server.Transport.Stdio
  in
  let port =
    match Sys.getenv "FASTMCP_PORT" with
    | Some p -> Int.of_string p
    | None -> 8000
  in
  Ox_fast_mcp_server.Server.Ox_fast_mcp.run_async ~transport ~port hub_mcp
    ~log_level:"INFO" () (* Optional: add logging for visibility *)
  |> don't_wait_for;
  never_returns (Scheduler.go ())
