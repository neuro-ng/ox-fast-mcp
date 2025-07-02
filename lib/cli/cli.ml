(** CLI implementation for FastMCP OCaml *)

open Utilities.Types

(** CLI configuration options *)
type cli_config = {
  mutable transport : transport_config;
  mutable auth : auth_config option;
  mutable log_level : log_level;
  mutable config_file : string option;
}

(** Create default CLI configuration *)
let default_config () = {
  transport = Stdio;
  auth = None;
  log_level = Info;
  config_file = None;
}

(** Parse command line arguments *)
let parse_args config =
  let set_transport_stdio () = config.transport <- Stdio in
  let set_transport_http host port path = 
    config.transport <- Http { host; port; path } 
  in
  let set_auth_bearer token = config.auth <- Some (Bearer token) in
  let set_log_level level = 
    let log_level = match level with
      | "debug" -> Debug
      | "info" -> Info  
      | "notice" -> Notice
      | "warning" -> Warning
      | "error" -> Error
      | _ -> Info
    in
    config.log_level <- log_level
  in
  let set_config_file file = config.config_file <- Some file in
  
  let http_args = ref [] in
  let add_http_arg arg = http_args := arg :: !http_args in
  
  let usage_msg = "fastmcp [OPTIONS]" in
  let spec = [
    ("--stdio", Arg.Unit set_transport_stdio, " Use stdio transport");
    ("--http-host", Arg.String add_http_arg, " <host> HTTP host");
    ("--http-port", Arg.String add_http_arg, " <port> HTTP port");
    ("--http-path", Arg.String add_http_arg, " <path> HTTP path");
    ("--auth-bearer", Arg.String set_auth_bearer, " <token> Use bearer token authentication");
    ("--log-level", Arg.String set_log_level, " <level> Set log level (debug|info|notice|warning|error)");
    ("--config", Arg.String set_config_file, " <file> Load configuration from file");
  ] in
  
  Arg.parse spec (fun _ -> ()) usage_msg;
  
  (* Process HTTP arguments if provided *)
  match List.rev !http_args with
  | [path; port_str; host] ->
    (try
       let port = int_of_string port_str in
       set_transport_http host port path
     with Failure _ ->
       Printf.eprintf "Error: Invalid port number: %s\n" port_str)
  | [] -> () (* No HTTP args provided *)
  | _ -> Printf.eprintf "Error: HTTP transport requires --http-host, --http-port, and --http-path\n"

(** Print CLI configuration *)
let print_config config =
  let transport_str = match config.transport with
    | Stdio -> "stdio"
    | Http {host; port; path} -> Printf.sprintf "http://%s:%d%s" host port path
    | Sse {host; port; path} -> Printf.sprintf "sse://%s:%d%s" host port path
    | StreamableHttp {host; port; path} -> Printf.sprintf "streamable-http://%s:%d%s" host port path
  in
  
  let auth_str = match config.auth with
    | None -> "none"
    | Some (Bearer _) -> "bearer"
    | Some (OAuth _) -> "oauth"
  in
  
  let log_level_str = match config.log_level with
    | Debug -> "debug" | Info -> "info" | Notice -> "notice"
    | Warning -> "warning" | Error -> "error"
  in
  
  Printf.printf "FastMCP Configuration:\n";
  Printf.printf "  Transport: %s\n" transport_str;
  Printf.printf "  Authentication: %s\n" auth_str;
  Printf.printf "  Log Level: %s\n" log_level_str;
  Printf.printf "  Config File: %s\n" (Option.value config.config_file ~default:"none")

(** CLI subcommands *)
type subcommand = 
  | Server
  | Client
  | Test
  | Help

(** Parse subcommand *)
let parse_subcommand () =
  if Array.length Sys.argv > 1 then
    match Sys.argv.(1) with
    | "server" -> Server
    | "client" -> Client  
    | "test" -> Test
    | "help" | "--help" | "-h" -> Help
    | _ -> Help
  else
    Help

(** Print help message *)
let print_help () =
  Printf.printf "FastMCP OCaml - Model Context Protocol implementation\n\n";
  Printf.printf "USAGE:\n";
  Printf.printf "    fastmcp <SUBCOMMAND> [OPTIONS]\n\n";
  Printf.printf "SUBCOMMANDS:\n";
  Printf.printf "    server    Start MCP server\n";
  Printf.printf "    client    Start MCP client\n";
  Printf.printf "    test      Run tests\n";
  Printf.printf "    help      Show this help message\n\n";
  Printf.printf "OPTIONS:\n";
  Printf.printf "    --stdio                     Use stdio transport (default)\n";
  Printf.printf "    --http-host <host>          HTTP host (use with --http-port and --http-path)\n";
  Printf.printf "    --http-port <port>          HTTP port\n";
  Printf.printf "    --http-path <path>          HTTP path\n";
  Printf.printf "    --auth-bearer <token>       Use bearer token authentication\n";
  Printf.printf "    --log-level <level>         Set log level\n";
  Printf.printf "    --config <file>             Load configuration from file\n\n";
  Printf.printf "EXAMPLES:\n";
  Printf.printf "    fastmcp server --stdio\n";
  Printf.printf "    fastmcp client --http-host localhost --http-port 8080 --http-path /mcp\n";
  Printf.printf "    fastmcp server --auth-bearer secret123 --log-level debug\n"

(** Main CLI entry point *)
let main () =
  let config = default_config () in
  
  match parse_subcommand () with
  | Help -> 
    print_help ();
    exit 0
  | Server ->
    parse_args config;
    Printf.printf "Starting FastMCP server...\n";
    print_config config;
    (* Server implementation would go here *)
    Printf.printf "Server functionality not yet implemented\n"
  | Client ->
    parse_args config;
    Printf.printf "Starting FastMCP client...\n";
    print_config config;
    (* Client implementation would go here *)
    Printf.printf "Client functionality not yet implemented\n"
  | Test ->
    Printf.printf "Running FastMCP tests...\n";
    (* Test runner would go here *)
    Printf.printf "Test functionality not yet implemented\n" 