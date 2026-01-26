open! Core
open! Async
open Fastmcp_config_lib

let main () =
  let open Deferred.Let_syntax in
  (* Create the FastMCP server instance *)
  (* Python: mcp = FastMCP("Config Example Server") *)
  let server = Server.Ox_fast_mcp.create ~name:"Config Example Server" () in

  (* Python: @mcp.tool def echo(text: str) -> str: """Echo the provided text
     back to the user.""" return f"You said: {text}" *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"echo"
    ~description:"Echo the provided text back to the user."
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc [ ("text", `Assoc [ ("type", `String "string") ]) ] );
          ("required", `List [ `String "text" ]);
        ])
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let text =
          List.Assoc.find fields ~equal:String.equal "text"
          |> Option.value_map ~default:"" ~f:(function
               | `String s -> s
               | _ -> "")
        in
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "text");
                        ("text", `String (sprintf "You said: %s" text));
                      ];
                  ] );
            ])
      | _ -> return (`Assoc [ ("error", `String "Expected object") ]))
    server;

  (* Python: @mcp.tool def add(a: int, b: int) -> int: """Add two numbers
     together.""" return a + b *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"add"
    ~description:"Add two numbers together."
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ("a", `Assoc [ ("type", `String "integer") ]);
                ("b", `Assoc [ ("type", `String "integer") ]);
              ] );
          ("required", `List [ `String "a"; `String "b" ]);
        ])
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let a =
          List.Assoc.find fields ~equal:String.equal "a"
          |> Option.value_map ~default:0 ~f:(function
               | `Int i -> i
               | _ -> 0)
        in
        let b =
          List.Assoc.find fields ~equal:String.equal "b"
          |> Option.value_map ~default:0 ~f:(function
               | `Int i -> i
               | _ -> 0)
        in
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "text");
                        ("text", `String (Int.to_string (a + b)));
                      ];
                  ] );
            ])
      | _ -> return (`Assoc [ ("error", `String "Expected object") ]))
    server;

  (* Python: @mcp.resource("config://example") def get_example_config() -> str:
     """Return an example configuration.""" return """...""" *)
  let reader () =
    let%map config = Config.load ~path:"env_interpolation_example.json" () in
    match config with
    | Some c ->
      let env_str =
        match c.Config.Types.deployment with
        | Some { env = Some vars; _ } ->
          List.map vars ~f:(fun (k, v) -> sprintf "%s=%s" k v)
          |> String.concat ~sep:"\n"
        | _ -> "No env vars"
      in
      sprintf
        "Loaded Configuration:\nEntrypoint: %s\nEnvironment Variables:\n%s"
        (Option.value c.entrypoint ~default:"None")
        env_str
    | None -> "Failed to load configuration or no configuration found."
  in
  let resource =
    Server.Resource.create ~uri:"config://example" ~name:"example_config"
      ~description:"Return an example configuration." ~mime_type:"text/plain"
      ~reader ()
  in
  Server.Ox_fast_mcp.add_resource server resource;

  (* Load and apply config to process *)
  let%bind config = Config.load ~path:"env_interpolation_example.json" () in
  Config.apply_to_process config;

  Log.Global.info "Config Example Server starting...";

  (* Run with STDIO transport *)
  (* Python: asyncio.run(mcp.run_async()) *)
  Server.Ox_fast_mcp.run_async server ~transport:Stdio ()

let () =
  Command.async ~summary:"Config Example Server" (Command.Param.return main)
  |> Command_unix.run
