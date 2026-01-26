open! Core
open! Async

let main () =
  let open Deferred.Let_syntax in
  (* Create the FastMCP server instance - dependencies are now in fastmcp.json *)
  (* Python: mcp = FastMCP("Screenshot Demo") *)
  let server = Server.Ox_fast_mcp.create ~name:"Screenshot Demo" () in

  (* Python: @mcp.tool def take_screenshot() -> Image: """Take a screenshot of
     the user's screen and return it as an image.""" ... *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"take_screenshot"
    ~description:
      "Take a screenshot of the user's screen and return it as an image.\n\
       Use this tool anytime the user wants you to look at something on their \
       screen."
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("required", `List []);
        ])
    ~handler:(fun _params ->
      (* Mock implementation - returning a placeholder image *)
      (* In a real implementation we would use a library bindings but that is out of scope *)
      let placeholder_data = "DATA" in
      let image_content =
        `Assoc
          [
            ("type", `String "image");
            ("data", `String placeholder_data);
            ("mimeType", `String "image/jpeg");
          ]
      in
      return (`Assoc [ ("content", `List [ image_content ]) ]))
    server;

  (* Python: @mcp.tool def analyze_colors() -> dict: """Analyze the dominant
     colors in the current screen.""" ... *)
  Server.Ox_fast_mcp.add_simple_tool ~name:"analyze_colors"
    ~description:
      "Analyze the dominant colors in the current screen.\n\
       Returns a dictionary with color statistics from the screen."
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("required", `List []);
        ])
    ~handler:(fun _params ->
      (* Mock implementation *)
      return
        (`Assoc
          [
            ( "content",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "text");
                      ( "text",
                        `String
                          (Yojson.Safe.to_string
                             (`Assoc
                               [
                                 ( "top_colors",
                                   `List
                                     [
                                       `Assoc
                                         [
                                           ("count", `Int 5000);
                                           ( "rgb",
                                             `List
                                               [ `Int 255; `Int 255; `Int 255 ]
                                           );
                                         ];
                                       `Assoc
                                         [
                                           ("count", `Int 3000);
                                           ( "rgb",
                                             `List [ `Int 0; `Int 0; `Int 0 ] );
                                         ];
                                     ] );
                                 ("total_pixels", `Int 10000);
                               ])) );
                    ];
                ] );
          ]))
    server;

  (* Load and apply config to process *)
  let%bind config = Config.load ~path:"fastmcp.json" () in
  Config.apply_to_process config;

  (match config with
  | Some { Config.Types.deployment = Some { log_level = Some level; _ }; _ } ->
    Log.Global.info "Configured Log Level: %s" level
  | _ -> ());

  Log.Global.info "Screenshot Demo Server starting...";

  (* Run with STDIO transport *)
  (* Python: asyncio.run(mcp.run_async()) *)
  Server.Ox_fast_mcp.run_async server ~transport:Stdio ()

let () =
  Command.async ~summary:"Screenshot Demo Server" (Command.Param.return main)
  |> Command_unix.run
