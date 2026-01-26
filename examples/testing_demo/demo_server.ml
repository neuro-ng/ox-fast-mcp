open Core
open Async
(* open Server - removed as we qualify usage *)

(* -------------------------------------------------------------------------- *)
(* Testing Demo Server                                                       *)
(* -------------------------------------------------------------------------- *)

(* Define server creation function for both main entrypoint and tests *)
let create () =
  let server =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.create ~name:"Testing Demo" ()
  in

  (* Tools *)
  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_tool server ~name:"add"
    ~description:"Add two numbers together"
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
      | _ -> return (`Assoc [ ("error", `String "Expected object") ]));

  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_tool server ~name:"greet"
    ~description:"Greet someone with a customizable greeting"
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ("name", `Assoc [ ("type", `String "string") ]);
                ( "greeting",
                  `Assoc
                    [ ("type", `String "string"); ("default", `String "Hello") ]
                );
              ] );
          ("required", `List [ `String "name" ]);
        ])
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let name =
          List.Assoc.find fields ~equal:String.equal "name"
          |> Option.value_map ~default:"User" ~f:(function
               | `String s -> s
               | _ -> "User")
        in
        let greeting =
          List.Assoc.find fields ~equal:String.equal "greeting"
          |> Option.value_map ~default:"Hello" ~f:(function
               | `String s -> s
               | _ -> "Hello")
        in
        let message = sprintf "%s, %s!" greeting name in
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [ ("type", `String "text"); ("text", `String message) ];
                  ] );
            ])
      | _ -> return (`Assoc [ ("error", `String "Expected object") ]));

  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_tool server
    ~name:"async_multiply" ~description:"Multiply two numbers (async example)"
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ("x", `Assoc [ ("type", `String "number") ]);
                ("y", `Assoc [ ("type", `String "number") ]);
              ] );
          ("required", `List [ `String "x"; `String "y" ]);
        ])
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let x =
          List.Assoc.find fields ~equal:String.equal "x"
          |> Option.value_map ~default:0.0 ~f:(function
               | `Float f -> f
               | `Int i -> Float.of_int i
               | _ -> 0.0)
        in
        let y =
          List.Assoc.find fields ~equal:String.equal "y"
          |> Option.value_map ~default:0.0 ~f:(function
               | `Float f -> f
               | `Int i -> Float.of_int i
               | _ -> 0.0)
        in
        (* Simulate some async work if desired, but here just return *)
        let result = x *. y in
        (* Returning float as string for text output, as is standard in MCP text
           responses *)
        let result_str = Float.to_string result in
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [ ("type", `String "text"); ("text", `String result_str) ];
                  ] );
            ])
      | _ -> return (`Assoc [ ("error", `String "Expected object") ]));

  (* Resources *)
  let info_reader () = return "This is the FastMCP Testing Demo server" in
  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_resource server
    ~uri:"demo://info" ~name:"demo://info" ~description:"Get server information"
    ~mime_type:"text/plain" ~reader:info_reader;

  (* Resource Template: demo://greeting/{name} *)
  let greeting_template =
    Ox_fast_mcp_server.Server.Resource_template.create
      ~uri_template:"demo://greeting/{name}" ~name:"greeting_resource"
      ~description:"Get a personalized greeting resource"
      ~mime_type:"text/plain"
      ~create_resource:(fun ~params ->
        let name =
          List.Assoc.find params ~equal:String.equal "name"
          |> Option.value ~default:"User"
        in
        let uri = sprintf "demo://greeting/%s" name in
        let reader () = return (sprintf "Welcome to FastMCP, %s!" name) in
        return
          (Ox_fast_mcp_server.Server.Resource.create ~uri ~name:"greeting_inst"
             ~mime_type:"text/plain" ~reader ()))
      ()
  in
  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_template server greeting_template;

  (* Prompts *)
  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_prompt server ~name:"hello"
    ~description:"Generate a hello world prompt"
    ~arguments:
      [
        { name = "name"; description = Some "Name to greet"; required = false };
      ]
    ~render:(fun params ->
      let name =
        match params with
        | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal "name"
          |> Option.value_map ~default:"World" ~f:(function
               | `String s -> s
               | _ -> "World")
        | _ -> "World"
      in
      return
        (`Assoc
          [
            ( "messages",
              `List
                [
                  `Assoc
                    [
                      ("role", `String "user");
                      ( "content",
                        `Assoc
                          [
                            ("type", `String "text");
                            ( "text",
                              `String
                                (sprintf "Say hello to %s in a friendly way."
                                   name) );
                          ] );
                    ];
                ] );
          ]));

  Ox_fast_mcp_server.Server.Ox_fast_mcp.add_simple_prompt server ~name:"explain"
    ~description:"Generate a prompt to explain a topic"
    ~arguments:
      [
        {
          name = "topic";
          description = Some "Topic to explain";
          required = true;
        };
        {
          name = "detail_level";
          description = Some "Level of detail";
          required = false;
        };
      ]
    ~render:(fun params ->
      let args_map =
        match params with
        | `Assoc fs -> fs
        | _ -> []
      in
      let topic =
        List.Assoc.find args_map ~equal:String.equal "topic"
        |> Option.value_map ~default:"something" ~f:(function
             | `String s -> s
             | _ -> "something")
      in
      let detail_level =
        List.Assoc.find args_map ~equal:String.equal "detail_level"
        |> Option.value_map ~default:"medium" ~f:(function
             | `String s -> s
             | _ -> "medium")
      in
      let prompt_text =
        match detail_level with
        | "simple" -> sprintf "Explain %s in simple terms for beginners." topic
        | "detailed" ->
          sprintf "Provide a detailed, technical explanation of %s." topic
        | _ -> sprintf "Explain %s with moderate technical detail." topic
      in
      return
        (`Assoc
          [
            ( "messages",
              `List
                [
                  `Assoc
                    [
                      ("role", `String "user");
                      ( "content",
                        `Assoc
                          [
                            ("type", `String "text");
                            ("text", `String prompt_text);
                          ] );
                    ];
                ] );
          ]));

  server

(* Main Entrypoint *)
let main () =
  let server = create () in
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
  Ox_fast_mcp_server.Server.Ox_fast_mcp.run_async ~transport ~port server
    ~log_level:"INFO" ()

let () =
  (* Only run main if this file is executed as an entrypoint, but since this is
     OCaml, we usually control execution via dune. we'll use a specific
     condition or just let dune handling wrapping it. For this file, we want it
     to be both a library (for testing) and executable. We will put the main run
     call inside a conditional or separate module if needed.

     Actually, a common pattern in OCaml is to have `main.ml` for the executable
     and `lib.ml` for the library. Since implementing all in one file for
     simplicity matching python structure: We can skip top-level side effects if
     we are just running tests via inline test runner. However, dune executables
     run top-level. We will handle this by making this file a library in dune,
     and having a tiny separate executable runner, OR just not running the main
     loop if we detect we are in a test environment? Actually, we will make this
     file `server.ml` purely a library that exports `create` and `main`, and
     have a `main.ml` entrypoint that calls `Ox_fast_mcp_server.Server.main`. *)
  ()
