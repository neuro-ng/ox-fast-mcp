(** Shared test fixtures for proxy tests.

    Provides helper types and mock implementations for proxy testing. *)

open! Core

(* =============================================================================
   Mock Client for Testing
   ============================================================================= *)

module Mock_client = struct
  type t = {
    name : string;
    tools : (string * Yojson.Safe.t) list;
    resources : (string * Yojson.Safe.t) list;
    prompts : (string * Yojson.Safe.t) list;
    mutable connected : bool;
  }

  let create ?(name = "MockClient") ?(tools = []) ?(resources = [])
      ?(prompts = []) () =
    { name; tools; resources; prompts; connected = false }

  let connect t = t.connected <- true
  let disconnect t = t.connected <- false
  let is_connected t = t.connected

  let list_tools t =
    List.map t.tools ~f:(fun (name, schema) ->
        `Assoc [ ("name", `String name); ("inputSchema", schema) ])

  let call_tool t ~name ~arguments:_ =
    match List.Assoc.find t.tools ~equal:String.equal name with
    | Some _ ->
      `Assoc
        [
          ( "content",
            `List
              [
                `Assoc
                  [
                    ("type", `String "text");
                    ("text", `String ("Result from " ^ name));
                  ];
              ] );
        ]
    | None ->
      `Assoc
        [
          ("isError", `Bool true);
          ( "content",
            `List
              [
                `Assoc
                  [ ("type", `String "text"); ("text", `String "Unknown tool") ];
              ] );
        ]

  let list_resources t =
    List.map t.resources ~f:(fun (uri, _) ->
        `Assoc
          [
            ("uri", `String uri);
            ("name", `String uri);
            ("mimeType", `String "text/plain");
          ])

  let read_resource t ~uri =
    match List.Assoc.find t.resources ~equal:String.equal uri with
    | Some content -> `Assoc [ ("contents", `List [ content ]) ]
    | None -> `Assoc [ ("contents", `List []) ]

  let list_prompts t =
    List.map t.prompts ~f:(fun (name, schema) ->
        `Assoc [ ("name", `String name); ("arguments", schema) ])

  let get_prompt t ~name ~arguments:_ =
    match List.Assoc.find t.prompts ~equal:String.equal name with
    | Some _ ->
      `Assoc
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
                          ("text", `String ("Prompt: " ^ name));
                        ] );
                  ];
              ] );
        ]
    | None -> `Assoc [ ("messages", `List []) ]
end

(* =============================================================================
   Sample Data
   ============================================================================= *)

let sample_tool_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc [ ("message", `Assoc [ ("type", `String "string") ]) ] );
      ("required", `List [ `String "message" ]);
    ]

let sample_mcp_tool =
  `Assoc
    [
      ("name", `String "echo");
      ("description", `String "Echo a message");
      ("inputSchema", sample_tool_schema);
    ]

let sample_mcp_tool_with_meta =
  `Assoc
    [
      ("name", `String "greet");
      ("description", `String "Greet someone");
      ("inputSchema", sample_tool_schema);
      ( "meta",
        `Assoc [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "greet" ]) ]) ]
      );
    ]

let sample_mcp_resource =
  `Assoc
    [
      ("uri", `String "resource://wave");
      ("name", `String "wave");
      ("description", `String "A wave emoji");
      ("mimeType", `String "text/plain");
    ]

let sample_mcp_resource_with_meta =
  `Assoc
    [
      ("uri", `String "resource://wave");
      ("name", `String "wave");
      ("mimeType", `String "text/plain");
      ( "meta",
        `Assoc [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "wave" ]) ]) ]
      );
    ]

let sample_mcp_template =
  `Assoc
    [
      ("uriTemplate", `String "data://user/{user_id}");
      ("name", `String "get_user");
      ("description", `String "Get user by ID");
      ("mimeType", `String "application/json");
    ]

let sample_mcp_prompt =
  `Assoc
    [
      ("name", `String "welcome");
      ("description", `String "Welcome message");
      ( "arguments",
        `List [ `Assoc [ ("name", `String "name"); ("required", `Bool true) ] ]
      );
    ]

let sample_users =
  `List
    [
      `Assoc
        [
          ("id", `String "1"); ("name", `String "Alice"); ("active", `Bool true);
        ];
      `Assoc
        [ ("id", `String "2"); ("name", `String "Bob"); ("active", `Bool true) ];
      `Assoc
        [
          ("id", `String "3");
          ("name", `String "Charlie");
          ("active", `Bool false);
        ];
    ]
