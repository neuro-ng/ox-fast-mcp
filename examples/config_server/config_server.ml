open Core
open Async
module FastMCP = Server.Ox_fast_mcp

let create ~name ~debug () =
  let server_name = if debug then name ^ " (Debug)" else name in
  let server = FastMCP.create ~name:server_name () in

  (* Tool: get_status *)
  FastMCP.add_simple_tool server ~name:"get_status"
    ~description:"Get the current server configuration and status."
    ~handler:(fun _ ->
      let status =
        [
          ("server_name", `String server_name);
          ("debug_mode", `Bool debug);
          ("original_name", `String name);
        ]
      in
      return (`Assoc status));

  (* Tool: echo_message *)
  FastMCP.add_simple_tool server ~name:"echo_message"
    ~description:"Echo a message, with debug info if debug mode is enabled."
    ~parameters:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc [ ("message", `Assoc [ ("type", `String "string") ]) ] );
          ("required", `List [ `String "message" ]);
        ])
    ~handler:(fun args ->
      let message =
        match args with
        | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal "message"
          |> Option.value_map ~default:"" ~f:(function
               | `String s -> s
               | _ -> "")
        | _ -> ""
      in
      let response_text =
        if debug then sprintf "[DEBUG] Echoing: %s" message else message
      in
      return (`String response_text));

  server

let main ~name ~debug () =
  let server = create ~name ~debug () in
  let transport = Server.Transport.Stdio in
  FastMCP.run_async ~transport server ()
