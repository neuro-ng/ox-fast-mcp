(** Session Group - Manages multiple concurrent MCP sessions *)

open Core
open Async

(** {1 Server Parameters} *)

module Server_parameters = struct
  type t = Mcp_client_transports.Stdio.stdio_server_parameters
  [@@deriving sexp_of]

  let create ~command ?(args = []) ?env ?cwd ?(encoding = "utf-8")
      ?(encoding_error_handler = `Strict) () =
    {
      Mcp_client_transports.Stdio.command;
      args;
      env;
      cwd;
      encoding;
      encoding_error_handler;
    }
end

(** {1 Session Parameters} *)

module Client_session_parameters = struct
  type t = {
    read_timeout_seconds : Time_ns.Span.t option;
    sampling_callback : Session.sampling_fn option;
    elicitation_callback : Session.elicitation_fn option;
    list_roots_callback : Session.list_roots_fn option;
    logging_callback : Session.logging_fn option;
    message_handler : Session.message_handler option;
    client_info : Mcp.Types.implementation option;
  }
  [@@deriving fields]

  let create ?read_timeout_seconds ?sampling_callback ?elicitation_callback
      ?list_roots_callback ?logging_callback ?message_handler ?client_info () =
    {
      read_timeout_seconds;
      sampling_callback;
      elicitation_callback;
      list_roots_callback;
      logging_callback;
      message_handler;
      client_info;
    }

  let default =
    {
      read_timeout_seconds = None;
      sampling_callback = None;
      elicitation_callback = None;
      list_roots_callback = None;
      logging_callback = None;
      message_handler = None;
      client_info = None;
    }
end

(** {1 Internal Types} *)

(** Component names associated with a session *)
module Component_names = struct
  type t = {
    prompts : String.Set.t;
    resources : String.Set.t;
    tools : String.Set.t;
  }
  [@@deriving sexp_of]

  let empty =
    {
      prompts = String.Set.empty;
      resources = String.Set.empty;
      tools = String.Set.empty;
    }
end

type component_name_hook = string -> Mcp.Types.implementation -> string
(** Hook for customizing component names *)

type t = {
  (* Component dictionaries *)
  mutable prompts : Mcp.Types.prompt String.Map.t;
  mutable resources : Mcp.Types.resource String.Map.t;
  mutable tools : Mcp.Types.tool String.Map.t;
  (* Session tracking - use polymorphic hashtable *)
  mutable sessions : (Session.t, Component_names.t) Hashtbl.t;
  mutable tool_to_session : Session.t String.Map.t;
  (* Cleanup tracking *)
  mutable session_cleanups : (Session.t, Writer.t * Process.t) Hashtbl.t;
  (* Optional naming hook *)
  component_name_hook : component_name_hook option;
}
(** Main session group type *)

(** {1 Creation} *)

let create ?component_name_hook () =
  {
    prompts = String.Map.empty;
    resources = String.Map.empty;
    tools = String.Map.empty;
    sessions = Hashtbl.Poly.create ();
    tool_to_session = String.Map.empty;
    session_cleanups = Hashtbl.Poly.create ();
    component_name_hook;
  }

(** {1 Property Accessors} *)

let prompts t = t.prompts
let resources t = t.resources
let tools t = t.tools
let sessions t = Hashtbl.keys t.sessions

(** {1 Helper Functions} *)

(** Helper to apply component name hook *)
let component_name t name server_info =
  match t.component_name_hook with
  | Some hook -> hook name server_info
  | None -> name

(** Aggregate components from a session *)
let aggregate_components t server_info session =
  let component_names = ref Component_names.empty in

  (* Temporary storage - only commit if no duplicates *)
  let prompts_temp = ref String.Map.empty in
  let resources_temp = ref String.Map.empty in
  let tools_temp = ref String.Map.empty in
  let tool_to_session_temp = ref String.Map.empty in

  (* Helper to handle MCP errors gracefully *)
  let handle_mcp_error f error_msg =
    Monitor.try_with f >>= function
    | Ok result -> return (Ok result)
    | Error exn ->
      Logs.warn (fun m -> m "%s: %s" error_msg (Exn.to_string exn));
      return (Error ())
  in

  (* Fetch prompts *)
  let%bind _ =
    handle_mcp_error
      (fun () ->
        let%bind result = Session.list_prompts session () in
        let prompts = result.Mcp.Types.prompts in
        List.iter prompts ~f:(fun prompt ->
            let name =
              component_name t prompt.Mcp.Types.base_metadata.name server_info
            in
            prompts_temp := Map.add_exn !prompts_temp ~key:name ~data:prompt;
            component_names :=
              {
                !component_names with
                prompts = Set.add !component_names.prompts name;
              });
        return ())
      "Could not fetch prompts"
  in

  (* Fetch resources *)
  let%bind _ =
    handle_mcp_error
      (fun () ->
        let%bind result = Session.list_resources session () in
        let resources = result.Mcp.Types.resources in
        List.iter resources ~f:(fun resource ->
            let name =
              component_name t resource.Mcp.Types.base_metadata.name server_info
            in
            resources_temp :=
              Map.add_exn !resources_temp ~key:name ~data:resource;
            component_names :=
              {
                !component_names with
                resources = Set.add !component_names.resources name;
              });
        return ())
      "Could not fetch resources"
  in

  (* Fetch tools *)
  let%bind _ =
    handle_mcp_error
      (fun () ->
        let%bind result = Session.list_tools session () in
        let tools = result.Mcp.Types.tools in
        List.iter tools ~f:(fun tool ->
            let name =
              component_name t tool.Mcp.Types.base_metadata.name server_info
            in
            tools_temp := Map.add_exn !tools_temp ~key:name ~data:tool;
            tool_to_session_temp :=
              Map.add_exn !tool_to_session_temp ~key:name ~data:session;
            component_names :=
              {
                !component_names with
                tools = Set.add !component_names.tools name;
              });
        return ())
      "Could not fetch tools"
  in

  (* Check for duplicates *)
  let check_duplicates map_temp existing map_name =
    let matching = Map.keys map_temp |> String.Set.of_list in
    let existing_keys = Map.keys existing |> String.Set.of_list in
    let duplicates = Set.inter matching existing_keys in
    if not (Set.is_empty duplicates) then
      raise
        (Mcp_shared.Exceptions.Mcp_error
           {
             Mcp.Types.code = -32602;
             (* INVALID_PARAMS *)
             message =
               sprintf "%s already exist in group %s."
                 (Set.to_list duplicates |> String.concat ~sep:", ")
                 map_name;
             data = None;
           })
  in

  check_duplicates !prompts_temp t.prompts "prompts";
  check_duplicates !resources_temp t.resources "resources";
  check_duplicates !tools_temp t.tools "tools";

  (* Commit changes *)
  Hashtbl.set t.sessions ~key:session ~data:!component_names;
  t.prompts <-
    Map.merge t.prompts !prompts_temp ~f:(fun ~key:_ -> function
      | `Left p -> Some p
      | `Right p -> Some p
      | `Both (_, p) -> Some p);
  t.resources <-
    Map.merge t.resources !resources_temp ~f:(fun ~key:_ -> function
      | `Left r -> Some r
      | `Right r -> Some r
      | `Both (_, r) -> Some r);
  t.tools <-
    Map.merge t.tools !tools_temp ~f:(fun ~key:_ -> function
      | `Left tl -> Some tl
      | `Right tl -> Some tl
      | `Both (_, tl) -> Some tl);
  t.tool_to_session <-
    Map.merge t.tool_to_session !tool_to_session_temp ~f:(fun ~key:_ -> function
      | `Left s -> Some s
      | `Right s -> Some s
      | `Both (_, s) -> Some s);

  return ()

(** {1 Connection Management} *)

let connect_to_server t ~server_params ?session_params ~stderr () =
  let session_params =
    Option.value session_params ~default:Client_session_parameters.default
  in

  (* Create stdio client that spawns the server process *)
  let%bind read_stream, write_stream =
    Mcp_client_transports.Stdio.stdio_client server_params ~stderr
  in

  (* Create session from the transport pipes *)
  let session =
    Session.create_from_pipes ~read_stream ~write_stream
      ?read_timeout:session_params.read_timeout_seconds
      ?sampling_callback:session_params.sampling_callback
      ?elicitation_callback:session_params.elicitation_callback
      ?list_roots_callback:session_params.list_roots_callback
      ?logging_callback:session_params.logging_callback
      ?message_handler:session_params.message_handler
      ?client_info:session_params.client_info ()
  in

  (* Initialize the session *)
  let%bind init_result = Session.initialize session in
  let server_info = init_result.Mcp.Types.server_info in

  (* PLACEHOLDER: Track cleanup resources (writer, process) for graceful
     disconnect. The stdio_client returns pipes but not the Process.t handle.
     Either: 1. Modify stdio_client to also return Process.t, or 2. Store a
     custom cleanup function. See session_group.todo for details. *)

  (* Aggregate components from this server *)
  let%bind () = aggregate_components t server_info session in

  return session

let disconnect_from_server t ~session =
  (* Check if session exists *)
  let session_known_for_components = Hashtbl.mem t.sessions session in
  let session_known_for_cleanup = Hashtbl.mem t.session_cleanups session in

  if not (session_known_for_components || session_known_for_cleanup) then
    raise
      (Mcp_shared.Exceptions.Mcp_error
         {
           Mcp.Types.code = -32602;
           (* INVALID_PARAMS *)
           message = "Provided session is not managed or already disconnected.";
           data = None;
         });

  (* Remove components *)
  (match Hashtbl.find t.sessions session with
  | None -> ()
  | Some component_names ->
    Hashtbl.remove t.sessions session;

    (* Remove prompts *)
    Set.iter component_names.prompts ~f:(fun name ->
        t.prompts <- Map.remove t.prompts name);

    (* Remove resources *)
    Set.iter component_names.resources ~f:(fun name ->
        t.resources <- Map.remove t.resources name);

    (* Remove tools *)
    Set.iter component_names.tools ~f:(fun name ->
        t.tools <- Map.remove t.tools name;
        t.tool_to_session <- Map.remove t.tool_to_session name));

  (* Cleanup session resources *)
  match Hashtbl.find t.session_cleanups session with
  | None -> return ()
  | Some (writer, process) -> (
    Hashtbl.remove t.session_cleanups session;

    (* Graceful shutdown sequence: 1. Close writer (stdin) to signal EOF to
       server 2. Wait up to 2 seconds for process to exit 3. Send SIGTERM if
       still running, wait 2 more seconds 4. Send SIGKILL as last resort *)

    (* Step 1: Close stdin pipe to signal server *)
    let%bind () = Writer.close writer in

    (* Step 2: Wait up to 2 seconds for graceful exit *)
    let wait_result =
      Clock_ns.with_timeout (Time_ns.Span.of_sec 2.0) (Process.wait process)
    in
    match%bind wait_result with
    | `Result _ -> return () (* Process exited gracefully *)
    | `Timeout -> (
      (* Step 3: Send SIGTERM *)
      Signal_unix.send_i Signal.term (`Pid (Process.pid process));
      let term_result =
        Clock_ns.with_timeout (Time_ns.Span.of_sec 2.0) (Process.wait process)
      in
      match%bind term_result with
      | `Result _ -> return () (* Process exited after SIGTERM *)
      | `Timeout ->
        (* Step 4: Send SIGKILL as last resort *)
        Signal_unix.send_i Signal.kill (`Pid (Process.pid process));
        let%bind _ = Process.wait process in
        return ()))

(** {1 Tool Calling} *)

let call_tool t ~name ~arguments ?read_timeout ?progress_callback () =
  (* Find the session for this tool *)
  match Map.find t.tool_to_session name with
  | None -> raise_s [%message "Tool not found" (name : string)]
  | Some session ->
    (* Get the original tool name (before name hook) *)
    let tool = Map.find_exn t.tools name in
    let original_name = tool.Mcp.Types.base_metadata.name in

    (* Call tool on the appropriate session *)
    Session.call_tool session original_name ~arguments ?read_timeout
      ?progress_callback ()
