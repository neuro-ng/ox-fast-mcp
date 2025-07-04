open Core
open Async
open Mcp.Types
open Exceptions

module DuplicateBehavior = struct
  type t =
    | Warn
    | Replace
    | Error
    | Ignore
  [@@deriving sexp, compare, equal, enumerate]

  let of_string = function
    | "warn" -> Ok Warn
    | "replace" -> Ok Replace
    | "error" -> Ok Error
    | "ignore" -> Ok Ignore
    | s -> 
      let valid_values = List.map all ~f:(fun x -> 
        match sexp_of_t x with
        | Sexp.Atom s -> String.lowercase s
        | _ -> assert false)
      in
      Error (sprintf "Invalid duplicate_behavior: %s. Must be one of: %s"
               s (String.concat ~sep:", " valid_values))

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error msg -> failwith msg
end

module Tool = struct
  type t = {
    key : string;
    name : string option;
    description : string option;
    tags : string list;
    annotations : (string * string) list;
    parameters : Yojson.Safe.t;
    enabled : bool;
    fn : tool_handler;
  } [@@deriving fields]

  let with_key t new_key = { t with key = new_key }

  let enable t = 
    (* TODO: Implement enable functionality *)
    { t with enabled = true }

  let disable t = 
    (* TODO: Implement disable functionality *)
    { t with enabled = false }

  let to_mcp_tool ?overrides t =
    let base = 
      [ ("name", Option.value ~default:t.key t.name |> Yojson.Safe.from_string)
      ; ("description", Option.value ~default:"" t.description |> Yojson.Safe.from_string)
      ; ("parameters", t.parameters)
      ; ("annotations", `Assoc (List.map t.annotations ~f:(fun (k, v) -> (k, `String v))))
      ] 
    in
    match overrides with
    | Some o -> `Assoc (base @ o)
    | None -> `Assoc base

  let default_serializer data =
    Yojson.Safe.pretty_to_string ~std:true data

  let convert_to_content ?serializer result =
    let serialize = Option.value serializer ~default:default_serializer in
    match result with
    | `String s -> [create_text_content s]
    | `List l -> List.map l ~f:(fun x -> create_text_content (serialize x))
    | x -> [create_text_content (serialize x)]

  let from_function
      ?name
      ?description
      ?(tags = [])
      ?(annotations = [])
      ?(exclude_args = [])
      ?serializer
      ?(enabled = true)
      fn =
    let key =
      match name with
      | Some n -> String.lowercase n
      | None -> 
        (* TODO: Extract function name using Caml.Obj.extension_constructor *)
        "<function>"
    in
    { key
    ; name
    ; description
    ; tags
    ; annotations
    ; parameters = `Assoc []  (* TODO: Generate JSON schema from function type *)
    ; enabled
    ; fn = (fun ctx args -> 
        let%bind result = fn args in
        return (convert_to_content ?serializer result))
    }
end

type mounted_server = {
  prefix : string option;
  server : Server.t;  (* TODO: Define Server module *)
}

type t = {
  mutable tools : Tool.t String.Map.t;
  mutable mounted_servers : mounted_server list;
  mask_error_details : bool;
  duplicate_behavior : DuplicateBehavior.t;
}

let create ?(duplicate_behavior = DuplicateBehavior.Warn) ?(mask_error_details = true) () =
  { tools = String.Map.empty
  ; mounted_servers = []
  ; mask_error_details
  ; duplicate_behavior
  }

let mount t ~server ~prefix =
  t.mounted_servers <- { prefix; server } :: t.mounted_servers

let rec load_tools t ~via_server =
  let%bind all_tools = 
    Deferred.List.fold t.mounted_servers ~init:String.Map.empty ~f:(fun acc mounted ->
      Monitor.try_with (fun () ->
        let%bind child_results =
          if via_server then
            Server.list_tools mounted.server  (* TODO: Implement in Server module *)
          else
            Server.get_tools mounted.server   (* TODO: Implement in Server module *)
        in
        let child_dict =
          List.fold child_results ~init:String.Map.empty ~f:(fun acc tool ->
            Map.set acc ~key:tool.Tool.key ~data:tool)
        in
        match mounted.prefix with
        | Some prefix ->
          Map.fold child_dict ~init:acc ~f:(fun ~key ~data acc ->
            let prefixed_name = prefix ^ "_" ^ data.Tool.key in
            let prefixed_tool = Tool.with_key data prefixed_name in
            Map.set acc ~key:prefixed_tool.Tool.key ~data:prefixed_tool)
        | None ->
          return (Map.merge acc child_dict ~f:(fun ~key:_ -> function
            | `Left x | `Right x -> Some x
            | `Both (_, x) -> Some x)))
      >>| function
      | Ok result -> result
      | Error exn ->
          Log.Global.error "Failed to get tools from mounted server '%s': %s"
            (Option.value mounted.prefix ~default:"<no prefix>")
            (Exn.to_string exn);
          acc)
  in
  return (Map.merge all_tools t.tools ~f:(fun ~key:_ -> function
    | `Left x | `Right x -> Some x
    | `Both (_, x) -> Some x))

let has_tool t key =
  let%bind tools = load_tools t ~via_server:false in
  return (Map.mem tools key)

let get_tool t key =
  let%bind tools = load_tools t ~via_server:false in
  match Map.find tools key with
  | Some tool -> return tool
  | None -> raise (Not_found_s (Sexp.message "Tool not found" [("key", Atom key)]))

let get_tools t = load_tools t ~via_server:false

let list_tools t =
  let%bind tools = load_tools t ~via_server:true in
  return (Map.data tools)

let add_tool_from_fn
    t
    fn
    ?name
    ?description
    ?(tags = [])
    ?(annotations = [])
    ?serializer
    ?(exclude_args = [])
    () =
  if Settings.deprecation_warnings then
    Log.Global.deprecated ~since:"2.7.0"
      "ToolManager.add_tool_from_fn() is deprecated. Use Tool.from_function() and call add_tool() instead.";
  let tool =
    Tool.from_function
      fn
      ?name
      ?description
      ~tags
      ~annotations
      ~exclude_args
      ?serializer
  in
  add_tool t tool

let add_tool t tool =
  if not tool.Tool.enabled then
    tool
  else
    match Map.find t.tools tool.Tool.key with
    | Some existing ->
      (match t.duplicate_behavior with
       | Warn ->
         Log.Global.warning "Tool already exists: %s" tool.Tool.key;
         t.tools <- Map.set t.tools ~key:tool.Tool.key ~data:tool;
         tool
       | Replace ->
         t.tools <- Map.set t.tools ~key:tool.Tool.key ~data:tool;
         tool
       | Error ->
         raise (Tool_error (sprintf "Tool already exists: %s" tool.Tool.key))
       | Ignore -> existing)
    | None ->
      t.tools <- Map.set t.tools ~key:tool.Tool.key ~data:tool;
      tool

let remove_tool t key =
  match Map.find t.tools key with
  | Some _ -> t.tools <- Map.remove t.tools key
  | None -> raise (Not_found_s (Sexp.message "Tool not found" [("key", Atom key)]))

let call_tool t key arguments =
  match Map.find t.tools key with
  | Some tool ->
    Monitor.try_with ~extract_exn:true
      (fun () -> tool.Tool.fn arguments)
    >>| function
    | Ok result -> result
    | Error exn ->
      Log.Global.error "Error calling tool %s: %s" key (Exn.to_string exn);
      if t.mask_error_details then
        raise (Tool_error (sprintf "Error calling tool %s" key))
      else
        raise exn
  | None ->
    (* Try mounted servers *)
    let rec try_mounted = function
      | [] -> raise (Not_found_s (Sexp.message "Tool not found" [("key", Atom key)]))
      | mounted :: rest ->
        let tool_key =
          match mounted.prefix with
          | Some prefix when String.is_prefix key ~prefix:(prefix ^ "_") ->
            String.chop_prefix_exn key ~prefix:(prefix ^ "_")
          | _ -> key
        in
        match%bind
          Monitor.try_with (fun () -> 
            Server.call_tool mounted.server tool_key arguments)
        with
        | Ok result -> return result
        | Error (Not_found_s _) -> try_mounted rest
        | Error exn ->
            Log.Global.error "Error calling tool %s on mounted server %s: %s"
              key (Option.value mounted.prefix ~default:"<no prefix>") (Exn.to_string exn);
            try_mounted rest
    in
    try_mounted (List.rev t.mounted_servers) 