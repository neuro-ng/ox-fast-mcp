open Core
open Async
open Errors

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
    fn : (Yojson.Safe.t -> Content_block.t list Deferred.t [@sexp.opaque]);
  }
  [@@deriving sexp]

  let with_key t new_key = { t with key = new_key }

  let enable t =
    match Context.get () with
    | Some ctx -> Context.queue_tool_list_changed ctx
    | None -> ()

  let disable t =
    match Context.get () with
    | Some ctx -> Context.queue_tool_list_changed ctx
    | None -> ()

  let to_mcp_tool ?overrides t =
    let base = 
      [ ("name", Option.value ~default:t.key t.name |> Yojson.Safe.from_string)
      ; ("description", Option.value ~default:"" t.description |> Yojson.Safe.from_string)
      ; ("inputSchema", t.parameters)
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
    | `String s -> [Content_block.create_text s]
    | `List l -> List.map l ~f:(fun x -> Content_block.create_text (serialize x))
    | x -> [Content_block.create_text (serialize x)]

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
      | None -> "<function>"  (* TODO: Get function name *)
    in
    { key
    ; name
    ; description
    ; tags
    ; annotations
    ; parameters = `Assoc []  (* TODO: Generate proper JSON schema *)
    ; enabled
    ; fn = (fun args -> 
        let%bind result = fn args in
        return (convert_to_content ?serializer result))
    }
end

type mounted_server = {
  prefix : string option;
  server : Server.t;
}
[@@deriving sexp]

type t = {
  mutable tools : Tool.t String.Map.t;
  mutable mounted_servers : mounted_server list;
  mask_error_details : bool;
  duplicate_behavior : DuplicateBehavior.t;
}
[@@deriving sexp]

let create ?(duplicate_behavior = DuplicateBehavior.Warn) ?(mask_error_details = true) () =
  let mask_error_details = Option.value mask_error_details ~default:Settings.mask_error_details in
  let duplicate_behavior = 
    match duplicate_behavior with
    | None -> DuplicateBehavior.Warn
    | Some behavior -> behavior
  in
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
      try%bind
        let%bind child_results =
          if via_server then
            Server.list_tools mounted.server
          else
            load_tools mounted.server.tool_manager ~via_server:false
        in
        let child_dict =
          List.fold child_results ~init:String.Map.empty ~f:(fun acc tool ->
            Map.set acc ~key:tool.Tool.key ~data:tool)
        in
        match mounted.prefix with
        | Some prefix ->
          Map.fold child_dict ~init:acc ~f:(fun ~key ~data acc ->
            let prefixed_tool = Tool.with_key data (prefix ^ "_" ^ data.Tool.key) in
            Map.set acc ~key:prefixed_tool.Tool.key ~data:prefixed_tool)
        | None ->
          return (Map.merge acc child_dict ~f:(fun ~key:_ -> function
            | `Left x | `Right x -> Some x
            | `Both (_, x) -> Some x))
      with exn ->
        Log.Global.warning "Failed to get tools from mounted server '%s': %s"
          (Option.value mounted.prefix ~default:"<no prefix>")
          (Exn.to_string exn);
        return acc)
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
  | None -> not_found_error (sprintf "Tool %s not found" key)

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
         tool_error (sprintf "Tool already exists: %s" tool.Tool.key)
       | Ignore -> existing)
    | None ->
      t.tools <- Map.set t.tools ~key:tool.Tool.key ~data:tool;
      tool

let remove_tool t key =
  match Map.find t.tools key with
  | Some _ -> t.tools <- Map.remove t.tools key
  | None -> not_found_error (sprintf "Tool %s not found" key)

let call_tool t key arguments =
  let%bind () = return () in
  match Map.find t.tools key with
  | Some tool ->
    (try%bind
       let%bind result = tool.fn arguments in
       return result
     with
     | Tool_error _ as e ->
       Log.Global.error ~exn:e "Error calling tool %s" key;
       raise e
     | exn ->
       Log.Global.error ~exn "Error calling tool %s" key;
       if t.mask_error_details then
         tool_error (sprintf "Error calling tool %s" key)
       else
         tool_error ~exn (sprintf "Error calling tool %s" key))
  | None ->
    let rec try_mounted = function
      | [] -> not_found_error (sprintf "Tool %s not found" key)
      | mounted :: rest ->
        let tool_key =
          match mounted.prefix with
          | Some prefix when String.is_prefix key ~prefix:(prefix ^ "_") ->
            String.chop_prefix_exn key ~prefix:(prefix ^ "_")
          | _ -> key
        in
        try%bind
          Server.call_tool mounted.server tool_key arguments
        with
        | Not_found_error _ -> try_mounted rest
        | exn ->
          Log.Global.error ~exn "Error calling tool %s on mounted server %s"
            key (Option.value mounted.prefix ~default:"<no prefix>");
          try_mounted rest
    in
    try_mounted (List.rev t.mounted_servers) 

let get_tools_from_mounted_servers mounted_servers =
  List.fold mounted_servers ~init:(return []) ~f:(fun acc mounted ->
    match%bind acc with
    | Error e -> return (Error e)
    | Ok acc ->
      try%bind
        let%bind child_results =
          if mounted.via_server then
            Server.list_tools mounted.server
          else
            mounted.server.list_tools ()
        in
        let tools =
          List.map child_results ~f:(fun tool ->
            let name =
              match mounted.prefix with
              | Some prefix -> sprintf "%s.%s" prefix tool.name
              | None -> tool.name
            in
            { tool with name }
          )
        in
        return (Ok (acc @ tools))
      with exn ->
        Log.Global.warning "Failed to get tools from mounted server '%s': %s"
          (Option.value mounted.prefix ~default:"<no prefix>")
          (Exn.to_string exn);
        return (Ok acc)) 