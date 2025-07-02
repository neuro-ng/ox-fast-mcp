(** Base module for FastMCP components *)

(** Convert a list to a set, defaulting to an empty set if None *)
let convert_set_default_none (maybe_set : 'a list option) : 'a list =
  match maybe_set with
  | None -> []
  | Some lst -> lst |> List.sort_uniq compare

(** Base type for FastMCP components *)
type 'a fastmcp_component = {
  name : string;  (** The name of the component *)
  description : string option;  (** The description of the component *)
  tags : string list;  (** Tags for the component *)
  enabled : bool;  (** Whether the component is enabled *)
  key : string option;  (** Internal key for bookkeeping *)
  extra : 'a;  (** Extra data specific to each component type *)
}

(** Create a new FastMCP component *)
let create_component
    ?(description = None)
    ?(tags = [])
    ?(enabled = true)
    ?(key = None)
    ~name
    ~extra
    () =
  {
    name;
    description;
    tags = convert_set_default_none (Some tags);
    enabled;
    key;
    extra;
  }

(** Get the component's key, falling back to name if not set *)
let get_key component =
  match component.key with
  | Some k -> k
  | None -> component.name

(** Create a copy of the component with a new key *)
let with_key component new_key =
  { component with key = Some new_key }

(** Enable the component *)
let enable component =
  { component with enabled = true }

(** Disable the component *)
let disable component =
  { component with enabled = false }

(** Compare two components for equality *)
let equal a b =
  a.name = b.name &&
  a.description = b.description &&
  List.sort_uniq compare a.tags = List.sort_uniq compare b.tags &&
  a.enabled = b.enabled &&
  a.extra = b.extra

(** Convert component to string representation *)
let to_string component =
  let tags_str = String.concat ", " component.tags in
  let desc_str = match component.description with
    | Some d -> d
    | None -> "None"
  in
  Printf.sprintf
    "FastMCPComponent(name=%s, description=%s, tags=[%s], enabled=%b)"
    component.name
    desc_str
    tags_str
    component.enabled

(** JSON serialization *)
let component_to_yojson component =
  let base = [
    ("name", `String component.name);
    ("enabled", `Bool component.enabled);
    ("tags", `List (List.map (fun t -> `String t) component.tags))
  ] in
  let with_desc = match component.description with
    | Some desc -> ("description", `String desc) :: base
    | None -> base
  in
  `Assoc with_desc

(** JSON deserialization *)
let component_of_yojson json =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "name" fields with
     | Some (`String name) ->
       let description = match List.assoc_opt "description" fields with
         | Some (`String desc) -> Some desc
         | _ -> None
       in
       let enabled = match List.assoc_opt "enabled" fields with
         | Some (`Bool e) -> e
         | _ -> true
       in
       let tags = match List.assoc_opt "tags" fields with
         | Some (`List tags) ->
           List.filter_map (function
             | `String tag -> Some tag
             | _ -> None
           ) tags
         | _ -> []
       in
       Ok { name; description; tags; enabled; key = None; extra = () }
     | _ -> Error "Missing or invalid 'name' field")
  | _ -> Error "Invalid JSON format for component" 