(** Prompt implementation for FastMCP OCaml *)

open Utilities.Types

(** Prompt handler signature *)
type prompt_handler = execution_context -> (string * json) list -> prompt_message list Lwt.t

(** Function prompt definition *)
type function_prompt = {
  name : string;
  description : string;
  arguments : prompt_argument list option;
  handler : prompt_handler;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}

(** Prompt manager for managing multiple prompts *)
type prompt_manager = {
  mutable prompts : (string, function_prompt) Hashtbl.t;
  mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  mutable mask_error_details : bool;
}

(** Create a new prompt manager *)
let create_manager ?(duplicate_behavior = `Warn) ?(mask_error_details = false) () =
  {
    prompts = Hashtbl.create 16;
    duplicate_behavior;
    mask_error_details;
  }

(** Create a new function prompt *)
let create_prompt ~name ~description ?(arguments = None) ?(enabled = true) 
    ?(tags = []) ?(annotations = None) handler =
  {
    name;
    description;
    arguments;
    handler;
    enabled;
    tags;
    annotations;
  }

(** Convert prompt to MCP prompt definition *)
let to_mcp_prompt prompt =
  {
    name = prompt.name;
    description = prompt.description;
    arguments = prompt.arguments;
  }

(** Validate prompt arguments *)
let validate_arguments args =
  List.for_all (fun arg -> String.length arg.name > 0) args

(** Register a prompt with the manager *)
let register_prompt manager prompt =
  let existing = Hashtbl.mem manager.prompts prompt.name in
  match existing, manager.duplicate_behavior with
  | true, `Error ->
    failwith ("Prompt with name '" ^ prompt.name ^ "' already exists")
  | true, `Warn ->
    Printf.eprintf "Warning: Prompt '%s' already exists, replacing\n%!" prompt.name;
    Hashtbl.replace manager.prompts prompt.name prompt
  | true, `Replace ->
    Hashtbl.replace manager.prompts prompt.name prompt
  | true, `Ignore ->
    (* Do nothing - keep existing prompt *)
    ()
  | false, _ ->
    Hashtbl.add manager.prompts prompt.name prompt

(** Remove a prompt from the manager *)
let remove_prompt manager name =
  Hashtbl.remove manager.prompts name

(** Get a prompt by name *)
let get_prompt manager name =
  try
    Some (Hashtbl.find manager.prompts name)
  with Not_found ->
    None

(** Get all prompts *)
let get_all_prompts manager =
  Hashtbl.fold (fun _name prompt acc -> prompt :: acc) manager.prompts []

(** Get enabled prompts only *)
let get_enabled_prompts manager =
  Hashtbl.fold (fun _name prompt acc ->
    if prompt.enabled then prompt :: acc else acc
  ) manager.prompts []

(** Filter prompts by tags *)
let filter_prompts_by_tags manager tags =
  Hashtbl.fold (fun _name prompt acc ->
    let has_any_tag = List.exists (fun tag ->
      List.mem tag prompt.tags
    ) tags in
    if has_any_tag then prompt :: acc else acc
  ) manager.prompts []

(** Get prompt count *)
let prompt_count manager = Hashtbl.length manager.prompts

(** Execute a prompt *)
let execute_prompt manager prompt_name context args =
  match get_prompt manager prompt_name with
  | None ->
    Lwt.fail (Failure ("Prompt not found: " ^ prompt_name))
  | Some prompt when not prompt.enabled ->
    Lwt.fail (Failure ("Prompt disabled: " ^ prompt_name))
  | Some prompt ->
    Lwt.catch
      (fun () -> prompt.handler context args)
      (fun exn ->
        let error_msg = 
          if manager.mask_error_details then
            "Prompt execution failed"
          else
            "Prompt execution failed: " ^ (Printexc.to_string exn)
        in
        Lwt.fail (Failure error_msg))

(** Enable a prompt *)
let enable_prompt manager name =
  match get_prompt manager name with
  | Some prompt ->
    let enabled_prompt = { prompt with enabled = true } in
    Hashtbl.replace manager.prompts name enabled_prompt;
    true
  | None -> false

(** Disable a prompt *)
let disable_prompt manager name =
  match get_prompt manager name with
  | Some prompt ->
    let disabled_prompt = { prompt with enabled = false } in
    Hashtbl.replace manager.prompts name disabled_prompt;
    true
  | None -> false

(** Check if a prompt is enabled *)
let is_prompt_enabled manager name =
  match get_prompt manager name with
  | Some prompt -> prompt.enabled
  | None -> false

(** Update prompt tags *)
let update_prompt_tags manager name new_tags =
  match get_prompt manager name with
  | Some prompt ->
    let updated_prompt = { prompt with tags = new_tags } in
    Hashtbl.replace manager.prompts name updated_prompt;
    true
  | None -> false

(** Add tags to a prompt *)
let add_prompt_tags manager name additional_tags =
  match get_prompt manager name with
  | Some prompt ->
    let combined_tags = List.rev_append additional_tags prompt.tags |> 
                       List.sort_uniq String.compare in
    let updated_prompt = { prompt with tags = combined_tags } in
    Hashtbl.replace manager.prompts name updated_prompt;
    true
  | None -> false

(** Remove tags from a prompt *)
let remove_prompt_tags manager name tags_to_remove =
  match get_prompt manager name with
  | Some prompt ->
    let filtered_tags = List.filter (fun tag ->
      not (List.mem tag tags_to_remove)
    ) prompt.tags in
    let updated_prompt = { prompt with tags = filtered_tags } in
    Hashtbl.replace manager.prompts name updated_prompt;
    true
  | None -> false

(** Helper: Create a simple summarization prompt for testing *)
let create_summarization_prompt () =
  let handler _ctx args =
    let text = match List.assoc_opt "text" args with
      | Some (`String t) -> t
      | _ -> "No text provided"
    in
    
    let max_length = match List.assoc_opt "max_length" args with
      | Some (`Int l) -> l
      | _ -> 100
    in
    
    let summary_text = if String.length text > max_length then
      String.sub text 0 max_length ^ "..."
    else
      text
    in
    
    Lwt.return [
      { role = "system"; content = create_text_content "You are a helpful summarization assistant." };
      { role = "user"; content = create_text_content ("Please summarize: " ^ text) };
      { role = "assistant"; content = create_text_content ("Summary: " ^ summary_text) }
    ]
  in
  
  let arguments = Some [
    { name = "text"; description = Some "Text to summarize"; required = true };
    { name = "max_length"; description = Some "Maximum summary length"; required = false }
  ] in
  
  create_prompt
    ~name:"summarize"
    ~description:"Summarize text content"
    ~arguments
    ~tags:["text"; "summarization"]
    handler

(** Helper: Create a translation prompt *)
let create_translation_prompt () =
  let handler _ctx args =
    let text = match List.assoc_opt "text" args with
      | Some (`String t) -> t
      | _ -> "No text provided"
    in
    
    let target_lang = match List.assoc_opt "target_language" args with
      | Some (`String lang) -> lang
      | _ -> "English"
    in
    
    Lwt.return [
      { role = "system"; content = create_text_content ("You are a professional translator. Translate to " ^ target_lang ^ ".") };
      { role = "user"; content = create_text_content ("Translate: " ^ text) };
    ]
  in
  
  let arguments = Some [
    { name = "text"; description = Some "Text to translate"; required = true };
    { name = "target_language"; description = Some "Target language"; required = true }
  ] in
  
  create_prompt
    ~name:"translate"
    ~description:"Translate text to target language"
    ~arguments
    ~tags:["text"; "translation"]
    handler

(** Helper: Create a creative writing prompt *)
let create_creative_writing_prompt () =
  let handler _ctx args =
    let topic = match List.assoc_opt "topic" args with
      | Some (`String t) -> t
      | _ -> "adventure"
    in
    
    let style = match List.assoc_opt "style" args with
      | Some (`String s) -> s
      | _ -> "narrative"
    in
    
    Lwt.return [
      { role = "system"; content = create_text_content ("You are a creative writer specializing in " ^ style ^ " style.") };
      { role = "user"; content = create_text_content ("Write about: " ^ topic) };
    ]
  in
  
  let arguments = Some [
    { name = "topic"; description = Some "Topic to write about"; required = true };
    { name = "style"; description = Some "Writing style"; required = false }
  ] in
  
  create_prompt
    ~name:"creative_write"
    ~description:"Generate creative writing content"
    ~arguments
    ~tags:["creative"; "writing"]
    handler

(** Clear all prompts from manager *)
let clear_prompts manager =
  Hashtbl.clear manager.prompts

(** Prompt statistics *)
type prompt_stats = {
  total_prompts : int;
  enabled_prompts : int;
  disabled_prompts : int;
  tags_used : string list;
}

(** Get prompt statistics *)
let get_prompt_stats manager =
  let all_prompts = get_all_prompts manager in
  let enabled_count = List.length (List.filter (fun p -> p.enabled) all_prompts) in
  let all_tags = List.fold_left (fun acc prompt ->
    List.rev_append prompt.tags acc
  ) [] all_prompts |> List.sort_uniq String.compare in
  
  {
    total_prompts = List.length all_prompts;
    enabled_prompts = enabled_count;
    disabled_prompts = List.length all_prompts - enabled_count;
    tags_used = all_tags;
  }

(** Template rendering utilities *)
module Template = struct
  (** Simple template variable substitution *)
  let substitute_variables template vars =
    List.fold_left (fun acc (key, value) ->
      let placeholder = "{{" ^ key ^ "}}" in
      let value_str = match value with
        | `String s -> s
        | `Int i -> string_of_int i
        | `Float f -> string_of_float f
        | `Bool b -> string_of_bool b
        | _ -> "null"
      in
      (* Simple string replacement - split on placeholder and rejoin with value *)
      let rec replace s =
        let len_s = String.length s in
        let len_p = String.length placeholder in
        try
          let idx = String.index_from s 0 placeholder.[0] in
          if idx + len_p <= len_s && String.sub s idx len_p = placeholder then
            let before = String.sub s 0 idx in
            let after = String.sub s (idx + len_p) (len_s - idx - len_p) in
            before ^ value_str ^ replace after
          else
            let before = String.sub s 0 (idx + 1) in
            let after = String.sub s (idx + 1) (len_s - idx - 1) in
            before ^ replace after
        with Not_found -> s
      in
      replace acc
    ) template vars
  
  (** Create a template-based prompt *)
  let create_template_prompt ~name ~description ~template ?(arguments = None) ?(tags = []) () =
    let handler _ctx args =
      let rendered_text = substitute_variables template args in
      Lwt.return [
        { role = "user"; content = create_text_content rendered_text }
      ]
    in
    
    create_prompt ~name ~description ~arguments ~tags handler
end 