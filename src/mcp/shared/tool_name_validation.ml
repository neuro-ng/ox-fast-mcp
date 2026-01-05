(** Tool name validation utilities according to SEP-986.

    Tool names SHOULD be between 1 and 128 characters in length (inclusive).
    Tool names are case-sensitive. Allowed characters: uppercase and lowercase
    ASCII letters (A-Z, a-z), digits (0-9), underscore (_), dash (-), and dot
    (.). Tool names SHOULD NOT contain spaces, commas, or other special
    characters.

    See:
    https://modelcontextprotocol.io/specification/2025-11-25/server/tools#tool-names *)

open Core

(** Regular expression for valid tool names according to SEP-986 specification *)
let tool_name_regex = Re.Posix.compile_pat "^[A-Za-z0-9._-]{1,128}$"

(** SEP reference URL for warning messages *)
let sep_986_url =
  "https://modelcontextprotocol.io/specification/2025-11-25/server/tools#tool-names"

type tool_name_validation_result = { is_valid : bool; warnings : string list }
[@@deriving sexp, compare]
(** Result of tool name validation *)

(** Validate a tool name according to the SEP-986 specification.

    @param name The tool name to validate
    @return Validation result containing validity status and any warnings *)
let validate_tool_name (name : string) : tool_name_validation_result =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in

  (* Check for empty name *)
  if String.is_empty name then
    { is_valid = false; warnings = [ "Tool name cannot be empty" ] }
  else if String.length name > 128 then
    (* Check length *)
    {
      is_valid = false;
      warnings =
        [
          sprintf
            "Tool name exceeds maximum length of 128 characters (current: %d)"
            (String.length name);
        ];
    }
  else (
    (* Check for problematic patterns (warnings, not validation failures) *)
    if String.contains name ' ' then
      add_warning "Tool name contains spaces, which may cause parsing issues";

    if String.contains name ',' then
      add_warning "Tool name contains commas, which may cause parsing issues";

    (* Check for potentially confusing leading/trailing characters *)
    if String.is_prefix name ~prefix:"-" || String.is_suffix name ~suffix:"-"
    then
      add_warning
        "Tool name starts or ends with a dash, which may cause parsing issues \
         in some contexts";

    if String.is_prefix name ~prefix:"." || String.is_suffix name ~suffix:"."
    then
      add_warning
        "Tool name starts or ends with a dot, which may cause parsing issues \
         in some contexts";

    (* Check for invalid characters *)
    if not (Re.execp tool_name_regex name) then (
      (* Find all invalid characters (unique, preserving order) *)
      let invalid_chars =
        String.to_list name
        |> List.filter ~f:(fun c ->
               not
                 (Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'
                || Char.equal c '.'))
        |> List.dedup_and_sort ~compare:Char.compare
      in
      add_warning
        (sprintf "Tool name contains invalid characters: %s"
           (String.concat ~sep:", "
              (List.map invalid_chars ~f:(fun c -> sprintf "'%c'" c))));
      add_warning
        "Allowed characters are: A-Z, a-z, 0-9, underscore (_), dash (-), and \
         dot (.)";
      { is_valid = false; warnings = List.rev !warnings })
    else { is_valid = true; warnings = List.rev !warnings })

(** Log warnings for non-conforming tool names.

    @param name The tool name that triggered the warnings
    @param warnings List of warning messages to log *)
let issue_tool_name_warning (name : string) (warnings : string list) : unit =
  if not (List.is_empty warnings) then (
    Logs.warn (fun m -> m "Tool name validation warning for \"%s\":" name);
    List.iter warnings ~f:(fun warning ->
        Logs.warn (fun m -> m "  - %s" warning));
    Logs.warn (fun m ->
        m
          "Tool registration will proceed, but this may cause compatibility \
           issues.");
    Logs.warn (fun m ->
        m
          "Consider updating the tool name to conform to the MCP tool naming \
           standard.");
    Logs.warn (fun m -> m "See SEP-986 (%s) for more details." sep_986_url))

(** Validate a tool name and issue warnings for non-conforming names.

    This is the primary entry point for tool name validation. It validates the
    name and logs any warnings via the logging module.

    @param name The tool name to validate
    @return True if the name is valid, False otherwise *)
let validate_and_warn_tool_name (name : string) : bool =
  let result = validate_tool_name name in
  issue_tool_name_warning name result.warnings;
  result.is_valid
