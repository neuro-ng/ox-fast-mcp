(** Tool name validation utilities according to SEP-986.

    See:
    https://modelcontextprotocol.io/specification/2025-11-25/server/tools#tool-names *)

type tool_name_validation_result = { is_valid : bool; warnings : string list }
[@@deriving sexp, compare]
(** Result of tool name validation *)

val validate_tool_name : string -> tool_name_validation_result
(** Validate a tool name according to the SEP-986 specification.

    @param name The tool name to validate
    @return Validation result containing validity status and any warnings *)

val issue_tool_name_warning : string -> string list -> unit
(** Log warnings for non-conforming tool names.

    @param name The tool name that triggered the warnings
    @param warnings List of warning messages to log *)

val validate_and_warn_tool_name : string -> bool
(** Validate a tool name and issue warnings for non-conforming names.

    This is the primary entry point for tool name validation.

    @param name The tool name to validate
    @return True if the name is valid, False otherwise *)
