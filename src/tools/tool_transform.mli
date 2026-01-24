(** Tool transformation for OxFastMCP

    Provides functions for creating transformed tools and forwarding tool calls.
    This enables tool composition, argument transformation, and result
    processing. *)

open Fmcp_types
open Tool_types
open Async

(** {1 Forwarding Functions} *)

val forward :
  Tool_types.t -> execution_context -> json -> content_type list Deferred.t
(** Forward a tool call and return processed content.

    Takes a tool and forwards the call with optional result processing. Returns
    content_type list for backward compatibility.

    @param tool The tool to forward to
    @param context Execution context
    @param arguments JSON arguments for the tool
    @return Content list from the tool execution *)

val forward_raw :
  Tool_types.t ->
  execution_context ->
  json ->
  (tool_result, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Forward a tool call and return the raw result.

    Similar to forward but returns the full tool_result including structured
    content, or propagates errors.

    @param tool The tool to forward to
    @param context Execution context
    @param arguments JSON arguments for the tool
    @return Result with tool_result or error_data *)

(** {1 Tool Creation} *)

val create_from_tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_fn:(json -> json) ->
  ?transform_args:(string * Arg_transform.t) list ->
  ?_annotations:'a ->
  ?_serializer:'b ->
  ?enabled:bool ->
  Tool_types.t ->
  Tool_types.t
(** Create a transformed tool from an existing tool.

    This wraps a tool with optional overrides for name, description, tags, and
    transformation functions. The resulting tool forwards calls to the original
    with any specified transformations applied.

    @param name Optional new name (defaults to original)
    @param description Optional new description
    @param tags Optional new tags
    @param transform_fn
      Optional function to transform arguments before forwarding
    @param transform_args Argument-specific transformations
    @param annotations Optional annotations (currently unused)
    @param serializer Optional custom serializer (currently unused)
    @param enabled Whether the new tool is enabled
    @param parent_tool The original tool to wrap
    @return A new transformed tool *)
