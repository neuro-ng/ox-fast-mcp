(** Tool transformation for OxFastMCP

    Provides functions for creating transformed tools and forwarding tool calls.
    This enables tool composition, argument transformation, and result processing.

    See: COMPLIANCE_ACTION_PLAN.md Task 1.1 *)

open! Core
open! Async
open Tool_types
open Fmcp_types

(** {1 Forwarding Functions} *)

(** Forward a tool call and return processed content.
    
    Takes a tool and forwards the call with optional result processing.
    Returns content_type list for backward compatibility.
    
    @param tool The tool to forward to
    @param context Execution context
    @param arguments JSON arguments for the tool
    @return Content list from the tool execution *)
let forward (tool : Tool_types.t) (context : execution_context)
    (arguments : Yojson.Safe.t) : content_type list Deferred.t =
  let%bind result = Tool_types.run tool ~context ~arguments in
  match result with
  | Ok tool_result -> return tool_result.content
  | Error error_data ->
    (* Convert error to error content *)
    return [ create_text_content (sprintf "Error: %s" error_data.Ox_fast_mcp.Exceptions.message) ]

(** Forward a tool call and return the raw result.
    
    Similar to forward but returns the full tool_result including
    structured content, or propagates errors.
    
    @param tool The tool to forward to
    @param context Execution context
    @param arguments JSON arguments for the tool
    @return Result with tool_result or error_data *)
let forward_raw (tool : Tool_types.t) (context : execution_context)
    (arguments : Yojson.Safe.t) :
    (tool_result, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t =
  Tool_types.run tool ~context ~arguments

(** {1 Tool Creation} *)

(** Create a transformed tool from an existing tool.

    This wraps a tool with optional overrides for name, description, tags,
    and transformation functions. The resulting tool forwards calls to
    the original with any specified transformations applied.

    @param name Optional new name (defaults to original)
    @param description Optional new description
    @param tags Optional new tags
    @param transform_fn Optional function to transform arguments before forwarding
    @param transform_args Argument-specific transformations
    @param annotations Optional annotations
    @param serializer Optional custom serializer (currently unused)
    @param enabled Whether the new tool is enabled
    @param parent_tool The original tool to wrap
    @return A new transformed tool *)
let create_from_tool ?name ?description ?tags ?transform_fn
    ?transform_args:(args_list : (string * Arg_transform.t) list option)
    ?_annotations ?_serializer ?(enabled = true)
    (parent_tool : Tool_types.t) : Tool_types.t =
  (* Convert list-based transform_args to Map if provided *)
  let transform_args =
    match args_list with
    | None -> Core.String.Map.empty
    | Some args ->
      Core.List.fold args ~init:Core.String.Map.empty
        ~f:(fun acc (param_name, transform) ->
          Core.Map.set acc ~key:param_name ~data:transform)
  in
  
  Tool_types.create_transformed_tool parent_tool ?name ?description ?tags
    ?transform_fn ~transform_args ~enabled ()
