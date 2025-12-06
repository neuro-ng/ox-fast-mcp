(** Tool transformation for OxFastMCP - TEMPORARILY SIMPLIFIED

    NOTE: This module is temporarily simplified to get the build working. Full
    implementation will be completed after core tool types are stable.

    See: COMPLIANCE_ACTION_PLAN.md Task 1.1 TODO: Fully implement after
    tool_types stabilizes *)

open Core

(** Placeholder forward functions - to be properly implemented *)
let forward _ctx _arguments =
  failwith
    "forward() temporarily disabled - needs full refactor with new tool types"

let forward_raw _ctx _arguments =
  failwith
    "forward_raw() temporarily disabled - needs full refactor with new tool \
     types"

(** Placeholder create_from_tool - to be properly implemented *)
let create_from_tool ?name:_ ?description:_ ?tags:_ ?transform_fn:_
    ?transform_args:_ ?_annotations:_ ?_serializer:_ ?enabled:_
    (_parent_tool : Tool_types.tool_function) : Tool_types.t =
  failwith
    "create_from_tool temporarily disabled - needs full refactor with new tool \
     types"
