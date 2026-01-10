(** Helper functions for pure task management.

    These helpers work with pure task operations and don't require server
    dependencies.

    WARNING: These APIs are experimental and may change without notice. *)

module Types = Mcp.Types

type task_status = string
(** Task status type - using string until proper task types are added to
    Mcp.Types *)

let is_terminal status =
  match status with
  | "completed" | "failed" | "cancelled" -> true
  | "working" | "input_required" | _ -> false

(* Metadata key for model-immediate-response (per MCP spec) *)
let model_immediate_response_key =
  "io.modelcontextprotocol/model-immediate-response"

(* Metadata key for associating requests with a task (per MCP spec) *)
let related_task_metadata_key = "io.modelcontextprotocol/related-task"
