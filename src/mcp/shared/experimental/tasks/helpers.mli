(** Helper functions for pure task management.

    These helpers work with pure task operations and don't require server
    dependencies.

    WARNING: These APIs are experimental and may change without notice. *)

module Types = Mcp.Types

type task_status = string
(** Task status type - using string until proper task types are added to
    Mcp.Types *)

val is_terminal : task_status -> bool
(** Check if a task status represents a terminal state.

    Terminal states are those where the task has finished and will not change.

    Args: status: The task status to check

    Returns: True if the status is terminal (completed, failed, or cancelled) *)

val model_immediate_response_key : string
(** Metadata key for model-immediate-response (per MCP spec). Servers MAY
    include this in CreateTaskResult._meta to provide an immediate response
    string while the task executes in the background. *)

val related_task_metadata_key : string
(** Metadata key for associating requests with a task (per MCP spec). *)
