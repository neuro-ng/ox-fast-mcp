open Cohttp_lwt_unix

(** Basic timing middleware *)
module TimingMiddleware : sig
  type t

  val create :
    ?logger:Logs.src ->
    ?log_level:Logs.level ->
    unit -> t

  val middleware :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t
end

(** Detailed timing middleware with per-operation breakdowns *)
module DetailedTimingMiddleware : sig
  type t

  val create :
    ?logger:Logs.src ->
    ?log_level:Logs.level ->
    unit -> t

  val middleware :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_call_tool :
    t ->
    string ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_read_resource :
    t ->
    string ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_get_prompt :
    t ->
    string ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_list_tools :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_list_resources :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_list_resource_templates :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t

  val on_list_prompts :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t
end 