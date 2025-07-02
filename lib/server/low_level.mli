open Core

(** Notification options for server capabilities *)
type notification_options = {
  prompts_changed : bool;
  resources_changed : bool;
  tools_changed : bool;
}

(** Server initialization options *)
type initialization_options = {
  notification_options : notification_options option;
  experimental_capabilities : (string, Yojson.Safe.t) Hashtbl.t option;
  additional_options : (string, Yojson.Safe.t) Hashtbl.t;
}

(** Low level server implementation *)
module LowLevelServer : sig
  type t
  
  (** Create a new low level server
      @param name Optional server name, defaults to "FastMCP Server"
      @return New server instance *)
  val create : ?name:string -> unit -> t

  (** Create initialization options for the server
      @param notification_options Optional notification settings
      @param experimental_capabilities Optional map of experimental features
      @param additional_options Additional initialization options
      @return Initialization options structure *)
  val create_initialization_options :
    t ->
    ?notification_options:notification_options option ->
    ?experimental_capabilities:(string, Yojson.Safe.t) Hashtbl.t option ->
    ?additional_options:(string, Yojson.Safe.t) Hashtbl.t ->
    unit ->
    initialization_options

  (** Get the server's notification options
      @return Current notification settings *)
  val get_notification_options : t -> notification_options

  (** Get the server's name
      @return Server name *)
  val get_name : t -> string
end 