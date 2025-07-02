type t = {
  deprecation_warnings : bool;
}

val create : ?deprecation_warnings:bool -> unit -> t
(** Create settings with optional parameters.
    @param deprecation_warnings Whether to show deprecation warnings (default: true) *)

val default : t
(** Default settings instance *)

val get_deprecation_warnings : t -> bool
(** Get whether deprecation warnings are enabled *)

val set_deprecation_warnings : t -> bool -> t
(** Set whether deprecation warnings are enabled *) 