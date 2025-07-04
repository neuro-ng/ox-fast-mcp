(** [find_available_port ()] finds an available port by letting the OS assign one.
    Returns the port number assigned by the OS. *)
val find_available_port : unit -> int 