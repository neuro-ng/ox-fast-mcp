(** HTTP utilities for FastMCP *)

(** Find an available port by letting the OS assign one.
    @return The port number assigned by the OS.
    @raise Failure if unable to get an INET socket address.
    @raise Unix_error for other socket-related errors. *)
val find_available_port : unit -> int 