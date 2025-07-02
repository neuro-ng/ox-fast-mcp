(** HTTP utilities for FastMCP *)

open Unix

(** Find an available port by letting the OS assign one *)
let find_available_port () =
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    let localhost = inet_addr_of_string "127.0.0.1" in
    bind sock (ADDR_INET (localhost, 0));
    let addr = getsockname sock in
    match addr with
    | ADDR_INET(_, port) ->
      close sock;
      port
    | _ ->
      close sock;
      raise (Failure "Expected INET socket address")
  with e ->
    close sock;
    raise e 