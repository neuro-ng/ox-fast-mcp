open Core

(* Settings type to hold configuration *)
type settings = {
  mutable log_level : string; (* Add other settings as needed *)
}

(* Global settings instance *)
let global_settings = { log_level = "INFO" }

(* Get current settings *)
let get_settings () = global_settings

(* Set a setting value *)
let set_setting = function
  | `Log_level level -> global_settings.log_level <- level

(* Temporarily override settings and restore them after *)
let with_temporary_settings settings_list f =
  (* Save old settings *)
  let old_settings = { log_level = global_settings.log_level } in

  try
    (* Apply new settings *)
    List.iter settings_list ~f:set_setting;

    (* Run the function *)
    let result = f () in

    (* Restore old settings *)
    global_settings.log_level <- old_settings.log_level;

    result
  with exn ->
    (* Restore settings even if exception occurs *)
    global_settings.log_level <- old_settings.log_level;
    raise exn

(* Find an available port *)
let find_available_port () =
  let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, 0) in
  let sock =
    Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0
  in
  Unix.bind sock addr;
  let _, port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, port) ->
      Unix.close sock;
      (Unix.Inet_addr.localhost, port)
    | _ ->
      Unix.close sock;
      failwith "Expected INET socket"
  in
  port

(* Run server in a context *)
let with_server f =
  let port = find_available_port () in
  let host = "127.0.0.1" in
  let url = Printf.sprintf "http://%s:%d" host port in
  (* In a real implementation, we would start the server here *)
  let result = f url in
  (* In a real implementation, we would stop the server here *)
  result
