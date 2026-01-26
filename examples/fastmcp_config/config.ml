open! Core
open! Async
open Cohttp
open Cohttp_async

(** Configuration Types matching fastmcp.json structure *)
module Types = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type env_vars = (string * string) list

  let env_vars_of_yojson json =
    match json with
    | `Assoc pairs ->
      List.map pairs ~f:(fun (k, v) ->
          match v with
          | `String s -> (k, s)
          | _ -> (k, Yojson.Safe.to_string v))
    | _ -> []

  let yojson_of_env_vars vars =
    `Assoc (List.map vars ~f:(fun (k, v) -> (k, `String v)))

  type deployment = {
    transport : string option; [@yojson.option]
    host : string option; [@yojson.option]
    port : int option; [@yojson.option]
    env : env_vars option; [@yojson.option]
  }
  [@@deriving yojson]

  type t = {
    entrypoint : string option; [@yojson.option]
    deployment : deployment option; [@yojson.option]
  }
  [@@deriving yojson]
end

(** Environment variable interpolation *)
let interpolate_env text =
  (* Match pattern ${VAR_NAME} *)
  let re = Re.Pcre.regexp "\\$\\{([^}]+)\\}" in
  try
    Re.replace re ~all:true
      ~f:(fun group ->
        let var_name = Re.Group.get group 1 in
        match Sys.getenv var_name with
        | Some v -> v
        | None ->
          Log.Global.error "Env var not found during interpolation: %s" var_name;
          (* Leave it as is if not found, or maybe empty string? python example
             implies substitution *)
          "${" ^ var_name ^ "}")
      text
  with _ -> text

(** Parse content helper *)
let parse_content content path =
  try
    let json = Yojson.Safe.from_string content in
    (* Parse first *)
    let config = Types.t_of_yojson json in

    (* Apply interpolation to environment variables if present *)
    let config =
      match config.deployment with
      | Some deployment ->
        let env =
          match deployment.env with
          | Some vars ->
            Some (List.map vars ~f:(fun (k, v) -> (k, interpolate_env v)))
          | None -> None
        in
        { config with deployment = Some { deployment with env } }
      | None -> config
    in
    Some config
  with exn ->
    Log.Global.error "Failed to parse %s: %s" path (Exn.to_string exn);
    None

(** Load function *)
let load ?(path = "fastmcp.json") () =
  let%bind content_opt =
    if
      String.is_prefix path ~prefix:"http://"
      || String.is_prefix path ~prefix:"https://"
    then
      let uri = Uri.of_string path in
      match%bind Client.get uri with
      | resp, body -> (
        match Response.status resp with
        | `OK ->
          let%map body_str = Body.to_string body in
          Some body_str
        | status ->
          Log.Global.error "Failed to fetch %s: %s" path
            (Code.string_of_status status);
          return None
        | exception exn ->
          Log.Global.error "Exception fetching %s: %s" path (Exn.to_string exn);
          return None)
    else
      let%bind exists = Sys.file_exists path in
      match exists with
      | `Yes ->
        let%map c = Reader.file_contents path in
        Some c
      | _ -> return None
  in
  return
    (Option.bind content_opt ~f:(fun content -> parse_content content path))

(** Apply configuration to the current process *)
let apply_to_process config =
  match config with
  | Some { Types.deployment = Some { env = Some vars; _ }; _ } ->
    List.iter vars ~f:(fun (k, v) ->
        (let (_ : unit) = Unix.putenv ~key:k ~data:v in
         ());
        Log.Global.info "Set env var: %s=%s" k v)
  | _ -> ()
[@@alert "-unsafe_multidomain"]
