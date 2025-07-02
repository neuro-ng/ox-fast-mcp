open Core
open Cohttp
open Cohttp_lwt_unix

(* Context management *)
module Context = struct
  type t = {
    mutable current_context: Server.Context.t option;
  }

  let global_context = { current_context = None }

  let set_context ctx =
    global_context.current_context <- Some ctx

  let clear_context () =
    global_context.current_context <- None

  let get_context () =
    match global_context.current_context with
    | Some ctx -> ctx
    | None -> failwith "No active context found."
end

(* HTTP Request management *)
module Http_request = struct
  type t = {
    mutable current_request: Request.t option;
  }

  let global_request = { current_request = None }

  let set_request req =
    global_request.current_request <- Some req

  let clear_request () =
    global_request.current_request <- None

  let get_request () =
    match global_request.current_request with
    | Some req -> req
    | None -> failwith "No active HTTP request found."

  let excluded_headers = String.Set.of_list [
    "host";
    "content-length";
    "connection";
    "transfer-encoding";
    "upgrade";
    "te";
    "keep-alive";
    "expect";
    "accept";
    "proxy-authenticate";
    "proxy-authorization";
    "proxy-connection";
  ]

  let get_headers ?(include_all = false) () =
    try
      let req = get_request () in
      let headers = Request.headers req in
      let header_list = Header.to_list headers in
      
      List.filter_map header_list ~f:(fun (name, value) ->
        let name_lower = String.lowercase name in
        if include_all || not (Set.mem excluded_headers name_lower) then
          Some (name_lower, value)
        else
          None)
      |> String.Map.of_alist_exn
    with
    | _ -> String.Map.empty
end

(* Access token management - reusing from auth middleware *)
module Access_token = struct
  include Auth.Middleware.Bearer_auth

  let get_token () = 
    try
      let req = Http_request.get_request () in
      get_access_token req
    with
    | _ -> None
end 