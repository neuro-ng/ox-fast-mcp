open Core
open Async
open Alcotest_async

(* Test server fixture *)
let create_test_server () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a prompt that accepts all string args but client sends mixed types *)
  let echo_args_prompt = Prompt.create ~name:"echo_args" ~f:(fun args ->
    let arg1 = List.Assoc.find_exn args ~equal:String.equal "arg1" |> Yojson.Safe.to_string in
    let arg2 = List.Assoc.find_exn args ~equal:String.equal "arg2" |> Yojson.Safe.to_string in
    let arg3 = List.Assoc.find_exn args ~equal:String.equal "arg3" |> Yojson.Safe.to_string in
    return (sprintf "arg1: %s, arg2: %s, arg3: %s" arg1 arg2 arg3)
  ) in
  Server.add_prompt server echo_args_prompt;

  (* Add a prompt that expects typed args *)
  let typed_prompt = Prompt.create ~name:"typed_prompt" ~f:(fun args ->
    let numbers = List.Assoc.find_exn args ~equal:String.equal "numbers" |> Yojson.Safe.to_string in
    let config = List.Assoc.find_exn args ~equal:String.equal "config" |> Yojson.Safe.to_string in
    let numbers_list = Yojson.Safe.from_string numbers |> Yojson.Safe.Util.to_list |> List.map ~f:Yojson.Safe.Util.to_int in
    let config_map = Yojson.Safe.from_string config |> Yojson.Safe.Util.to_assoc in
    return (sprintf "Got %d numbers and %d config items" (List.length numbers_list) (List.length config_map))
  ) in
  Server.add_prompt server typed_prompt;

  server

(* Test client serializes all non-string arguments *)
let test_client_serializes_all_non_string_arguments switch () =
  let server = create_test_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () = Client.with_client client (fun () ->
    let%bind result = Client.get_prompt client ~name:"echo_args" ~arguments:[
      ("arg1", `String "hello");  (* string - should pass through *)
      ("arg2", `List [`Int 1; `Int 2; `Int 3]);  (* list - should be JSON serialized *)
      ("arg3", `Assoc [("key", `String "value")]);  (* dict - should be JSON serialized *)
    ] () in

    let content = match List.hd_exn result.Types.messages with
      | Types.Text_content {text; _} -> text
      | _ -> "" in

    check bool "has string arg" true (String.is_substring ~substring:"arg1: hello" content);
    check bool "has serialized list" true (String.is_substring ~substring:"arg2: [1,2,3]" content);
    check bool "has serialized dict" true (String.is_substring ~substring:"arg3: {\"key\":\"value\"}" content);
    return ()
  ) in
  return ()

(* Test client-server type conversion integration *)
let test_client_server_type_conversion_integration switch () =
  let server = create_test_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () = Client.with_client client (fun () ->
    let%bind result = Client.get_prompt client ~name:"typed_prompt" ~arguments:[
      ("numbers", `List [`Int 1; `Int 2; `Int 3; `Int 4]);
      ("config", `Assoc [("theme", `String "dark"); ("lang", `String "en")]);
    ] () in

    let content = match List.hd_exn result.Types.messages with
      | Types.Text_content {text; _} -> text
      | _ -> "" in

    check bool "correct conversion" true (String.equal "Got 4 numbers and 2 config items" content);
    return ()
  ) in
  return ()

(* Test client serialization error *)
let test_client_serialization_error switch () =
  let server = create_test_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  (* Create an unserializable value *)
  let unserializable = `Assoc [("func", `String "lambda")] in

  let%bind () = Client.with_client client (fun () ->
    match%bind Client.get_prompt client ~name:"any_prompt" ~arguments:[("data", unserializable)] () with
    | Ok _ -> failwith "Expected serialization error"
    | Error e ->
      check bool "has error message" true
        (String.is_substring ~substring:"Unable to serialize" (Error.to_string_hum e));
      return ()
  ) in
  return ()

(* Test server deserialization error *)
let test_server_deserialization_error switch () =
  let server = create_test_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () = Client.with_client client (fun () ->
    match%bind Client.get_prompt client ~name:"typed_prompt" ~arguments:[
      ("numbers", `String "not valid json")  (* This will fail server-side conversion *)
    ] () with
    | Ok _ -> failwith "Expected deserialization error"
    | Error e ->
      check bool "has error message" true
        (String.is_substring ~substring:"Error rendering prompt" (Error.to_string_hum e));
      return ()
  ) in
  return ()

let () =
  run "Client Serialization Tests" [
    "serialization", [
      test_case "client serializes all non-string arguments" `Quick test_client_serializes_all_non_string_arguments;
      test_case "client-server type conversion integration" `Quick test_client_server_type_conversion_integration;
      test_case "client serialization error" `Quick test_client_serialization_error;
      test_case "server deserialization error" `Quick test_server_deserialization_error;
    ];
  ] 