open Core
open Async
open Alcotest_async

module Error_test_server = struct
  let create_with_error_tool () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a tool that raises an error *)
    let error_tool =
      Tool.create ~name:"error_tool" ~f:(fun _args ->
          raise (Failure "This is a test error (abc)"))
    in
    Server.add_tool server error_tool;

    server

  let create_with_custom_error_tool () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a tool that raises a custom error *)
    let custom_error_tool =
      Tool.create ~name:"custom_error_tool" ~f:(fun _args ->
          raise (Tool_error.Tool_error "This is a test error (abc)"))
    in
    Server.add_tool server custom_error_tool;

    server

  let create_with_error_resource () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a resource that raises an error *)
    let error_resource =
      Resource.create ~uri:"exception://resource" ~f:(fun () ->
          raise (Failure "This is an internal error (sensitive)"))
    in
    Server.add_resource server error_resource;

    server

  let create_with_custom_error_resource () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a resource that raises a custom error *)
    let error_resource =
      Resource.create ~uri:"error://resource" ~f:(fun () ->
          raise (Resource_error.Resource_error "This is a resource error (xyz)"))
    in
    Server.add_resource server error_resource;

    server

  let create_with_error_template () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a template that raises an error *)
    let error_template =
      Resource_template.create ~uri_template:"exception://resource/{id}"
        ~f:(fun ~id:_ ->
          raise (Failure "This is an internal error (sensitive)"))
    in
    Server.add_resource_template server error_template;

    server

  let create_with_custom_error_template () =
    let server = Server.create ~name:"TestServer" () in

    (* Add a template that raises a custom error *)
    let error_template =
      Resource_template.create ~uri_template:"error://resource/{id}"
        ~f:(fun ~id:_ ->
          raise (Resource_error.Resource_error "This is a resource error (xyz)"))
    in
    Server.add_resource_template server error_template;

    server
end

(* Test general tool exceptions *)
let test_general_tool_exceptions switch () =
  let server = Error_test_server.create_with_error_tool () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind result =
          Client.call_tool_mcp client ~name:"error_tool" ~arguments:[] ()
        in
        check bool "is error" true result.Types.is_error;
        check bool "error contains message" true
          (match List.hd_exn result.Types.content with
          | Types.Text_content { text; _ } ->
            String.is_substring ~substring:"test error" text
            && String.is_substring ~substring:"abc" text
          | _ -> false);
        return ())
  in
  return ()

(* Test custom tool errors *)
let test_custom_tool_errors switch () =
  let server = Error_test_server.create_with_custom_error_tool () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind result =
          Client.call_tool_mcp client ~name:"custom_error_tool" ~arguments:[] ()
        in
        check bool "is error" true result.Types.is_error;
        check bool "error contains message" true
          (match List.hd_exn result.Types.content with
          | Types.Text_content { text; _ } ->
            String.is_substring ~substring:"test error" text
            && String.is_substring ~substring:"abc" text
          | _ -> false);
        return ())
  in
  return ()

(* Test general resource exceptions *)
let test_general_resource_exceptions switch () =
  let server = Error_test_server.create_with_error_resource () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "exception://resource")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error contains message" true
            (String.is_substring ~substring:"Error reading resource"
               (Error.to_string_hum e)
            && String.is_substring ~substring:"sensitive"
                 (Error.to_string_hum e)
            && String.is_substring ~substring:"internal error"
                 (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test custom resource errors *)
let test_custom_resource_errors switch () =
  let server = Error_test_server.create_with_custom_error_resource () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "error://resource")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error contains message" true
            (String.is_substring ~substring:"This is a resource error (xyz)"
               (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test general template exceptions *)
let test_general_template_exceptions switch () =
  let server = Error_test_server.create_with_error_template () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "exception://resource/123")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error contains message" true
            (String.is_substring ~substring:"Error reading resource"
               (Error.to_string_hum e)
            && String.is_substring ~substring:"sensitive"
                 (Error.to_string_hum e)
            && String.is_substring ~substring:"internal error"
                 (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test custom template errors *)
let test_custom_template_errors switch () =
  let server = Error_test_server.create_with_custom_error_template () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "error://resource/123")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error contains message" true
            (String.is_substring ~substring:"This is a resource error (xyz)"
               (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test general tool exceptions are masked when enabled *)
let test_general_tool_exceptions_are_masked_when_enabled switch () =
  let server = Server.create ~name:"TestServer" ~mask_error_details:true () in

  (* Add a tool that raises an error *)
  let error_tool =
    Tool.create ~name:"error_tool" ~f:(fun _args ->
        raise (Failure "This is a test error (abc)"))
  in
  Server.add_tool server error_tool;

  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind result =
          Client.call_tool_mcp client ~name:"error_tool" ~arguments:[] ()
        in
        check bool "is error" true result.Types.is_error;
        check bool "error details are masked" true
          (match List.hd_exn result.Types.content with
          | Types.Text_content { text; _ } ->
            (not (String.is_substring ~substring:"test error" text))
            && not (String.is_substring ~substring:"abc" text)
          | _ -> false);
        return ())
  in
  return ()

(* Test general resource exceptions are masked when enabled *)
let test_general_resource_exceptions_are_masked_when_enabled switch () =
  let server = Server.create ~name:"TestServer" ~mask_error_details:true () in

  (* Add a resource that raises an error *)
  let error_resource =
    Resource.create ~uri:"exception://resource" ~f:(fun () ->
        raise (Failure "This is an internal error (sensitive)"))
  in
  Server.add_resource server error_resource;

  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "exception://resource")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error details are masked" true
            (String.is_substring ~substring:"Error reading resource"
               (Error.to_string_hum e)
            && (not
                  (String.is_substring ~substring:"sensitive"
                     (Error.to_string_hum e)))
            && not
                 (String.is_substring ~substring:"internal error"
                    (Error.to_string_hum e)));
          return ())
  in
  return ()

(* Test general template exceptions are masked when enabled *)
let test_general_template_exceptions_are_masked_when_enabled switch () =
  let server = Server.create ~name:"TestServer" ~mask_error_details:true () in

  (* Add a template that raises an error *)
  let error_template =
    Resource_template.create ~uri_template:"exception://resource/{id}"
      ~f:(fun ~id:_ -> raise (Failure "This is an internal error (sensitive)"))
  in
  Server.add_resource_template server error_template;

  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.read_resource client (Uri.of_string "exception://resource/123")
        with
        | Ok _ -> failwith "Expected error but got success"
        | Error e ->
          check bool "error details are masked" true
            (String.is_substring ~substring:"Error reading resource"
               (Error.to_string_hum e)
            && (not
                  (String.is_substring ~substring:"sensitive"
                     (Error.to_string_hum e)))
            && not
                 (String.is_substring ~substring:"internal error"
                    (Error.to_string_hum e)));
          return ())
  in
  return ()

(* Test timeout *)
let test_timeout switch () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a sleep tool *)
  let sleep_tool =
    Tool.create ~name:"sleep" ~f:(fun args ->
        let seconds =
          List.Assoc.find_exn args ~equal:String.equal "seconds"
          |> Yojson.Safe.to_float
        in
        let%bind () = Clock.after (Time.Span.of_sec seconds) in
        return [ Content_block.text (sprintf "Slept for %f seconds" seconds) ])
  in
  Server.add_tool server sleep_tool;

  let client =
    Client.create ~timeout:(Time.Span.of_sec 0.05)
      (Transports.FastMCP_transport server)
  in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.call_tool client ~name:"sleep"
            ~arguments:[ ("seconds", `Float 0.1) ]
            ()
        with
        | Ok _ -> failwith "Expected timeout error but got success"
        | Error e ->
          check bool "error contains timeout message" true
            (String.is_substring
               ~substring:"Timed out while waiting for response"
               (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test timeout tool call *)
let test_timeout_tool_call switch () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a sleep tool *)
  let sleep_tool =
    Tool.create ~name:"sleep" ~f:(fun args ->
        let seconds =
          List.Assoc.find_exn args ~equal:String.equal "seconds"
          |> Yojson.Safe.to_float
        in
        let%bind () = Clock.after (Time.Span.of_sec seconds) in
        return [ Content_block.text (sprintf "Slept for %f seconds" seconds) ])
  in
  Server.add_tool server sleep_tool;

  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.call_tool ~timeout:(Time.Span.of_sec 0.01) client ~name:"sleep"
            ~arguments:[ ("seconds", `Float 0.1) ]
            ()
        with
        | Ok _ -> failwith "Expected timeout error but got success"
        | Error e ->
          check bool "error contains timeout message" true
            (String.is_substring
               ~substring:"Timed out while waiting for response"
               (Error.to_string_hum e));
          return ())
  in
  return ()

let () =
  run "Client Error Tests"
    [
      ( "error handling",
        [
          test_case "general tool exceptions" `Quick
            test_general_tool_exceptions;
          test_case "custom tool errors" `Quick test_custom_tool_errors;
          test_case "general resource exceptions" `Quick
            test_general_resource_exceptions;
          test_case "custom resource errors" `Quick test_custom_resource_errors;
          test_case "general template exceptions" `Quick
            test_general_template_exceptions;
          test_case "custom template errors" `Quick test_custom_template_errors;
          test_case "general tool exceptions are masked" `Quick
            test_general_tool_exceptions_are_masked_when_enabled;
          test_case "general resource exceptions are masked" `Quick
            test_general_resource_exceptions_are_masked_when_enabled;
          test_case "general template exceptions are masked" `Quick
            test_general_template_exceptions_are_masked_when_enabled;
        ] );
      ( "timeouts",
        [
          test_case "client timeout" `Quick test_timeout;
          test_case "tool call timeout" `Quick test_timeout_tool_call;
        ] );
    ]
