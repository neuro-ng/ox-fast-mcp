open Core
open Async
open Alcotest_async

(* Test server fixture *)
let create_tagged_resources_server () =
  let server = Server.create ~name:"TaggedResourcesServer" () in

  (* Add a resource with tags *)
  let tagged_resource =
    Resource.create ~uri:"data://tagged" ~tags:[ "test"; "metadata" ]
      ~description:"A tagged resource" ~f:(fun () ->
        return (`Assoc [ ("type", `String "tagged_data") ]))
  in
  Server.add_resource server tagged_resource;

  (* Add a resource template with tags *)
  let tagged_template =
    Resource_template.create ~uri_template:"template://{id}"
      ~tags:[ "template"; "parameterized" ] ~description:"A tagged template"
      ~f:(fun ~id ->
        return
          (`Assoc [ ("id", `String id); ("type", `String "template_data") ]))
  in
  Server.add_resource_template server tagged_template;

  server

(* Test tagged resource metadata *)
let test_tagged_resource_metadata switch () =
  let server = create_tagged_resources_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind resources = Client.list_resources client in
        check int "resource count" 1 (List.length resources);
        let resource = List.hd_exn resources in
        check string "resource uri" "data://tagged"
          (Uri.to_string resource.Types.uri);
        check string "resource description" "A tagged resource"
          (Option.value_exn resource.Types.description);
        return ())
  in
  return ()

(* Test tagged template metadata *)
let test_tagged_template_metadata switch () =
  let server = create_tagged_resources_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind templates = Client.list_resource_templates client in
        check int "template count" 1 (List.length templates);
        let template = List.hd_exn templates in
        check string "template uri" "template://{id}"
          template.Types.uri_template;
        check string "template description" "A tagged template"
          (Option.value_exn template.Types.description);
        return ())
  in
  return ()

(* Test tagged template functionality *)
let test_tagged_template_functionality switch () =
  let server = create_tagged_resources_server () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind result =
          Client.read_resource client (Uri.of_string "template://123")
        in
        match result with
        | Ok content ->
          check string "template id" "123"
            (Yojson.Safe.Util.member "id" content |> Yojson.Safe.Util.to_string);
          check string "template type" "template_data"
            (Yojson.Safe.Util.member "type" content
            |> Yojson.Safe.Util.to_string);
          return ()
        | Error e -> failwith (Error.to_string_hum e))
  in
  return ()

let () =
  run "Client Tagged Tests"
    [
      ( "tagged resources",
        [
          test_case "tagged resource metadata" `Quick
            test_tagged_resource_metadata;
          test_case "tagged template metadata" `Quick
            test_tagged_template_metadata;
          test_case "tagged template functionality" `Quick
            test_tagged_template_functionality;
        ] );
    ]
