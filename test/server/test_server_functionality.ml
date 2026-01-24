(** Tests for New Server Functionality **)

open! Core
module Ox_fast_mcp = Server.Ox_fast_mcp

(** {1 Test: Mutable Instructions}
    **)

let%expect_test "create server without instructions" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  print_s [%sexp (Ox_fast_mcp.instructions server : string option)];
  [%expect {| () |}]

let%expect_test "set and get instructions" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  Ox_fast_mcp.set_instructions server "New instructions for the server";
  print_s [%sexp (Ox_fast_mcp.instructions server : string option)];
  [%expect {| ("New instructions for the server") |}]

let%expect_test "overwrite existing instructions" =
  let server =
    Ox_fast_mcp.create ~name:"test" ~instructions:"Original instructions" ()
  in
  print_s [%sexp (Ox_fast_mcp.instructions server : string option)];
  Ox_fast_mcp.set_instructions server "Updated instructions";
  print_s [%sexp (Ox_fast_mcp.instructions server : string option)];
  [%expect {|
    ("Original instructions")
    ("Updated instructions") |}]

(** {1 Test: Server Info Helper}
    **)

let%expect_test "server_info with version" =
  let server = Ox_fast_mcp.create ~name:"test-server" ~version:"1.2.3" () in
  let info = Ox_fast_mcp.server_info server in
  print_endline (Yojson.Safe.to_string info);
  [%expect {| {"name":"test-server","version":"1.2.3"} |}]

let%expect_test "server_info without version uses default" =
  let server = Ox_fast_mcp.create ~name:"my-server" () in
  let info = Ox_fast_mcp.server_info server in
  print_endline (Yojson.Safe.to_string info);
  [%expect {| {"name":"my-server","version":"0.1.0"} |}]

(** {1 Test: Capabilities Helper}
    **)

let%expect_test "capabilities returns proper MCP format" =
  let server = Ox_fast_mcp.create () in
  let caps = Ox_fast_mcp.capabilities server in
  print_endline (Yojson.Safe.to_string caps);
  [%expect
    {| {"tools":{"listChanged":true,"supportsProgress":false},"resources":{"subscribe":false,"listChanged":true},"prompts":{"listChanged":true}} |}]

(** {1 Test: Integration with Initialize}
    **)

let%expect_test "server_info and capabilities used in initialization" =
  let server =
    Ox_fast_mcp.create ~name:"integration-test" ~version:"2.0.0" ()
  in
  (* Verify that server_info and capabilities are correct *)
  let info = Ox_fast_mcp.server_info server in
  let caps = Ox_fast_mcp.capabilities server in

  (* Check server_info *)
  (match info with
  | `Assoc fields -> (
    (match List.Assoc.find fields ~equal:String.equal "name" with
    | Some (`String name) -> print_endline ("Name: " ^ name)
    | _ -> print_endline "Name not found");
    match List.Assoc.find fields ~equal:String.equal "version" with
    | Some (`String version) -> print_endline ("Version: " ^ version)
    | _ -> print_endline "Version not found")
  | _ -> print_endline "Info not an object");

  (* Check capabilities *)
  (match caps with
  | `Assoc fields ->
    print_s [%sexp (List.length fields : int)];
    (* Should have 3 capabilities *)
    print_s [%sexp (List.Assoc.mem fields ~equal:String.equal "tools" : bool)];
    print_s
      [%sexp (List.Assoc.mem fields ~equal:String.equal "resources" : bool)];
    print_s [%sexp (List.Assoc.mem fields ~equal:String.equal "prompts" : bool)]
  | _ -> print_endline "Capabilities not an object");

  [%expect
    {|
    Name: integration-test
    Version: 2.0.0
    3
    true
    true
    true |}]
