(** Tests for Server Statistics *)

open! Core
open! Async
module Ox_fast_mcp = Server.Ox_fast_mcp
module Tool = Server.Tool
module Resource = Server.Resource
module Resource_template = Server.Resource_template
module Prompt = Server.Prompt

(** {1 Test: Server Statistics}
    **)

let%expect_test "get_stats returns correct counts for empty server" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in
  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);
  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats updates after adding a tool" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add a tool *)
  let tool =
    Tool.create ~name:"test_tool"
      ~handler:(fun _ -> return (`String "result"))
      ()
  in
  Ox_fast_mcp.add_tool server tool;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":1,"resources_count":0,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats updates after adding a resource" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add a resource *)
  let resource =
    Resource.create ~uri:"file:///test.txt" ~name:"test_resource"
      ~reader:(fun () -> return "test content")
      ()
  in
  Ox_fast_mcp.add_resource server resource;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":1,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats updates after adding a prompt" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add a prompt *)
  let prompt =
    Prompt.create ~name:"test_prompt"
      ~render:(fun _ -> return (`Assoc [ ("messages", `List []) ]))
      ()
  in
  Ox_fast_mcp.add_prompt server prompt;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":1,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats updates after adding a template" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add a template *)
  let template =
    Resource_template.create ~uri_template:"file:///{path}"
      ~name:"test_template"
      ~create_resource:(fun ~params ->
        let path = List.Assoc.find_exn params ~equal:String.equal "path" in
        return
          (Resource.create
             ~uri:(sprintf "file:///%s" path)
             ~name:path
             ~reader:(fun () -> return "template content")
             ()))
      ()
  in
  Ox_fast_mcp.add_template server template;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":0,"templates_count":1,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats tracks all component types" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add a tool *)
  let tool =
    Tool.create ~name:"test_tool"
      ~handler:(fun _ -> return (`String "result"))
      ()
  in
  Ox_fast_mcp.add_tool server tool;

  (* Add a resource *)
  let resource =
    Resource.create ~uri:"file:///test.txt" ~name:"test_resource"
      ~reader:(fun () -> return "test content")
      ()
  in
  Ox_fast_mcp.add_resource server resource;

  (* Add a prompt *)
  let prompt =
    Prompt.create ~name:"test_prompt"
      ~render:(fun _ -> return (`Assoc [ ("messages", `List []) ]))
      ()
  in
  Ox_fast_mcp.add_prompt server prompt;

  (* Add a template *)
  let template =
    Resource_template.create ~uri_template:"file:///{path}"
      ~name:"test_template"
      ~create_resource:(fun ~params ->
        let path = List.Assoc.find_exn params ~equal:String.equal "path" in
        return
          (Resource.create
             ~uri:(sprintf "file:///%s" path)
             ~name:path
             ~reader:(fun () -> return "template content")
             ()))
      ()
  in
  Ox_fast_mcp.add_template server template;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":1,"resources_count":1,"prompts_count":1,"templates_count":1,"mounted_servers_count":0} |}];
  return ()

let%expect_test "get_stats tracks mounted servers" =
  let server1 = Ox_fast_mcp.create ~name:"server1" () in
  let server2 = Ox_fast_mcp.create ~name:"server2" () in

  (* Add a tool to server2 *)
  let tool =
    Tool.create ~name:"tool_from_server2"
      ~handler:(fun _ -> return (`String "result"))
      ()
  in
  Ox_fast_mcp.add_tool server2 tool;

  (* Mount server2 into server1 *)
  Ox_fast_mcp.import_server server1 ~server:server2 ~prefix:"imported" ();

  let stats = Ox_fast_mcp.get_stats server1 in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":0,"templates_count":0,"mounted_servers_count":1} |}];
  return ()

let%expect_test "get_stats updates after removing components" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  (* Add multiple tools *)
  let tool1 =
    Tool.create ~name:"tool1" ~handler:(fun _ -> return (`String "result1")) ()
  in
  let tool2 =
    Tool.create ~name:"tool2" ~handler:(fun _ -> return (`String "result2")) ()
  in
  Ox_fast_mcp.add_tool server tool1;
  Ox_fast_mcp.add_tool server tool2;

  (* Add a resource *)
  let resource =
    Resource.create ~uri:"file:///test.txt" ~name:"test_resource"
      ~reader:(fun () -> return "test content")
      ()
  in
  Ox_fast_mcp.add_resource server resource;

  (* Check initial stats *)
  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);
  [%expect
    {| {"tools_count":2,"resources_count":1,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];

  (* Remove a tool *)
  Ox_fast_mcp.remove_tool server ~name:"tool1";

  (* Check updated stats *)
  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);
  [%expect
    {| {"tools_count":1,"resources_count":1,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];

  return ()

let%expect_test "get_stats with bulk initialization" =
  (* Create server with bulk components *)
  let tool1 =
    Tool.create ~name:"tool1" ~handler:(fun _ -> return (`String "result1")) ()
  in
  let tool2 =
    Tool.create ~name:"tool2" ~handler:(fun _ -> return (`String "result2")) ()
  in
  let resource =
    Resource.create ~uri:"file:///test.txt" ~name:"test_resource"
      ~reader:(fun () -> return "test content")
      ()
  in

  let server =
    Ox_fast_mcp.create ~name:"test-server" ~tools:[ tool1; tool2 ]
      ~resources:[ resource ] ()
  in

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":2,"resources_count":1,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

(** {1 Test: Batch Operation Helpers} *)

let%expect_test "add_tools batch operation" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  let tools =
    [
      Tool.create ~name:"batch_tool1"
        ~handler:(fun _ -> return (`String "result1"))
        ();
      Tool.create ~name:"batch_tool2"
        ~handler:(fun _ -> return (`String "result2"))
        ();
      Tool.create ~name:"batch_tool3"
        ~handler:(fun _ -> return (`String "result3"))
        ();
    ]
  in

  Ox_fast_mcp.add_tools server tools;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":3,"resources_count":0,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "add_resources batch operation" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  let resources =
    [
      Resource.create ~uri:"file:///file1.txt" ~name:"file1"
        ~reader:(fun () -> return "content1")
        ();
      Resource.create ~uri:"file:///file2.txt" ~name:"file2"
        ~reader:(fun () -> return "content2")
        ();
    ]
  in

  Ox_fast_mcp.add_resources server resources;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":2,"prompts_count":0,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "add_prompts batch operation" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  let prompts =
    [
      Prompt.create ~name:"prompt1"
        ~render:(fun _ -> return (`Assoc [ ("messages", `List []) ]))
        ();
      Prompt.create ~name:"prompt2"
        ~render:(fun _ -> return (`Assoc [ ("messages", `List []) ]))
        ();
    ]
  in

  Ox_fast_mcp.add_prompts server prompts;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":2,"templates_count":0,"mounted_servers_count":0} |}];
  return ()

let%expect_test "add_templates batch operation" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in

  let templates =
    [
      Resource_template.create ~uri_template:"file:///{path1}" ~name:"template1"
        ~create_resource:(fun ~params ->
          let path = List.Assoc.find_exn params ~equal:String.equal "path1" in
          return
            (Resource.create
               ~uri:(sprintf "file:///%s" path)
               ~name:path
               ~reader:(fun () -> return "content1")
               ()))
        ();
      Resource_template.create ~uri_template:"file:///{path2}" ~name:"template2"
        ~create_resource:(fun ~params ->
          let path = List.Assoc.find_exn params ~equal:String.equal "path2" in
          return
            (Resource.create
               ~uri:(sprintf "file:///%s" path)
               ~name:path
               ~reader:(fun () -> return "content2")
               ()))
        ();
    ]
  in

  Ox_fast_mcp.add_templates server templates;

  let stats = Ox_fast_mcp.get_stats server in
  print_endline (Yojson.Safe.to_string stats);

  [%expect
    {| {"tools_count":0,"resources_count":0,"prompts_count":0,"templates_count":2,"mounted_servers_count":0} |}];
  return ()
