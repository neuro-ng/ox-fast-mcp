open Core
open Async
open Expect_test_helpers_core
open Expect_test_helpers_async
open Tools.Tool_types
open Tools.Tool_manager
open Utilities.Types

(* Stub modules for testing *)
module Server = struct
  type t = {
    mutable tools : Tool.t list;
    prefix : string option;
  }

  let create ?prefix () = {
    tools = [];
    prefix;
  }

  let list_tools t = return t.tools
  let get_tools t = return t.tools
  let call_tool t key args =
    match List.find t.tools ~f:(fun tool -> String.equal tool.Tool.key key) with
    | Some tool -> tool.Tool.fn args
    | None -> raise (Not_found_s (Sexp.message "Tool not found" [("key", Atom key)]))
end

module Context = struct
  type t = {
    request_id : string option;
    client_id : string option;
    session_data : (string, string) Hashtbl.t;
    tools_changed : bool;
    resources_changed : bool;
    prompts_changed : bool;
  }

  let create ?request_id ?client_id () = {
    request_id;
    client_id;
    session_data = Hashtbl.create (module String);
    tools_changed = false;
    resources_changed = false;
    prompts_changed = false;
  }
end

module FastMCP = struct
  type t = {
    tool_serializer : Yojson.Safe.t -> string;
    tool_manager : Tool_manager.t;
  }

  let create ?(tool_serializer = Yojson.Safe.pretty_to_string) () = {
    tool_serializer;
    tool_manager = Tool_manager.create ();
  }
end

(* Test helper functions *)
let create_test_tool name description =
  let handler args =
    match args with
    | `Assoc args_list ->
        let x = List.Assoc.find_exn args_list ~equal:String.equal "x" |> function
          | `Int x -> x
          | _ -> failwith "Expected integer for x"
        in
        let y = 
          List.Assoc.find args_list ~equal:String.equal "y" 
          |> Option.value ~default:(`Int 10)
          |> function
            | `Int y -> y
            | _ -> failwith "Expected integer for y"
        in
        return [create_text_content (Int.to_string (x + y))]
    | _ -> failwith "Expected object arguments"
  in
  Tool.from_function
    handler
    ~name
    ~description
    ~parameters:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [
          ("type", `String "integer");
          ("description", `String "First number")
        ]);
        ("y", `Assoc [
          ("type", `String "integer");
          ("default", `Int 10);
          ("description", `String "Second number")
        ])
      ]);
      ("required", `List [`String "x"])
    ])

let create_async_test_tool name description =
  let handler args =
    match args with
    | `Assoc args_list ->
        let x = List.Assoc.find_exn args_list ~equal:String.equal "x" |> function
          | `Int x -> x
          | _ -> failwith "Expected integer for x"
        in
        let%bind () = after (Time_float.Span.of_ms 10.0) in
        return [create_text_content (Int.to_string (x * 2))]
    | _ -> failwith "Expected object arguments"
  in
  Tool.from_function
    handler
    ~name
    ~description
    ~parameters:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number to double")
        ])
      ]);
      ("required", `List [`String "x"])
    ])

let with_test_context f =
  let ctx = Context.create 
    ~request_id:"test-request" 
    ~client_id:"test-client" 
    () 
  in
  f ctx

(* Test tool manager creation *)
let%expect_test "create tool manager" =
  let manager = create () in
  let%bind tools = get_tools manager in
  print_s [%sexp (Map.length tools : int)];
  [%expect {| 0 |}]

(* Test adding tool *)
let%expect_test "add tool" =
  let manager = create () in
  let tool = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager tool in
  let%bind has_tool = has_tool manager "add" in
  print_s [%sexp (has_tool : bool)];
  let%bind tools = get_tools manager in
  print_s [%sexp (Map.length tools : int)];
  [%expect {|
    true
    1 |}]

(* Test getting tool *)
let%expect_test "get tool" =
  let manager = create () in
  let tool = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager tool in
  let%bind found_tool = get_tool manager "add" in
  print_s [%sexp (found_tool.Tool.key : string)];
  print_s [%sexp (found_tool.Tool.description : string option)];
  [%expect {|
    add
    (Add two numbers) |}]

(* Test removing tool *)
let%expect_test "remove tool" =
  let manager = create () in
  let tool = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager tool in
  remove_tool manager "add";
  let%bind has_tool = has_tool manager "add" in
  print_s [%sexp (has_tool : bool)];
  [%expect {| false |}]

(* Test listing tools *)
let%expect_test "list tools" =
  let manager = create () in
  let tool1 = create_test_tool "add" "Add two numbers" in
  let tool2 = create_test_tool "subtract" "Subtract two numbers" in
  let _added_tool1 = add_tool manager tool1 in
  let _added_tool2 = add_tool manager tool2 in
  let%bind tools = list_tools manager in
  List.iter tools ~f:(fun t ->
    print_s [%sexp (t.Tool.key : string)]
  );
  [%expect {|
    add
    subtract |}]

(* Test tool execution through manager *)
let%expect_test "execute tool through manager" =
  let manager = create () in
  let tool = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager tool in
  let%bind result = call_tool manager "add" (`Assoc [("x", `Int 5); ("y", `Int 3)]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 8) |}]

(* Tool Function Types Tests *)
let%expect_test "basic function tool" =
  let manager = create () in
  let add = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager add in
  let%bind result = call_tool manager "add" (`Assoc [("x", `Int 5); ("y", `Int 3)]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 8) |}]

let%expect_test "async function tool" =
  let manager = create () in
  let double = create_async_test_tool "double" "Double a number" in
  let _added_tool = add_tool manager double in
  let%bind result = call_tool manager "double" (`Assoc [("x", `Int 5)]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 10) |}]

let%expect_test "callable object tool" =
  let manager = create () in
  let module Adder = struct
    let name = "adder"
    let description = "Adds two numbers"
    let call args =
      match args with
      | `Assoc args_list ->
          let x = List.Assoc.find_exn args_list ~equal:String.equal "x" |> function
            | `Int x -> x
            | _ -> failwith "Expected integer for x"
          in
          let y = List.Assoc.find_exn args_list ~equal:String.equal "y" |> function
            | `Int y -> y
            | _ -> failwith "Expected integer for y"
          in
          return [create_text_content (Int.to_string (x + y))]
      | _ -> failwith "Expected object arguments"
  end in
  let tool = Tool.from_function
    Adder.call
    ~name:Adder.name
    ~description:Adder.description
    ~parameters:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [("type", `String "integer")]);
        ("y", `Assoc [("type", `String "integer")])
      ]);
      ("required", `List [`String "x"; `String "y"])
    ])
  in
  let _added_tool = add_tool manager tool in
  let%bind result = call_tool manager "adder" (`Assoc [("x", `Int 5); ("y", `Int 3)]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 8) |}]

(* Error Handling Tests *)
let%expect_test "tool not found error" =
  let manager = create () in
  match%bind 
    Monitor.try_with (fun () -> 
      call_tool manager "nonexistent" (`Assoc [("x", `Int 5)]))
  with
  | Ok _ -> failwith "Expected tool not found error"
  | Error exn ->
      print_s [%sexp (Error.of_exn exn : Error.t)];
      [%expect {| "Tool nonexistent not found" |}];
      return ()

let%expect_test "invalid arguments error" =
  let manager = create () in
  let add = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager add in
  match%bind
    Monitor.try_with (fun () ->
      call_tool manager "add" (`Assoc [("x", `String "not a number")]))
  with
  | Ok _ -> failwith "Expected invalid arguments error"
  | Error exn ->
      print_s [%sexp (Error.of_exn exn : Error.t)];
      [%expect {| "Expected integer for x" |}];
      return ()

let%expect_test "missing required argument error" =
  let manager = create () in
  let add = create_test_tool "add" "Add two numbers" in
  let _added_tool = add_tool manager add in
  match%bind
    Monitor.try_with (fun () ->
      call_tool manager "add" (`Assoc []))
  with
  | Ok _ -> failwith "Expected missing argument error"
  | Error exn ->
      print_s [%sexp (Error.of_exn exn : Error.t)];
      [%expect {| "Required argument x not found" |}];
      return ()

(* Duplicate Tool Behavior Tests *)
let%expect_test "warn on duplicate tool" =
  let manager = create ~duplicate_behavior:DuplicateBehavior.Warn () in
  let tool1 = create_test_tool "add" "Add two numbers" in
  let tool2 = create_test_tool "add" "Add two numbers differently" in
  let _added_tool1 = add_tool manager tool1 in
  let _added_tool2 = add_tool manager tool2 in
  let%bind tool = get_tool manager "add" in
  print_s [%sexp (tool.Tool.description : string option)];
  [%expect {| (Add two numbers differently) |}]

let%expect_test "error on duplicate tool" =
  let manager = create ~duplicate_behavior:DuplicateBehavior.Error () in
  let tool1 = create_test_tool "add" "Add two numbers" in
  let tool2 = create_test_tool "add" "Add two numbers differently" in
  let _added_tool1 = add_tool manager tool1 in
  match Monitor.try_with (fun () -> return (add_tool manager tool2)) with
  | Ok _ -> failwith "Expected duplicate tool error"
  | Error exn ->
      print_s [%sexp (Error.of_exn exn : Error.t)];
      [%expect {| "Tool already exists: add" |}];
      return ()

let%expect_test "ignore duplicate tool" =
  let manager = create ~duplicate_behavior:DuplicateBehavior.Ignore () in
  let tool1 = create_test_tool "add" "Add two numbers" in
  let tool2 = create_test_tool "add" "Add two numbers differently" in
  let _added_tool1 = add_tool manager tool1 in
  let _added_tool2 = add_tool manager tool2 in
  let%bind tool = get_tool manager "add" in
  print_s [%sexp (tool.Tool.description : string option)];
  [%expect {| (Add two numbers) |}]

(* Server integration tests *)
let%expect_test "mount server and list tools" =
  let manager = create () in
  let server = Server.create ~prefix:"test" () in
  mount manager ~server ~prefix:"test";
  let tool = create_test_tool "add" "Add two numbers" in
  server.tools <- [tool];
  let%bind tools = list_tools manager in
  List.iter tools ~f:(fun t ->
    print_s [%sexp (t.Tool.key : string)];
    print_s [%sexp (t.Tool.description : string option)]
  );
  [%expect {|
    test_add
    (Add two numbers) |}]

let%expect_test "call tool through mounted server" =
  let manager = create () in
  let server = Server.create ~prefix:"test" () in
  mount manager ~server ~prefix:"test";
  let tool = create_test_tool "add" "Add two numbers" in
  server.tools <- [tool];
  let%bind result = call_tool manager "test_add" (`Assoc [("x", `Int 5); ("y", `Int 3)]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 8) |}]

(* Tool tags tests *)
let%expect_test "add tool with tags" =
  let manager = create () in
  let tool = Tool.from_function
    (fun _ -> return [create_text_content "42"])
    ~name:"example"
    ~description:"Example tool"
    ~tags:(Set.of_list (module String) ["math"; "utility"])
    ~parameters:(`Assoc [])
  in
  let _added_tool = add_tool manager tool in
  let%bind found_tool = get_tool manager "example" in
  print_s [%sexp (found_tool.Tool.tags : Set.M(String).t)];
  [%expect {| (math utility) |}]

let%expect_test "list tools with tag filter" =
  let manager = create () in
  let tool1 = Tool.from_function
    (fun _ -> return [create_text_content "42"])
    ~name:"math_tool"
    ~description:"Math tool"
    ~tags:(Set.of_list (module String) ["math"])
    ~parameters:(`Assoc [])
  in
  let tool2 = Tool.from_function
    (fun _ -> return [create_text_content "hello"])
    ~name:"string_tool"
    ~description:"String tool"
    ~tags:(Set.of_list (module String) ["string"; "utility"])
    ~parameters:(`Assoc [])
  in
  let tool3 = Tool.from_function
    (fun _ -> return [create_text_content "mixed"])
    ~name:"mixed_tool"
    ~description:"Mixed tool"
    ~tags:(Set.of_list (module String) ["math"; "utility"; "string"])
    ~parameters:(`Assoc [])
  in
  let _added_tool1 = add_tool manager tool1 in
  let _added_tool2 = add_tool manager tool2 in
  let _added_tool3 = add_tool manager tool3 in
  let%bind tools = get_tools manager in
  let math_tools = 
    Map.data tools 
    |> List.filter ~f:(fun tool -> Set.mem tool.Tool.tags "math")
  in
  print_endline "Math tools:";
  List.iter math_tools ~f:(fun t ->
    print_s [%sexp (t.Tool.key : string)]
  );
  let utility_tools =
    Map.data tools
    |> List.filter ~f:(fun tool -> Set.mem tool.Tool.tags "utility")
  in
  print_endline "\nUtility tools:";
  List.iter utility_tools ~f:(fun t ->
    print_s [%sexp (t.Tool.key : string)]
  );
  [%expect {|
    Math tools:
    math_tool
    mixed_tool

    Utility tools:
    string_tool
    mixed_tool |}]

(* Complex type tests *)
let%expect_test "tool with list parameter" =
  let manager = create () in
  let tool = Tool.from_function
    (fun args ->
      match args with
      | `Assoc args_list ->
          let vals = List.Assoc.find_exn args_list ~equal:String.equal "vals" |> function
            | `List nums -> List.map nums ~f:(function
                | `Int n -> n
                | _ -> failwith "Expected list of integers")
            | _ -> failwith "Expected list"
          in
          return [create_text_content (Int.to_string (List.fold vals ~init:0 ~f:(+)))]
      | _ -> failwith "Expected object arguments")
    ~name:"sum_vals"
    ~description:"Sum a list of values"
    ~parameters:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("vals", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "integer")]);
          ("description", `String "List of numbers to sum")
        ])
      ]);
      ("required", `List [`String "vals"])
    ])
  in
  let _added_tool = add_tool manager tool in
  let%bind result = call_tool manager "sum_vals" (`Assoc [("vals", `List [`Int 1; `Int 2; `Int 3])]) in
  print_s [%sexp (List.hd_exn result : Content_block.t)];
  [%expect {| (Text 6) |}] 