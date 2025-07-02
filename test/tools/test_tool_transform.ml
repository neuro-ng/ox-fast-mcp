open Alcotest
open Lwt.Syntax
open Utilities.Types
open Tools.Tool_manager
open Tools.Tool_transform

(** Test helper functions *)
let get_property tool name =
  match tool.parameters with
  | `Assoc [("properties", `Assoc props)] -> List.assoc name props
  | _ -> failwith "Invalid tool parameters structure"

(** Test fixtures *)
let create_add_tool () =
  let add_fn ctx args =
    let* () = Lwt.return_unit in
    match args with
    | `Assoc [("old_x", `Int x); ("old_y", `Int y)] ->
        Lwt.return [create_text_content (string_of_int (x + y))]
    | _ -> Lwt.fail (Invalid_argument "Expected old_x and old_y integers")
  in
  {
    name = "add";
    description = "Add two numbers";
    parameters = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("old_x", `Assoc [
          ("type", `String "integer");
          ("description", `String "old_x description")
        ]);
        ("old_y", `Assoc [
          ("type", `String "integer");
          ("default", `Int 10)
        ])
      ]);
      ("required", `List [`String "old_x"])
    ];
    handler = add_fn;
    enabled = true;
    tags = [];
  }

(** Basic Transformation Tests *)
let basic_transformation_tests = [
  test_case "No change transformation" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool in
    
    check string "tool name" add_tool.name new_tool.name;
    check string "tool description" add_tool.description new_tool.description;
    check bool "tool enabled" add_tool.enabled new_tool.enabled;
    check bool "parameters match" true (new_tool.parameters = add_tool.parameters)
  );
  
  test_case "Maintain arg description" `Quick (fun () ->
    Lwt_main.run (
      let add_tool = create_add_tool () in
      let new_tool = Tool.from_tool add_tool ~transform_args:[
        ("old_x", { name = Some "new_x"; description = None; hide = false; 
                    type_ = None; default = None; default_factory = None;
                    required = None; examples = None })
      ] in
      
      let old_desc = (get_property add_tool "old_x") |> fun x ->
        match x with
        | `Assoc props -> List.assoc "description" props
        | _ -> failwith "Invalid property structure" in
      
      let new_desc = (get_property new_tool "new_x") |> fun x ->
        match x with
        | `Assoc props -> List.assoc "description" props
        | _ -> failwith "Invalid property structure" in
      
      check bool "descriptions match" true (old_desc = new_desc);
      Lwt.return_unit
    )
  );
]

(** Argument Handling Tests *)
let argument_handling_tests = [
  test_case "Maintain defaults" `Quick (fun () ->
    Lwt_main.run (
      let add_tool = create_add_tool () in
      let new_tool = Tool.from_tool add_tool ~transform_args:[
        ("old_x", { name = Some "new_x"; description = None; hide = false;
                    type_ = None; default = None; default_factory = None;
                    required = None; examples = None })
      ] in
      
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 1)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "11" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Drop argument" `Quick (fun () ->
    Lwt_main.run (
      let add_tool = create_add_tool () in
      let new_tool = Tool.from_tool add_tool ~transform_args:[
        ("old_y", { name = None; description = None; hide = true;
                    type_ = None; default = None; default_factory = None;
                    required = None; examples = None })
      ] in
      
      match new_tool.parameters with
      | `Assoc [("properties", `Assoc props)] ->
          check bool "old_y hidden" false (List.mem_assoc "old_y" props);
          let* result = new_tool.handler (create_execution_context ()) (`Assoc [
            ("old_x", `Int 1)
          ]) in
          
          match result with
          | [Text t] -> check string "result" "11" t; Lwt.return_unit
          | _ -> Lwt.fail (Failure "Expected text result")
      | _ -> Lwt.fail (Failure "Invalid parameters structure")
    )
  );
]

(** Error Handling Tests *)
let error_handling_tests = [
  test_case "Error handling" `Quick (fun () ->
    Lwt_main.run (
      let add_tool = create_add_tool () in
      let new_tool = Tool.from_tool add_tool ~transform_args:[
        ("old_y", { name = None; description = None; hide = true;
                    type_ = None; default = Some (`Int 20); default_factory = None;
                    required = None; examples = None })
      ] in
      
      (* Test providing hidden argument raises error *)
      match%lwt new_tool.handler (create_execution_context ()) (`Assoc [
        ("old_x", `Int 1);
        ("old_y", `Int 2)
      ]) with
      | exception Invalid_argument _ -> Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected error for hidden argument")
    )
  );
  
  test_case "Validation errors" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Test: Cannot have both default and default_factory *)
    let invalid_transform () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = true;
                  type_ = None; default = Some (`Int 42);
                  default_factory = Some (fun () -> `Int 24);
                  required = None; examples = None })
    ] in
    
    check_raises "default and default_factory"
      (Invalid_argument "Cannot specify both 'default' and 'default_factory'")
      invalid_transform;
    
    (* Test: default_factory requires hide=true *)
    let invalid_factory () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None;
                  default_factory = Some (fun () -> `Int 42);
                  required = None; examples = None })
    ] in
    
    check_raises "default_factory without hide"
      (Invalid_argument "default_factory can only be used with hide=true")
      invalid_factory;
    
    (* Test: Cannot have both hide=true and required=true *)
    let invalid_hide_required () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = true;
                  type_ = None; default = None; default_factory = None;
                  required = Some true; examples = None })
    ] in
    
    check_raises "hide and required"
      (Invalid_argument "Cannot specify both 'hide=True' and 'required=True'")
      invalid_hide_required;
      
    (* Test: Cannot have required=true with default *)
    let invalid_required_default () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = Some (`Int 42);
                  default_factory = None; required = Some true;
                  examples = None })
    ] in
    
    check_raises "required with default"
      (Invalid_argument "Cannot specify 'required=True' with 'default'")
      invalid_required_default;
      
    (* Test: Cannot have required=true with default_factory *)
    let invalid_required_factory () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None;
                  default_factory = Some (fun () -> `Int 42);
                  required = Some true; examples = None })
    ] in
    
    check_raises "required with default_factory"
      (Invalid_argument "default_factory can only be used with hide=True")
      invalid_required_factory;
      
    (* Test: Unknown arguments in transform_args *)
    let invalid_unknown_arg () = Tool.from_tool add_tool ~transform_args:[
      ("unknown_param", { name = Some "new_name"; description = None; hide = false;
                         type_ = None; default = None; default_factory = None;
                         required = None; examples = None })
    ] in
    
    check_raises "unknown argument"
      (Invalid_argument "Unknown arguments in transform_args: unknown_param")
      invalid_unknown_arg;
      
    (* Test: Duplicate parameter names *)
    let invalid_duplicate_names () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = Some "same_name"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "same_name"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    check_raises "duplicate names"
      (Invalid_argument "Multiple arguments would be mapped to the same names: same_name")
      invalid_duplicate_names;
      
    (* Test: Cannot have required=false without default *)
    let invalid_required_false () = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = Some false; examples = None })
    ] in
    
    check_raises "required false without default"
      (Invalid_argument "Cannot specify 'required=False'. Set a default value instead.")
      invalid_required_false;
      
    (* Test: Cannot call forward outside transform context *)
    let invalid_forward () = 
      Lwt_main.run (
        match%lwt forward (create_execution_context ()) (`Assoc [
          ("new_x", `Int 1);
          ("old_y", `Int 2)
        ]) with
        | exception Invalid_argument _ -> Lwt.return_unit
        | _ -> Lwt.fail (Failure "Expected error for forward outside context")
      )
    in
    
    check_raises "forward outside context"
      (Invalid_argument "forward() can only be called within a transformed tool")
      invalid_forward;
      
    (* Test: Cannot call forward_raw outside transform context *)
    let invalid_forward_raw () = 
      Lwt_main.run (
        match%lwt forward_raw (create_execution_context ()) (`Assoc [
          ("new_x", `Int 1);
          ("old_y", `Int 2)
        ]) with
        | exception Invalid_argument _ -> Lwt.return_unit
        | _ -> Lwt.fail (Failure "Expected error for forward_raw outside context")
      )
    in
    
    check_raises "forward_raw outside context"
      (Invalid_argument "forward_raw() can only be called within a transformed tool")
      invalid_forward_raw
  );
]

(** Type Handling Tests *)
let type_handling_tests = [
  test_case "Type transformation" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = Some "string"; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type changed" "string" 
          (match List.assoc "type" props with `String t -> t | _ -> "");
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Examples in schema" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = Some [`Int 1; `Int 2; `Int 3] })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check bool "has examples" true (List.mem_assoc "examples" props);
        check bool "examples match" true (
          match List.assoc "examples" props with
          | `List [`Int 1; `Int 2; `Int 3] -> true
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Nested examples in schema" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None;
                  examples = Some [
                    `List [`String "a"; `String "b"];
                    `List [`String "c"; `String "d"]
                  ] })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check bool "has examples" true (List.mem_assoc "examples" props);
        check bool "examples match" true (
          match List.assoc "examples" props with
          | `List [
              `List [`String "a"; `String "b"];
              `List [`String "c"; `String "d"]
            ] -> true
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "No examples in schema" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check bool "no examples" false (List.mem_assoc "examples" props);
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Type precedence" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("x", `String x)] ->
          let x_int = int_of_string x in
          let* result = forward_raw ctx (`Assoc [("old_x", `Int x_int); ("old_y", `Int 3)]) in
          match result with
          | [Text t] -> Lwt.return [create_text_content (Printf.sprintf "String input '%s' converted to result: %s" x t)]
          | _ -> Lwt.fail (Failure "Expected text result")
      | _ -> Lwt.fail (Invalid_argument "Expected string x")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "x"; description = None; hide = false;
                  type_ = Some "string"; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    (* Verify schema shows string type *)
    match get_property new_tool "x" with
    | `Assoc props ->
        check string "type is string" "string"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        
        (* Test it works with string input *)
        Lwt_main.run (
          let* result = new_tool.handler (create_execution_context ()) (`Assoc [
            ("x", `String "5")
          ]) in
          
          match result with
          | [Text t] ->
              check bool "contains input" true (String.contains t '5');
              check bool "contains result" true (String.contains t '8');
              Lwt.return_unit
          | _ -> Lwt.fail (Failure "Expected text result")
        )
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Type precedence with constraints" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = Some "string"; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is string" "string"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        
        (* Add string constraints *)
        let props = `Assoc (("minLength", `Int 1) :: ("maxLength", `Int 10) :: props) in
        let new_tool = { new_tool with parameters = `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [("old_x", props)])
        ]} in
        
        match get_property new_tool "old_x" with
        | `Assoc props ->
            check bool "has minLength" true (List.mem_assoc "minLength" props);
            check bool "has maxLength" true (List.mem_assoc "maxLength" props);
            check int "minLength is 1" 1 
              (match List.assoc "minLength" props with `Int n -> n | _ -> 0);
            check int "maxLength is 10" 10
              (match List.assoc "maxLength" props with `Int n -> n | _ -> 0);
            ()
        | _ -> failwith "Invalid property structure"
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Combined attributes" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = Some "renamed_param"; description = Some "New description";
                  hide = false; type_ = Some "string"; 
                  default = Some (`String "default_value");
                  default_factory = None; required = None; examples = None })
    ] in
    
    match new_tool.parameters with
    | `Assoc props ->
        (* Check renamed parameter exists *)
        check bool "renamed param exists" true (
          match List.assoc "properties" props with
          | `Assoc props -> List.mem_assoc "renamed_param" props
          | _ -> false
        );
        
        (* Check old parameter doesn't exist *)
        check bool "old param doesn't exist" false (
          match List.assoc "properties" props with
          | `Assoc props -> List.mem_assoc "old_x" props
          | _ -> false
        );
        
        (* Check all attributes were applied *)
        match get_property new_tool "renamed_param" with
        | `Assoc props ->
            check string "type is string" "string"
              (match List.assoc "type" props with `String t -> t | _ -> "");
            check string "description changed" "New description"
              (match List.assoc "description" props with `String d -> d | _ -> "");
            check string "default value set" "default_value"
              (match List.assoc "default" props with `String d -> d | _ -> "");
            ()
        | _ -> failwith "Invalid property structure"
    | _ -> failwith "Invalid parameters structure"
  );
]

(** Default Value Tests *)
let default_value_tests = [
  test_case "Default factory" `Quick (fun () ->
    let counter = ref 0 in
    let counter_factory () = 
      incr counter;
      `Int !counter
    in
    
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_y", { name = None; description = None; hide = true;
                  type_ = None; default = None; 
                  default_factory = Some counter_factory;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result1 = new_tool.handler (create_execution_context ()) (`Assoc [
        ("old_x", `Int 1)
      ]) in
      
      let* result2 = new_tool.handler (create_execution_context ()) (`Assoc [
        ("old_x", `Int 2)
      ]) in
      
      match result1, result2 with
      | [Text t1], [Text t2] ->
          check string "first call" "2" t1;  (* 1 + 1 *)
          check string "second call" "4" t2;  (* 2 + 2 *)
          Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text results")
    )
  );
  
  test_case "Default factory with request id" `Quick (fun () ->
    let make_request_id () = `String "req_123" in
    
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_y", { name = None; description = None; hide = true;
                  type_ = None; default = None;
                  default_factory = Some make_request_id;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("old_x", `Int 42)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "42_req_123" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
]

(** Required Parameter Tests *)
let required_parameter_tests = [
  test_case "Required parameter" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_y", { name = None; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = Some true; examples = None })
    ] in
    
    match new_tool.parameters with
    | `Assoc props ->
        let required = List.assoc "required" props in
        check bool "old_y is required" true (
          match required with
          | `List reqs -> List.exists (fun x -> x = `String "old_y") reqs
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid parameters structure"
  );
  
  test_case "Required with rename" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_y", { name = Some "new_y"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = Some true; examples = None })
    ] in
    
    match new_tool.parameters with
    | `Assoc props ->
        let required = List.assoc "required" props in
        check bool "new_y is required" true (
          match required with
          | `List reqs -> List.exists (fun x -> x = `String "new_y") reqs
          | _ -> false
        );
        check bool "old_y not in properties" false (
          match List.assoc "properties" props with
          | `Assoc props -> List.mem_assoc "old_y" props
          | _ -> false
        );
        check bool "new_y in properties" true (
          match List.assoc "properties" props with
          | `Assoc props -> List.mem_assoc "new_y" props
          | _ -> false
        );
        check bool "new_y has no default" true (
          match List.assoc "properties" props with
          | `Assoc props -> 
              match List.assoc "new_y" props with
              | `Assoc props -> not (List.mem_assoc "default" props)
              | _ -> false
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid parameters structure"
  );
  
  test_case "Required no change" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = Some "req"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "opt"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match new_tool.parameters with
    | `Assoc props ->
        let required = List.assoc "required" props in
        check bool "req is required" true (
          match required with
          | `List reqs -> List.exists (fun x -> x = `String "req") reqs
          | _ -> false
        );
        check bool "opt not required" true (
          match required with
          | `List reqs -> not (List.exists (fun x -> x = `String "opt") reqs)
          | _ -> false
        );
        check bool "opt has default" true (
          match List.assoc "properties" props with
          | `Assoc props -> 
              match List.assoc "opt" props with
              | `Assoc props -> List.mem_assoc "default" props
              | _ -> false
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid parameters structure"
  );
]

(** Forward/Transform Tests *)
let forward_transform_tests = [
  test_case "Forward functionality" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Test forward with argument mapping *)
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("new_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("new_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and new_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "new_y"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 2);
        ("new_y", `Int 3)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "5" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Forward raw functionality" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Test forward_raw with original argument names *)
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("new_y", `Int y)] ->
          let* result = forward_raw ctx (`Assoc [("old_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and new_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "new_y"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 2);
        ("new_y", `Int 3)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "5" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Tool transform chaining" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* First transformation: old_x -> x *)
    let tool1 = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = Some "x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    (* Second transformation: x -> final_x *)
    let tool2 = Tool.from_tool tool1 ~transform_args:[
      ("x", { name = Some "final_x"; description = None; hide = false;
              type_ = None; default = None; default_factory = None;
              required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = tool2.handler (create_execution_context ()) (`Assoc [
        ("final_x", `Int 5)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "15" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
]

(** Enable/Disable Tests *)
let enable_disable_tests = [
  test_case "Enable/disable" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let disabled_tool = { add_tool with enabled = false } in
    
    (* Test that transformed tool can be enabled even if parent is disabled *)
    let new_tool = Tool.from_tool disabled_tool ~enabled:true in
    check bool "transformed tool enabled" true new_tool.enabled;
    
    (* Test that transformed tool can be disabled *)
    let disabled_new_tool = Tool.from_tool add_tool ~enabled:false in
    check bool "transformed tool disabled" false disabled_new_tool.enabled
  );
]

(** Kwargs Tests *)
let kwargs_tests = [
  test_case "Kwargs with no transform args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("extra", `Int extra); ("old_x", `Int x); ("old_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("old_x", `Int x); ("old_y", `Int y)]) in
          match result with
          | [Text t] -> 
              let sum = int_of_string t in
              Lwt.return [create_text_content (string_of_int (sum + extra))]
          | _ -> Lwt.fail (Failure "Expected text result")
      | _ -> Lwt.fail (Invalid_argument "Expected extra, old_x and old_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("extra", `Int 1);
        ("old_x", `Int 2);
        ("old_y", `Int 3)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "6" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Kwargs with transform args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("old_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and old_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 2);
        ("old_y", `Int 3)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "5" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Kwargs with dropped args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x integer")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = None; description = None; hide = true;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 8)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "18" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Kwargs with extra params" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("old_y", `Int y); ("extra_param", `String extra)] ->
          let* result = forward ctx (`Assoc [("old_x", `Int x); ("old_y", `Int y)]) in
          match result with
          | [Text t] -> Lwt.return [create_text_content (Printf.sprintf "%s: %s" extra t)]
          | _ -> Lwt.fail (Failure "Expected text result")
      | _ -> Lwt.fail (Invalid_argument "Expected new_x, old_y and extra_param")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 2);
        ("old_y", `Int 3);
        ("extra_param", `String "test")
      ]) in
      
      match result with
      | [Text t] -> check string "result" "test: 5" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Kwargs with mixed mapped and unmapped args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("old_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and old_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 1);
        ("old_y", `Int 5)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "6" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
  
  test_case "Kwargs with partial explicit args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("old_y", `Int y); ("some_other_param", `String _)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x, old_y and some_other_param")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 3);
        ("old_y", `Int 7);
        ("some_other_param", `String "test")
      ]) in
      
      match result with
      | [Text t] -> check string "result" "10" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
]

(** Test proxy functionality *)
let proxy_tests = [
  test_case "Transform proxy" `Quick (fun () ->
    Lwt_main.run (
      let add_tool = create_add_tool () in
      let new_add_tool = Tool.from_tool add_tool ~name:"add_transformed" ~transform_args:[
        ("old_x", { name = Some "new_x"; description = None; hide = false;
                    type_ = None; default = None; default_factory = None;
                    required = None; examples = None })
      ] in
      
      let* result = new_add_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 1);
        ("old_y", `Int 2)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "3" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
];

(** Test type precedence at runtime *)
let type_precedence_runtime_tests = [
  test_case "Type precedence at runtime" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Transform x to string type but keep same logic *)
    let transform_fn ctx args =
      match args with
      | `Assoc [("x", `String x)] ->
          let x_int = int_of_string x in
          let* result = forward_raw ctx (`Assoc [("old_x", `Int x_int); ("old_y", `Int 3)]) in
          match result with
          | [Text t] -> Lwt.return [create_text_content (Printf.sprintf "String input '%s' converted to result: %s" x t)]
          | _ -> Lwt.fail (Failure "Expected text result")
      | _ -> Lwt.fail (Invalid_argument "Expected string x")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "x"; description = None; hide = false;
                  type_ = Some "string"; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    (* Verify schema shows string type *)
    match get_property new_tool "x" with
    | `Assoc props ->
        check string "type is string" "string"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        
        (* Test it works with string input *)
        Lwt_main.run (
          let* result = new_tool.handler (create_execution_context ()) (`Assoc [
            ("x", `String "5")
          ]) in
          
          match result with
          | [Text t] ->
              check bool "contains input" true (String.contains t '5');
              check bool "contains result" true (String.contains t '8');
              Lwt.return_unit
          | _ -> Lwt.fail (Failure "Expected text result")
        )
    | _ -> failwith "Invalid property structure"
  );
];

(** Test function with kwargs and partial explicit args *)
let kwargs_partial_explicit_tests = [
  test_case "Kwargs with partial explicit args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("some_other_param", `String _); ("old_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x, old_y and some_other_param")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 3);
        ("old_y", `Int 7);
        ("some_other_param", `String "test")
      ]) in
      
      match result with
      | [Text t] -> check string "result" "10" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
];

(** Test function with kwargs and mixed mapped/unmapped args *)
let kwargs_mixed_args_tests = [
  test_case "Kwargs with mixed mapped and unmapped args" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let transform_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("old_y", `Int y)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("old_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and old_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    Lwt_main.run (
      let* result = new_tool.handler (create_execution_context ()) (`Assoc [
        ("new_x", `Int 1);
        ("old_y", `Int 5)
      ]) in
      
      match result with
      | [Text t] -> check string "result" "6" t; Lwt.return_unit
      | _ -> Lwt.fail (Failure "Expected text result")
    )
  );
];

(** Test annotated types *)
let annotated_type_tests = [
  test_case "Annotated types" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = Some "An annotated integer";
                  hide = false; type_ = Some "integer";
                  default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is integer" "integer"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        check string "description is set" "An annotated integer"
          (match List.assoc "description" props with `String d -> d | _ -> "");
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Annotated string with constraints" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = Some "string"; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is string" "string"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        
        (* Add string constraints *)
        let props = `Assoc (
          ("minLength", `Int 1) ::
          ("maxLength", `Int 10) ::
          ("pattern", `String "^[a-z]+$") ::
          props
        ) in
        let new_tool = { new_tool with parameters = `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [("old_x", props)])
        ]} in
        
        match get_property new_tool "old_x" with
        | `Assoc props ->
            check bool "has minLength" true (List.mem_assoc "minLength" props);
            check bool "has maxLength" true (List.mem_assoc "maxLength" props);
            check bool "has pattern" true (List.mem_assoc "pattern" props);
            check int "minLength is 1" 1 
              (match List.assoc "minLength" props with `Int n -> n | _ -> 0);
            check int "maxLength is 10" 10
              (match List.assoc "maxLength" props with `Int n -> n | _ -> 0);
            check string "pattern is correct" "^[a-z]+$"
              (match List.assoc "pattern" props with `String p -> p | _ -> "");
            ()
        | _ -> failwith "Invalid property structure"
    | _ -> failwith "Invalid property structure"
  );
];

(** Test function validation *)
let function_validation_tests = [
  test_case "Function missing parameters" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Function missing required transformed parameter *)
    let invalid_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x integer")
    in
    
    let invalid_transform () = Tool.from_tool add_tool ~transform_fn:invalid_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "new_y"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    check_raises "missing parameters"
      (Invalid_argument "Function missing parameters required after transformation: new_y")
      invalid_transform
  );
  
  test_case "Function with extra parameters" `Quick (fun () ->
    let add_tool = create_add_tool () in
    
    (* Function with extra parameter that has default *)
    let valid_fn ctx args =
      match args with
      | `Assoc [("new_x", `Int x); ("new_y", `Int y); ("extra", `String _)] ->
          let* result = forward ctx (`Assoc [("new_x", `Int x); ("new_y", `Int y)]) in
          Lwt.return result
      | _ -> Lwt.fail (Invalid_argument "Expected new_x and new_y integers")
    in
    
    let new_tool = Tool.from_tool add_tool ~transform_fn:valid_fn ~transform_args:[
      ("old_x", { name = Some "new_x"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None });
      ("old_y", { name = Some "new_y"; description = None; hide = false;
                  type_ = None; default = None; default_factory = None;
                  required = None; examples = None })
    ] in
    
    match new_tool.parameters with
    | `Assoc props ->
        check bool "has extra param" true (
          match List.assoc "properties" props with
          | `Assoc props -> List.mem_assoc "extra" props
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid parameters structure"
  );
];

(** Type System Tests *)
let type_system_tests = [
  test_case "Basic types" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let test_type name json_type =
      let new_tool = Tool.from_tool add_tool ~transform_args:[
        ("old_x", { name = None; description = None; hide = false;
                    type_ = Some json_type; default = None; default_factory = None;
                    required = None; examples = None })
      ] in
      match get_property new_tool "old_x" with
      | `Assoc props ->
          check string (Printf.sprintf "%s type" name) json_type
            (match List.assoc "type" props with `String t -> t | _ -> "");
          ()
      | _ -> failwith "Invalid property structure"
    in
    
    test_type "integer" "integer";
    test_type "number" "number";
    test_type "string" "string";
    test_type "boolean" "boolean";
    test_type "array" "array";
    test_type "object" "object"
  );
  
  test_case "Type annotations with constraints" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = Some "An annotated string";
                  hide = false; type_ = Some "string"; default = None;
                  default_factory = None; required = None; examples = None })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is string" "string"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        check string "description is set" "An annotated string"
          (match List.assoc "description" props with `String d -> d | _ -> "");
        
        (* Add string constraints *)
        let props = `Assoc (
          ("minLength", `Int 1) ::
          ("maxLength", `Int 10) ::
          ("pattern", `String "^[a-z]+$") ::
          props
        ) in
        let new_tool = { new_tool with parameters = `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [("old_x", props)])
        ]} in
        
        match get_property new_tool "old_x" with
        | `Assoc props ->
            check bool "has minLength" true (List.mem_assoc "minLength" props);
            check bool "has maxLength" true (List.mem_assoc "maxLength" props);
            check bool "has pattern" true (List.mem_assoc "pattern" props);
            check int "minLength is 1" 1 
              (match List.assoc "minLength" props with `Int n -> n | _ -> 0);
            check int "maxLength is 10" 10
              (match List.assoc "maxLength" props with `Int n -> n | _ -> 0);
            check string "pattern is correct" "^[a-z]+$"
              (match List.assoc "pattern" props with `String p -> p | _ -> "");
            ()
        | _ -> failwith "Invalid property structure"
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Array type with items" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = Some "array"; default = None; default_factory = None;
                  required = None; examples = Some [
                    `List [`Int 1; `Int 2];
                    `List [`Int 3; `Int 4]
                  ] })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is array" "array"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        check bool "has examples" true (List.mem_assoc "examples" props);
        check bool "examples match" true (
          match List.assoc "examples" props with
          | `List [
              `List [`Int 1; `Int 2];
              `List [`Int 3; `Int 4]
            ] -> true
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid property structure"
  );
  
  test_case "Object type with properties" `Quick (fun () ->
    let add_tool = create_add_tool () in
    let new_tool = Tool.from_tool add_tool ~transform_args:[
      ("old_x", { name = None; description = None; hide = false;
                  type_ = Some "object"; default = None; default_factory = None;
                  required = None; examples = Some [
                    `Assoc [("x", `Int 1); ("y", `String "a")];
                    `Assoc [("x", `Int 2); ("y", `String "b")]
                  ] })
    ] in
    
    match get_property new_tool "old_x" with
    | `Assoc props ->
        check string "type is object" "object"
          (match List.assoc "type" props with `String t -> t | _ -> "");
        check bool "has examples" true (List.mem_assoc "examples" props);
        check bool "examples match" true (
          match List.assoc "examples" props with
          | `List [
              `Assoc [("x", `Int 1); ("y", `String "a")];
              `Assoc [("x", `Int 2); ("y", `String "b")]
            ] -> true
          | _ -> false
        );
        ()
    | _ -> failwith "Invalid property structure"
  );
];

let test_suite = [
  "Basic Transformation", basic_transformation_tests;
  "Argument Handling", argument_handling_tests;
  "Error Handling", error_handling_tests;
  "Type Handling", type_handling_tests;
  "Type System", type_system_tests;
  "Default Values", default_value_tests;
  "Required Parameters", required_parameter_tests;
  "Forward/Transform", forward_transform_tests;
  "Enable/Disable", enable_disable_tests;
  "Kwargs", kwargs_tests;
  "Type Precedence with Constraints", type_precedence_with_constraints_tests;
  "Combined Attributes", combined_attributes_tests;
  "Proxy", proxy_tests;
  "Type Precedence Runtime", type_precedence_runtime_tests;
  "Kwargs Partial Explicit", kwargs_partial_explicit_tests;
  "Kwargs Mixed Args", kwargs_mixed_args_tests;
  "Annotated Types", annotated_type_tests;
  "Function Validation", function_validation_tests;
]

let () = Alcotest.run "Tool Transform Tests" test_suite