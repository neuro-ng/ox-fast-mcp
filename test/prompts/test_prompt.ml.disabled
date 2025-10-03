open Alcotest
open Lwt.Syntax
open Utilities.Types

module TestPrompts = struct
  (* Expected prompt interface based on Python FastMCP *)

  type prompt_handler =
    execution_context -> (string * json) list -> prompt_message list Lwt.t

  type function_prompt = {
    name : string;
    description : string;
    arguments : prompt_argument list option;
    handler : prompt_handler;
    enabled : bool;
    tags : string list;
  }

  type prompt_manager = {
    mutable prompts : (string, function_prompt) Hashtbl.t;
    mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  }
end

(** Test prompt creation *)
let test_prompt_creation () =
  let name = "summarize_text" in
  let description = "Generate a summary of the given text" in
  let arguments =
    Some
      [
        {
          name = "text";
          description = Some "Text to summarize";
          required = true;
        };
        {
          name = "max_length";
          description = Some "Maximum summary length";
          required = false;
        };
      ]
  in

  let handler _ctx _args =
    let* () = Lwt.return_unit in
    Lwt.return
      [
        {
          role = "user";
          content = create_text_content "Please summarize this text";
        };
        {
          role = "assistant";
          content = create_text_content "Here's a summary...";
        };
      ]
  in

  let prompt =
    TestPrompts.
      {
        name;
        description;
        arguments;
        handler;
        enabled = true;
        tags = [ "text"; "summarization" ];
      }
  in

  check string "prompt name" name prompt.name;
  check string "prompt description" description prompt.description;
  check bool "prompt enabled" true prompt.enabled;
  check int "prompt tags count" 2 (List.length prompt.tags);
  check bool "has text tag" true (List.mem "text" prompt.tags);
  check bool "has arguments" true (Option.is_some prompt.arguments)

(** Test prompt execution *)
let test_prompt_execution () =
  Lwt_main.run
    (let ctx = create_execution_context ~request_id:"test-123" () in
     let args =
       [ ("text", `String "This is a long text that needs summarization") ]
     in

     let handler _ctx _args =
       let* () = Lwt.return_unit in
       Lwt.return
         [
           {
             role = "user";
             content = create_text_content "Summarize: This is a long text";
           };
           {
             role = "assistant";
             content = create_text_content "Summary: A text.";
           };
         ]
     in

     let prompt =
       TestPrompts.
         {
           name = "summarizer";
           description = "Text summarizer";
           arguments = None;
           handler;
           enabled = true;
           tags = [];
         }
     in

     let* result = prompt.handler ctx args in

     check int "result count" 2 (List.length result);
     let first_result = List.hd result in
     check string "first message role" "user" first_result.role;
     check bool "first message is text" true
       (match first_result.content with
       | Text _ -> true
       | _ -> false);

     Lwt.return_unit)

(** Test prompt manager creation *)
let test_prompt_manager_creation () =
  let manager =
    TestPrompts.{ prompts = Hashtbl.create 16; duplicate_behavior = `Warn }
  in

  check int "initial prompt count" 0 (Hashtbl.length manager.prompts);
  check bool "correct duplicate behavior" true
    (match manager.duplicate_behavior with
    | `Warn -> true
    | _ -> false)

(** Test prompt registration *)
let test_prompt_registration () =
  let manager =
    TestPrompts.{ prompts = Hashtbl.create 16; duplicate_behavior = `Error }
  in

  let prompt1 =
    TestPrompts.
      {
        name = "prompt1";
        description = "First prompt";
        arguments = None;
        handler = (fun _ctx _args -> Lwt.return []);
        enabled = true;
        tags = [];
      }
  in

  let prompt2 =
    TestPrompts.
      {
        name = "prompt2";
        description = "Second prompt";
        arguments =
          Some [ { name = "input"; description = None; required = true } ];
        handler = (fun _ctx _args -> Lwt.return []);
        enabled = true;
        tags = [ "tag1" ];
      }
  in

  (* Register prompts *)
  Hashtbl.add manager.prompts prompt1.name prompt1;
  Hashtbl.add manager.prompts prompt2.name prompt2;

  check int "prompt count after registration" 2 (Hashtbl.length manager.prompts);
  check bool "prompt1 exists" true (Hashtbl.mem manager.prompts "prompt1");
  check bool "prompt2 exists" true (Hashtbl.mem manager.prompts "prompt2");

  (* Test retrieval *)
  let retrieved_prompt1 = Hashtbl.find manager.prompts "prompt1" in
  check string "retrieved prompt name" "prompt1" retrieved_prompt1.name;
  check string "retrieved prompt description" "First prompt"
    retrieved_prompt1.description

(** Test prompt argument validation *)
let test_prompt_argument_validation () =
  let valid_args =
    [
      { name = "text"; description = Some "Input text"; required = true };
      { name = "format"; description = Some "Output format"; required = false };
    ]
  in

  let invalid_args =
    [ { name = ""; description = None; required = true } (* Empty name *) ]
  in

  (* These would be validation functions we expect to implement *)
  let validate_prompt_arguments (args : prompt_argument list) =
    List.for_all
      (fun (arg : prompt_argument) -> String.length arg.name > 0)
      args
  in

  check bool "valid args accepted" true (validate_prompt_arguments valid_args);
  check bool "invalid args rejected" false
    (validate_prompt_arguments invalid_args)

(** Test prompt serialization to MCP format *)
let test_prompt_mcp_serialization () =
  let prompt =
    TestPrompts.
      {
        name = "serialize_test";
        description = "Prompt for serialization testing";
        arguments =
          Some
            [
              {
                name = "input";
                description = Some "Input text";
                required = true;
              };
            ];
        handler = (fun _ctx _args -> Lwt.return []);
        enabled = true;
        tags = [ "test" ];
      }
  in

  (* This would be the function to convert to MCP prompt definition *)
  let to_mcp_prompt (p : TestPrompts.function_prompt) =
    { name = p.name; description = p.description; arguments = p.arguments }
  in

  let mcp_prompt = to_mcp_prompt prompt in
  check string "mcp prompt name" "serialize_test" mcp_prompt.name;
  check string "mcp prompt description" "Prompt for serialization testing"
    mcp_prompt.description;
  check bool "mcp prompt has arguments" true
    (Option.is_some mcp_prompt.arguments)

(** Test prompt with template rendering *)
let test_prompt_template_rendering () =
  Lwt_main.run
    (let ctx = create_execution_context () in
     let args = [ ("name", `String "Alice"); ("age", `String "30") ] in

     let template_handler _ctx args =
       let name =
         match List.assoc_opt "name" args with
         | Some (`String n) -> n
         | _ -> "Unknown"
       in
       let age =
         match List.assoc_opt "age" args with
         | Some (`String a) -> a
         | _ -> "Unknown"
       in

       let* () = Lwt.return_unit in
       Lwt.return
         [
           {
             role = "system";
             content = create_text_content "You are a helpful assistant.";
           };
           {
             role = "user";
             content =
               create_text_content
                 ("Tell me about " ^ name ^ " who is " ^ age ^ " years old.");
           };
         ]
     in

     let prompt =
       TestPrompts.
         {
           name = "person_info";
           description = "Generate info about a person";
           arguments =
             Some
               [
                 {
                   name = "name";
                   description = Some "Person's name";
                   required = true;
                 };
                 {
                   name = "age";
                   description = Some "Person's age";
                   required = true;
                 };
               ];
           handler = template_handler;
           enabled = true;
           tags = [ "template"; "person" ];
         }
     in

     let* result = prompt.handler ctx args in

     check int "template result count" 2 (List.length result);

     let system_msg = List.nth result 0 in
     let user_msg = List.nth result 1 in

     check string "system message role" "system" system_msg.role;
     check string "user message role" "user" user_msg.role;

     Lwt.return_unit)

(** Test prompt error handling *)
let test_prompt_error_handling () =
  Lwt_main.run
    (let ctx = create_execution_context () in
     let args = [ ("invalid", `String "data") ] in

     let failing_handler _ctx _args =
       Lwt.fail (Failure "Prompt rendering failed")
     in

     let prompt =
       TestPrompts.
         {
           name = "failing_prompt";
           description = "A prompt that fails";
           arguments = None;
           handler = failing_handler;
           enabled = true;
           tags = [];
         }
     in

     (* Test error handling *)
     let* result =
       Lwt.catch
         (fun () -> prompt.handler ctx args)
         (function
           | Failure msg ->
             Lwt.return
               [
                 {
                   role = "error";
                   content = create_text_content ("Error: " ^ msg);
                 };
               ]
           | exn -> Lwt.fail exn)
     in

     check int "error result count" 1 (List.length result);
     let error_message = List.hd result in
     check string "error message role" "error" error_message.role;

     Lwt.return_unit)

(** Test prompt filtering by tags *)
let test_prompt_filtering () =
  let manager =
    TestPrompts.{ prompts = Hashtbl.create 16; duplicate_behavior = `Warn }
  in

  let prompts =
    [
      ("prompt1", [ "text"; "summarization" ]);
      ("prompt2", [ "image"; "generation" ]);
      ("prompt3", [ "text"; "translation" ]);
      ("prompt4", []);
    ]
  in

  List.iter
    (fun (name, tags) ->
      let prompt =
        TestPrompts.
          {
            name;
            description = "Test prompt";
            arguments = None;
            handler = (fun _ctx _args -> Lwt.return []);
            enabled = true;
            tags;
          }
      in
      Hashtbl.add manager.prompts name prompt)
    prompts;

  (* Filter by tag *)
  let filter_by_tag tag_filter =
    Hashtbl.fold
      (fun _name (prompt : TestPrompts.function_prompt) acc ->
        if List.exists (fun tag -> tag = tag_filter) prompt.tags then
          prompt :: acc
        else acc)
      manager.prompts []
  in

  let text_prompts = filter_by_tag "text" in
  check int "text prompts count" 2 (List.length text_prompts);

  let image_prompts = filter_by_tag "image" in
  check int "image prompts count" 1 (List.length image_prompts)

(** Test prompt enabling/disabling *)
let test_prompt_enable_disable () =
  let prompt =
    TestPrompts.
      {
        name = "toggle_prompt";
        description = "A prompt that can be toggled";
        arguments = None;
        handler = (fun _ctx _args -> Lwt.return []);
        enabled = true;
        tags = [];
      }
  in

  check bool "prompt initially enabled" true prompt.enabled;

  (* This would be how we'd disable a prompt *)
  let disabled_prompt = { prompt with enabled = false } in
  check bool "prompt disabled" false disabled_prompt.enabled;

  let re_enabled_prompt = { disabled_prompt with enabled = true } in
  check bool "prompt re-enabled" true re_enabled_prompt.enabled

(** Test complex prompt with multiple message types *)
let test_complex_prompt_messages () =
  Lwt_main.run
    (let ctx = create_execution_context () in
     let args = [ ("mode", `String "mixed") ] in

     let complex_handler _ctx args =
       let mode =
         match List.assoc_opt "mode" args with
         | Some (`String m) -> m
         | _ -> "text"
       in

       let* () = Lwt.return_unit in
       match mode with
       | "text" ->
         Lwt.return
           [
             {
               role = "user";
               content = create_text_content "Simple text prompt";
             };
           ]
       | "mixed" ->
         Lwt.return
           [
             {
               role = "system";
               content = create_text_content "System instructions";
             };
             { role = "user"; content = create_text_content "User query" };
             {
               role = "assistant";
               content =
                 create_image_content ~data:"base64data" ~mime_type:"image/png"
                   ();
             };
           ]
       | _ -> Lwt.return []
     in

     let prompt =
       TestPrompts.
         {
           name = "complex_prompt";
           description = "Prompt with complex message types";
           arguments = None;
           handler = complex_handler;
           enabled = true;
           tags = [ "complex" ];
         }
     in

     let* result = prompt.handler ctx args in

     check int "complex result count" 3 (List.length result);

     let system_msg = List.nth result 0 in
     let user_msg = List.nth result 1 in
     let assistant_msg = List.nth result 2 in

     check string "system role" "system" system_msg.role;
     check string "user role" "user" user_msg.role;
     check string "assistant role" "assistant" assistant_msg.role;

     check bool "system content is text" true
       (match system_msg.content with
       | Text _ -> true
       | _ -> false);
     check bool "user content is text" true
       (match user_msg.content with
       | Text _ -> true
       | _ -> false);
     check bool "assistant content is image" true
       (match assistant_msg.content with
       | Image _ -> true
       | _ -> false);

     Lwt.return_unit)

(** Main test suite *)
let () =
  run "Prompts Module Tests"
    [
      ( "Prompt Creation",
        [ test_case "Basic prompt creation" `Quick test_prompt_creation ] );
      ( "Prompt Execution",
        [
          test_case "Prompt handler execution" `Quick test_prompt_execution;
          test_case "Prompt error handling" `Quick test_prompt_error_handling;
          test_case "Complex prompt messages" `Quick
            test_complex_prompt_messages;
        ] );
      ( "Prompt Manager",
        [
          test_case "Prompt manager creation" `Quick
            test_prompt_manager_creation;
          test_case "Prompt registration" `Quick test_prompt_registration;
        ] );
      ( "Prompt Validation",
        [
          test_case "Argument validation" `Quick test_prompt_argument_validation;
        ] );
      ( "Prompt Serialization",
        [ test_case "MCP serialization" `Quick test_prompt_mcp_serialization ]
      );
      ( "Prompt Management",
        [
          test_case "Prompt filtering by tags" `Quick test_prompt_filtering;
          test_case "Prompt enable/disable" `Quick test_prompt_enable_disable;
        ] );
      ( "Template Rendering",
        [ test_case "Template rendering" `Quick test_prompt_template_rendering ]
      );
    ]
