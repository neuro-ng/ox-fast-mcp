open Alcotest
open Utilities.Components

(** Test component creation and basic properties *)
let test_component_creation () =
  let component = create_component
    ~name:"test_component"
    ~description:(Some "A test component")
    ~tags:["test"; "example"]
    ~enabled:true
    ~extra:()
    () in
  
  check string "component name" "test_component" component.name;
  check (option string) "component description" (Some "A test component") component.description;
  check (list string) "component tags" ["example"; "test"] (List.sort String.compare component.tags);
  check bool "component enabled" true component.enabled

(** Test key functionality *)
let test_key_handling () =
  let component = create_component ~name:"test" ~extra:() () in
  check string "default key is name" "test" (get_key component);
  
  let with_custom_key = with_key component "custom_key" in
  check string "custom key set" "custom_key" (get_key with_custom_key)

(** Test enable/disable functions *)
let test_enable_disable () =
  let component = create_component ~name:"test" ~enabled:false ~extra:() () in
  check bool "initially disabled" false component.enabled;
  
  let enabled = enable component in
  check bool "component enabled" true enabled.enabled;
  
  let disabled = disable enabled in
  check bool "component disabled" false disabled.enabled

(** Test equality comparison *)
let test_equality () =
  let comp1 = create_component
    ~name:"test"
    ~description:(Some "desc")
    ~tags:["tag1"; "tag2"]
    ~extra:()
    () in
  
  let comp2 = create_component
    ~name:"test"
    ~description:(Some "desc")
    ~tags:["tag2"; "tag1"]
    ~extra:()
    () in
  
  let comp3 = create_component
    ~name:"different"
    ~description:(Some "desc")
    ~tags:["tag1"; "tag2"]
    ~extra:()
    () in
  
  check bool "identical components equal" true (equal comp1 comp2);
  check bool "different components not equal" false (equal comp1 comp3)

(** Test JSON serialization *)
let test_json_serialization () =
  let component = create_component
    ~name:"test_json"
    ~description:(Some "JSON test")
    ~tags:["json"; "test"]
    ~extra:()
    () in
  
  let json = component_to_yojson component in
  match component_of_yojson json with
  | Ok decoded ->
    check string "decoded name" component.name decoded.name;
    check (option string) "decoded description" component.description decoded.description;
    check (list string) "decoded tags" 
      (List.sort String.compare component.tags)
      (List.sort String.compare decoded.tags);
    check bool "decoded enabled" component.enabled decoded.enabled
  | Error msg -> fail msg

(** Test string representation *)
let test_to_string () =
  let component = create_component
    ~name:"test_str"
    ~description:(Some "String test")
    ~tags:["str"; "test"]
    ~extra:()
    () in
  
  let str = to_string component in
  let contains s1 s2 =
    try
      let _ = Str.search_forward (Str.regexp_string s2) s1 0 in
      true
    with Not_found -> false
  in
  check bool "string contains name" (contains str "test_str") true;
  check bool "string contains description" (contains str "String test") true;
  check bool "string contains tags" (contains str "str, test") true

(** Main test suite *)
let () =
  run "Components Module Tests" [
    ("Component Creation", [
      test_case "Basic component creation" `Quick test_component_creation;
    ]);
    ("Key Handling", [
      test_case "Key functionality" `Quick test_key_handling;
    ]);
    ("Enable/Disable", [
      test_case "Enable and disable functions" `Quick test_enable_disable;
    ]);
    ("Equality", [
      test_case "Component equality" `Quick test_equality;
    ]);
    ("JSON", [
      test_case "JSON serialization" `Quick test_json_serialization;
    ]);
    ("String Representation", [
      test_case "String conversion" `Quick test_to_string;
    ]);
  ] 