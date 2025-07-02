open Alcotest
open Yojson.Safe
open Utilities.Json_schema

let test_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("name", `Assoc [
      ("type", `String "string");
      ("title", `String "Name");
    ]);
    ("age", `Assoc [
      ("type", `String "integer");
      ("title", `String "Age");
    ]);
  ]);
  ("required", `List [`String "name"; `String "age"]);
  ("additionalProperties", `Bool false);
  ("$defs", `Assoc [
    ("unused_def", `Assoc [
      ("type", `String "string")
    ]);
    ("used_def", `Assoc [
      ("type", `String "object")
    ]);
  ]);
]

let schema_with_ref = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("person", `Assoc [
      ("$ref", `String "#/$defs/used_def")
    ])
  ])
]

let test_prune_param () =
  let schema = test_schema in
  let pruned = prune_param schema "age" in
  let props = match pruned |> Util.member "properties" with
    | `Assoc props -> props
    | _ -> [] in
  let required = match pruned |> Util.member "required" with
    | `List required -> required
    | _ -> [] in
  
  (* Check age was removed from properties *)
  check bool "age removed from properties"
    false (List.mem_assoc "age" props);
  
  (* Check age was removed from required *)
  check bool "age removed from required"
    false (List.exists (fun x -> x = `String "age") required)

let test_prune_unused_defs () =
  (* Combine schemas by merging their properties and keeping defs from test_schema *)
  let combined = match test_schema, schema_with_ref with
    | `Assoc fields1, `Assoc fields2 ->
        let merged_props = match List.assoc_opt "properties" fields1, List.assoc_opt "properties" fields2 with
          | Some (`Assoc props1), Some (`Assoc props2) ->
              ("properties", `Assoc (props1 @ props2))
          | _ -> ("properties", `Assoc []) in
        let fields = List.filter (fun (k, _) -> k <> "properties") fields1 in
        `Assoc (merged_props :: fields)
    | _ -> test_schema in
  
  let pruned = prune_unused_defs combined in
  let defs = match pruned |> Util.member "$defs" with
    | `Assoc defs -> defs
    | _ -> [] in
  
  (* Check unused_def was removed *)
  check bool "unused_def removed"
    false (List.mem_assoc "unused_def" defs);
  
  (* Check used_def was kept *)
  check bool "used_def kept"
    true (List.mem_assoc "used_def" defs)

let test_walk_and_prune () =
  let schema = test_schema in
  let pruned = walk_and_prune ~prune_titles:true ~prune_additional_properties:true schema in
  
  (* Check titles were removed *)
  let has_title = ref false in
  let rec check_title = function
    | `Assoc fields ->
        if List.mem_assoc "title" fields then
          has_title := true
        else
          List.iter (fun (_, v) -> check_title v) fields
    | `List items ->
        List.iter check_title items
    | _ -> () in
  check_title pruned;
  check bool "titles removed" false !has_title;
  
  (* Check additionalProperties was removed *)
  check bool "additionalProperties removed"
    false (match pruned with
      | `Assoc fields -> List.mem_assoc "additionalProperties" fields
      | _ -> true)

let test_compress_schema () =
  let schema = test_schema in
  let compressed = compress_schema
    ~prune_params:["age"]
    ~prune_defs:true
    ~prune_additional_properties:true
    ~prune_titles:true
    schema in
  
  (* Check all pruning operations were applied *)
  let props = match compressed |> Util.member "properties" with
    | `Assoc props -> props
    | _ -> [] in
  check bool "age removed" false (List.mem_assoc "age" props);
  
  let has_title = ref false in
  let rec check_title = function
    | `Assoc fields ->
        if List.mem_assoc "title" fields then
          has_title := true
        else
          List.iter (fun (_, v) -> check_title v) fields
    | `List items ->
        List.iter check_title items
    | _ -> () in
  check_title compressed;
  check bool "titles removed" false !has_title;
  
  check bool "additionalProperties removed"
    false (match compressed with
      | `Assoc fields -> List.mem_assoc "additionalProperties" fields
      | _ -> true)

let () =
  run "JSON Schema Tests" [
    ("JSON Schema", [
      test_case "Prune parameter" `Quick test_prune_param;
      test_case "Prune unused definitions" `Quick test_prune_unused_defs;
      test_case "Walk and prune" `Quick test_walk_and_prune;
      test_case "Compress schema" `Quick test_compress_schema;
    ]);
  ] 