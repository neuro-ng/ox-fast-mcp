open! Core
open! Async
open! Json_schema

let rec sexp_of_yojson (json : Yojson.Safe.t) : Sexp.t =
  match json with
  | `Null -> Sexp.Atom "null"
  | `Bool b -> Sexp.Atom (Bool.to_string b)
  | `Int i -> Sexp.Atom (Int.to_string i)
  | `Float f -> Sexp.Atom (Float.to_string f)
  | `String s -> Sexp.Atom s
  | `List xs -> Sexp.List (List.map xs ~f:sexp_of_yojson)
  | `Assoc xs ->
    Sexp.List
      (List.map xs ~f:(fun (k, v) ->
           Sexp.List [ Sexp.Atom k; sexp_of_yojson v ]))
  | `Intlit s -> Sexp.Atom s
  | `Tuple _ | `Variant _ -> failwith "Unsupported JSON type"

let print_json json =
  print_s (sexp_of_yojson json);
  return ()

let%expect_test "test_prune_param_nonexistent" =
  let schema =
    `Assoc
      [
        ("properties", `Assoc [ ("foo", `Assoc [ ("type", `String "string") ]) ]);
      ]
  in
  let%bind () =
    let result = prune_param schema "bar" in
    print_json result
  in
  [%expect {| ((properties ((foo ((type string)))))) |}];
  return ()

let%expect_test "test_prune_param_exists" =
  let schema =
    `Assoc
      [
        ( "properties",
          `Assoc
            [
              ("foo", `Assoc [ ("type", `String "string") ]);
              ("bar", `Assoc [ ("type", `String "integer") ]);
            ] );
      ]
  in
  let%bind () =
    let result = prune_param schema "bar" in
    print_json result
  in
  [%expect {| ((properties ((foo ((type string)))))) |}];
  return ()

let%expect_test "test_prune_param_last_property" =
  let schema =
    `Assoc
      [
        ("properties", `Assoc [ ("foo", `Assoc [ ("type", `String "string") ]) ]);
      ]
  in
  let%bind () =
    let result = prune_param schema "foo" in
    print_json result
  in
  [%expect {| ((properties ())) |}];
  return ()

let%expect_test "test_prune_param_from_required" =
  let schema =
    `Assoc
      [
        ( "properties",
          `Assoc
            [
              ("foo", `Assoc [ ("type", `String "string") ]);
              ("bar", `Assoc [ ("type", `String "integer") ]);
            ] );
        ("required", `List [ `String "foo"; `String "bar" ]);
      ]
  in
  let%bind () =
    let result = prune_param schema "bar" in
    print_json result
  in
  [%expect {| ((properties ((foo ((type string))))) (required (foo))) |}];
  return ()

let%expect_test "test_prune_param_last_required" =
  let schema =
    `Assoc
      [
        ( "properties",
          `Assoc
            [
              ("foo", `Assoc [ ("type", `String "string") ]);
              ("bar", `Assoc [ ("type", `String "integer") ]);
            ] );
        ("required", `List [ `String "foo" ]);
      ]
  in
  let%bind () =
    let result = prune_param schema "foo" in
    print_json result
  in
  [%expect {| ((properties ((bar ((type integer)))))) |}];
  return ()

let%expect_test "test_prune_unused_defs_removes_unreferenced" =
  let schema =
    `Assoc
      [
        ( "properties",
          `Assoc [ ("foo", `Assoc [ ("$ref", `String "#/$defs/foo_def") ]) ] );
        ( "$defs",
          `Assoc
            [
              ("foo_def", `Assoc [ ("type", `String "string") ]);
              ("unused_def", `Assoc [ ("type", `String "integer") ]);
            ] );
      ]
  in
  let%bind () =
    let result = prune_unused_defs schema in
    print_json result
  in
  [%expect
    {|
    ((properties ((foo (($ref #/$defs/foo_def)))))
     ($defs ((foo_def ((type string))))))
    |}];
  return ()

let%expect_test "test_prune_unused_defs_nested_references_kept" =
  let schema =
    `Assoc
      [
        ( "properties",
          `Assoc [ ("foo", `Assoc [ ("$ref", `String "#/$defs/foo_def") ]) ] );
        ( "$defs",
          `Assoc
            [
              ( "foo_def",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "nested",
                            `Assoc [ ("$ref", `String "#/$defs/nested_def") ] );
                        ] );
                  ] );
              ("nested_def", `Assoc [ ("type", `String "string") ]);
              ("unused_def", `Assoc [ ("type", `String "number") ]);
            ] );
      ]
  in
  let%bind () = print_json (prune_unused_defs schema) in
  [%expect
    {|
    ((properties ((foo (($ref #/$defs/foo_def)))))
     ($defs
      ((foo_def
        ((type object) (properties ((nested (($ref #/$defs/nested_def)))))))
       (nested_def ((type string))))))
    |}];
  return ()

let%expect_test "test_walk_and_prune_additional_properties" =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [ ("foo", `Assoc [ ("type", `String "string") ]) ]);
        ("additionalProperties", `Bool false);
      ]
  in
  let%bind () =
    let result = walk_and_prune ~prune_additional_properties:true schema in
    print_json result
  in
  [%expect {| ((type object) (properties ((foo ((type string)))))) |}];
  return ()

let%expect_test "test_walk_and_prune_titles" =
  let schema =
    `Assoc
      [
        ("title", `String "Root Schema");
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "foo",
                `Assoc
                  [
                    ("title", `String "Foo Property"); ("type", `String "string");
                  ] );
            ] );
      ]
  in
  let%bind () =
    let result = walk_and_prune ~prune_titles:true schema in
    print_json result
  in
  [%expect {| ((type object) (properties ((foo ((type string)))))) |}];
  return ()

let%expect_test "test_compress_schema_all_features" =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("title", `String "Root Schema");
        ( "properties",
          `Assoc
            [
              ("keep", `Assoc [ ("type", `String "string") ]);
              ("remove", `Assoc [ ("$ref", `String "#/$defs/remove_def") ]);
            ] );
        ("required", `List [ `String "keep"; `String "remove" ]);
        ("additionalProperties", `Bool false);
        ( "$defs",
          `Assoc
            [
              ("remove_def", `Assoc [ ("type", `String "string") ]);
              ("unused_def", `Assoc [ ("type", `String "number") ]);
            ] );
      ]
  in
  let%bind () =
    let result =
      compress_schema ~prune_params:[ "remove" ] ~prune_titles:true
        ~prune_additional_properties:true schema
    in
    print_json result
  in
  [%expect
    {| ((type object) (properties ((keep ((type string))))) (required (keep))) |}];
  return ()
