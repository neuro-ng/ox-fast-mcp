open Core
open Alcotest

let test_components = [
  test_case "create with defaults" `Quick (fun () ->
    let t = Utilities.Components.create ~name:"test" ~extra:() () |> Utilities.Components.to_or_error |> ok_exn in
    check string "name" "test" t.name;
    check (option string) "description" None t.description;
    check (list string) "tags" [] t.tags;
    check bool "enabled" true t.enabled;
    check (option string) "key" None t.key);

  test_case "create with all fields" `Quick (fun () ->
    let t = Utilities.Components.create
      ~name:"test"
      ~description:(Some "desc")
      ~tags:["a"; "b"]
      ~enabled:false
      ~key:(Some "key")
      ~version:(Some 1)
      ~extra:42
      ()
      |> Utilities.Components.to_or_error |> ok_exn
    in
    check string "name" "test" t.name;
    check (option string) "description" (Some "desc") t.description;
    check (list string) "tags" ["a"; "b"] t.tags;
    check bool "enabled" false t.enabled;
    check (option string) "key" (Some "key") t.key;
    check (option int) "version" (Some 1) t.version;
    check int "extra" 42 t.extra);

  test_case "create with invalid name" `Quick (fun () ->
    let result = Utilities.Components.create ~name:"" ~extra:() () in
    check bool "empty name" true (Result.is_error result);
    let result = Utilities.Components.create ~name:"invalid name" ~extra:() () in
    check bool "name with whitespace" true (Result.is_error result));

  test_case "create with invalid tags" `Quick (fun () ->
    let result = Utilities.Components.create ~name:"test" ~tags:[""] ~extra:() () in
    check bool "empty tag" true (Result.is_error result);
    let result = Utilities.Components.create ~name:"test" ~tags:["invalid tag"] ~extra:() () in
    check bool "tag with whitespace" true (Result.is_error result));

  test_case "create with invalid version" `Quick (fun () ->
    let result = Utilities.Components.create ~name:"test" ~version:(Some (-1)) ~extra:() () in
    check bool "negative version" true (Result.is_error result));

  test_case "get_key fallback" `Quick (fun () ->
    let t = Utilities.Components.create ~name:"test" ~extra:() () |> Utilities.Components.to_or_error |> ok_exn in
    check string "key fallback" "test" (Utilities.Components.get_key t);
    let t_with_key = Utilities.Components.with_key t "custom_key" in
    check string "custom key" "custom_key" (Utilities.Components.get_key t_with_key));

  test_case "enable/disable" `Quick (fun () ->
    let t = Utilities.Components.create ~name:"test" ~enabled:false ~extra:() () |> Utilities.Components.to_or_error |> ok_exn in
    check bool "initially disabled" false t.enabled;
    let t = Utilities.Components.enable t in
    check bool "enabled" true t.enabled;
    let t = Utilities.Components.disable t in
    check bool "disabled" false t.enabled);

  test_case "equal comparison" `Quick (fun () ->
    let t1 = Utilities.Components.create ~name:"test" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn in
    let t2 = Utilities.Components.create ~name:"test" ~description:(Some "desc") ~tags:["b"; "a"] ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn in
    let t3 = Utilities.Components.create ~name:"test2" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn in
    check bool "equal" true (Utilities.Components.equal t1 t2);
    check bool "not equal" false (Utilities.Components.equal t1 t3));

  test_case "version operations" `Quick (fun () ->
    let t = Utilities.Components.create ~name:"test" ~extra:() () |> Utilities.Components.to_or_error |> ok_exn in
    check (option int) "initial version" None (Utilities.Components.get_version t);
    let t = Utilities.Components.set_version t 1 |> Utilities.Components.to_or_error |> ok_exn in
    check (option int) "set version" (Some 1) (Utilities.Components.get_version t);
    let t = Utilities.Components.clear_version t |> Utilities.Components.to_or_error |> ok_exn in
    check (option int) "cleared version" None (Utilities.Components.get_version t));

  test_case "list operations" `Quick (fun () ->
    let ts = [
      Utilities.Components.create ~name:"b" ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn;
      Utilities.Components.create ~name:"a" ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn;
      Utilities.Components.create ~name:"c" ~extra:1 () |> Utilities.Components.to_or_error |> ok_exn;
    ] in
    let sorted = Utilities.Components.to_list ts in
    check int "length" 3 (List.length sorted);
    check string "first" "a" (List.hd_exn sorted).name;
    check string "last" "c" (List.last_exn sorted).name);
]

let () =
  run "Utilities" [
    "Components", test_components;
  ] 