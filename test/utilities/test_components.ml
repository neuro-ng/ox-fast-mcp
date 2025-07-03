open Core
open Expect_test_helpers_core
open Utilities.Components

let%expect_test "create with defaults" =
  let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
  print_s [%sexp (t : unit t)];
  [%expect {|
    ((name test) (description ()) (tags ()) (enabled true) (key ()) (extra ())
     (version ())) |}]

let%expect_test "create with all fields" =
  let t = create
    ~name:"test"
    ~description:(Some "desc")
    ~tags:["a"; "b"]
    ~enabled:false
    ~key:(Some "key")
    ~version:(Some 1)
    ~extra:42
    ()
    |> to_or_error |> ok_exn
  in
  print_s [%sexp (t : int t)];
  [%expect {|
    ((name test) (description (desc)) (tags (a b)) (enabled false) (key (key))
     (extra 42) (version (1))) |}]

let%expect_test "create with invalid name" =
  let result = create ~name:"" ~extra:() () in
  print_s [%sexp (result : (unit t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_name "Name cannot be empty")) |}];
  let result = create ~name:"invalid name" ~extra:() () in
  print_s [%sexp (result : (unit t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_name "Name cannot contain whitespace")) |}]

let%expect_test "create with invalid tags" =
  let result = create ~name:"test" ~tags:[""] ~extra:() () in
  print_s [%sexp (result : (unit t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_tags (""))) |}];
  let result = create ~name:"test" ~tags:["invalid tag"] ~extra:() () in
  print_s [%sexp (result : (unit t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_tags ("invalid tag"))) |}]

let%expect_test "create with invalid version" =
  let result = create ~name:"test" ~version:(Some (-1)) ~extra:() () in
  print_s [%sexp (result : (unit t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_version "Version cannot be negative")) |}]

let%expect_test "get_key fallback" =
  let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
  print_s [%sexp (get_key t : string)];
  [%expect {| test |}];
  let t_with_key = with_key t "custom_key" in
  print_s [%sexp (get_key t_with_key : string)];
  [%expect {| custom_key |}]

let%expect_test "enable/disable" =
  let t = create ~name:"test" ~enabled:false ~extra:() () |> to_or_error |> ok_exn in
  print_s [%sexp (t.enabled : bool)];
  [%expect {| false |}];
  let t = enable t in
  print_s [%sexp (t.enabled : bool)];
  [%expect {| true |}];
  let t = disable t in
  print_s [%sexp (t.enabled : bool)];
  [%expect {| false |}]

let%expect_test "equal comparison" =
  let t1 = create ~name:"test" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:1 () |> to_or_error |> ok_exn in
  let t2 = create ~name:"test" ~description:(Some "desc") ~tags:["b"; "a"] ~extra:1 () |> to_or_error |> ok_exn in
  let t3 = create ~name:"test2" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:1 () |> to_or_error |> ok_exn in
  print_s [%sexp (equal t1 t2 : bool)];
  [%expect {| true |}];
  print_s [%sexp (equal t1 t3 : bool)];
  [%expect {| false |}]

let%expect_test "to_string" =
  let t = create ~name:"test" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:() () |> to_or_error |> ok_exn in
  print_s [%sexp (to_string t : string)];
  [%expect {| "FastMCPComponent(name=test, description=desc, tags=a, b, enabled=true)" |}]

let%expect_test "copy with validation" =
  let t = create ~name:"test" ~extra:1 () |> to_or_error |> ok_exn in
  let t2 = copy ~name:"test2" t |> to_or_error |> ok_exn in
  print_s [%sexp (t2 : int t)];
  [%expect {|
    ((name test2) (description ()) (tags ()) (enabled true) (key ()) (extra 1)
     (version ())) |}];
  let result = copy ~name:"" t in
  print_s [%sexp (result : (int t, Error.t) Result.t)];
  [%expect {| (Error (Invalid_name "Name cannot be empty")) |}]

let%expect_test "version operations" =
  let t = create ~name:"test" ~extra:() () |> to_or_error |> ok_exn in
  print_s [%sexp (get_version t : int option)];
  [%expect {| () |}];
  let t = set_version t 1 |> to_or_error |> ok_exn in
  print_s [%sexp (get_version t : int option)];
  [%expect {| (1) |}];
  let t = clear_version t |> to_or_error |> ok_exn in
  print_s [%sexp (get_version t : int option)];
  [%expect {| () |}]

let%expect_test "comparison functions" =
  let t1 = create ~name:"a" ~extra:1 () |> to_or_error |> ok_exn in
  let t2 = create ~name:"b" ~extra:1 () |> to_or_error |> ok_exn in
  let t3 = create ~name:"a" ~version:(Some 1) ~extra:1 () |> to_or_error |> ok_exn in
  let t4 = create ~name:"a" ~version:(Some 2) ~extra:1 () |> to_or_error |> ok_exn in
  print_s [%sexp (compare_by_name t1 t2 < 0 : bool)];
  [%expect {| true |}];
  print_s [%sexp (compare_by_version t3 t4 < 0 : bool)];
  [%expect {| true |}];
  print_s [%sexp (compare_by_key t1 t2 = 0 : bool)];
  [%expect {| true |}]

let%expect_test "list operations" =
  let ts = [
    create ~name:"b" ~extra:1 () |> to_or_error |> ok_exn;
    create ~name:"a" ~extra:1 () |> to_or_error |> ok_exn;
    create ~name:"c" ~extra:1 () |> to_or_error |> ok_exn;
  ] in
  let sorted = to_list ts in
  print_s [%sexp (List.length sorted : int)];
  [%expect {| 3 |}];
  print_s [%sexp ((List.hd_exn sorted).name : string)];
  [%expect {| a |}];
  print_s [%sexp ((List.last_exn sorted).name : string)];
  [%expect {| c |}]

let%expect_test "json serialization" =
  let t = create ~name:"test" ~description:(Some "desc") ~tags:["a"; "b"] ~extra:42 () |> to_or_error |> ok_exn in
  let json = to_yojson (fun x -> `Int x) t |> to_or_error |> ok_exn in
  print_s [%sexp (Yojson.Safe.to_string json : string)];
  [%expect {|
    "{\"name\":\"test\",\"description\":\"desc\",\"tags\":[\"a\",\"b\"],\"enabled\":true,\"_key\":null,\"extra\":42,\"version\":null}" |}];
  match of_yojson (fun j -> match j with `Int i -> Ok i | _ -> Error "expected int") json with
  | Ok t' -> print_s [%sexp (equal t t' : bool)];
    [%expect {| true |}]
  | Error msg -> print_s [%sexp (msg : string)];
    [%expect {| |}] 