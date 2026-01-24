(** Tests for parameter name collisions between OpenAPI parameter locations.

    Translated from Python test_parameter_collisions.py to OCaml. Tests handling
    of parameter name collisions between path/query/header and body parameters.

    Note: Python tests use httpx.AsyncClient mocking to verify HTTP requests.
    These OCaml tests focus on the parameter routing and schema handling logic. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Types for Parameter Collision Handling
   ============================================================================= *)

type parameter_location = Path | Query | Header | Cookie | Body
[@@deriving sexp, equal]

let location_to_string = function
  | Path -> "path"
  | Query -> "query"
  | Header -> "header"
  | Cookie -> "cookie"
  | Body -> "body"

type parameter_info = {
  name : string;
  location : parameter_location;
  original_name : string;
  required : bool;
}
[@@deriving sexp]

(* =============================================================================
   Parameter Collision Detection
   ============================================================================= *)

(** Detect parameter name collisions between different locations *)
let detect_collisions (params : parameter_info list) :
    (string * parameter_location list) list =
  let grouped =
    List.fold params
      ~init:(Map.empty (module String))
      ~f:(fun acc p ->
        Map.update acc p.original_name ~f:(function
          | None -> [ p.location ]
          | Some locs -> p.location :: locs))
  in
  Map.to_alist grouped |> List.filter ~f:(fun (_, locs) -> List.length locs > 1)

(** Generate suffixed parameter name for collision resolution *)
let suffix_for_location = function
  | Path -> "__path"
  | Query -> "__query"
  | Header -> "__header"
  | Cookie -> "__cookie"
  | Body -> ""

(* Body parameters keep original name *)

(** Resolve parameter collisions by adding location suffixes *)
let resolve_collisions (params : parameter_info list) : parameter_info list =
  let collisions = detect_collisions params in
  let collision_names =
    List.map collisions ~f:fst |> Set.of_list (module String)
  in
  List.map params ~f:(fun p ->
      if
        Set.mem collision_names p.original_name
        && not (equal_parameter_location p.location Body)
      then { p with name = p.original_name ^ suffix_for_location p.location }
      else p)

(* =============================================================================
   Tests: Parameter Collision Detection
   ============================================================================= *)

let%expect_test "detect path-body collision" =
  let params =
    [
      { name = "id"; location = Path; original_name = "id"; required = true };
      { name = "id"; location = Body; original_name = "id"; required = true };
      {
        name = "name";
        location = Body;
        original_name = "name";
        required = true;
      };
    ]
  in
  let collisions = detect_collisions params in
  printf "collision_count: %d\n" (List.length collisions);
  List.iter collisions ~f:(fun (name, locs) ->
      printf "%s: [%s]\n" name
        (List.map locs ~f:location_to_string |> String.concat ~sep:", "));
  [%expect {|
    collision_count: 1
    id: [body, path]
    |}]

let%expect_test "detect query-body collision" =
  let params =
    [
      {
        name = "limit";
        location = Query;
        original_name = "limit";
        required = false;
      };
      {
        name = "limit";
        location = Body;
        original_name = "limit";
        required = false;
      };
      {
        name = "query";
        location = Body;
        original_name = "query";
        required = true;
      };
    ]
  in
  let collisions = detect_collisions params in
  printf "collision_count: %d\n" (List.length collisions);
  List.iter collisions ~f:(fun (name, _) -> printf "colliding: %s\n" name);
  [%expect {|
    collision_count: 1
    colliding: limit
    |}]

let%expect_test "no collisions detected" =
  let params =
    [
      {
        name = "user_id";
        location = Path;
        original_name = "user_id";
        required = true;
      };
      {
        name = "name";
        location = Body;
        original_name = "name";
        required = true;
      };
      {
        name = "email";
        location = Body;
        original_name = "email";
        required = false;
      };
    ]
  in
  let collisions = detect_collisions params in
  printf "collision_count: %d\n" (List.length collisions);
  [%expect {| collision_count: 0 |}]

(* =============================================================================
   Tests: Parameter Collision Resolution
   ============================================================================= *)

let%expect_test "resolve path-body collision with suffixing" =
  let params =
    [
      { name = "id"; location = Path; original_name = "id"; required = true };
      { name = "id"; location = Body; original_name = "id"; required = true };
      {
        name = "name";
        location = Body;
        original_name = "name";
        required = true;
      };
    ]
  in
  let resolved = resolve_collisions params in
  printf "resolved params:\n";
  List.iter resolved ~f:(fun p ->
      printf "  %s (%s)\n" p.name (location_to_string p.location));
  [%expect
    {|
    resolved params:
      id__path (path)
      id (body)
      name (body)
    |}]

let%expect_test "resolve query-body collision with suffixing" =
  let params =
    [
      {
        name = "limit";
        location = Query;
        original_name = "limit";
        required = false;
      };
      {
        name = "limit";
        location = Body;
        original_name = "limit";
        required = false;
      };
      {
        name = "query";
        location = Body;
        original_name = "query";
        required = true;
      };
    ]
  in
  let resolved = resolve_collisions params in
  printf "resolved params:\n";
  List.iter resolved ~f:(fun p ->
      printf "  %s (%s)\n" p.name (location_to_string p.location));
  [%expect
    {|
    resolved params:
      limit__query (query)
      limit (body)
      query (body)
    |}]

let%expect_test "no change when no collisions" =
  let params =
    [
      {
        name = "user_id";
        location = Path;
        original_name = "user_id";
        required = true;
      };
      {
        name = "name";
        location = Body;
        original_name = "name";
        required = true;
      };
    ]
  in
  let resolved = resolve_collisions params in
  printf "resolved params:\n";
  List.iter resolved ~f:(fun p ->
      printf "  %s (%s)\n" p.name (location_to_string p.location));
  [%expect
    {|
    resolved params:
      user_id (path)
      name (body)
    |}]

(* =============================================================================
   Parameter Routing Logic
   ============================================================================= *)

(** Route arguments to their appropriate destinations based on suffixed names *)
let route_arguments ~(params : parameter_info list)
    (arguments : (string * Yojson.Safe.t) list) :
    (string * Yojson.Safe.t) list
    * (string * Yojson.Safe.t) list
    * (string * Yojson.Safe.t) list =
  (* Returns: path_params, query_params, body *)
  let path_params = ref [] in
  let query_params = ref [] in
  let body_params = ref [] in
  List.iter arguments ~f:(fun (arg_name, value) ->
      (* Find matching parameter *)
      match List.find params ~f:(fun p -> String.equal p.name arg_name) with
      | Some p -> (
        match p.location with
        | Path -> path_params := (p.original_name, value) :: !path_params
        | Query -> query_params := (p.original_name, value) :: !query_params
        | Body -> body_params := (p.original_name, value) :: !body_params
        | _ -> ())
      | None ->
        (* Assume body if not found in params *)
        body_params := (arg_name, value) :: !body_params);
  (List.rev !path_params, List.rev !query_params, List.rev !body_params)

let%expect_test "route arguments with path-body collision" =
  let params =
    [
      {
        name = "id__path";
        location = Path;
        original_name = "id";
        required = true;
      };
      { name = "id"; location = Body; original_name = "id"; required = true };
      {
        name = "name";
        location = Body;
        original_name = "name";
        required = true;
      };
    ]
  in
  let arguments =
    [
      ("id__path", `Int 123);
      ("id", `Int 123);
      ("name", `String "John Doe");
      ("email", `String "john@example.com");
    ]
  in
  let path_params, query_params, body_params =
    route_arguments ~params arguments
  in
  printf "path params:\n";
  List.iter path_params ~f:(fun (k, v) ->
      printf "  %s=%s\n" k (Yojson.Safe.to_string v));
  printf "query params: %d\n" (List.length query_params);
  printf "body params:\n";
  List.iter body_params ~f:(fun (k, v) ->
      printf "  %s=%s\n" k (Yojson.Safe.to_string v));
  [%expect
    {|
    path params:
      id=123
    query params: 0
    body params:
      id=123
      name="John Doe"
      email="john@example.com"
    |}]

let%expect_test "route arguments with query-body collision" =
  let params =
    [
      {
        name = "limit__query";
        location = Query;
        original_name = "limit";
        required = false;
      };
      {
        name = "limit";
        location = Body;
        original_name = "limit";
        required = false;
      };
      {
        name = "query";
        location = Body;
        original_name = "query";
        required = true;
      };
    ]
  in
  let arguments =
    [ ("limit__query", `Int 5); ("limit", `Int 100); ("query", `String "john") ]
  in
  let path_params, query_params, body_params =
    route_arguments ~params arguments
  in
  printf "path params: %d\n" (List.length path_params);
  printf "query params:\n";
  List.iter query_params ~f:(fun (k, v) ->
      printf "  %s=%s\n" k (Yojson.Safe.to_string v));
  printf "body params:\n";
  List.iter body_params ~f:(fun (k, v) ->
      printf "  %s=%s\n" k (Yojson.Safe.to_string v));
  [%expect
    {|
    path params: 0
    query params:
      limit=5
    body params:
      limit=100
      query="john"
    |}]

(* =============================================================================
   Tests: URL Building with Path Parameters
   ============================================================================= *)

(** Replace path parameters in URL template *)
let build_url ~(url_template : string)
    (path_params : (string * Yojson.Safe.t) list) : string =
  List.fold path_params ~init:url_template ~f:(fun url (name, value) ->
      let value_str =
        match value with
        | `String s -> s
        | `Int i -> Int.to_string i
        | `Float f -> Float.to_string f
        | _ -> Yojson.Safe.to_string value
      in
      String.substr_replace_all url ~pattern:(sprintf "{%s}" name)
        ~with_:value_str)

let%expect_test "build URL with path parameters" =
  let url_template = "/users/{id}" in
  let path_params = [ ("id", `Int 123) ] in
  let url = build_url ~url_template path_params in
  printf "url: %s\n" url;
  [%expect {| url: /users/123 |}]

let%expect_test "build URL with resolved collision" =
  let params =
    [
      {
        name = "id__path";
        location = Path;
        original_name = "id";
        required = true;
      };
      { name = "id"; location = Body; original_name = "id"; required = true };
    ]
  in
  let arguments = [ ("id__path", `Int 123); ("id", `Int 456) ] in
  let path_params, _, _ = route_arguments ~params arguments in
  let url = build_url ~url_template:"/users/{id}" path_params in
  printf "url: %s\n" url;
  printf "path id: 123 (from id__path)\n";
  [%expect {|
    url: /users/123
    path id: 123 (from id__path)
    |}]
