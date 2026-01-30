open Core
open Async
open Complex_inputs_lib

let%expect_test "name_shrimp tool" =
  let server = Shrimp_server.create () in
  let args =
    `Assoc
      [
        ( "tank",
          `Assoc
            [
              ( "shrimp",
                `List
                  [
                    `Assoc [ ("name", `String "Bubba") ];
                    `Assoc [ ("name", `String "Gump") ];
                  ] );
            ] );
        ("extra_names", `List [ `String "Forest" ]);
      ]
  in
  let%bind result =
    Ox_fast_mcp_server.Server.Ox_fast_mcp.call_tool server ~name:"name_shrimp"
      ~arguments:args
  in
  match result with
  | `List names ->
    let names =
      List.map names ~f:(function
        | `String s -> s
        | _ -> "")
    in
    print_s [%sexp (names : string list)];
    [%expect {| (Bubba Gump Forest) |}];
    return ()
  | _ ->
    print_endline "Unexpected result format";
    return ()
