open! Core
open Lwt.Syntax
open Server_auth_providers.Debug

[@@@alert "-unsafe_multidomain"]

let%expect_test "test default validator allows all" =
  Lwt_main.run
    (let module Provider = (val Debug_provider.create ()) in
    let* token_opt = Provider.load_access_token "any-token" in
    match token_opt with
    | Some token ->
      print_s [%sexp (token.token : string)];
      print_s [%sexp (token.client_id : string)];
      Lwt.return ()
    | None ->
      print_endline "Unexpected rejection";
      Lwt.return ());
  [%expect {|
    any-token
    debug-client
    |}]

let%expect_test "test custom validator" =
  Lwt_main.run
    (let validate token = Lwt.return (String.equal token "secret-token") in
     let module Provider =
       (val Debug_provider.create ~validate ~client_id:"custom-client" ())
     in
     (* Valid token *)
     let* valid_opt = Provider.load_access_token "secret-token" in
     (match valid_opt with
     | Some token ->
       print_s [%sexp (token.token : string)];
       print_s [%sexp (token.client_id : string)]
     | None -> print_endline "Unexpected rejection");

     (* Invalid token *)
     let* invalid_opt = Provider.load_access_token "wrong-token" in
     (match invalid_opt with
     | Some _ -> print_endline "Unexpected acceptance"
     | None -> print_endline "Correctly rejected");
     Lwt.return ());
  [%expect {|
    secret-token
    custom-client
    Correctly rejected
    |}]

let%expect_test "test empty token rejection" =
  Lwt_main.run
    (let module Provider = (val Debug_provider.create ()) in
    let* token_opt1 = Provider.load_access_token "" in
    (match token_opt1 with
    | Some _ -> print_endline "Unexpected acceptance"
    | None -> print_endline "Rejected empty token");

    let* token_opt2 = Provider.load_access_token "   " in
    (match token_opt2 with
    | Some _ -> print_endline "Unexpected acceptance"
    | None -> print_endline "Rejected whitespace token");
    Lwt.return ());
  [%expect {| 
    Rejected empty token
    Rejected whitespace token 
  |}]

let%expect_test "test validator exception handling" =
  Lwt_main.run
    (let validate _ = Lwt.fail (Failure "Boom") in
     let module Provider = (val Debug_provider.create ~validate ()) in
     let* token_opt = Provider.load_access_token "token" in
     (match token_opt with
     | Some _ -> print_endline "Unexpected acceptance"
     | None -> print_endline "Handled exception");
     Lwt.return ());
  [%expect {| Handled exception |}]
