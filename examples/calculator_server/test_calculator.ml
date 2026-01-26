(* Placeholder test *)
let%expect_test "placeholder" = 
  print_endline "Calculator server builds.";
  [%expect {| Calculator server builds. |}]
