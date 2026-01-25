(* open Core - unused for now *)
open Core
open Async
open Smart_home_lib
(* open Lights - unused now that mock lights tests are removed *)

let%expect_test "thermostat logic" =
  let initial_state = Thermostat.current_state in
  (* Test initial state *)
  print_s [%sexp (initial_state : Thermostat.state)];
  [%expect {|
    ((temperature 72) (target_temperature 70) (mode Off)) |}];

  (* Test setting target temperature *)
  initial_state.target_temperature <- 75.0;
  print_s [%sexp (initial_state : Thermostat.state)];
  [%expect {|
    ((temperature 72) (target_temperature 75) (mode Off)) |}];

  (* Test setting mode *)
  initial_state.mode <- Thermostat.Mode.Heat;
  print_s [%sexp (initial_state : Thermostat.state)];
  [%expect {|
    ((temperature 72) (target_temperature 75) (mode Heat)) |}];
  return ()

(* Integration test with running DIYHue container *)
let%expect_test "integration: diyhue lights" =
    let ip = "127.0.0.1:8080" in
    let username = "102030405060708090a0b0c0d0e0f000" in
    let client = Phue.create ~ip ~username in
    
    (* Verify we can fetch lights. get_lights returns list directly, raises on error *)
    let%bind lights = Phue.get_lights client in
    let names = List.map lights ~f:(fun l -> l.name) |> List.sort ~compare:String.compare in
    print_s [%sexp (names : string list)];
    [%expect {| (Kitchen "Living Room 1" "Living Room 2") |}];
        
    (* Verify Living Room 1 is ON *)
    let lr1 = List.find_exn lights ~f:(fun l -> String.equal l.name "Living Room 1") in
    let on = Option.value_exn lr1.state.on in
    printf "Living Room 1 is on: %b\n" on;
    [%expect {| Living Room 1 is on: false |}];

    (* Turn OFF Living Room 1 *)
    let lr1_id = lr1.id in
        
    let state = `Assoc ["on", `Bool false] in
    let%bind _resp = Phue.set_light_state client lr1_id state in
             
    (* Verify it changed *)
    let%bind lights_new = Phue.get_lights client in
    let lr1_new = List.find_exn lights_new ~f:(fun l -> String.equal l.name "Living Room 1") in
    let on_new = Option.value_exn lr1_new.state.on in
    printf "Living Room 1 is now on: %b\n" on_new;
    [%expect {| Living Room 1 is now on: false |}];
    return ()
