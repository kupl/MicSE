(* Integration Tests *)

open Core
open OUnit2
open Lib

(******************************************************************************)
(******************************************************************************)
(* Testcases                                                                  *)
(******************************************************************************)
(******************************************************************************)

let test_parse : name:string -> arg:string array option -> test_fun =
  fun ~name ~arg _ ->
  let _ = ExecFlow.upto_parsing arg in
  assert_bool (name ^ " - parsing test") true
(* function test_parse end *)

let test_tzrep : name:string -> arg:string array option -> test_fun =
  fun ~name ~arg _ ->
  let _ = ExecFlow.upto_tz_rep arg in
  assert_bool (name ^ " - tz representation test") true
(* function test_tzrep end *)

let test_symex : name:string -> arg:string array option -> test_fun =
  fun ~name ~arg _ ->
  let _ = ExecFlow.upto_sym_exec arg in
  assert_bool (name ^ " - symbolic execution test") true
(* function test_symex end *)

let test_result :
    name:string -> arg:string array option -> result:int * int * int -> test_fun
    =
   let open Res in
   fun ~name ~arg ~result:(p, r, f) _ ->
   let (_, (res : res)) = ExecFlow.prover_adv_refuter_toss arg in
   let (qrc : qres_classified) = classify_qres res in
   assert_bool (name ^ " - result test")
     (p = QRSet.length qrc.qrc_p
     && r = QRSet.length qrc.qrc_r
     && f
        = QRSet.length qrc.qrc_uu
          + QRSet.length qrc.qrc_uf
          + QRSet.length qrc.qrc_fu
          + QRSet.length qrc.qrc_ff
     )
(* function test_result end *)

(******************************************************************************)
(******************************************************************************)
(* Test Suite Generation                                                      *)
(******************************************************************************)
(******************************************************************************)

let gen_basic_test :
    name:string ->
    file_path:string * string ->
    timeout:int ->
    result:int * int * int ->
    test =
  fun ~name ~file_path:(tz_file_path, strg_file_path) ~timeout ~result ->
  let (arg : string array option) =
     Some
       (Printf.sprintf "-I %s -S %s -Z 30 -T %d" tz_file_path strg_file_path
          timeout
       |> String.split ~on:' '
       |> Array.of_list
       )
  in
  let (testsuite : test) =
     name ^ " - integration test"
     >::: [
            "Parsing Test" >:: test_parse ~name ~arg;
            "Tz Representation Test" >:: test_tzrep ~name ~arg;
            "Symbolic Execution Test" >:: test_symex ~name ~arg;
            "Overall Result Test" >:: test_result ~name ~arg ~result;
          ]
  in
  testsuite
(* function gen_basic_test end *)

let gen_picked_test :
    name:string ->
    file_path:string * string ->
    timeout:int ->
    result:int * int * int ->
    query:int * int ->
    test =
  fun ~name ~file_path:(tz_file_path, strg_file_path) ~timeout ~result
      ~query:(l, c) ->
  let (arg : string array option) =
     Some
       (Printf.sprintf "-I %s -S %s -Z 30 -T %d -q %d %d" tz_file_path
          strg_file_path timeout l c
       |> String.split ~on:' '
       |> Array.of_list
       )
  in
  let (testsuite : test) =
     name ^ " - integration test"
     >::: [
            "Parsing Test" >:: test_parse ~name ~arg;
            "Tz Representation Test" >:: test_tzrep ~name ~arg;
            "Symbolic Execution Test" >:: test_symex ~name ~arg;
            "Overall Result Test" >:: test_result ~name ~arg ~result;
          ]
  in
  testsuite
(* function gen_picked_test end *)

(******************************************************************************)
(******************************************************************************)
(* Integration Test list                                                      *)
(******************************************************************************)
(******************************************************************************)

let test_list : unit -> OUnit2.test =
  fun () ->
  OUnit2.test_list
    [
      gen_basic_test ~name:"transfer_overall"
        ~file_path:
          ( "../benchmarks/examples/transfer.tz",
            "../benchmarks/examples/transfer.storage.tz"
          )
        ~timeout:180 ~result:(3, 0, 0);
      gen_picked_test ~name:"deposit_query_126_29"
        ~file_path:
          ( "../benchmarks/examples/deposit.tz",
            "../benchmarks/examples/deposit.storage.tz"
          )
        ~timeout:600 ~result:(1, 0, 0) ~query:(126, 29);
      gen_picked_test ~name:"deposit_query_344_22"
        ~file_path:
          ( "../benchmarks/examples/deposit.tz",
            "../benchmarks/examples/deposit.storage.tz"
          )
        ~timeout:300 ~result:(0, 1, 0) ~query:(344, 22);
    ]
(* function test_list end *)
