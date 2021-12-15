(* Integration Tests *)

open Core
open OUnit2
open Lib

(******************************************************************************)
(******************************************************************************)
(* Testcases                                                                  *)
(******************************************************************************)
(******************************************************************************)

let test_parse :
    name:string ->
    arg:string array option ->
    target:string * (string array option -> Res.config * Res.res) ->
    test_fun =
  fun ~name ~arg ~target ctxt ->
  let _ =
     logf ctxt `Info "[%s / Integration Test] %s - parsing test: start\n"
       (fst target) name
  in
  let _ = ExecFlow.upto_parsing arg in
  let _ =
     logf ctxt `Info "[%s / Integration Test] %s - parsing test: done\n"
       (fst target) name
  in
  assert_bool (name ^ " - parsing test") true
(* function test_parse end *)

let test_tzrep :
    name:string ->
    arg:string array option ->
    target:string * (string array option -> Res.config * Res.res) ->
    test_fun =
  fun ~name ~arg ~target ctxt ->
  let _ =
     logf ctxt `Info
       "[%s / Integration Test] %s - tz representation test: start\n"
       (fst target) name
  in
  let _ = ExecFlow.upto_tz_rep arg in
  let _ =
     logf ctxt `Info
       "[%s / Integration Test] %s - tz representation test: done\n"
       (fst target) name
  in
  assert_bool (name ^ " - tz representation test") true
(* function test_tzrep end *)

let test_symex :
    name:string ->
    arg:string array option ->
    target:string * (string array option -> Res.config * Res.res) ->
    test_fun =
  fun ~name ~arg ~target ctxt ->
  let _ =
     logf ctxt `Info
       "[%s / Integration Test] %s - symbolic execution test: start\n"
       (fst target) name
  in
  let _ = ExecFlow.upto_sym_exec arg in
  let _ =
     logf ctxt `Info
       "[%s / Integration Test] %s - symbolic execution test: done\n"
       (fst target) name
  in
  assert_bool (name ^ " - symbolic execution test") true
(* function test_symex end *)

let test_result :
    name:string ->
    arg:string array option ->
    target:string * (string array option -> Res.config * Res.res) ->
    result:int * int * int ->
    test_fun =
   let open Res in
   fun ~name ~arg ~target ~result:(ep, er, ef) ctxt ->
   let _ =
      logf ctxt `Info
        "[%s / Integration Test] %s - result test: start (expectation: P=%d / R=%d / F=%d)\n"
        (fst target) name ep er ef
   in
   let (_, (res : res)) = (snd target) arg in
   let (qrc : qres_classified) = classify_qres res in
   let ((rp : int), (rr : int), (rf : int), (re : int)) =
      ( QRSet.length qrc.qrc_p,
        QRSet.length qrc.qrc_r,
        QRSet.length qrc.qrc_uu
        + QRSet.length qrc.qrc_uf
        + QRSet.length qrc.qrc_fu
        + QRSet.length qrc.qrc_ff,
        QRSet.length qrc.qrc_err
      )
   in
   let _ =
      logf ctxt `Info
        "[%s / Integration Test] %s - result test: done (result: P=%d / R=%d / F=%d / ERR=%d)\n"
        (fst target) name rp rr rf re
   in
   assert_bool (name ^ " - result test") (ep = rp && er = rr && ef = rf)
(* function test_result end *)

(******************************************************************************)
(******************************************************************************)
(* Test Suite Generation                                                      *)
(******************************************************************************)
(******************************************************************************)

let gen_basic_test :
    name:string ->
    target:string * (string array option -> Res.config * Res.res) ->
    file_path:string * string ->
    timeout:int ->
    result:int * int * int ->
    test =
  fun ~name ~target ~file_path:(tz_file_path, strg_file_path) ~timeout ~result ->
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
            "Parsing Test" >:: test_parse ~name ~arg ~target;
            "Tz Representation Test" >:: test_tzrep ~name ~arg ~target;
            "Symbolic Execution Test" >:: test_symex ~name ~arg ~target;
            "Overall Result Test" >:: test_result ~name ~arg ~target ~result;
          ]
  in
  testsuite
(* function gen_basic_test end *)

let gen_picked_test :
    name:string ->
    target:string * (string array option -> Res.config * Res.res) ->
    file_path:string * string ->
    timeout:int ->
    result:int * int * int ->
    query:int * int ->
    test =
  fun ~name ~target ~file_path:(tz_file_path, strg_file_path) ~timeout ~result
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
            "Parsing Test" >:: test_parse ~name ~arg ~target;
            "Tz Representation Test" >:: test_tzrep ~name ~arg ~target;
            "Symbolic Execution Test" >:: test_symex ~name ~arg ~target;
            "Overall Result Test" >:: test_result ~name ~arg ~target ~result;
          ]
  in
  testsuite
(* function gen_picked_test end *)

(******************************************************************************)
(******************************************************************************)
(* Integration Test list                                                      *)
(******************************************************************************)
(******************************************************************************)

let micse_test : unit -> OUnit2.test =
  fun () ->
  let (target : string * (string array option -> Res.config * Res.res)) =
     ("MicSE", ExecFlow.prover_adv_refuter_toss)
  in
  OUnit2.test_list
    [
      gen_basic_test ~name:"transfer_overall" ~target
        ~file_path:
          ( "../benchmarks/examples/transfer.tz",
            "../benchmarks/examples/transfer.storage.tz"
          )
        ~timeout:180 ~result:(3, 0, 0);
      gen_picked_test ~name:"deposit_query_126_29" ~target
        ~file_path:
          ( "../benchmarks/examples/deposit.tz",
            "../benchmarks/examples/deposit.storage.tz"
          )
        ~timeout:600 ~result:(1, 0, 0) ~query:(126, 29);
      gen_picked_test ~name:"deposit_query_344_22" ~target
        ~file_path:
          ( "../benchmarks/examples/deposit.tz",
            "../benchmarks/examples/deposit.storage.tz"
          )
        ~timeout:300 ~result:(0, 1, 0) ~query:(344, 22);
    ]
(* function micse_test end *)
