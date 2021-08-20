
let test_sample = Test_sample.test

let _ =
  (* 1. sample test *)
  let _ = OUnit2.run_test_tt_main test_sample in
  ()