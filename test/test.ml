let test_sample = Test_sample.test

let _ =
   (* 1. sample test *)
   (* let _ = OUnit2.run_test_tt_main test_sample in *)
   (* 2. test_upto_se.ml *)
   let _ = OUnit2.run_test_tt_main Test_upto_se.test in
   (* 3. test_naive_refute.ml *)
   let _ = OUnit2.run_test_tt_main Test_naive_refute.test in
   ()
