(* Tests *)

let _ =
   (* 1. Integration test *)
   let _ =
      OUnit2.run_test_tt_main (Integ_test.micse_test ()) ~exit:(fun _ -> ())
   in
   ()
