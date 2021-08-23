open Core

(******************************************************************************)
(******************************************************************************)
(* Template Argument, Test-Suite, and Template-Functor                        *)
(******************************************************************************)
(******************************************************************************)

module type ARG = sig
  val tz_file_name : string

  val strg_file_name : string

  val se_bpnum : int
end
(* module type ARG end *)

module type TS = sig
  val argv_opt : string array option

  val tf_parse_success : OUnit2.test_fun

  val tf_tzrep_success : OUnit2.test_fun

  val tf_se_bpnum_eq : OUnit2.test_fun

  val testsuite : OUnit2.test
end
(* module type TS end *)

module MakeTS (A : ARG) : TS = struct
  open OUnit2

  let argv_opt =
     Some
       ("-I " ^ A.tz_file_name ^ " -S " ^ A.strg_file_name
       |> String.split ~on:' '
       |> Array.of_list
       )

  let tf_parse_success _ =
     assert_bool
       (A.tz_file_name ^ " - ts_parse_success")
       ( Lib.ExecFlow.upto_parsing argv_opt |> ignore;
         true
       )

  let tf_tzrep_success _ =
     assert_bool
       (A.tz_file_name ^ " - ts_tzrep_success")
       ( Lib.ExecFlow.upto_tz_rep argv_opt |> ignore;
         true
       )

  let tf_se_bpnum_eq ctx =
     assert_equal ~ctxt:ctx ~printer:string_of_int ~cmp:Int.equal A.se_bpnum
       (let r = Lib.ExecFlow.upto_sym_exec argv_opt in
        Lib.Se.SSet.length r.sr_blocked
       )

  let testsuite =
     A.tz_file_name ^ " - testsuite"
     >::: [
            "tf_parse_success" >:: tf_parse_success;
            "tf_tzrep_success" >:: tf_tzrep_success;
            "tf_se_bpnum_eq" >:: tf_se_bpnum_eq;
          ]
end
(* functor MakeTS end *)

(******************************************************************************)
(******************************************************************************)
(* Template Argument, Test-Suite, and Template-Functor                        *)
(******************************************************************************)
(******************************************************************************)

module A_condition_insts : ARG = struct
  let tz_file_name = "./testcases/condition_insts.tz"

  let strg_file_name = "./testcases/condition_insts.storage.tz"

  let se_bpnum = 8
end

module A_loop_insts : ARG = struct
  let tz_file_name = "./testcases/loop_insts.tz"

  let strg_file_name = "./testcases/loop_insts.storage.tz"

  let se_bpnum = 6
end

let test : OUnit2.test =
   let module TS_1 = MakeTS (A_condition_insts) in
   let module TS_2 = MakeTS (A_loop_insts) in
   OUnit2.test_list [ TS_1.testsuite; TS_2.testsuite ]
