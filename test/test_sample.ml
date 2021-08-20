(* Test for sample *)

open Core

module Sample = struct
  let hello : string -> string = (fun s -> s ^ "Hello")

  let world : string -> string = (fun s -> s ^ "world") (* Wrong! *)

  let wow : string -> string = (fun s -> s ^ "!!")
end

(******************************************************************************)
(******************************************************************************)
(* Tests                                                                      *)
(******************************************************************************)
(******************************************************************************)

module TestSuite1 = struct
  open OUnit2

  let hello : string = Sample.hello ""

  let helloworld : string = Sample.world hello

  let helloworldwow : string = Sample.wow helloworld

  let test1 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx
      ~printer:(fun s -> s)
      ~cmp:String.equal (Sample.world "Hello") helloworld

  let test2 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx
      ~printer:(fun s -> s)
      ~cmp:String.equal (Sample.wow "HelloWorld") helloworldwow

  let test3 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx ~printer:(fun s -> s) "HelloWorld!!" helloworldwow

  let testsuite : test =
     "sampleTestSuite1"
     >::: [ "test1" >:: test1; "test2" >:: test2; "test3" >:: test3 ]
end

module TestSuite2 = struct
  open OUnit2

  let test1 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx
      ~printer:(fun s -> s)
      ~cmp:(fun a b ->
        let fourth : string -> char = (fun s -> String.nget s 4) in
        Char.equal (fourth a) (fourth b))
      "hello" (* Wrong! *) (Sample.hello "")

  let test2 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx
      ~printer:(fun s -> s)
      ~cmp:(fun a b ->
        let split : string -> string list = String.split ~on:'W' in
        List.equal String.equal (split a) (split b))
      "HelloWorld" (Sample.world "Hello")

  let test3 : test_fun =
    fun ctx ->
    assert_equal ~ctxt:ctx
      ~printer:(fun s -> s)
      ~cmp:(fun a b ->
        let count : string -> int = String.count ~f:(Char.equal '!') in
        Int.equal (count a) (count b))
      "HelloWorld!!" (Sample.wow "HelloWorld")

  let testsuite : test =
     "sampleTestSuite2"
     >::: [ "test1" >:: test1; "test2" >:: test2; "test3" >:: test3 ]
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let test : OUnit2.test =
   OUnit2.test_list [ TestSuite1.testsuite; TestSuite2.testsuite ]
