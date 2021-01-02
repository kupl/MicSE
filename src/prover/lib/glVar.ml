(* Global Variables *)

let prefix_param : string = "-trxParam-"
let prefix_storage : string = "-trxStorage-"
let prefix_amount : string = "-trxAmount-"
let prefix_balance : string = "-trxBalance-"
let prefix_sender : string = "-trxSender-"
let prefix_source : string = "-trxSource-"

let gen_param : int -> string = fun n -> prefix_param ^ (Stdlib.string_of_int n)
let gen_storage : int -> string = fun n -> prefix_storage ^ (Stdlib.string_of_int n)
let gen_amount : int -> string = fun n -> prefix_amount ^ (Stdlib.string_of_int n)
let gen_balance : int -> string = fun n -> prefix_balance ^ (Stdlib.string_of_int n)
let gen_sender : int -> string = fun n -> prefix_sender ^ (Stdlib.string_of_int n)
let gen_source : int -> string = fun n -> prefix_source ^ (Stdlib.string_of_int n)

let is_param_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_param
let is_storage_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_storage
let is_amount_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_amount
let is_balance_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_balance
let is_sender_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_sender
let is_source_var : string -> bool = fun s -> Core.String.is_prefix s ~prefix:prefix_source

module Env = struct
  type t = {
    gv_param : string;
    gv_storage : string;
    gv_amount : string;
    gv_balance : string;
    gv_sender : string;
    gv_source : string;
  }

  let gen : int -> t 
  =fun n -> {
    gv_param = gen_param n;
    gv_storage = gen_storage n;
    gv_amount = gen_amount n;
    gv_balance = gen_amount n;
    gv_sender = gen_sender n;
    gv_source = gen_source n;
  }

  module JsonRep = struct
    exception ParseErr of Yojson.Basic.t

    module Const = struct
      let fname_gv_param    : string = "gv_param"
      let fname_gv_storage  : string = "gv_storage"
      let fname_gv_amount   : string = "gv_amount"
      let fname_gv_balance  : string = "gv_balance"
      let fname_gv_sender   : string = "gv_sender"
      let fname_gv_source   : string = "gv_source"
    end (* module Env.JsonRep.Const end *)
  
    let of_t : t -> Yojson.Basic.t
    = let open Const in
      fun t -> `Assoc [
        fname_gv_param, `String t.gv_param;
        fname_gv_storage, `String t.gv_storage;
        fname_gv_amount, `String t.gv_amount;
        fname_gv_balance, `String t.gv_balance;
        fname_gv_sender, `String t.gv_sender;
        fname_gv_source, `String t.gv_source;
      ]

    let to_t : Yojson.Basic.t -> t
    = let open Const in
      fun js ->
      let assert_nonnull = function | `Null -> Stdlib.raise (ParseErr js) | _ as json_i -> json_i in
      let extract_strfld = fun fname -> Yojson.Basic.Util.member fname js |> assert_nonnull |> Yojson.Basic.Util.to_string in
      { gv_param=(extract_strfld fname_gv_param);
        gv_storage=(extract_strfld fname_gv_storage);
        gv_amount=(extract_strfld fname_gv_amount);
        gv_balance=(extract_strfld fname_gv_balance);
        gv_sender=(extract_strfld fname_gv_sender);
        gv_source=(extract_strfld fname_gv_source);
      }

  end (* module Env.JsonRep end *)

end (* module Env end *)
