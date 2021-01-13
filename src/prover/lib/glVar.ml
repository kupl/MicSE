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
  (* Custom construction of Env.t IS NOT ALLOWED. 
    It may harm some consistency, e.g. "Prover.VlGen.read_type_cfgvar" implementation.
    Use "gen" function instead.
  *)
  type t = {
    gv_num : int;
    gv_param : string;
    gv_storage : string;
    gv_amount : string;
    gv_balance : string;
    gv_sender : string;
    gv_source : string;
  }

  let gen : int -> t 
  =fun n -> {
    gv_num = n;
    gv_param = gen_param n;
    gv_storage = gen_storage n;
    gv_amount = gen_amount n;
    gv_balance = gen_balance n;
    gv_sender = gen_sender n;
    gv_source = gen_source n;
  }

  let t_for_single_contract_verification : t = gen 0

  module JsonRep = struct
    exception ParseErr of Yojson.Basic.t

    let of_t : t -> Yojson.Basic.t
    = fun t -> `Int t.gv_num
    let to_t : Yojson.Basic.t -> t
    = function
      | `Int n -> gen n
      | _ as js -> Stdlib.raise (ParseErr js)

  end (* module Env.JsonRep end *)

end (* module Env end *)
