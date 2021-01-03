(* Global Variables *)
(* They are special kinds of variable name in Michelson smart contract. 
  They should not reappeared for other meanings in CFG, BasicPath, or any other contexts.
*)
(* Please do not modify this module in user-level customization. *)

val prefix_param : string
val prefix_storage : string
val prefix_amount : string
val prefix_balance : string
val prefix_sender : string
val prefix_source : string

val gen_param : int -> string
val gen_storage : int -> string
val gen_amount : int -> string
val gen_balance : int -> string
val gen_sender : int -> string
val gen_source : int -> string

val is_param_var : string -> bool
val is_storage_var : string -> bool
val is_amount_var : string -> bool
val is_balance_var : string -> bool
val is_sender_var : string -> bool
val is_source_var : string -> bool

module Env : sig
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

  val gen : int -> t
  val t_for_single_contract_verification : t

  module JsonRep : sig
    exception ParseErr of Yojson.Basic.t
  
    val of_t : t -> Yojson.Basic.t
    val to_t : Yojson.Basic.t -> t
  end (* module Env.JsonRep end *)

end (* module Env end *)
