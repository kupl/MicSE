(* Json Constant - PLEASE DO NOT MAKE MLI FILE for this module *)


(*****************************************************************************)
(*****************************************************************************)
(* Variable Names in "Se.run_contract_in_fog"                                *)
(*****************************************************************************)
(*****************************************************************************)

(* "Rcfv" is a acronym of "Run Contract in Fog Variable" *)
module Rcfv = struct
  (* 0. address *)
  let address = "vaddr"
  (* 1. blockchain *)
  let storage = "vstorage"
  let balance = "vbalance"
  let delegate = "vdelegate"
  let chainid = "vchainid"
  let lastblocktime = "vlastblocktime"
  (* 2. transfer-token operation *)
  let tt_amount = "vamount"
  let tt_source = "vsource"
  let tt_param = "vparam"
  let optt_sender = "vsender"
end (* module Rcfv end *)


(*****************************************************************************)
(*****************************************************************************)
(* Structured Variable Name (for state merging)                              *)
(*****************************************************************************)
(*****************************************************************************)

(* structured variable name contains trasaction number & loop number, which is useful when renaming duplicated variable names from "state_set". *)
(* Current Design: (stvn_vn ^ _delim ^ trx_n ^ _delim ^ loop_n) *)

module Stvn : sig
  type t = {
    stvn_vn : string;
    trx_n : int option;
    loop_n : int option;
  }
  val of_string : string -> t
  val to_string : t -> string
  (* String Utilities *)
  val set_trx_n : int option -> string -> string
  val set_loop_n : int option -> string -> string
  val set_both_n : (int option * int option) -> string -> string
end 
= struct
  type t = {
    stvn_vn : string;
    trx_n : int option;
    loop_n : int option;
  }

  let _delim = '^'
  let _soio = (function | None -> "" | Some i -> string_of_int i)

  let of_string : string -> t = 
    let cstr : (string * string * string) -> t = fun (stvn, trxn, loopn) -> 
      {stvn_vn=stvn; trx_n=(Stdlib.int_of_string_opt trxn); loop_n=(Stdlib.int_of_string_opt loopn);} 
    in
    fun s -> begin
    let sl = String.split_on_char _delim s in
    match List.length sl with 
    | 1 -> cstr (List.nth sl 0, "", "")
    | 2 -> cstr (List.nth sl 0, List.nth sl 1, "")
    | 3 -> cstr (List.nth sl 0, List.nth sl 1, List.nth sl 2)
    | n -> Stdlib.failwith ("Length=" ^ (string_of_int n) ^ " -- " ^ Stdlib.__LOC__)
  end (* function of_string end *)
  let to_string : t -> string = 
    fun t -> begin
    match (t.trx_n, t.loop_n) with
    | None, None -> t.stvn_vn
    | None, Some _ -> t.stvn_vn ^ (Char.escaped _delim) ^ (_soio t.trx_n) ^ (Char.escaped _delim) ^ (_soio t.loop_n)
    | Some _, None -> t.stvn_vn ^ (Char.escaped _delim) ^ (_soio t.trx_n)
    | Some _, Some _ -> t.stvn_vn ^ (Char.escaped _delim) ^ (_soio t.trx_n) ^ (Char.escaped _delim) ^ (_soio t.loop_n)
  end (* function to_string end *)
  (* String Utilities *)
  let set_trx_n : int option -> string -> string
  = (fun nopt s -> of_string s |> (fun r -> {r with trx_n=nopt}) |> to_string)
  let set_loop_n : int option -> string -> string
  = (fun nopt s -> of_string s |> (fun r -> {r with loop_n=nopt}) |> to_string)
  let set_both_n : (int option * int option) -> string -> string
  = (fun (topt, lopt) s -> of_string s |> (fun r -> {r with trx_n=topt; loop_n=lopt}) |> to_string)
end (* module Stvn end *)


(*****************************************************************************)
(*****************************************************************************)
(* Location Variable Name in Value Stack                                     *)
(*****************************************************************************)
(*****************************************************************************)

module Locvn : sig
  type t = {
    loc: int;
    acc_l: string list;
  }
  val of_string : string -> t
  val to_string : t -> string
  val for_strg : string
end
= struct
  type t = {
    loc: int;
    acc_l: string list;
  }

  let _prefix = "vstack"
  let _delim = '|'
  let _inner_delim = '.'

  let of_string : string -> t =
    (* function of_string start *)
    fun s -> begin
    let sl : string list = Core.String.split s ~on:_delim in
    match sl with
    | "vstack"::loc::acc::[] -> {
      loc=(loc |> Stdlib.int_of_string_opt |> (function Some ii -> ii | None -> Stdlib.failwith ("Location information is invalid")));
      acc_l=(Core.String.split acc ~on:_inner_delim); }
    | _ -> Stdlib.failwith ("Invalid name of variable")
  end (* function of_string end *)

  let to_string : t -> string =
    (* function to_string start *)
    fun t -> begin
    _prefix ^ (Core.Char.escaped _delim) ^ (string_of_int t.loc) ^ (Core.Char.escaped _delim) ^ (Core.String.concat t.acc_l ~sep:(Core.Char.escaped _inner_delim))
  end (* function to_string end *)

  let for_strg : string = to_string { loc=0; acc_l=[("strg")] }
end


(*****************************************************************************)
(*****************************************************************************)
(* Variable name for MF_sigma_*                                              *)
(*****************************************************************************)
(*****************************************************************************)

module Fsvn : sig
  type t = {
    typ: [`Elem | `Remain];
    c_vn: string;
    c_acc_l: string list;
    e_acc_l: string list;
  }
  val of_string : string -> t
  val to_string : t -> string
end
= struct
  type t = {
    typ: [`Elem | `Remain];
    c_vn: string;
    c_acc_l: string list;
    e_acc_l: string list;
  }
  let _elem_prefix = "e"
  let _remain_prefix = "r"

  let _delim = '/'
  let _inner_delim = '.'

  let of_string : string -> t =
    (* function of_string start *)
    fun s -> begin
    let sl : string list = Core.String.split s ~on:_delim in
    match sl with
    | "e"::vn::c_acc::e_acc::[] -> {
      typ=`Elem;
      c_vn=vn;
      c_acc_l=(Core.String.split c_acc ~on:_inner_delim);
      e_acc_l=(Core.String.split e_acc ~on:_inner_delim); }
    | "r"::vn::c_acc::e_acc::[] -> {
      typ=`Remain;
      c_vn=vn;
      c_acc_l=(Core.String.split c_acc ~on:_inner_delim);
      e_acc_l=(Core.String.split e_acc ~on:_inner_delim); }
    | _ -> Stdlib.failwith ("Invalid name of variable")
  end (* function of_string end *)

  let to_string : t -> string =
    let name_without_prefix : t -> string =
      (* function name_without_prefix start *)
      fun t -> begin
      t.c_vn ^ (Core.Char.escaped _delim)
      ^ (Core.String.concat t.c_acc_l ~sep:(Core.Char.escaped _inner_delim)) ^ (Core.Char.escaped _delim) 
      ^ (Core.String.concat t.e_acc_l ~sep:(Core.Char.escaped _inner_delim))
    end in (* function name_without_prefix end *)
    (* function to_string start *)
    fun t -> begin
    match t.typ with
    | `Elem -> _elem_prefix ^ (Core.Char.escaped _delim) ^ (name_without_prefix t)
    | `Remain -> _remain_prefix ^ (Core.Char.escaped _delim) ^ (name_without_prefix t)
  end (* function to_string end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Code Component                                                            *)
(*****************************************************************************)
(*****************************************************************************)

let cc_l_unk = "U"
let cc_l_pos = "P"
let cc_a_typ = ":"
let cc_a_var = "@"
let cc_a_fld = "%"
let cc_loc = "Loc"
let cc_anl = "Ann"
let cc_val = "value"


(*****************************************************************************)
(*****************************************************************************)
(* Tezos Types                                                               *)
(*****************************************************************************)
(*****************************************************************************)

let t_key = "key"
let t_unit = "unit"
let t_signature = "signature"
let t_option = "option"
let t_list = "list"
let t_set = "set"
let t_operation = "operation"
let t_contract = "contract"
let t_pair = "pair"
let t_or = "or"
let t_lambda = "lambda"
let t_map = "map"
let t_big_map = "big_map"
let t_chain_id = "chain_id"
let t_int = "int"
let t_nat = "nat"
let t_string = "string"
let t_bytes = "bytes"
let t_mutez = "mutez"
let t_bool = "bool"
let t_key_hash = "key_hash"
let t_timestamp = "timestamp"
let t_address = "address"


(*****************************************************************************)
(*****************************************************************************)
(* Michelson Values                                                          *)
(*****************************************************************************)
(*****************************************************************************)


(*************************************************************************)
(* Symbol & Polymorphic                                                  *)
(*************************************************************************)

let v_symbol = "var"
let v_car = "car"
let v_cdr = "cdr"
let v_unlift_option = "ul_option"
let v_unlift_left = "ul_left"
let v_unlift_right = "ul_right"
let v_hd_l = "hd_l"
let v_elt = "elt"


(*************************************************************************)
(* Integer                                                               *)
(*************************************************************************)

let v_lit_int = "lit_int"
let v_neg_ni = "neg_ni"
let v_neg_ii = "neg_ii"
let v_not_ni = "not_ni"
let v_not_ii = "not_ii"
let v_add_nii = "add_nii"
let v_add_ini = "add_ini"
let v_add_iii = "add_iii"
let v_sub_nni = "sub_nni"
let v_sub_nii = "sub_nii"
let v_sub_ini = "sub_ini"
let v_sub_iii = "sub_iii"
let v_sub_tti = "sub_tti"
let v_mul_nii = "mul_nii"
let v_mul_ini = "mul_ini"
let v_mul_iii = "mul_iii"
let v_compare = "compare"
let v_int_of_nat = "int_of_nat"


(*************************************************************************)
(* Natural Number                                                        *)
(*************************************************************************)

let v_lit_nat = "lit_nat"
let v_abs_in = "abs_in"
let v_add_nnn = "add_nnn"
let v_mul_nnn = "mul_nnn"
let v_shiftL_nnn = "shiftL_nnn"
let v_shiftR_nnn = "shiftR_nnn"
let v_and_nnn = "and_nnn"
let v_and_inn = "and_inn"
let v_or_nnn = "or_nnn"
let v_xor_nnn = "xor_nnn"
let v_size_s = "size_s"
let v_size_m = "size_m"
let v_size_l = "size_l"
let v_size_str = "size_str"
let v_size_b = "size_b"


(*************************************************************************)
(* String                                                                *)
(*************************************************************************)

let v_lit_string = "lit_string"
let v_concat_sss = "concat_sss"
let v_concat_list_s = "concat_list_s"


(*************************************************************************)
(* Bytes                                                                 *)
(*************************************************************************)

let v_lit_bytes = "lit_bytes"
let v_concat_bbb = "concat_bbb"
let v_concat_list_b = "concat_list_b"
let v_pack = "pack"
let v_blake2b = "blake2b"
let v_sha256 = "sha256"
let v_sha512 = "sha512"


(*************************************************************************)
(* Mutez                                                                 *)
(*************************************************************************)

let v_lit_mutez = "lit_mutez"
let v_add_mmm = "add_mmm"
let v_sub_mmm = "sub_mmm"
let v_mul_mnm = "mul_mnm"
let v_mul_nmm = "mul_nmm"


(*************************************************************************)
(* Bool                                                                  *)
(*************************************************************************)

let v_lit_bool = "lit_bool"
let v_not_bb = "not_bb"
let v_and_bbb = "and_bbb"
let v_or_bbb = "or_bbb"
let v_xor_bbb = "xor_bbb"
let v_eq_ib = "eq_ib"
let v_neq_ib = "neq_ib"
let v_lt_ib = "lt_ib"
let v_gt_ib = "gt_ib"
let v_leq_ib = "leq_ib"
let v_geq_ib = "geq_ib"
let v_mem_xsb = "mem_xsb"
let v_mem_xmb = "mem_xmb"
let v_mem_xbmb = "mem_xbmb"
let v_check_signature = "check_signature"


(*************************************************************************)
(* Key Hash                                                              *)
(*************************************************************************)

let v_lit_key_hash = "lit_key_hash"
let v_hash_key = "hash_key"


(*************************************************************************)
(* Timestamp                                                             *)
(*************************************************************************)

let v_lit_timestamp_str = "lit_timestamp_str"
let v_lit_timestamp_sec = "lit_timestamp_sec"
let v_add_tit = "add_tit"
let v_add_itt = "add_itt"
let v_sub_tit = "sub_tit"


(*************************************************************************)
(* Address                                                               *)
(*************************************************************************)

let v_lit_address = "lit_address"
let v_address_of_contract = "address_of_contract"


(*************************************************************************)
(* Key                                                                   *)
(*************************************************************************)

let v_lit_key = "lit_key"


(*************************************************************************)
(* Unit                                                                  *)
(*************************************************************************)

let v_unit = "()"


(*************************************************************************)
(* Signature                                                             *)
(*************************************************************************)

let v_lit_signature_str = "lit_signature_str"
let v_lit_signature_signed = "lit_signature_signed"


(*************************************************************************)
(* Option                                                                *)
(*************************************************************************)

let v_some = "some"
let v_none = "none"
let v_ediv_nnnn = "ediv_nnnn"
let v_ediv_niin = "ediv_niin"
let v_ediv_inin = "ediv_inin"
let v_ediv_iiin = "ediv_iiin"
let v_ediv_mnmm = "ediv_mnmm"
let v_ediv_mmnm = "ediv_mmnm"
let v_get_xmoy = "get_xmoy"
let v_get_xbmo = "get_xbmo"
let v_slice_nnso = "slice_nnso"
let v_slice_nnbo = "slice_nnbo"
let v_unpack = "unpack"
let v_contract_of_address = "contract_of_address"
let v_isnat = "isnat"


(*************************************************************************)
(* List                                                                  *)
(*************************************************************************)

let v_lit_list = "lit_list"
let v_nil = "nil"
let v_cons = "cons"
let v_tl_l = "tl_l"


(*************************************************************************)
(* Set                                                                   *)
(*************************************************************************)

let v_lit_set = "lit_set"
let v_empty_set = "empty_set"
let v_update_xbss = "update_xbss"


(*************************************************************************)
(* Operation                                                             *)
(*************************************************************************)

let v_create_contract = "create_contract"
let v_transfer_tokens = "transfer_tokens"
let v_set_delegate = "set_delegate"


(*************************************************************************)
(* Contract                                                              *)
(*************************************************************************)

let v_lit_contract = "lit_contract"
let v_self = "self"
let v_implicit_account = "implicit_account"


(*************************************************************************)
(* Pair                                                                  *)
(*************************************************************************)

let v_pair = "pair"


(*************************************************************************)
(* Or                                                                    *)
(*************************************************************************)

let v_left = "left"
let v_right = "right"


(*************************************************************************)
(* Lambda                                                                *)
(*************************************************************************)

let v_lit_lambda = "lit_lambda"
let v_lambda_unknown = "lambda_unknown"
let v_lambda_closure = "lambda_closure"


(*************************************************************************)
(* Map                                                                   *)
(*************************************************************************)

let v_lit_map = "lit_map"
let v_empty_map = "empty_map"
let v_update_xomm = "update_xomm"


(*************************************************************************)
(* Big Map                                                               *)
(*************************************************************************)

let v_lit_big_map = "lit_big_map"
let v_empty_big_map = "empty_big_map"
let v_update_xobmbm = "update_xobmbm"


(*************************************************************************)
(* Chain Id                                                              *)
(*************************************************************************)

let v_lit_chain_id = "lit_chain_id"


(*************************************************************************)
(* Custom Domain Value for Invariant Synthesis                           *)
(*************************************************************************)

let v_sigma_lm = "sigma_lm"


(*****************************************************************************)
(*****************************************************************************)
(* Michelson instruction                                                     *)
(*****************************************************************************)
(*****************************************************************************)


let i_seq = "seq"
let i_drop = "drop"
let i_dup = "dup"
let i_swap = "swap"
let i_dig = "dig"
let i_dug = "dug"
let i_push = "push"
let i_some = "some"
let i_none = "none"
let i_unit = "unit"
let i_if_none = "if_none"
let i_pair = "pair"
let i_car = "car"
let i_cdr = "cdr"
let i_left = "left"
let i_right = "right"
let i_if_left = "if_left"
let i_nil = "nil"
let i_cons = "cons"
let i_if_cons = "if_cons"
let i_size = "size"
let i_empty_set = "empty_set"
let i_empty_map = "empty_map"
let i_empty_big_map = "empty_big_map"
let i_map = "map"
let i_iter = "iter"
let i_mem = "mem"
let i_get = "get"
let i_update = "update"
let i_if = "if"
let i_loop = "loop"
let i_loop_left = "loop_left"
let i_lambda = "lambda"
let i_exec = "exec"
let i_dip_n = "dip_n"
let i_failwith = "failwith"
let i_cast = "cast"
let i_rename = "rename"
let i_concat = "concat"
let i_slice = "slice"
let i_pack = "pack"
let i_unpack = "unpack"
let i_add = "add"
let i_sub = "sub"
let i_mul = "mul"
let i_ediv = "ediv"
let i_abs = "abs"
let i_isnat = "isnat"
let i_int = "int"
let i_neg = "neg"
let i_lsl = "lsl"
let i_lsr = "lsr"
let i_or = "or"
let i_and = "and"
let i_xor = "xor"
let i_not = "not"
let i_compare = "compare"
let i_eq = "eq"
let i_neq = "neq"
let i_lt = "lt"
let i_gt = "gt"
let i_le = "le"
let i_ge = "ge"
let i_self = "self"
let i_contract = "contract"
let i_transfer_tokens = "transfer_tokens"
let i_set_delegate = "set_delegate"
let i_create_account = "create_account"
let i_create_contract = "create_contract"
let i_implicit_account = "implicit_account"
let i_now = "now"
let i_amount = "amount"
let i_balance = "balance"
let i_check_signature = "check_signature"
let i_blake2b = "blake2b"
let i_sha256 = "sha256"
let i_sha512 = "sha512"
let i_hash_key = "hash_key"
let i_steps_to_quota = "steps_to_quota"
let i_source = "source"
let i_sender = "sender"
let i_address = "address"
let i_chain_id = "chain_id"
let i_unpair = "unpair"
let i_micse_check = "micse_check"


(*****************************************************************************)
(*****************************************************************************)
(* Formula                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let f_true = "true"
let f_false = "false"
let f_not = "not"
let f_and = "and"
let f_or = "or"
let f_eq = "eq"
let f_imply = "imply"
let f_is_true = "is_true"
let f_is_none = "is_none"
let f_is_left = "is_left"
let f_is_cons = "is_cons"
let f_add_mmm_no_overflow = "add_mmm_no_overflow"
let f_sub_mmm_no_underflow = "sub_mmm_no_underflow"
let f_mul_mnm_no_overflow = "mul_mnm_no_overflow"
let f_mul_nmm_no_overflow = "mul_nmm_no_overflow"
let f_shiftL_nnn_rhs_in_256 = "shiftL_nnn_rhs_in_256"
let f_shiftR_nnn_rhs_in_256 = "shiftR_nnn_rhs_in_256"
let f_sigma_equal = "sigma_equal"


(*****************************************************************************)
(*****************************************************************************)
(* Blockchain, Operation                                                     *)
(*****************************************************************************)
(*****************************************************************************)

let jc_bc_storage = "bc_storage"
let jc_bc_code = "bc_code"
let jc_bc_balance = "bc_balance"
let jc_bc_delegate = "bc_delegate"
let jc_bc_chain_id = "bc_chain_id"
let jc_bc_last_blocktime = "bc_last_blocktime"

let exop_transfer_token = "exop_transfer_token"

let jc_optt_addr = "optt_addr"
let jc_optt_source = "optt_source"
let jc_optt_sender = "optt_sender"
let jc_optt_amount = "optt_amount"
let jc_optt_param = "optt_param"
let jc_optt_now = "optt_now"


(*****************************************************************************)
(*****************************************************************************)
(* Cut Category, Cut Info, and Symbolic State                                *)
(*****************************************************************************)
(*****************************************************************************)

let mcc_trx_entry = "mcc_trx_entry"
let mcc_trx_exit = "mcc_trx_exit"
let mcc_ln_loop = "mcc_ln_loop"
let mcc_ln_loopleft = "mcc_ln_loopleft"
let mcc_ln_map = "mcc_ln_map"
let mcc_ln_iter = "mcc_ln_iter"
let mcc_lb_loop = "mcc_lb_loop"
let mcc_lb_loopleft = "mcc_lb_loopleft"
let mcc_lb_map = "mcc_lb_map"
let mcc_lb_iter = "mcc_lb_iter"
let mcc_query = "mcc_query"

let jc_mci_loc = "mci_loc"
let jc_mci_cutcat = "mci_cutcat"

let jc_ss_fixchain = "ss_fixchain"
let jc_ss_exop = "ss_exop"
let jc_ss_dynchain = "ss_dynchain"
let jc_ss_exec_addrs = "ss_exec_addrs"
let jc_ss_oper_queue = "ss_oper_queue"
let jc_ss_optt = "ss_optt"
let jc_ss_entry_mci = "ss_entry_mci"
let jc_ss_entry_symstack = "ss_entry_symstack"
let jc_ss_block_mci = "ss_block_mci"
let jc_ss_symstack = "ss_symstack"
let jc_ss_constraints = "ss_constraints"


(*****************************************************************************)
(*****************************************************************************)
(* Names in module Se                                                        *)
(*****************************************************************************)
(*****************************************************************************)

let q_mutez_add_no_overflow = "q_mutez_add_no_overflow"
let q_mutez_sub_no_underflow = "q_mutez_sub_no_underflow"
let q_mutez_mul_mnm_no_overflow = "q_mutez_mul_mnm_no_overflow"
let q_mutez_mul_nmm_no_overflow = "q_mutez_mul_nmm_no_overflow"
let q_shiftleft_safe = "q_shiftleft_safe"
let q_shiftright_safe = "q_shiftright_safe"
let q_assertion = "q_assertion"

let jc_running = "running"
let jc_blocked = "blocked"
let jc_queries = "queries"
let jc_terminated = "terminated"

let jc_ch_entered_loop = "ch_entered_loop"
let jc_ch_entered_lmbd = "ch_entered_lmbd"


(*****************************************************************************)
(*****************************************************************************)
(* Names in module LoopUr                                                    *)
(*****************************************************************************)
(*****************************************************************************)

let lur_r_list = "r_list"
let lur_r_state = "r_state"
let lur_r_loop = "r_loop"

let lur_jc_rq_r = "rq_r"
let lur_jc_rq_mci = "rq_mci"
let lur_jc_rq_qc = "rq_qc"

let lur_p_list = "p_list"
let lur_p_state = "p_state"
let lur_p_loop = "p_loop"

let lur_jc_pq_p = "pq_p"
let lur_jc_pq_mci = "pq_mci"
let lur_jc_pq_qc = "pq_qc"


(*****************************************************************************)
(*****************************************************************************)
(* Abbreviation                                                              *)
(*****************************************************************************)
(*****************************************************************************)

(*****************************************************************************)
(* Tezos Types                                                               *)
(*****************************************************************************)
let abr_t_address = "a"
let abr_t_bool = "b"
let abr_t_chain_id = "c"
let abr_t_lambda = "d"
let abr_symbol = "e"
let abr_t_contract = "f"
let abr_t_signature = "g"
let abr_t_key_hash = "h"
let abr_t_int = "i"
let abr_t_key = "k"
let abr_t_list = "l"
let abr_t_mutez = "m"
let abr_t_nat = "n"
let abr_t_or = "o"
let abr_t_pair = "p"
let abr_t_big_map = "q"
let abr_t_map = "r"
let abr_t_string = "s"
let abr_t_timestamp = "t"
let abr_t_unit = "u"
let abr_t_bytes = "v"
let abr_t_option = "w"
let abr_t_operation = "x"
let abr_t_set = "z"


(*****************************************************************************)
(* Michelson Values                                                          *)
(*****************************************************************************)

let abr_v_abs = "ab"
let abr_v_address_of_contract = "ac"
let abr_v_add = "ad"
let abr_v_and = "an"
let abr_v_blake2b = "b2"
let abr_v_car = "ca"
let abr_v_concat = "cc"
let abr_v_cdr = "cd"
let abr_v_cons = "co"
let abr_v_compare = "cp"
let abr_v_contract_of_address = "cr"
let abr_v_check_signature = "cs"
let abr_v_create_contract = "ct"
let abr_v_ediv = "ed"
let abr_v_empty = "ep"
let abr_v_eq = "eq"
let abr_v_geq = "ge"
let abr_v_get = "gg"
let abr_v_gt = "gt"
let abr_v_hd = "hd"
let abr_v_hash_key = "hk"
let abr_v_implicit_account = "ia"
let abr_v_int_of_nat = "in"
let abr_v_isnat = "is"
let abr_v_lambda_closure = "lc"
let abr_v_leq = "le"
let abr_v_left = "lf"
let abr_v_lit = "li"
let abr_v_lt = "lt"
let abr_v_lambda_unknown = "lu"
let abr_v_mul = "ml"
let abr_v_mem = "mm"
let abr_v_neq = "ne"
let abr_v_neg = "ng"
let abr_v_nil = "nl"
let abr_v_none = "no"
let abr_v_not = "nt"
let abr_v_or = "or"
let abr_v_pair = "pa"
let abr_v_pack = "pc"
let abr_v_right = "rt"
let abr_v_sha256 = "s2"
let abr_v_sha512 = "s5"
let abr_v_sub = "sb"
let abr_v_slice = "sc"
let abr_v_set_delegate = "sd"
let abr_v_self = "se"
let abr_v_sigma = "sg"
let abr_v_size = "si"
let abr_v_shiftL = "sl"
let abr_v_some = "so"
let abr_v_shiftR = "sr"
let abr_v_symbol = "sy"
let abr_v_tl = "tl"
let abr_v_transfer_tokens = "tt"
let abr_v_update = "ud"
let abr_v_unlift_left = "ul"
let abr_v_unit = "ul"
let abr_v_unlift_option = "uo"
let abr_v_unpack = "up"
let abr_v_unlift_right = "ur"
let abr_v_xor = "xr"


(*****************************************************************************)
(*****************************************************************************)
(* S expression                                                              *)
(*****************************************************************************)
(*****************************************************************************)

let cs_pos = "pos"
let cs_loc = "loc"
let cs_annot = "annot"
let cs_cc = "cc"
let cs_mich_t = "mich_t"
let cs_mich_v = "mich_v"
let cs_mich_i = "mich_i"
let cs_mich_f = "mich_f"
let cs_mich_t_cc = cs_mich_t ^ " cc"
let cs_mich_v_cc = cs_mich_v ^ " cc"
let cs_mich_i_cc = cs_mich_i ^ " cc"