(*****************************************************************************)
(*****************************************************************************)
(* Location in Code                                                          *)
(*****************************************************************************)
(*****************************************************************************)

type pos = { col : int; lin : int; }

type loc = Unknown | Pos of pos * pos


(*****************************************************************************)
(*****************************************************************************)
(* Michelson Code                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type annot = 
  | A_typ of string (* :type_annot   *)
  | A_var of string (* @var_annot    *)
  | A_fld of string (* %field_annot *)

type typ =
  | T_key
  | T_unit
  | T_signature
  | T_option    of typ t
  | T_list      of typ t
  | T_set       of typ t
  | T_operation
  | T_contract  of typ t
  | T_pair      of typ t * typ t
  | T_or        of typ t * typ t
  | T_lambda    of typ t * typ t
  | T_map       of typ t * typ t
  | T_big_map   of typ t * typ t
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez       (* D_int in Cfg-notation *)
  | T_bool
  | T_key_hash
  | T_timestamp   (* D_int in Cfg-notation *)
  | T_address

and inst =
  (* Standard Instructions : Michelson-Defined *)
  | I_seq           of inst t * inst t
  | I_drop
  | I_drop_n        of Z.t
  | I_dup
  | I_swap
  | I_dig           of Z.t
  | I_dug           of Z.t
  | I_push          of typ t * data t
  | I_some
  | I_none          of typ t
  | I_unit
  | I_if_none       of inst t * inst t
  | I_pair
  | I_car
  | I_cdr
  | I_left          of typ t
  | I_right         of typ t
  | I_if_left       of inst t * inst t
  | I_nil           of typ t
  | I_cons
  | I_if_cons       of inst t * inst t
  | I_size
  | I_empty_set     of typ t
  | I_empty_map     of typ t * typ t
  | I_empty_big_map of typ t * typ t
  | I_map           of inst t
  | I_iter          of inst t
  | I_mem
  | I_get
  | I_update
  | I_if            of inst t * inst t
  | I_loop          of inst t
  | I_loop_left     of inst t
  | I_lambda        of typ t * typ t * inst t
  | I_exec
  | I_dip           of inst t
  | I_dip_n         of Z.t * inst t
  | I_failwith
  | I_cast          of typ t
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack        of typ t
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract      of typ t
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_create_contract of program
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_unpair
  (* Standard Macros *)
  | M_plain of string             (* Macros with no following argument. e.g. FAIL *)
  | M_num   of string * Z.t       (* Macros with one number argument. e.g. DUP n *)
  | M_code  of string * (inst t)  (* Macros with one code argument. e.g. MAP_CAR *)
  | M_code2 of string * (inst t) * (inst t) (* Macros with two code arguments. e.g. IFCMPEQ *)
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check of inst t   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

and data =
  | D_int     of Z.t
  | D_string  of string
  | D_bytes   of string
  | D_unit
  | D_bool    of bool
  | D_pair    of data t * data t
  | D_left    of data t
  | D_right   of data t
  | D_some    of data t
  | D_none
  | D_list    of data t list
  | D_elt     of data t * data t
  | D_lambda  of inst t

and 'a t = { 
  pos     : loc; 
  ann     : annot list; 
  d       : 'a; 
}

and program = { param : typ t; storage : typ t; code : inst t; }


(*****************************************************************************)
(*****************************************************************************)
(* To String                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

val string_of_pos : pos -> string
val string_of_loc : loc -> string
val string_of_annot : annot -> string
val string_of_annots : annot list -> string

val string_of_typt_inner : bool -> typ t -> string
val string_of_typt : typ t -> string

(* Suffix "ol" for "one-line", that means the function with "ol" suffix will return 
    no newline characters. (except comment-related situations)
*)
val string_of_datat_ol_inner : bool -> data t -> string
val string_of_datat_ol : data t -> string

val string_of_seq_ol : inst t -> string
val string_of_instt_ol : inst t -> string

val string_of_pgm_ol : program -> string


(*****************************************************************************)
(*****************************************************************************)
(* Utility Functions (to type less)                                          *)
(*****************************************************************************)
(*****************************************************************************)

val get_d       : 'a t -> 'a  (* fun x -> x.d *)

val gen_t       :                      'a -> 'a t   (* fun x     -> {pos=Unknown; annot=A_none; d=x;} *)
val gen_t_a     :       annot list  -> 'a -> 'a t   (* fun a x   -> {pos=Unknown; annot=a; d=x;}      *)
val gen_t_p     : loc               -> 'a -> 'a t   (* fun p x   -> {pos=p; annot=[]; d=x;}           *)
val gen_t_pa    : loc -> annot list -> 'a -> 'a t   (* fun p a x -> {pos=p; annot=a; d=x;}            *)

val copy_info   : 'a t -> 'b   -> 'b t  (* fun x y -> {pos=x.pos; annot=x.annot; d=y;} *)
val copy_info_t : 'a t -> 'b t -> 'b t  (* fun x y -> {pos=x.pos; annot=x.annot; d=y.d;} *)

val get_min_pos : pos -> pos -> pos
val get_max_pos : pos -> pos -> pos
val join_pos    : loc -> loc -> loc  (* It assumes that two locations are already sanitized well *)
val join_pos_lst : loc list -> loc

val join_instt_seq  : annot list -> inst t -> inst t -> inst t    (* i1 i2 -> I_seq (i1, i2) *)

val empty_instt : inst t

val gen_instseq : (annot list) -> inst list     -> (inst t)
val gen_insttseq : (annot list) -> (inst t) list -> (inst t)   (* [A; B; C] -> I_seq( I_seq(a,b), c ) *)


(*****************************************************************************)
(*****************************************************************************)
(* Standard Macros                                                           *)
(*****************************************************************************)
(*****************************************************************************)

exception Not_Macro of string
val nm_fail : string -> 'a    (* fun s -> raise (Not Macro s) *)

val str_fst : string -> int -> string
val str_lst : string -> int -> string
val str_mid : string -> int -> int -> string

val m_if : inst -> inst -> inst

(*****************************************************************************)
(* Macros - Plain Macros                                                     *)
(*****************************************************************************)

val m_fail : inst t
val m_fail_d : inst

val m_cmpop : inst -> inst t

val m_assert : inst t
val m_assertop : inst -> inst t
val m_assertcmpop : inst -> inst t

val parse_duup : string -> bool * (inst t option)

val parse_ad : string -> bool * bool list

val parse_cadr : string -> bool * (inst t option)

val parse_set_cadr : string -> bool * (inst t option)

type pair_leaf = PT_A | PT_I
type pair_tree = PT_P of pair_tree * pair_tree | PT_L of pair_leaf
val construct_pair_tree : string -> (int * pair_tree) -> pair_leaf -> (int * pair_tree)
val decode_pair_tree : pair_tree -> inst list
val parse_pair : string -> bool * (inst t option)
val decode_unpair_tree : pair_tree -> inst list
val parse_unpair : string -> bool * (inst t option)

val resolve_plain_macro : inst t -> string -> inst t


(*****************************************************************************)
(* Macros - Macro with Number                                                *)
(*****************************************************************************)

val construct_duup : int -> string

val resolve_num_macro : string -> Z.t -> inst t


(*****************************************************************************)
(* Macros - Macro with a code                                                *)
(*****************************************************************************)

val parse_map_cadr : string -> inst t -> bool * (inst t option)

val resolve_code_macro : string -> inst t -> inst t


(*****************************************************************************)
(* Macros - Macro with two codes                                             *)
(*****************************************************************************)

val m_ifop : inst -> inst t -> inst t -> inst t
val m_ifcmpop : inst -> inst t -> inst t -> inst t

val resolve_code2_macro : string -> inst t -> inst t -> inst t


(*****************************************************************************)
(* Standard Macros - Overall                                                 *)
(*****************************************************************************)

val subst_standard_macro : inst t -> inst t

val subst_standard_macro_all : inst t -> inst t

val subst_standard_macro_all_data : data t -> data t

val subst_standard_macro_all_pgm : program -> program


(*****************************************************************************)
(*****************************************************************************)
(* Fill Unknown Position Informations                                        *)
(*****************************************************************************)
(*****************************************************************************)

val fill_position_all : ?update_loc_flag:bool -> (?update_loc:bool -> loc -> 'a -> (loc * 'a)) -> (loc -> ('a t) -> (loc * 'a t))

val fill_position_all_typ  : ?update_loc:bool -> loc -> typ  -> (loc * typ )
val fill_position_all_data : ?update_loc:bool -> loc -> data -> (loc * data)
val fill_position_all_inst : ?update_loc:bool -> loc -> inst -> (loc * inst)

val fill_position_all_typt  : ?update_loc:bool -> (loc -> typ t  -> (loc * typ t ))
val fill_position_all_datat : ?update_loc:bool -> (loc -> data t -> (loc * data t))
val fill_position_all_instt : ?update_loc:bool -> (loc -> inst t -> (loc * inst t))

val fill_position_all_pgm : ?update_loc:bool -> program -> program


(*****************************************************************************)
(*****************************************************************************)
(* Optimization                                                              *)
(*****************************************************************************)
(*****************************************************************************)

val optm_remove_noop_in_seq : inst t -> inst t

val optm_all_pgm : program -> program
