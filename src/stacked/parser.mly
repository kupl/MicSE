(* see menhir manual to see this mly grammar *)
%{
  open Mich

  let get_pos (startpos, endpos) =
    let open Lexing in
    Mich.Pos (
      {lin=startpos.pos_lnum; col=(startpos.pos_cnum - startpos.pos_bol + 1)}, 
      {lin=endpos.pos_lnum; col=(endpos.pos_cnum - endpos.pos_bol + 1)}
    )

  (*
  let pos_str (startpos, endpos) =
    let open Lexing in
    let spl, spc = startpos.pos_lnum, (startpos.pos_cnum - startpos.pos_bol + 1) in
    let epl, epc = endpos.pos_lnum, (endpos.pos_cnum - endpos.pos_bol + 1) in
    let s = Stdlib.string_of_int in
    (s(spl) ^ ":" ^ s(spc) ^ " - " ^ s(epl) ^ ":" ^ s(epc))
  *)

  let construct_pair_comb : (typ t list) -> (typ t) 
  = let rec foldf = function
      | [] | (_ :: []) -> Stdlib.failwith "Parser.mly : construct_pair_comb : length < 2"
      | [h1; h2] -> gen_t (T_pair (h1, h2))
      | hd :: tl -> gen_t (T_pair (hd, foldf tl))
    in
    fun ttl -> foldf ttl

%}




%token <Z.t> NUM
%token <bool> BOOLEAN
%token <string> HEX
%token <string> STRING
%token <string> IDENT

%token <string> PERCENT_ANNOT
%token <string> AT_ANNOT
%token <string> COLON_ANNOT

(* system-and-keyboard-symbols *)
%token LB RB LP RP
%token SEMICOLON MINUS EOF

(* Keywords *)
%token PARAMETER STORAGE CODE UNIT PAIR LEFT RIGHT SOME NONE ELT 

(* I_ for Instructions *)
%token I_DROP I_DUP I_SWAP I_DIG I_DUG I_PUSH I_SOME I_NONE I_UNIT I_IF_NONE I_PAIR
%token I_CAR I_CDR I_LEFT I_RIGHT I_IF_LEFT I_NIL I_CONS I_IF_CONS I_SIZE 
%token I_EMPTY_SET I_EMPTY_MAP I_EMPTY_BIG_MAP I_MAP I_ITER I_MEM I_GET I_UPDATE
%token I_IF I_LOOP I_LOOP_LEFT I_LAMBDA I_EXEC I_APPLY I_DIP I_FAILWITH I_CAST I_RENAME
%token I_CONCAT I_SLICE I_PACK I_UNPACK I_ADD I_SUB I_MUL I_EDIV I_ABS I_ISNAT
%token I_INT I_NEG I_LSL I_LSR I_OR I_AND I_XOR I_NOT I_COMPARE I_EQ I_NEQ I_LT
%token I_GT I_LE I_GE I_SELF I_CONTRACT I_TRANSFER_TOKENS I_SET_DELEGATE
%token I_CREATE_ACCOUNT I_CREATE_CONTRACT I_IMPLICIT_ACCOUNT I_NOW I_AMOUNT
%token I_BALANCE I_CHECK_SIGNATURE I_BLAKE2B I_SHA256 I_SHA512 I_HASH_KEY
%token I_STEPS_TO_QUOTA I_SOURCE I_SENDER I_ADDRESS I_CHAIN_ID I_UNPAIR T_KEY

(* T_ for Types *)
%token T_UNIT T_SIGNATURE T_OPTION T_LIST T_SET T_OPERATION T_CONTRACT T_PAIR
%token T_OR T_LAMBDA T_MAP T_BIG_MAP T_CHAIN_ID T_INT T_NAT T_STRING T_BYTES
%token T_MUTEZ T_BOOL T_KEY_HASH T_TIMESTAMP T_ADDRESS 

(* M_ for Macros *)
(*
%token M_FAIL
%token M_ASSERT M_ASSERT_NONE M_ASSERT_SOME M_ASSERT_LEFT M_ASSERT_RIGHT

%token        M_CMPEQ        M_CMPNEQ        M_CMPLT        M_CMPLE        M_CMPGT        M_CMPGE
%token         M_IFEQ         M_IFNEQ         M_IFLT         M_IFLE         M_IFGT         M_IFGE
%token      M_IFCMPEQ      M_IFCMPNEQ      M_IFCMPLT      M_IFCMPLE      M_IFCMPGT      M_IFCMPGE
%token    M_ASSERT_EQ    M_ASSERT_NEQ    M_ASSERT_LT    M_ASSERT_LE    M_ASSERT_GT    M_ASSERT_GE
%token M_ASSERT_CMPEQ M_ASSERT_CMPNEQ M_ASSERT_CMPLT M_ASSERT_CMPLE M_ASSERT_CMPGT M_ASSERT_CMPGE

%token M_IF_SOME M_IF_RIGHT M_SET_CAR M_SET_CDR M_MAP_CAR M_MAP_CDR
*)

(* MICSE-SPECIFIC INSTRUCTION *)
%token I_MICSE_CHECK



%start <Mich.program> start
%start <Mich.data Mich.t> data_entry



%%




(*****************************************************************************)
(*****************************************************************************)
(* PROGRAM                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

start:
  | LB? p=program RB? EOF { p }

program:
  | CODE code=br_code SEMICOLON STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON?
  | CODE code=br_code SEMICOLON PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON?
  | STORAGE storage=storage SEMICOLON CODE code=br_code SEMICOLON PARAMETER param=parameter SEMICOLON?
  | STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON CODE code=br_code SEMICOLON?
  | PARAMETER param=parameter SEMICOLON CODE code=br_code SEMICOLON STORAGE storage=storage SEMICOLON?
  | PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON CODE code=br_code SEMICOLON?  { { param; storage; code; } }

storage:
  | t=typ { t }

parameter:
  | t=typ { t }

%inline annot:
  | s=COLON_ANNOT     { A_typ s }
  | s=AT_ANNOT        { A_var s }
  | s=PERCENT_ANNOT   { A_fld s }

annots:
  | { [] }
  | hd=annot tl=annots { hd :: tl }

(*****************************************************************************)
(*****************************************************************************)
(* TYPE                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

typ:
  | LP t=typ RP {t}
  | t=typ_t {{ t with pos=get_pos($loc(t));} }

typ_t:
  | t=typ_t_ty_noreq a=annots                 { gen_t_a a t }
  | a=annots t=typ_t_ty_noreq                 { gen_t_a a t } (* This rule causes a few shift/reduce conflicts *)
  | LP T_OPTION a=annots t=typ RP             { gen_t_a a (T_option t) }
  | LP T_LIST a=annots t=typ RP               { gen_t_a a (T_list t) }
  | LP T_SET a=annots t=typ RP                { gen_t_a a (T_set t) }
  | LP T_CONTRACT a=annots t=typ RP           { gen_t_a a (T_contract t) }
  // | LP T_PAIR a=annots t_1=typ t_2=typ RP     { gen_t_a a (T_pair (t_1, t_2)) }
  | LP T_PAIR a=annots t_l=nonempty_list(typ) RP { gen_t_a a (construct_pair_comb(t_l)).d }
  | LP T_OR a=annots t_1=typ t_2=typ RP       { gen_t_a a (T_or (t_1, t_2)) }
  | LP T_LAMBDA a=annots t_1=typ t_2=typ RP   { gen_t_a a (T_lambda (t_1, t_2)) }
  | LP T_MAP a=annots t_1=typ t_2=typ RP      { gen_t_a a (T_map (t_1, t_2)) }
  | LP T_BIG_MAP a=annots t_1=typ t_2=typ RP  { gen_t_a a (T_big_map (t_1, t_2)) }

typ_t_ty_noreq:
  | T_KEY           { T_key }
  | T_UNIT          { T_unit }
  | T_SIGNATURE     { T_signature }
  | T_OPERATION     { T_operation }
  | T_CHAIN_ID      { T_chain_id }
  | T_INT           { T_int }
  | T_NAT           { T_nat }
  | T_STRING        { T_string }
  | T_BYTES         { T_bytes }
  | T_MUTEZ         { T_mutez }
  | T_BOOL          { T_bool }
  | T_KEY_HASH      { T_key_hash }
  | T_TIMESTAMP     { T_timestamp }
  | T_ADDRESS       { T_address }



(*****************************************************************************)
(*****************************************************************************)
(* CODE                                                                      *)
(*****************************************************************************)
(*****************************************************************************)


code:
  | i=inst_t SEMICOLON?    { i }
  | c=br_code             { c }
  | i=inst_t SEMICOLON c=code { (*"code - " ^ (pos_str($loc(i))) ^ "  ~  " ^ (pos_str($loc(c))) |> print_endline ;*) gen_insttseq [] [i; c] }
  | c_1=br_code SEMICOLON? c_2=code { (*"code - " ^ (pos_str($loc(c_1))) ^ "  ~  " ^ (pos_str($loc(c_2))) |> print_endline ;*) gen_insttseq [] [c_1; c_2] }

br_code:
  | LB RB { (*"br_code - " ^ (pos_str($loc)) |> print_endline;*) {pos=get_pos $loc; ann=[]; d=I_noop;} }
  | LB c=code RB { (*"br_code - " ^ (pos_str($loc)) |> print_endline;*) {c with pos=get_pos($loc(c));} }

inst_t:
  (* Rule to Add Location Information *)
  | i=inst_t_i  { (*"inst_t_i - " ^ (pos_str($loc)) |> print_endline;*) {i with pos=get_pos($loc(i));} }
  | m=macro_t_i { (*"macro_t_i - " ^ (pos_str($loc)) |> print_endline;*) {m with pos=get_pos($loc(m));} }

inst_t_noreq:
  | i=inst_t_i_noreq a=annots   { let ii = (gen_t_a a i) in {ii with pos=get_pos($loc(i))} }
  | m=macro_t_i_noreq a=annots  { let mm = (gen_t_a a m) in {mm with pos=get_pos($loc(m))} }

inst_t_i:
  (** Standard Instructions : Michelson-Defined (except I_seq and curly braces) **)
  (* Instructions which have no other requirements *)
  | i=inst_t_i_noreq  a=annots { gen_t_a a i }
  (* Other cases *)
  | I_DROP a=annots n=NUM       { gen_t_a a (I_drop_n n) }
  | I_DIG  a=annots n=NUM       { gen_t_a a (I_dig n) }
  | I_DUG  a=annots n=NUM       { gen_t_a a (I_dug n) }
  | I_DUP  a=annots n=NUM       { gen_t_a a (M_num ("DUP", n)) }
  | I_GET  a=annots n=NUM       { gen_t_a a (M_num ("GET", n)) }
  | I_PAIR a=annots n=NUM       { gen_t_a a (M_num ("PAIR", n)) }
  | I_UPDATE a=annots n=NUM     { gen_t_a a (M_num ("UPDATE", n)) }
  | I_UNPAIR a=annots n=NUM     { gen_t_a a (M_num ("UNPAIR", n)) }
  | I_PUSH a=annots t=typ d=data                  { gen_t_a a (I_push (t, d)) }
  | I_NONE a=annots t=typ       { gen_t_a a (I_none t) }
  | I_IF_NONE a=annots i_1=br_code i_2=br_code    { gen_t_a a (I_if_none (i_1, i_2)) }
  | I_IF_NONE a=annots i_1=inst_t_noreq i_2=br_code { gen_t_a a (I_if_none (i_1, i_2)) }
  | I_IF_NONE a=annots i_1=br_code i_2=inst_t_noreq { gen_t_a a (I_if_none (i_1, i_2)) }
  // | I_IF_NONE a=annots                            { gen_t_a a (I_if_none (gen_t I_noop, gen_t I_noop)) }
  | I_LEFT a=annots t=typ                         { gen_t_a a (I_left t) }
  | I_RIGHT a=annots t=typ                        { gen_t_a a (I_right t) }
  | I_IF_LEFT a=annots i_1=br_code i_2=br_code    { gen_t_a a (I_if_left (i_1, i_2)) }
  | I_IF_LEFT a=annots i_1=inst_t_noreq i_2=br_code { gen_t_a a (I_if_left (i_1, i_2)) }
  | I_IF_LEFT a=annots i_1=br_code i_2=inst_t_noreq { gen_t_a a (I_if_left (i_1, i_2)) }
  // | I_IF_LEFT a=annots                            { gen_t_a a (I_if_left (gen_t I_noop, gen_t I_noop)) }
  | I_NIL a=annots t=typ                          { gen_t_a a (I_nil t) }
  | I_IF_CONS a=annots i_1=br_code i_2=br_code    { gen_t_a a (I_if_cons (i_1, i_2)) }
  | I_IF_CONS a=annots i_1=inst_t_noreq i_2=br_code { gen_t_a a (I_if_cons (i_1, i_2)) }
  | I_IF_CONS a=annots i_1=br_code i_2=inst_t_noreq { gen_t_a a (I_if_cons (i_1, i_2)) }
  // | I_IF_CONS a=annots                            { gen_t_a a (I_if_cons (gen_t I_noop, gen_t I_noop)) }
  | I_EMPTY_SET a=annots t=typ                    { gen_t_a a (I_empty_set t) }
  | I_EMPTY_MAP a=annots t_1=typ t_2=typ          { gen_t_a a (I_empty_map (t_1, t_2)) }
  | I_EMPTY_BIG_MAP a=annots t_1=typ t_2=typ      { gen_t_a a (I_empty_big_map (t_1, t_2)) }
  | I_MAP a=annots i=br_code    { gen_t_a a (I_map i) }
  | I_ITER a=annots i=br_code   { gen_t_a a (I_iter i) }
  | I_IF a=annots i_1=br_code i_2=br_code         { gen_t_a a (I_if (i_1, i_2)) }
  | I_IF a=annots i_1=inst_t_noreq i_2=br_code    { gen_t_a a (I_if (i_1, i_2)) }
  | I_IF a=annots i_1=br_code i_2=inst_t_noreq    { gen_t_a a (I_if (i_1, i_2)) }
  // | I_IF a=annots                                 { gen_t_a a (I_if (gen_t I_noop, gen_t I_noop)) }
  | I_LOOP a=annots i=br_code   { gen_t_a a (I_loop i) }
  | I_LOOP_LEFT a=annots i=br_code                { gen_t_a a (I_loop_left i) }
  | I_LAMBDA a=annots t_1=typ t_2=typ i=br_code   { gen_t_a a (I_lambda (t_1, t_2, i)) }
  | I_DIP a=annots i=inst_t     { gen_t_a a (I_dip i) }
  | I_DIP a=annots c=br_code    { gen_t_a a (I_dip c)}
  | I_DIP a=annots n=NUM i=inst_t                   { gen_t_a a (I_dip_n (n, i)) }
  | I_DIP a=annots n=NUM c=br_code                  { gen_t_a a (I_dip_n (n, c)) }
  | I_CAST a=annots t=typ       { gen_t_a a (I_cast t) }
  | I_UNPACK a=annots t=typ     { gen_t_a a (I_unpack t) }
  | I_CONTRACT a=annots t=typ   { gen_t_a a (I_contract t) }
  | I_CREATE_CONTRACT a=annots LB c=program RB  { gen_t_a a (I_create_contract c) }

  (** Non-Standard Instruction : Special Comment : MicSE user defined safety property **)
  | I_MICSE_CHECK i=br_code { gen_t (I_micse_check i) }   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)


%inline inst_t_i_noreq:
  | I_DROP { I_drop }
  | I_DUP  { I_dup  }
  | I_SWAP { I_swap }
  | I_SOME { I_some }
  | I_UNIT { I_unit }
  | I_PAIR { I_pair }
  | I_CAR  { I_car  }
  | I_CDR  { I_cdr  }
  | I_CONS { I_cons }
  | I_SIZE { I_size }
  | I_MEM  { I_mem  }
  | I_GET  { I_get  }
  | I_UPDATE   { I_update   }
  | I_EXEC { I_exec }
  | I_APPLY    { I_apply    }
  | I_FAILWITH { I_failwith }
  | I_RENAME   { I_rename   }
  | I_CONCAT   { I_concat   }
  | I_SLICE    { I_slice    }
  | I_PACK { I_pack }
  | I_ADD  { I_add  }
  | I_SUB  { I_sub  }
  | I_MUL  { I_mul  }
  | I_EDIV { I_ediv }
  | I_ABS  { I_abs  }
  | I_ISNAT    { I_isnat    }
  | I_INT  { I_int  }
  | I_NEG  { I_neg  }
  | I_LSL  { I_lsl  }
  | I_LSR  { I_lsr  }
  | I_OR   { I_or   }
  | I_AND  { I_and  }
  | I_XOR  { I_xor  }
  | I_NOT  { I_not  }
  | I_COMPARE  { I_compare  }
  | I_EQ   { I_eq   }
  | I_NEQ  { I_neq  }
  | I_LT   { I_lt   }
  | I_GT   { I_gt   }
  | I_LE   { I_le   }
  | I_GE   { I_ge   }
  | I_SELF { I_self }
  | I_TRANSFER_TOKENS  { I_transfer_tokens  }
  | I_SET_DELEGATE     { I_set_delegate     }
  | I_CREATE_ACCOUNT   { I_create_account   }
  | I_IMPLICIT_ACCOUNT { I_implicit_account }
  | I_NOW  { I_now  }
  | I_AMOUNT   { I_amount   }
  | I_BALANCE  { I_balance  }
  | I_CHECK_SIGNATURE  { I_check_signature  }
  | I_BLAKE2B  { I_blake2b  }
  | I_SHA256   { I_sha256   }
  | I_SHA512   { I_sha512   }
  | I_HASH_KEY { I_hash_key }
  | I_STEPS_TO_QUOTA   { I_steps_to_quota   }
  | I_SOURCE   { I_source   }
  | I_SENDER   { I_sender   }
  | I_ADDRESS  { I_address  }
  | I_CHAIN_ID { I_chain_id }
  | I_UNPAIR   { I_unpair   }


macro_t_i:
  (* Standard Macros *)
  | m=macro_t_i_noreq a=annots          { gen_t_a a m }
  | m=IDENT a=annots n=NUM              { gen_t_a a (M_num (m, n)) }
  | m=IDENT a=annots i=br_code          { gen_t_a a (M_code (m, i)) }
  | m=IDENT a=annots i_1=br_code i_2=br_code { gen_t_a a (M_code2 (m, i_1, i_2))}

macro_t_i_noreq:
  | m=IDENT                             { M_plain m }


(*****************************************************************************)
(*****************************************************************************)
(* DATA                                                                      *)
(*****************************************************************************)
(*****************************************************************************)


int:
  | n=NUM { n }
  | MINUS n=NUM { Z.neg n }

data:
  | LP d=data RP {d}
  | t=data_t { {t with pos=get_pos($loc(t));} }

data_entry:
  | d=data EOF { d } 

data_t:
  | d=data_t_dt_noreq a=annots { gen_t_a a d }
  | PAIR a=annots d_1=data d_2=data  { gen_t_a a (D_pair (d_1, d_2)) }
  | LEFT a=annots d=data { gen_t_a a (D_left d) }
  | RIGHT a=annots d=data  { gen_t_a a (D_right d) }
  | SOME a=annots d=data { gen_t_a a (D_some d) }
  | ELT a=annots d_1=data d_2=data { gen_t_a a (D_elt (d_1, d_2)) }
  | d=delimited(LB, separated_nonempty_list(SEMICOLON, data), RB) { gen_t (D_list d) }
  | LB RB a=annots { gen_t_a a (D_list []) }
  (* | LAMBDA *) (* No lambda can be directly parsed (please use the LAMBDA michelson instruction instead. ) *)

data_t_dt_noreq:
  | n=int { D_int n }
  | s=STRING  { D_string s }
  | b=HEX { D_bytes b }
  | UNIT  { D_unit }
  | b=BOOLEAN  { D_bool b }
  | NONE  { D_none }
