(*****************************************************************************)
(*****************************************************************************)
(* Verification Language                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Pre.Lib.Adt.typ
and data = Pre.Lib.Adt.data
and operation = Pre.Lib.Cfg.operation

type var = Pre.Lib.Cfg.ident
and exp = Pre.Lib.Cfg.expr

type t = v_formula

and v_obj = {
  exp: v_exp;
  typ: typ;
}

and v_formula =
  | VF_true  | VF_false
  | VF_not of v_formula
  | VF_and of v_formula list
  | VF_or of v_formula list
  | VF_uni_rel of v_uni_rel * v_obj
  | VF_bin_rel of v_bin_rel * v_obj * v_obj
  | VF_imply of v_formula * v_formula
  | VF_iff of v_formula * v_formula
  | VF_forall of v_obj list * v_formula
  (* Customized formula *)
  | VF_sigma_equal of v_obj * v_obj (* VF_sigma_equal a b = VF_bin_rel VF_eq Sigma(a) b *)

and v_uni_rel =
  | VF_is_true  | VF_is_none  | VF_is_left  | VF_is_cons

and v_bin_rel =
  | VF_eq       | VF_neq      | VF_lt       | VF_le
  | VF_gt       | VF_ge

and v_exp =
  | VE_int of Z.t
  | VE_string of string
  | VE_bool of v_formula
  | VE_unit
  | VE_none
  | VE_uni_cont of v_uni_cont * v_obj
  | VE_bin_cont of v_bin_cont * v_obj * v_obj
  | VE_list of v_obj list
  | VE_var of var
  | VE_nul_op of v_nul_op
  | VE_uni_op of v_uni_op * v_obj
  | VE_bin_op of v_bin_op * v_obj * v_obj
  | VE_ter_op of v_ter_op * v_obj * v_obj * v_obj
  | VE_lambda
  | VE_operation of v_operation

and v_uni_cont =
  | VE_left     | VE_right    | VE_some

and v_bin_cont =
  | VE_pair     | VE_elt

and v_nul_op =
  | VE_self     | VE_now      | VE_amount   | VE_balance  | VE_steps_to_quota
  | VE_source   | VE_sender   | VE_chain_id

and v_uni_op =
  | VE_car      | VE_cdr      | VE_abs      | VE_neg      | VE_not
  | VE_eq       | VE_neq      | VE_lt       | VE_gt       | VE_leq
  | VE_geq      | VE_cast     | VE_pack     | VE_unpack   | VE_list_concat
  | VE_contract | VE_account  | VE_blake2b  | VE_sha256   | VE_sha512
  | VE_hash_key | VE_address  | VE_un_opt   | VE_un_left  | VE_un_right
  | VE_hd       | VE_tl       | VE_size     | VE_isnat    | VE_to_int
  
and v_bin_op =
  | VE_add      | VE_sub      | VE_mul      | VE_ediv     | VE_div
  | VE_mod      | VE_lsl      | VE_lsr      | VE_and      | VE_or
  | VE_xor      | VE_cmp      | VE_cons     | VE_concat   | VE_exec
  | VE_append   | VE_get      | VE_mem

and v_ter_op =
  | VE_slice    | VE_check_signature        | VE_update

and v_operation =
  | VE_transaction
  | VE_origination
  | VE_delegation


(*****************************************************************************)
(*****************************************************************************)
(* Verification object                                                       *)
(*****************************************************************************)
(*****************************************************************************)

let create_obj_of_exp : exp:v_exp -> typ:typ -> v_obj
=fun ~exp ~typ -> { exp=exp; typ=typ }


(*****************************************************************************)
(*****************************************************************************)
(* Verification Formula                                                      *)
(*****************************************************************************)
(*****************************************************************************)

let create_formula_true : v_formula
=VF_true

let create_formula_false : v_formula
=VF_false

let create_formula_not : v_formula -> v_formula
=fun f -> VF_not f

let create_formula_and : v_formula list -> v_formula
=fun fl -> VF_and fl

let create_formula_or : v_formula list -> v_formula
=fun fl -> VF_or fl

let create_formula_uni_rel : rel:v_uni_rel -> o1:v_obj -> v_formula
=fun ~rel ~o1 -> VF_uni_rel (rel, o1)

let create_formula_is_true : v_obj -> v_formula
=fun o1 -> create_formula_uni_rel ~rel:VF_is_true ~o1:o1

let create_formula_is_none : v_obj -> v_formula
=fun o1 -> create_formula_uni_rel ~rel:VF_is_none ~o1:o1

let create_formula_is_some : v_obj -> v_formula
=fun o1 -> create_formula_not (create_formula_uni_rel ~rel:VF_is_none ~o1:o1)

let create_formula_is_left : v_obj -> v_formula
=fun o1 -> create_formula_uni_rel ~rel:VF_is_left ~o1:o1

let create_formula_is_right : v_obj -> v_formula
=fun o1 -> create_formula_not (create_formula_uni_rel ~rel:VF_is_left ~o1:o1)

let create_formula_is_cons : v_obj -> v_formula
=fun o1 -> create_formula_uni_rel ~rel:VF_is_cons ~o1:o1

let create_formula_is_nil : v_obj -> v_formula
=fun o1 -> create_formula_not (create_formula_uni_rel ~rel:VF_is_cons ~o1:o1)

let create_formula_bin_rel : rel:v_bin_rel -> o1:v_obj -> o2:v_obj -> v_formula
=fun ~rel ~o1 ~o2 -> VF_bin_rel (rel, o1, o2)

let create_formula_eq : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_eq ~o1:o1 ~o2:o2

let create_formula_neq : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_neq ~o1:o1 ~o2:o2

let create_formula_lt : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_lt ~o1:o1 ~o2:o2

let create_formula_le : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_le ~o1:o1 ~o2:o2

let create_formula_gt : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_gt ~o1:o1 ~o2:o2

let create_formula_ge : v_obj -> v_obj -> v_formula
=fun o1 o2 -> create_formula_bin_rel ~rel:VF_ge ~o1:o1 ~o2:o2

let create_formula_imply : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_imply (f1, f2)

let create_formula_iff : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_iff (f1, f2)

let create_formula_forall : bnd:v_obj list -> formula:v_formula -> v_formula
=fun ~bnd ~formula -> VF_forall (bnd, formula)

(* Customized formula *)

let create_formula_no_overflow : v_obj -> v_formula
=fun o -> begin
  match o.exp with
  | VE_bin_op (bop, o1, o2) -> begin
      match o.typ.d, bop with
      | T_mutez, VE_add -> create_formula_ge o o2
      | T_mutez, VE_mul -> begin
          if o2.typ.d = T_mutez
          then create_formula_ge o o2
          else create_formula_ge o o1
        end
      | _ -> raise (Failure "Vlang.create_formula_no_overflow: Wrong type or operation of VE_bin_op")
    end
  | _ -> raise (Failure "Vlang.create_formula_no_overflow: Wrong expression of object")
end

let create_formula_no_underflow : v_obj -> v_formula
=fun o -> begin
  match o.exp with
  | VE_bin_op (bop, o1, _) -> begin
      match o.typ.d, bop with
      | T_mutez, VE_sub -> create_formula_le o o1
      | _ -> raise (Failure "Vlang.create_formula_no_underflow: Wrong type or operation of VE_bin_op")
    end
  | _ -> raise (Failure "Vlang.create_formula_no_underflow: Wrong expression of object")
end

let create_formula_sigma_equal : map:v_obj -> mutez:v_obj -> v_formula
=fun ~map ~mutez -> VF_sigma_equal (map, mutez)

(*****************************************************************************)
(*****************************************************************************)
(* Verification Expression                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let create_exp_int : Z.t -> v_exp
=fun n -> VE_int n

let create_exp_int_of_small_int : int -> v_exp
=fun i -> create_exp_int (Z.of_int i)

let create_exp_int_of_string : string -> v_exp
=fun s -> create_exp_int (Z.of_string s)

let create_exp_string : string -> v_exp
=fun s -> VE_string s

let create_exp_bool : v_formula -> v_exp
=fun f -> VE_bool f

let create_exp_bool_true : v_exp
=create_exp_bool create_formula_true

let create_exp_bool_false : v_exp
=create_exp_bool create_formula_false

let create_exp_unit : v_exp
=VE_unit

let create_exp_none : v_exp
=VE_none

let create_exp_uni_cont : cont:v_uni_cont -> o1:v_obj -> v_exp
=fun ~cont ~o1 -> VE_uni_cont (cont, o1)

let create_exp_uni_cont_left : v_obj -> v_exp
=fun o1 -> create_exp_uni_cont ~cont:VE_left ~o1:o1

let create_exp_uni_cont_right : v_obj -> v_exp
=fun o1 -> create_exp_uni_cont ~cont:VE_right ~o1:o1

let create_exp_uni_cont_some : v_obj -> v_exp
=fun o1 -> create_exp_uni_cont ~cont:VE_some ~o1

let create_exp_bin_cont : cont:v_bin_cont -> o1:v_obj -> o2:v_obj -> v_exp
=fun ~cont ~o1 ~o2 -> VE_bin_cont (cont, o1, o2)

let create_exp_bin_cont_pair : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_cont ~cont:VE_pair ~o1:o1 ~o2:o2

let create_exp_bin_cont_elt : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_cont ~cont:VE_elt ~o1:o1 ~o2:o2

let create_exp_list : v_obj list -> v_exp
=fun ol -> VE_list ol

let create_exp_var : var -> v_exp
=fun v -> VE_var v

let create_exp_nul_op : op:v_nul_op -> v_exp
=fun ~op -> VE_nul_op op

let create_exp_nul_op_self : v_exp
=create_exp_nul_op ~op:VE_self

let create_exp_nul_op_now : v_exp
=create_exp_nul_op ~op:VE_now

let create_exp_nul_op_amount : v_exp
=create_exp_nul_op ~op:VE_amount

let create_exp_nul_op_balance : v_exp
=create_exp_nul_op ~op:VE_balance

let create_exp_nul_op_steps_to_quota : v_exp
=create_exp_nul_op ~op:VE_steps_to_quota

let create_exp_nul_op_source : v_exp
=create_exp_nul_op ~op:VE_source

let create_exp_nul_op_sender : v_exp
=create_exp_nul_op ~op:VE_sender

let create_exp_nul_op_chain_id : v_exp
=create_exp_nul_op ~op:VE_chain_id

let create_exp_uni_op : op:v_uni_op -> o1:v_obj -> v_exp
=fun ~op ~o1 -> VE_uni_op (op, o1)

let create_exp_uni_op_car : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_car ~o1:o1

let create_exp_uni_op_cdr : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_cdr ~o1:o1

let create_exp_uni_op_abs : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_abs ~o1:o1

let create_exp_uni_op_neg : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_neg ~o1:o1

let create_exp_uni_op_not : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_not ~o1:o1

let create_exp_uni_op_eq : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_eq ~o1:o1

let create_exp_uni_op_neq : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_neq ~o1:o1

let create_exp_uni_op_lt : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_lt ~o1:o1

let create_exp_uni_op_gt : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_gt ~o1:o1

let create_exp_uni_op_leq : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_leq ~o1:o1

let create_exp_uni_op_geq : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_geq ~o1:o1

let create_exp_uni_op_cast : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_cast ~o1:o1

let create_exp_uni_op_concat : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_list_concat ~o1:o1

let create_exp_uni_op_pack : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_pack ~o1:o1

let create_exp_uni_op_unpack : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_unpack ~o1:o1

let create_exp_uni_op_contract : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_contract ~o1:o1

let create_exp_uni_op_account : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_account ~o1:o1

let create_exp_uni_op_blake2b : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_blake2b ~o1:o1

let create_exp_uni_op_sha256 : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_sha256 ~o1:o1

let create_exp_uni_op_sha512 : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_sha512 ~o1:o1

let create_exp_uni_op_hash_key : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_hash_key ~o1:o1

let create_exp_uni_op_address : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_address ~o1:o1

let create_exp_uni_op_un_opt : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_un_opt ~o1:o1

let create_exp_uni_op_un_left : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_un_left ~o1:o1

let create_exp_uni_op_un_right : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_un_right ~o1:o1

let create_exp_uni_op_hd : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_hd ~o1:o1

let create_exp_uni_op_tl : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_tl ~o1:o1

let create_exp_uni_op_size : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_size ~o1:o1

let create_exp_uni_op_isnat : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_isnat ~o1:o1

let create_exp_uni_op_int : v_obj -> v_exp
=fun o1 -> create_exp_uni_op ~op:VE_to_int ~o1:o1

let create_exp_bin_op : op:v_bin_op -> o1:v_obj -> o2:v_obj -> v_exp
=fun ~op ~o1 ~o2 -> VE_bin_op (op, o1, o2)

let create_exp_bin_op_add : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_add ~o1:o1 ~o2:o2

let create_exp_bin_op_sub : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_sub ~o1:o1 ~o2:o2

let create_exp_bin_op_mul : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_mul ~o1:o1 ~o2:o2

let create_exp_bin_op_ediv : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_ediv ~o1:o1 ~o2:o2

let create_exp_bin_op_div : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_div ~o1:o1 ~o2:o2

let create_exp_bin_op_mod : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_mod ~o1:o1 ~o2:o2

let create_exp_bin_op_lsl : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_lsl ~o1:o1 ~o2:o2

let create_exp_bin_op_lsr : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_lsr ~o1:o1 ~o2:o2

let create_exp_bin_op_and : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_and ~o1:o1 ~o2:o2

let create_exp_bin_op_or : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_or ~o1:o1 ~o2:o2

let create_exp_bin_op_xor : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_xor ~o1:o1 ~o2:o2

let create_exp_bin_op_cmp : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_cmp ~o1:o1 ~o2:o2

let create_exp_bin_op_cons : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_cons ~o1:o1 ~o2:o2

let create_exp_bin_op_concat : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_concat ~o1:o1 ~o2:o2

let create_exp_bin_op_exec : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_exec ~o1:o1 ~o2:o2

let create_exp_bin_op_append : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_append ~o1:o1 ~o2:o2

let create_exp_bin_op_get : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_get ~o1:o1 ~o2:o2

let create_exp_bin_op_mem : v_obj -> v_obj -> v_exp
=fun o1 o2 -> create_exp_bin_op ~op:VE_mem ~o1:o1 ~o2:o2

let create_exp_ter_op : op:v_ter_op -> o1:v_obj -> o2:v_obj -> o3:v_obj -> v_exp
=fun ~op ~o1 ~o2 ~o3 -> VE_ter_op (op, o1, o2, o3)

let create_exp_ter_op_slice : v_obj -> v_obj -> v_obj -> v_exp
=fun o1 o2 o3 -> create_exp_ter_op ~op:VE_slice ~o1:o1 ~o2:o2 ~o3:o3

let create_exp_ter_op_check_signature : v_obj -> v_obj -> v_obj -> v_exp
=fun o1 o2 o3 -> create_exp_ter_op ~op:VE_check_signature ~o1:o1 ~o2:o2 ~o3:o3

let create_exp_ter_op_update : v_obj -> v_obj -> v_obj -> v_exp
=fun o1 o2 o3 -> create_exp_ter_op ~op:VE_update ~o1:o1 ~o2:o2 ~o3:o3

let create_exp_lambda : v_exp
=VE_lambda

let create_exp_operation : v_operation -> v_exp
=fun vop -> VE_operation (vop)

let create_exp_operation_transaction : v_exp
=create_exp_operation VE_transaction

let create_exp_operation_origination : v_exp
=create_exp_operation VE_origination

let create_exp_operation_delegation : v_exp
=create_exp_operation VE_delegation


(*****************************************************************************)
(*****************************************************************************)
(* Stringify Modules                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let rec string_of_formula : v_formula -> string
=fun f -> begin
  match f with
  | VF_true -> "True"
  | VF_false -> "False"
  | VF_not f' -> "!(" ^ (string_of_formula f') ^ ")"
  | VF_and fl' -> Core.String.concat ~sep:" && " (Core.List.map fl' ~f:(fun f' -> "(" ^ (string_of_formula f') ^ ")"))
  | VF_or fl' -> Core.String.concat ~sep:" || " (Core.List.map fl' ~f:(fun f' -> "(" ^ (string_of_formula f') ^ ")"))
  | VF_uni_rel (vur, o1') -> begin
      match vur with
      | VF_is_true -> (string_of_formula VF_true) ^ " == (" ^ (string_of_obj o1') ^ ")"
      | VF_is_none -> "None == (" ^ (string_of_obj o1') ^ ")"
      | VF_is_left -> "Left x == (" ^ (string_of_obj o1') ^ ")"
      | VF_is_cons -> "Cons x == (" ^ (string_of_obj o1') ^ ")"
    end
  | VF_bin_rel (vbr, o1', o2') -> begin
      match vbr with
      | VF_eq -> "(" ^ (string_of_obj o1') ^ ") == (" ^ (string_of_obj o2') ^ ")"
      | VF_neq -> "(" ^ (string_of_obj o1') ^ ") != (" ^ (string_of_obj o2') ^ ")"
      | VF_lt -> "(" ^ (string_of_obj o1') ^ ") < (" ^ (string_of_obj o2') ^ ")"
      | VF_le -> "(" ^ (string_of_obj o1') ^ ") <= (" ^ (string_of_obj o2') ^ ")"
      | VF_gt -> "(" ^ (string_of_obj o1') ^ ") > (" ^ (string_of_obj o2') ^ ")"
      | VF_ge -> "(" ^ (string_of_obj o1') ^ ") >= (" ^ (string_of_obj o2') ^ ")"
    end
  | VF_imply (f1', f2') -> "(" ^ (string_of_formula f1') ^ ") -> (" ^ (string_of_formula f2') ^ ")"
  | VF_iff (f1', f2') -> "(" ^ (string_of_formula f1') ^ ") <-> (" ^ (string_of_formula f2') ^ ")"
  | VF_forall (ol', f') -> "ForAll " ^ (
        Core.String.concat ~sep:", " (Core.List.map ol' ~f:string_of_obj)
      ) ^ ". " ^ (string_of_formula f')
  | VF_sigma_equal (o1', o2') -> "(Σ" ^ (string_of_obj o1') ^ " == " ^ (string_of_obj o2') ^ ")"
end

and string_of_obj : v_obj -> string
=fun o -> string_of_exp o.exp

and string_of_exp : v_exp -> string
=fun e -> begin
  match e with
  | VE_int n -> Z.to_string n
  | VE_string s -> "\"" ^ s ^ "\""
  | VE_bool f -> string_of_formula f
  | VE_unit -> "Unit"
  | VE_none -> "None"
  | VE_uni_cont (vuc, o1') -> begin
      match vuc with
      | VE_left -> "Left (" ^ (string_of_obj o1') ^ ")"
      | VE_right -> "Right (" ^ (string_of_obj o1') ^ ")"
      | VE_some -> "Some (" ^ (string_of_obj o1') ^ ")"
    end
  | VE_bin_cont (vbc, o1', o2') -> begin
      match vbc with
      | VE_pair -> "Pair (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")"
      | VE_elt -> "Elt (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")"
    end
  | VE_list ol' -> begin
      "List [" ^ (Core.String.concat ~sep:"; " (Core.List.map ol' ~f:(fun o' -> "(" ^ (string_of_obj o') ^ ")"))) ^ "]"
    end
  | VE_var v -> v
  | VE_nul_op vno -> begin
      match vno with
      | VE_self -> "SELF"
      | VE_now -> "NOW"
      | VE_amount -> "AMOUNT"
      | VE_balance -> "BALANCE"
      | VE_steps_to_quota -> "STEPS_TO_QUOTA"
      | VE_source -> "SOURCE"
      | VE_sender -> "SENDER"
      | VE_chain_id -> "CHAIN_ID"
    end
  | VE_uni_op (vuo, o1') -> begin
      match vuo with
      | VE_car -> "CAR (" ^ (string_of_obj o1') ^ ")"
      | VE_cdr -> "CDR (" ^ (string_of_obj o1') ^ ")"
      | VE_abs -> "ABS (" ^ (string_of_obj o1') ^ ")"
      | VE_neg -> "NEG (" ^ (string_of_obj o1') ^ ")"
      | VE_not -> "NOT (" ^ (string_of_obj o1') ^ ")"
      | VE_eq -> "EQ (" ^ (string_of_obj o1') ^ ")"
      | VE_neq -> "NEQ (" ^ (string_of_obj o1') ^ ")"
      | VE_lt -> "LT (" ^ (string_of_obj o1') ^ ")"
      | VE_gt -> "GT (" ^ (string_of_obj o1') ^ ")"
      | VE_leq -> "LEQ (" ^ (string_of_obj o1') ^ ")"
      | VE_geq -> "GEQ (" ^ (string_of_obj o1') ^ ")"
      | VE_cast -> "CAST (" ^ (string_of_obj o1') ^ ")"
      | VE_list_concat -> "CONCAT (" ^ (string_of_obj o1') ^ ")"
      | VE_pack -> "PACK (" ^ (string_of_obj o1') ^ ")"
      | VE_unpack -> "UNPACK (" ^ (string_of_obj o1') ^ ")"
      | VE_contract -> "CONTRACT (" ^ (string_of_obj o1') ^ ")"
      | VE_account -> "ACCOUNT (" ^ (string_of_obj o1') ^ ")"
      | VE_blake2b -> "BLAKE2B (" ^ (string_of_obj o1') ^ ")"
      | VE_sha256 -> "SHA256 (" ^ (string_of_obj o1') ^ ")"
      | VE_sha512 -> "SHA512 (" ^ (string_of_obj o1') ^ ")"
      | VE_hash_key -> "HASH_KEY (" ^ (string_of_obj o1') ^ ")"
      | VE_address -> "ADDRESS (" ^ (string_of_obj o1') ^ ")"
      | VE_un_opt -> "UN_OPT (" ^ (string_of_obj o1') ^ ")"
      | VE_un_left -> "UN_LEFT (" ^ (string_of_obj o1') ^ ")"
      | VE_un_right -> "UN_RIGHT (" ^ (string_of_obj o1') ^ ")"
      | VE_hd -> "HD (" ^ (string_of_obj o1') ^ ")"
      | VE_tl -> "TL (" ^ (string_of_obj o1') ^ ")"
      | VE_size -> "SIZE (" ^ (string_of_obj o1') ^ ")"
      | VE_isnat -> "ISNAT (" ^ (string_of_obj o1') ^ ")"
      | VE_to_int -> "INT (" ^ (string_of_obj o1') ^ ")"
    end
  | VE_bin_op (vbo, o1', o2') -> begin
      match vbo with
      | VE_add -> "ADD (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_sub -> "SUB (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_mul -> "MUL (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_ediv -> "EDIV (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_div -> "DIV (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_mod -> "MOD (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_lsl -> "LSL (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_lsr -> "LSR (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_and -> "AND (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_or -> "OR (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_xor -> "XOR (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_cmp -> "COMPARE (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_cons -> "CONS (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_concat -> "CONCAT (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_exec -> "EXEC (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")" 
      | VE_append -> "APPEND (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")"
      | VE_get -> "GET (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")"
      | VE_mem -> "MEM (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ")"
    end
  | VE_ter_op (vto, o1', o2', o3') -> begin
      match vto with
      | VE_slice -> "SLICE (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ") (" ^ (string_of_obj o3') ^ ")"
      | VE_check_signature -> "CHECK_SIGNATURE (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ") (" ^ (string_of_obj o3') ^ ")"
      | VE_update -> "UPDATE (" ^ (string_of_obj o1') ^ ") (" ^ (string_of_obj o2') ^ ") (" ^ (string_of_obj o3') ^ ")"
    end
  | VE_lambda -> "LAMBDA"
  | VE_operation _ -> "OPERATION"
end

let string_of_vlang : t -> string
=fun f -> string_of_formula f