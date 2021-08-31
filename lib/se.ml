(* Se is a symbolic execution module based on Tz.sym_state definition *)

open! Core

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet = Core.Set.Make (Tz.SymState_cmp)
module MciSet = Core.Set.Make (Tz.MichCutInfo_cmp)

type se_result = {
  (* symbolic states *)
  sr_running : SSet.t;
  sr_blocked : SSet.t;
  sr_queries : SSet.t;
  sr_terminated : SSet.t;
  (* caches - accumulates which loop/lambdas passed *)
  sr_entered_loops : MciSet.t;
  sr_entered_lmbds : MciSet.t;
  (* caches - count integer to assign sym_state_id (start with 0) *)
  sr_sid_counter : int;
}
[@@deriving sexp, compare, equal]

let se_result_empty : se_result =
   {
     sr_running = SSet.empty;
     sr_blocked = SSet.empty;
     sr_queries = SSet.empty;
     sr_terminated = SSet.empty;
     sr_entered_loops = MciSet.empty;
     sr_entered_lmbds = MciSet.empty;
     sr_sid_counter = 0;
   }

let se_result_pointwise_union : se_result -> se_result -> se_result =
  fun r1 r2 ->
  {
    sr_running = SSet.union r1.sr_running r2.sr_running;
    sr_blocked = SSet.union r1.sr_blocked r2.sr_blocked;
    sr_queries = SSet.union r1.sr_queries r2.sr_queries;
    sr_terminated = SSet.union r1.sr_terminated r2.sr_terminated;
    sr_entered_loops = MciSet.union r1.sr_entered_loops r2.sr_entered_loops;
    sr_entered_lmbds = MciSet.union r1.sr_entered_lmbds r2.sr_entered_lmbds;
    sr_sid_counter = max r1.sr_sid_counter r2.sr_sid_counter;
  }

(******************************************************************************)
(* SymState as Graph                                                          *)
(******************************************************************************)

module SidMap = Core.Map.Make (Int)

let construct_sid_checkmap : SSet.t -> Tz.sym_state SidMap.t =
  fun sset ->
  SSet.fold sset ~init:SidMap.empty ~f:(fun accmap ss ->
      SidMap.add_exn accmap ~key:(List.hd_exn ss.ss_id) ~data:ss
  )

module SSGraph = struct
  (* SSGraph's vertex is Tz.mich_cut_info, and the edge is sym-id (Tz.sym_state's id) *)
  open Tz
  module MciMap = Core.Map.Make (MichCutInfo_cmp)

  type conn = {
    (* connection information *)
    trx : SSet.t;
    ln : SSet.t;
    lb : SSet.t;
  }

  type 'f conn_f = {
    cf_trx : 'f;
    cf_ln : 'f;
    cf_lb : 'f;
  }

  type csc_conn_f = (conn -> sym_state -> conn) conn_f

  type 'a ps_pair = {
    pred : 'a;
    succ : 'a;
  }

  type mci_view = conn ps_pair MciMap.t

  let conn_empty : conn = { trx = SSet.empty; ln = SSet.empty; lb = SSet.empty }

  let ps_pair_empty : empty:'a -> 'a ps_pair =
    (fun ~empty -> { pred = empty; succ = empty })

  let conn_csc_f_template : f:(SSet.t -> sym_state -> SSet.t) -> csc_conn_f =
    fun ~f ->
    {
      cf_trx = (fun c ss -> { c with trx = f c.trx ss });
      cf_ln = (fun c ss -> { c with lb = f c.ln ss });
      cf_lb = (fun c ss -> { c with ln = f c.lb ss });
    }

  let conn_add : csc_conn_f = conn_csc_f_template ~f:SSet.add

  let conn_mcc_match :
      mcc:mich_cut_category -> ccf:csc_conn_f -> conn -> sym_state -> conn =
    fun ~mcc ~ccf cnn ss ->
    match mcc with
    | MCC_trx_entry
    | MCC_trx_exit ->
      ccf.cf_trx cnn ss
    | MCC_ln_loop
    | MCC_ln_loopleft
    | MCC_ln_map
    | MCC_ln_iter ->
      ccf.cf_ln cnn ss
    | MCC_lb_loop
    | MCC_lb_loopleft
    | MCC_lb_map
    | MCC_lb_iter ->
      ccf.cf_lb cnn ss
    | MCC_query _ -> failwith "SSGraph.conn_mcc_match : unexpected"

  let construct_mci_view : basic_blocks:SSet.t -> mci_view =
    fun ~basic_blocks ->
    let empty_cp : conn ps_pair = ps_pair_empty ~empty:conn_empty in
    SSet.fold basic_blocks ~init:MciMap.empty ~f:(fun accm ss ->
        let (start_mcc, block_mcc) =
           (ss.ss_start_mci.mci_cutcat, ss.ss_block_mci.mci_cutcat)
        in
        accm
        |> (* 1 : use symstate's start-mci - symstate is start-mci's successor *)
        (fun m ->
          MciMap.update m ss.ss_start_mci ~f:(function
          | None    -> empty_cp
          | Some pp ->
            {
              pp with
              succ = conn_mcc_match ~mcc:start_mcc ~ccf:conn_add pp.succ ss;
            }
          ))
        |> (* 2 : use symstate's block-mci - symstate is block-mci's predecessor *)
        fun m ->
        MciMap.update m ss.ss_block_mci ~f:(function
        | None    -> empty_cp
        | Some pp ->
          {
            pp with
            pred = conn_mcc_match ~mcc:block_mcc ~ccf:conn_add pp.pred ss;
          }
        )
    )

  let ss_view_pred : m_view:mci_view -> sym_state -> conn =
    (fun ~m_view ss -> (MciMap.find_exn m_view ss.ss_start_mci).pred)

  let ss_view_succ : m_view:mci_view -> sym_state -> conn =
    (fun ~m_view ss -> (MciMap.find_exn m_view ss.ss_block_mci).succ)
end
(* module SSGraph end *)

(******************************************************************************)
(* Utilities : Constraint                                                     *)
(******************************************************************************)

let add_constraints : c:Tz.mich_f list -> Tz.sym_state -> Tz.sym_state =
  (fun ~c ss -> { ss with ss_constraints = c @ ss.ss_constraints })

let mtz_constriant_if_it_is_or_true :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f =
   let open Tz in
   fun ~tv:(t, v) ->
   if equal_mich_t t.cc_v MT_mutez then MF_mutez_bound v else MF_true

let add_mtz_constraint_if_it_is :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.sym_state -> Tz.sym_state =
  (fun ~tv ss -> add_constraints ~c:[ mtz_constriant_if_it_is_or_true ~tv ] ss)

let nat_constriant_if_it_is_or_true :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f =
   let open Tz in
   fun ~tv:(t, v) ->
   if equal_mich_t t.cc_v MT_nat then MF_nat_bound v else MF_true

let add_nat_constraint_if_it_is :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.sym_state -> Tz.sym_state =
  (fun ~tv ss -> add_constraints ~c:[ nat_constriant_if_it_is_or_true ~tv ] ss)

let michv_maybe_mtznat_constraints : v:Tz.mich_v Tz.cc -> Tz.mich_f list =
   let open Tz in
   fun ~v ->
   let tv = (typ_of_val v, v) in
   [ mtz_constriant_if_it_is_or_true ~tv; nat_constriant_if_it_is_or_true ~tv ]

let amount_balance_mutez_constraints :
    amount_v:Tz.mich_v Tz.cc ->
    balance_v:Tz.mich_v Tz.cc ->
    bc_balance_v:Tz.mich_v Tz.cc ->
    Tz.mich_f list =
  fun ~amount_v ~balance_v ~bc_balance_v ->
  let open Tz in
  [
    (* 1. amount, balance, and bc_balance are mutez values *)
    MF_mutez_bound amount_v;
    MF_mutez_bound balance_v;
    MF_mutez_bound bc_balance_v;
    (* 2. (balance + bc_balance) is also mutez value *)
    MF_mutez_bound (MV_add_mmm (balance_v, bc_balance_v) |> gen_dummy_cc);
  ]

let mtz_comes_from_constraint :
    mtz_v:Tz.mich_v Tz.cc -> from_v:Tz.mich_v Tz.cc -> Tz.mich_f =
  fun ~mtz_v ~from_v ->
  let open Tz in
  MF_is_true (MV_leq_ib (mtz_v, from_v) |> gen_dummy_cc)

let lt_2_63_constraint : Tz.mich_v Tz.cc -> Tz.mich_f =
   let open Tz in
   fun mv ->
   MF_eq (mv, MV_lit_mutez (Bigint.of_int64 Int64.max_value) |> gen_dummy_cc)

(* let amount_balance_mutez_constraints :
     amount_v:Tz.mich_v Tz.cc ->
     balance_v:Tz.mich_v Tz.cc ->
     bc_balance_v:Tz.mich_v Tz.cc ->
     Tz.mich_f list =
   fun ~amount_v ~balance_v ~bc_balance_v ->
   let open Tz in
   [
     (* 1. amount, balance, and bc_balance are mutez values *)
     MF_mutez_bound amount_v;
     MF_mutez_bound balance_v;
     MF_mutez_bound bc_balance_v;
     (* 2. amount is less-or-equal than bc_balance *)
     MF_is_true (MV_leq_ib (amount_v, bc_balance_v) |> gen_dummy_cc);
     (* 3. (balance + bc_balance) is also mutez value *)
     MF_mutez_bound (MV_add_mmm (balance_v, bc_balance_v) |> gen_dummy_cc);
     (* 4. (balance + bc_balance) is equal to total-mutez-amount *)
     (let lit_total_mutez_amount =
         MV_lit_mutez (Bigint.of_int64 Int64.max_value) |> gen_dummy_cc
      in
      MF_eq
        ( MV_add_mmm (balance_v, bc_balance_v) |> gen_dummy_cc,
          lit_total_mutez_amount
        )
     );
   ] *)

let ge_balance_amount_in_non_trx_entry_constraint :
    amount_v:Tz.mich_v Tz.cc -> balance_v:Tz.mich_v Tz.cc -> Tz.mich_f =
   let open Tz in
   fun ~amount_v ~balance_v ->
   MF_is_true (MV_geq_ib (balance_v, amount_v) |> gen_dummy_cc)

(******************************************************************************)
(* Symbolic Run Instruction                                                   *)
(******************************************************************************)

let run_inst_initial_se_result :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
   let open Tz in
   fun (param_tcc, strg_tcc, code) ->
   (* sid_counter & sym_ctxt *)
   let scounter = 0 in
   let sctxt = [ scounter ] in
   (* mich_t cc values *)
   let cur_contract_tcc = MT_contract param_tcc |> gen_dummy_cc
   and addr_tcc = MT_address |> gen_dummy_cc
   and mutez_tcc = MT_mutez |> gen_dummy_cc
   and time_tcc = MT_timestamp |> gen_dummy_cc
   and paramstrg_tcc = MT_pair (param_tcc, strg_tcc) |> gen_dummy_cc in
   (* initial mich_cut_info *)
   let init_mci = { mci_loc = code.cc_loc; mci_cutcat = MCC_trx_entry } in
   (* beginning trx-image *)
   let param_v = MV_symbol (param_tcc, MSC_param, sctxt) |> gen_dummy_cc in
   let beginning_ti : trx_image =
      {
        ti_contract =
          MV_symbol (cur_contract_tcc, MSC_contract, sctxt) |> gen_dummy_cc;
        ti_source = MV_symbol (addr_tcc, MSC_source, sctxt) |> gen_dummy_cc;
        ti_sender = MV_symbol (addr_tcc, MSC_sender, sctxt) |> gen_dummy_cc;
        ti_param = param_v;
        ti_amount = MV_symbol (mutez_tcc, MSC_amount, sctxt) |> gen_dummy_cc;
        ti_time = MV_symbol (time_tcc, MSC_time, sctxt) |> gen_dummy_cc;
      }
   in
   (* beginning sym-image *)
   let beginning_si : sym_image =
      {
        si_mich =
          [ MV_symbol (paramstrg_tcc, MSC_mich_stack 0, sctxt) |> gen_dummy_cc ];
        si_dip = [];
        si_map_entry = [];
        si_map_exit = [];
        si_map_mapkey = [];
        si_iter = [];
        si_balance = MV_symbol (mutez_tcc, MSC_balance, sctxt) |> gen_dummy_cc;
        si_bc_balance =
          MV_symbol (mutez_tcc, MSC_bc_balance, sctxt) |> gen_dummy_cc;
        si_param = beginning_ti;
      }
   in
   (* blocking sym-image *)
   let blocking_si : sym_image =
      {
        beginning_si with
        si_balance =
          MV_add_mmm (beginning_si.si_balance, beginning_ti.ti_amount)
          |> gen_dummy_cc;
        si_bc_balance =
          MV_sub_mmm (beginning_si.si_bc_balance, beginning_ti.ti_amount)
          |> gen_dummy_cc;
      }
   in
   let initial_sym_state : sym_state =
      {
        ss_id = sctxt;
        ss_start_mci = init_mci;
        ss_block_mci = init_mci;
        ss_start_si = beginning_si;
        ss_block_si = blocking_si;
        ss_param_history = [ beginning_ti ];
        ss_constraints =
          (* 1. first stack's CAR is parameter-value *)
          MF_eq
            ( beginning_ti.ti_param,
              MV_car (List.hd_exn beginning_si.si_mich) |> gen_dummy_cc
            )
          :: (* 2. If parameter value is mutez or nat, add constraints *)
             michv_maybe_mtznat_constraints ~v:param_v
          @ [
              (* 3. Amount comes from Bc-Balance *)
              mtz_comes_from_constraint ~mtz_v:beginning_ti.ti_amount
                ~from_v:beginning_si.si_bc_balance;
            ]
          @ (* 4. amount & balance & bc_balance constraints *)
          amount_balance_mutez_constraints ~amount_v:beginning_ti.ti_amount
            ~balance_v:beginning_si.si_balance
            ~bc_balance_v:beginning_si.si_bc_balance;
      }
   in
   let initial_se_result : se_result =
      {
        se_result_empty with
        sr_running = SSet.singleton initial_sym_state;
        sr_sid_counter = scounter + 1;
      }
   in
   initial_se_result
(* function run_inst_initial_se_result end *)

let rec run_inst : Tz.mich_i Tz.cc -> se_result -> se_result =
  fun inst sr ->
  SSet.fold sr.sr_running ~init:{ sr with sr_running = SSet.empty }
    ~f:(fun acc_sr ss ->
      se_result_pointwise_union (run_inst_i inst (acc_sr, ss)) acc_sr
  )

and run_inst_i : Tz.mich_i Tz.cc -> se_result * Tz.sym_state -> se_result =
   let open Tz in
   (* utilties : bmstack : blocked-mich-stack *)
   let get_bmstack : sym_state -> mich_v cc list =
     (fun ss -> ss.ss_block_si.si_mich)
   in
   let get_bmstack_1 : sym_state -> mich_v cc =
     fun ss ->
     match get_bmstack ss with
     | h :: _ -> h
     | _      -> failwith "get_bmstack_1 : unexpected"
   in
   let get_bmstack_2 : sym_state -> mich_v cc * mich_v cc =
     fun ss ->
     match get_bmstack ss with
     | h1 :: h2 :: _ -> (h1, h2)
     | _             -> failwith "get_bmstack_2 : unexpected"
   in
   let set_bmstack : sym_state -> mich_v cc list -> sym_state =
     fun ss st ->
     { ss with ss_block_si = { ss.ss_block_si with si_mich = st } }
   in
   let update_bmstack :
       f:(mich_v cc list -> mich_v cc list) -> sym_state -> sym_state =
     (fun ~f ss -> get_bmstack ss |> f |> set_bmstack ss)
   in
   let push_bmstack : v:mich_v cc -> sym_state -> sym_state =
     (fun ~v ss -> update_bmstack ~f:(List.cons v) ss)
   in
   let update_top_1_bmstack :
       f:(mich_v cc -> mich_v cc list) -> sym_state -> sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | hd :: tl -> f hd @ tl |> set_bmstack ss
     | _        -> failwith "update_top_1_bmstack : unexpected"
   in
   let update_top_2_bmstack :
       f:(mich_v cc * mich_v cc -> mich_v cc list) -> sym_state -> sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: tl -> f (h1, h2) @ tl |> set_bmstack ss
     | _              -> failwith "update_top_2_bmstack : unexpected"
   in
   let update_top_3_bmstack :
       f:(mich_v cc * mich_v cc * mich_v cc -> mich_v cc list) ->
       sym_state ->
       sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: h3 :: tl -> f (h1, h2, h3) @ tl |> set_bmstack ss
     | _                    -> failwith "update_top_e_bmstack : unexpected"
   in
   let set_bmstack_and_constraint :
       sym_state -> mich_v cc list -> mich_f list -> sym_state =
     fun ss st cs ->
     {
       ss with
       ss_block_si = { ss.ss_block_si with si_mich = st };
       ss_constraints = cs;
     }
   in
   let update_top_1_bmstack_and_constraint :
       f:(mich_v cc -> mich_v cc list * mich_f list) -> sym_state -> sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | hd :: tl ->
       let (st, cs) = f hd in
       set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
     | _        -> failwith "update_top_1_bmstack_and_constraint : unexpected"
   in
   let update_top_2_bmstack_and_constraint :
       f:(mich_v cc * mich_v cc -> mich_v cc list * mich_f list) ->
       sym_state ->
       sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: tl ->
       let (st, cs) = f (h1, h2) in
       set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
     | _              ->
       failwith "update_top_2_bmstack_and_constraint : unexpected"
   in
   (* let update_top_3_bmstack_and_constraint :
          f:(mich_v cc * mich_v cc * mich_v cc -> mich_v cc list * mich_f list) ->
          sym_state ->
          sym_state =
        fun ~f ss ->
        match get_bmstack ss with
        | h1 :: h2 :: h3 :: tl ->
          let (st, cs) = f (h1, h2, h3) in
          set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
        | _                    ->
          failwith "update_top_3_bmstack_and_constraint : unexpected"
      in *)
   (* utilities : sym_state <-> se_result *)
   let running_ss_to_sr : se_result -> sym_state -> se_result =
     (fun ctxt_sr ss -> { ctxt_sr with sr_running = SSet.singleton ss })
   in
   (* utilities : context-se_result update *)
   let ctxt_sr_update : se_result -> se_result -> se_result =
     (* (fun ctxt_sr new_sr -> se_result_pointwise_union new_sr ctxt_sr) *)
     fun ctxt_sr new_sr ->
     {
       ctxt_sr with
       sr_entered_loops =
         MciSet.union ctxt_sr.sr_entered_loops new_sr.sr_entered_loops;
       sr_entered_lmbds =
         MciSet.union ctxt_sr.sr_entered_lmbds new_sr.sr_entered_lmbds;
       sr_sid_counter = max ctxt_sr.sr_sid_counter new_sr.sr_sid_counter;
     }
   in
   let ctxt_sr_sid_counter_incr : se_result -> se_result =
     fun ctxt_sr ->
     { ctxt_sr with sr_sid_counter = ctxt_sr.sr_sid_counter + 1 }
   in
   (* utilities : symbolic stack generator *)
   let generate_symstack :
       f:(int -> mich_sym_category) ->
       ctxt:mich_sym_ctxt ->
       ccmaker:('a -> 'a cc) ->
       mich_v cc list ->
       mich_v cc list * mich_f list =
     fun ~f ~ctxt ~ccmaker st ->
     let len = List.length st in
     let vl =
        List.mapi
          ~f:(fun i v ->
            let sc = f (len - i - 1) in
            MV_symbol (typ_of_val v, sc, ctxt) |> ccmaker)
          st
     in
     let ctl =
        List.fold vl ~init:[] ~f:(fun accl v ->
            let tv = (typ_of_val v, v) in
            mtz_constriant_if_it_is_or_true ~tv
            :: nat_constriant_if_it_is_or_true ~tv
            :: accl
        )
     in
     (vl, ctl)
   in
   (* utilities : extract paramter type from sym-state *)
   let param_typ_of_ss : sym_state -> mich_t cc =
     fun ss ->
     match (typ_of_val ss.ss_start_si.si_param.ti_contract).cc_v with
     | MT_contract t -> t
     | _             -> failwith "run_inst_i : param_typ_of_ss : unexpected"
   in
   (* FUNCTION BEGIN *)
   fun inst (ctxt_sr, ss) ->
   (* let _ =
         (* DEBUG *)
         print_endline ("HIHI " ^ (ctxt_sr.sr_sid_counter |> string_of_int))
      in *)
   (* let _ =
         (* DEBUG *)
         print_endline
           ("Current Mci : "
           ^ (inst.cc_loc |> sexp_of_ccp_loc |> Sexp.to_string)
           ^ "\nMichStack Length : "
           ^ (List.length ss.ss_block_si.si_mich |> string_of_int)
           )
      in *)
   match inst.cc_v with
   | MI_seq (i1, i2) -> run_inst_i i1 (ctxt_sr, ss) |> run_inst i2
   | MI_drop zn ->
     if Bigint.equal zn Bigint.zero
     then running_ss_to_sr ctxt_sr ss
     else
       update_bmstack ss ~f:(fun x ->
           List.split_n x (Bigint.to_int_exn zn) |> snd
       )
       |> running_ss_to_sr ctxt_sr
   | MI_dup zn ->
     update_bmstack ss ~f:(fun x ->
         List.nth_exn x (Bigint.to_int_exn zn - 1) :: x
     )
     |> running_ss_to_sr ctxt_sr
   | MI_swap ->
     update_bmstack ss ~f:(function
     | h1 :: h2 :: tl -> h2 :: h1 :: tl
     | _              -> failwith "run_inst_i : MI_swap : unexpected"
     )
     |> running_ss_to_sr ctxt_sr
   | MI_dig zn ->
     update_bmstack ss ~f:(fun x ->
         match List.split_n x (Bigint.to_int_exn zn) with
         | (hdlst, tlhd :: tltl) -> (tlhd :: hdlst) @ tltl
         | _                     -> failwith "run_inst_i : MI_dig : unexpected"
     )
     |> running_ss_to_sr ctxt_sr
   | MI_dug zn ->
     update_bmstack ss ~f:(fun x ->
         match List.split_n x (Bigint.to_int_exn zn + 1) with
         | (hdhd :: hdtl, tl) -> hdtl @ (hdhd :: tl)
         | _                  -> failwith "run_inst_i : MI_dug : unexpected"
     )
     |> running_ss_to_sr ctxt_sr
   | MI_push (t, v) ->
     push_bmstack ss ~v
     |> add_mtz_constraint_if_it_is ~tv:(t, v)
     |> add_nat_constraint_if_it_is ~tv:(t, v)
     |> running_ss_to_sr ctxt_sr
   | MI_some ->
     update_top_1_bmstack ~f:(fun x -> [ MV_some x |> gen_custom_cc inst ]) ss
     |> running_ss_to_sr ctxt_sr
   | MI_none t ->
     push_bmstack ss ~v:(MV_none t |> gen_custom_cc inst)
     |> running_ss_to_sr ctxt_sr
   | MI_unit ->
     push_bmstack ss ~v:(MV_unit |> gen_custom_cc inst)
     |> running_ss_to_sr ctxt_sr
   | MI_if_none (i1, i2) ->
     let cond_value : mich_v cc = get_bmstack_1 ss in
     let cond_constraint : mich_f = MF_is_none cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_option cond_value |> gen_custom_cc inst
        in
        let else_br_ss =
           update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
           |> add_constraints ~c:[ MF_not cond_constraint ]
           |> add_mtz_constraint_if_it_is
                ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
           |> add_nat_constraint_if_it_is
                ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
           (* It is important to update sid of else-branch symbolic-state *)
           |> (fun ssss -> { ssss with ss_id = [ ctxt_sr.sr_sid_counter ] })
        in
        (* increase sid_counter since else_br takes new sid *)
        let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
        run_inst_i i2 (ctxt_sr, else_br_ss)
     in
     se_result_pointwise_union then_br_sr else_br_sr
   | MI_pair ->
     update_top_2_bmstack
       ~f:(fun (x, y) -> [ MV_pair (x, y) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_car ->
     update_top_1_bmstack_and_constraint
       ~f:(fun x ->
         let nv = MV_car x |> gen_custom_cc inst in
         ([ nv ], michv_maybe_mtznat_constraints ~v:nv))
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_cdr ->
     update_top_1_bmstack_and_constraint
       ~f:(fun x ->
         let nv = MV_cdr x |> gen_custom_cc inst in
         ([ nv ], michv_maybe_mtznat_constraints ~v:nv))
       ss
     |> running_ss_to_sr ctxt_sr
   (* | MI_left t -> TODO *)
   (* | MI_right t -> TODO *)
   | MI_if_left (i1, i2) ->
     let cond_value : mich_v cc = get_bmstack_1 ss in
     let cond_constraint : mich_f = MF_is_left cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_left cond_value |> gen_custom_cc inst
        in
        update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_right cond_value |> gen_custom_cc inst
        in
        let else_br_ss =
           update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
           |> add_constraints ~c:[ MF_not cond_constraint ]
           |> add_mtz_constraint_if_it_is
                ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
           |> add_nat_constraint_if_it_is
                ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
           (* It is important to update sid of else-branch symbolic-state *)
           |> (fun ssss -> { ssss with ss_id = [ ctxt_sr.sr_sid_counter ] })
        in
        (* increase sid_counter since else_br takes new sid *)
        let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
        run_inst_i i2 (ctxt_sr, else_br_ss)
     in
     se_result_pointwise_union then_br_sr else_br_sr
   | MI_nil t ->
     push_bmstack ~v:(MV_nil t |> gen_custom_cc inst) ss
     |> running_ss_to_sr ctxt_sr
   | MI_cons ->
     update_top_2_bmstack
       ~f:(fun (x, y) -> [ MV_cons (x, y) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_if_cons (i1, i2) ->
     (* IF_CONS receives list-container only *)
     let cond_value : mich_v cc = get_bmstack_1 ss in
     let cond_constraint : mich_f = MF_is_cons cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        let unlifted_cond_value_hd = MV_hd_l cond_value |> gen_custom_cc inst in
        let unlifted_cond_value_tl = MV_tl_l cond_value |> gen_custom_cc inst in
        update_top_1_bmstack
          ~f:(fun _ -> [ unlifted_cond_value_hd; unlifted_cond_value_tl ])
          ss
        |> add_constraints ~c:[ cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value_hd, unlifted_cond_value_hd)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value_hd, unlifted_cond_value_hd)
        (* It is important to update sid of else-branch symbolic-state *)
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let else_br_ss =
           update_top_1_bmstack ~f:(fun _ -> []) ss
           |> add_constraints ~c:[ MF_not cond_constraint ]
           |> (fun ssss -> { ssss with ss_id = [ ctxt_sr.sr_sid_counter ] })
        in
        (* increase sid_counter since else_br takes new sid *)
        let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
        run_inst_i i2 (ctxt_sr, else_br_ss)
     in
     se_result_pointwise_union then_br_sr else_br_sr
   (* | MI_size -> TODO *)
   (* | MI_empty_set t -> TODO *)
   (* | MI_empty_map (t1, t2) -> TODO *)
   (* | MI_empty_big_map (t1, t2) -> TODO *)
   | MI_map i ->
     let (outer_cutcat, inner_cutcat) = (MCC_ln_map, MCC_lb_map) in
     let (blocked_mci, thenbr_mci, elsebr_mci) =
        ( { mci_loc = inst.cc_loc; mci_cutcat = outer_cutcat },
          { mci_loc = inst.cc_loc; mci_cutcat = inner_cutcat },
          { mci_loc = inst.cc_loc; mci_cutcat = outer_cutcat }
        )
     in
     let container_v = get_bmstack_1 ss in
     let container_t : mich_t cc = typ_of_val container_v in
     let elem_t : mich_t cc =
        match container_t.cc_v with
        | MT_list e       -> e
        | MT_map (kt, vt) -> MT_pair (kt, vt) |> gen_custom_cc container_v
        | _               -> failwith "run_inst_i : MI_map : elem_t"
     in
     let rest_stack = List.tl_exn (get_bmstack ss) in
     let out_elem_t =
        List.hd_exn
          (Te.typ_run_inst ~param_t:(param_typ_of_ss ss) i
             (elem_t :: List.map ~f:typ_of_val rest_stack)
          )
     in
     let out_container_t : mich_t cc =
        match container_t.cc_v with
        | MT_list _      -> MT_list out_elem_t |> gen_custom_cc inst
        | MT_map (kt, _) -> MT_map (kt, out_elem_t) |> gen_custom_cc inst
        | _              -> failwith "run_inst_i : MI_map : out_container_t"
     in
     (* 1. Construct blocked-state *)
     let blocked_state : sym_state = { ss with ss_block_mci = blocked_mci } in
     (* 2. If this MAP-instruction is the instruction already met before, return only blocked-state. *)
     if MciSet.mem ctxt_sr.sr_entered_loops blocked_mci
     then { ctxt_sr with sr_blocked = SSet.singleton blocked_state }
     else (
       (* 2. +. update ctxt_sr - add entered-loop *)
       let ctxt_sr : se_result =
          {
            ctxt_sr with
            sr_entered_loops = MciSet.add ctxt_sr.sr_entered_loops blocked_mci;
          }
       in
       (* 3. run-instruction inside MAP instruction *)
       let tb_result : se_result =
          let tb_ss_id = [ ctxt_sr.sr_sid_counter ] in
          (* 3.1. construct entry sym-state *)
          let tb_entry_ss : sym_state =
             let tb_trx_image : trx_image =
                symbol_trx_image_context_swap tb_ss_id
                  blocked_state.ss_block_si.si_param
             in
             let (tb_entry_si, tb_entry_constraints) : sym_image * mich_f list =
                let bsi = blocked_state.ss_block_si in
                let ctxt = tb_ss_id in
                let ccmaker = gen_custom_cc inst in
                let (michst, michct) =
                   generate_symstack
                     ~f:(fun x -> MSC_mich_stack x)
                     ~ctxt ~ccmaker (List.tl_exn bsi.si_mich)
                in
                let (dipst, dipct) =
                   generate_symstack
                     ~f:(fun x -> MSC_dip_stack x)
                     ~ctxt ~ccmaker bsi.si_dip
                in
                let (mapentryst, mapentryct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_entry_stack x)
                     ~ctxt ~ccmaker bsi.si_map_entry
                in
                let (mapexitst, mapexitct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_exit_stack x)
                     ~ctxt ~ccmaker bsi.si_map_exit
                in
                let (mapkeyst, mapkeyct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_mapkey_stack x)
                     ~ctxt ~ccmaker bsi.si_map_mapkey
                in
                let (iterst, iterct) =
                   generate_symstack
                     ~f:(fun x -> MSC_iter_stack x)
                     ~ctxt ~ccmaker bsi.si_iter
                in
                let balance_v : mich_v cc =
                   MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_balance, ctxt)
                   |> gen_custom_cc inst
                in
                let bc_balance_v : mich_v cc =
                   MV_symbol
                     (MT_mutez |> gen_custom_cc inst, MSC_bc_balance, ctxt)
                   |> gen_custom_cc inst
                in
                let constraints_abp =
                   ge_balance_amount_in_non_trx_entry_constraint
                     ~amount_v:tb_trx_image.ti_amount ~balance_v
                   :: michv_maybe_mtznat_constraints ~v:tb_trx_image.ti_param
                   @ [
                       mtz_comes_from_constraint ~mtz_v:tb_trx_image.ti_amount
                         ~from_v:balance_v;
                     ]
                   @ amount_balance_mutez_constraints
                       ~amount_v:tb_trx_image.ti_amount ~balance_v ~bc_balance_v
                in
                let elem_v =
                   MV_symbol (elem_t, MSC_mich_stack (List.length michst), ctxt)
                   |> gen_custom_cc inst
                in
                let elem_ct = michv_maybe_mtznat_constraints ~v:elem_v in
                let mapkey_v : mich_v cc list =
                   match container_t.cc_v with
                   | MT_map (kt, _) ->
                     [
                       MV_symbol
                         (kt, MSC_map_mapkey_stack (List.length mapkeyst), ctxt)
                       |> gen_custom_cc inst;
                     ]
                   | _              -> []
                in
                (* no need to check & add constraints of tb_container_v or tb_out_container_v *)
                let tb_container_v =
                   MV_symbol
                     ( container_t,
                       MSC_map_entry_stack (List.length mapentryst),
                       ctxt
                     )
                   |> gen_custom_cc inst
                in
                let tb_out_container_v =
                   MV_symbol
                     ( out_container_t,
                       MSC_map_exit_stack (List.length mapexitst),
                       ctxt
                     )
                   |> gen_custom_cc inst
                in
                let mapkey_ct : mich_f list =
                   (* Precondition : container's type is map *)
                   match container_t.cc_v with
                   | MT_map _ ->
                     let mapkey = List.hd_exn mapkey_v in
                     [
                       (* 1. key is not the key of the container *)
                       MF_not
                         (MF_is_true
                            (MV_mem_xmb (mapkey, tb_container_v) |> gen_dummy_cc)
                         );
                       (* 2. CAR(elem_v) is equal to mapkey_v *)
                       MF_eq (MV_car elem_v |> gen_dummy_cc, mapkey);
                     ]
                   | _        -> []
                in
                ( {
                    si_mich = elem_v :: michst;
                    si_dip = dipst;
                    si_map_entry = tb_container_v :: mapentryst;
                    si_map_exit = tb_out_container_v :: mapexitst;
                    si_map_mapkey = mapkey_v @ mapkeyst;
                    si_iter = iterst;
                    si_balance = balance_v;
                    si_bc_balance = bc_balance_v;
                    si_param = tb_trx_image;
                  },
                  michct
                  @ dipct
                  @ mapentryct
                  @ mapexitct
                  @ mapkeyct
                  @ iterct
                  @ elem_ct
                  @ mapkey_ct
                  @ constraints_abp
                )
             in
             {
               ss_id = tb_ss_id;
               ss_start_mci = thenbr_mci;
               ss_block_mci = thenbr_mci;
               ss_start_si = tb_entry_si;
               ss_block_si = tb_entry_si;
               ss_param_history = blocked_state.ss_param_history;
               ss_constraints = tb_entry_constraints;
             }
          in
          (* be aware - between "after tb_symstate construction" and "before run-inst",
               update ctxt_sr (increase sid-counter)
               - becuase new sym-state constructed before.
          *)
          let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
          (* 3.2. run_inst_i recursive call *)
          let tb_sr_result_raw : se_result =
             run_inst_i i (ctxt_sr, tb_entry_ss)
          in
          (* 3.3. transform running states to blocked states *)
          {
            tb_sr_result_raw with
            sr_running = SSet.empty;
            sr_blocked =
              SSet.union
                (SSet.map tb_sr_result_raw.sr_running ~f:(fun rss ->
                     { rss with ss_block_mci = thenbr_mci }
                 )
                )
                tb_sr_result_raw.sr_blocked;
          }
       in
       (* 3. +. update ctxt_sr - override it using tb_result
          - it is okay to override since tb_result uses previous ctxt_sr in recursive call.
       *)
       let ctxt_sr : se_result = tb_result in
       (* 4. construct MAP instruction escaping sym-state (else-branch) *)
       let eb_symstate : sym_state =
          let eb_ss_id = [ ctxt_sr.sr_sid_counter ] in
          let eb_trx_image : trx_image =
             symbol_trx_image_context_swap eb_ss_id
               blocked_state.ss_block_si.si_param
          in
          let (eb_entry_si, eb_entry_constraints) : sym_image * mich_f list =
             let bsi = blocked_state.ss_block_si in
             let ctxt = eb_ss_id in
             let ccmaker = gen_custom_cc inst in
             let (michst, michct) =
                generate_symstack
                  ~f:(fun x -> MSC_mich_stack x)
                  ~ctxt ~ccmaker (List.tl_exn bsi.si_mich)
             in
             let (dipst, dipct) =
                generate_symstack
                  ~f:(fun x -> MSC_dip_stack x)
                  ~ctxt ~ccmaker bsi.si_dip
             in
             let (mapentryst, mapentryct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_entry_stack x)
                  ~ctxt ~ccmaker bsi.si_map_entry
             in
             let (mapexitst, mapexitct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_exit_stack x)
                  ~ctxt ~ccmaker bsi.si_map_exit
             in
             let (mapkeyst, mapkeyct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_mapkey_stack x)
                  ~ctxt ~ccmaker bsi.si_map_mapkey
             in
             let (iterst, iterct) =
                generate_symstack
                  ~f:(fun x -> MSC_iter_stack x)
                  ~ctxt ~ccmaker bsi.si_iter
             in
             let balance_v : mich_v cc =
                MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_balance, ctxt)
                |> gen_custom_cc inst
             in
             let bc_balance_v : mich_v cc =
                MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_bc_balance, ctxt)
                |> gen_custom_cc inst
             in
             let constraints_abp =
                ge_balance_amount_in_non_trx_entry_constraint
                  ~amount_v:eb_trx_image.ti_amount ~balance_v
                :: michv_maybe_mtznat_constraints ~v:eb_trx_image.ti_param
                @ [
                    mtz_comes_from_constraint ~mtz_v:eb_trx_image.ti_amount
                      ~from_v:balance_v;
                  ]
                @ amount_balance_mutez_constraints
                    ~amount_v:eb_trx_image.ti_amount ~balance_v ~bc_balance_v
             in
             (* no need to check & add constraints of eb_container_v or eb_out_container_v *)
             let eb_out_container_v =
                MV_symbol
                  (out_container_t, MSC_mich_stack (List.length michst), ctxt)
                |> gen_custom_cc inst
             in
             ( {
                 si_mich = eb_out_container_v :: michst;
                 si_dip = dipst;
                 si_map_entry = mapentryst;
                 si_map_exit = mapexitst;
                 si_map_mapkey = mapkeyst;
                 si_iter = iterst;
                 si_balance = balance_v;
                 si_bc_balance = bc_balance_v;
                 si_param = eb_trx_image;
               },
               michct
               @ dipct
               @ mapentryct
               @ mapexitct
               @ mapkeyct
               @ iterct
               @ constraints_abp
             )
          in
          {
            ss_id = eb_ss_id;
            ss_start_mci = elsebr_mci;
            ss_block_mci = elsebr_mci;
            ss_start_si = eb_entry_si;
            ss_block_si = eb_entry_si;
            ss_param_history = blocked_state.ss_param_history;
            ss_constraints = eb_entry_constraints;
          }
       in
       (* 4. +. update ctxt_sr - increase sid-counter
          - becuase new sym-state constructed before.
       *)
       let ctxt_sr : se_result = ctxt_sr_sid_counter_incr ctxt_sr in
       (* RETURN *)
       {
         (* remember - current ctxt_sr contains tb_result in "3. +." *)
         ctxt_sr with
         sr_running = SSet.singleton eb_symstate;
         sr_blocked = SSet.add ctxt_sr.sr_blocked blocked_state;
       }
     )
   (* | MI_iter -> TODO *)
   | MI_mem ->
     update_top_2_bmstack
       ~f:(fun (h, h2) ->
         let nv =
            match (typ_of_val h2).cc_v with
            | MT_set _     -> MV_mem_xsb (h, h2)
            | MT_map _     -> MV_mem_xmb (h, h2)
            | MT_big_map _ -> MV_mem_xbmb (h, h2)
            | _            -> failwith "run_inst_i : MI_mem : unexpected"
         in
         [ gen_custom_cc inst nv ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_get ->
     update_top_2_bmstack_and_constraint
       ~f:(fun (h, h2) ->
         match (typ_of_val h2).cc_v with
         | MT_map (_, t2)     ->
           let nv = MV_get_xmoy (h, h2) |> gen_custom_cc inst in
           ( [ nv ],
             [
               mtz_constriant_if_it_is_or_true ~tv:(t2, nv);
               nat_constriant_if_it_is_or_true ~tv:(t2, nv);
             ]
           )
         | MT_big_map (_, t2) ->
           let nv = MV_get_xbmo (h, h2) |> gen_custom_cc inst in
           ( [ nv ],
             [
               mtz_constriant_if_it_is_or_true ~tv:(t2, nv);
               nat_constriant_if_it_is_or_true ~tv:(t2, nv);
             ]
           )
         | _                  -> failwith "run_inst_i : MI_get : unexpected")
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_update ->
     update_top_3_bmstack
       ~f:(fun (h, h2, h3) ->
         let nv =
            match (typ_of_val h3).cc_v with
            | MT_set _     -> MV_update_xbss (h, h2, h3)
            | MT_map _     -> MV_update_xomm (h, h2, h3)
            | MT_big_map _ -> MV_update_xobmbm (h, h2, h3)
            | _            -> failwith "run_inst_i : MI_update : unexpected"
         in
         [ gen_custom_cc inst nv ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_if (i1, i2) ->
     (* let cond_value : mich_v cc = List.hd_exn (get_bmstack ss) in *)
     let cond_constraint : mich_f = MF_is_true (List.hd_exn (get_bmstack ss)) in
     (* then branch *)
     let then_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let else_br_ss =
           update_top_1_bmstack ~f:(fun _ -> []) ss
           |> add_constraints ~c:[ MF_not cond_constraint ]
           (* It is important to update sid of else-branch symbolic-state *)
           |> (fun ssss -> { ssss with ss_id = [ ctxt_sr.sr_sid_counter ] })
        in
        (* increase sid_counter since else_br takes new sid *)
        let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
        run_inst_i i2 (ctxt_sr, else_br_ss)
     in
     se_result_pointwise_union then_br_sr else_br_sr
   | MI_loop i ->
     (* refer MI_map case instead if you want to see
         the most detailed symbolic execution among loop instructions.
     *)
     let (outer_cutcat, inner_cutcat) = (MCC_ln_loop, MCC_lb_loop) in
     let (blocked_mci, thenbr_mci, elsebr_mci) =
        ( { mci_loc = inst.cc_loc; mci_cutcat = outer_cutcat },
          { mci_loc = inst.cc_loc; mci_cutcat = inner_cutcat },
          { mci_loc = inst.cc_loc; mci_cutcat = outer_cutcat }
        )
     in
     (* 1. Construct blocked-state *)
     let blocked_state : sym_state = { ss with ss_block_mci = blocked_mci } in
     (* 2. If this LOOP-instruction is the instruction already met before, return only blocked-state. *)
     if MciSet.mem ctxt_sr.sr_entered_loops blocked_mci
     then { ctxt_sr with sr_blocked = SSet.singleton blocked_state }
     else (
       (* 2. +. update ctxt_sr - add entered-loop *)
       let ctxt_sr : se_result =
          {
            ctxt_sr with
            sr_entered_loops = MciSet.add ctxt_sr.sr_entered_loops blocked_mci;
          }
       in
       (* 3. run-instruction inside LOOP instruction *)
       let tb_result : se_result =
          let tb_ss_id = [ ctxt_sr.sr_sid_counter ] in
          (* 3.1. construct entry sym-state *)
          let tb_entry_ss : sym_state =
             let tb_trx_image : trx_image =
                symbol_trx_image_context_swap tb_ss_id
                  blocked_state.ss_block_si.si_param
             in
             let (tb_entry_si, tb_entry_constraints) : sym_image * mich_f list =
                let bsi = blocked_state.ss_block_si in
                let ctxt = tb_ss_id in
                let ccmaker = gen_custom_cc inst in
                let (michst, michct) =
                   generate_symstack
                     ~f:(fun x -> MSC_mich_stack x)
                     ~ctxt ~ccmaker (List.tl_exn bsi.si_mich)
                in
                let (dipst, dipct) =
                   generate_symstack
                     ~f:(fun x -> MSC_dip_stack x)
                     ~ctxt ~ccmaker bsi.si_dip
                in
                let (mapentryst, mapentryct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_entry_stack x)
                     ~ctxt ~ccmaker bsi.si_map_entry
                in
                let (mapexitst, mapexitct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_exit_stack x)
                     ~ctxt ~ccmaker bsi.si_map_exit
                in
                let (mapkeyst, mapkeyct) =
                   generate_symstack
                     ~f:(fun x -> MSC_map_mapkey_stack x)
                     ~ctxt ~ccmaker bsi.si_map_mapkey
                in
                let (iterst, iterct) =
                   generate_symstack
                     ~f:(fun x -> MSC_iter_stack x)
                     ~ctxt ~ccmaker bsi.si_iter
                in
                let balance_v : mich_v cc =
                   MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_balance, ctxt)
                   |> gen_custom_cc inst
                in
                let bc_balance_v : mich_v cc =
                   MV_symbol
                     (MT_mutez |> gen_custom_cc inst, MSC_bc_balance, ctxt)
                   |> gen_custom_cc inst
                in
                let constraints_abp =
                   ge_balance_amount_in_non_trx_entry_constraint
                     ~amount_v:tb_trx_image.ti_amount ~balance_v
                   :: michv_maybe_mtznat_constraints ~v:tb_trx_image.ti_param
                   @ [
                       mtz_comes_from_constraint ~mtz_v:tb_trx_image.ti_amount
                         ~from_v:balance_v;
                     ]
                   @ amount_balance_mutez_constraints
                       ~amount_v:tb_trx_image.ti_amount ~balance_v ~bc_balance_v
                in
                ( {
                    si_mich = michst;
                    si_dip = dipst;
                    si_map_entry = mapentryst;
                    si_map_exit = mapexitst;
                    si_map_mapkey = mapkeyst;
                    si_iter = iterst;
                    si_balance = balance_v;
                    si_bc_balance = bc_balance_v;
                    si_param = tb_trx_image;
                  },
                  michct
                  @ dipct
                  @ mapentryct
                  @ mapexitct
                  @ mapkeyct
                  @ iterct
                  @ constraints_abp
                )
             in
             {
               ss_id = tb_ss_id;
               ss_start_mci = thenbr_mci;
               ss_block_mci = thenbr_mci;
               ss_start_si = tb_entry_si;
               ss_block_si = tb_entry_si;
               ss_param_history = blocked_state.ss_param_history;
               ss_constraints = tb_entry_constraints;
             }
          in
          (* be aware - between "after tb_symstate construction" and "before run-inst",
               update ctxt_sr (increase sid-counter)
               - becuase new sym-state constructed before.
          *)
          let ctxt_sr = ctxt_sr_sid_counter_incr ctxt_sr in
          (* 3.2. run_inst_i recursive call *)
          let tb_sr_result_raw : se_result =
             run_inst_i i (ctxt_sr, tb_entry_ss)
          in
          (* 3.3. transform running states to blocked states *)
          {
            tb_sr_result_raw with
            sr_running = SSet.empty;
            sr_blocked =
              SSet.union
                (SSet.map tb_sr_result_raw.sr_running ~f:(fun rss ->
                     { rss with ss_block_mci = thenbr_mci }
                 )
                )
                tb_sr_result_raw.sr_blocked;
          }
       in
       (* 3. +. update ctxt_sr - override it using tb_result
          - it is okay to override since tb_result uses previous ctxt_sr in recursive call.
       *)
       let ctxt_sr : se_result = tb_result in
       (* 4. construct LOOP instruction escaping sym-state (else-branch) *)
       let eb_symstate : sym_state =
          let eb_ss_id = [ ctxt_sr.sr_sid_counter ] in
          let eb_trx_image : trx_image =
             symbol_trx_image_context_swap eb_ss_id
               blocked_state.ss_block_si.si_param
          in
          let (eb_entry_si, eb_entry_constraints) : sym_image * mich_f list =
             let bsi = blocked_state.ss_block_si in
             let ctxt = eb_ss_id in
             let ccmaker = gen_custom_cc inst in
             let (michst, michct) =
                generate_symstack
                  ~f:(fun x -> MSC_mich_stack x)
                  ~ctxt ~ccmaker (List.tl_exn bsi.si_mich)
             in
             let (dipst, dipct) =
                generate_symstack
                  ~f:(fun x -> MSC_dip_stack x)
                  ~ctxt ~ccmaker bsi.si_dip
             in
             let (mapentryst, mapentryct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_entry_stack x)
                  ~ctxt ~ccmaker bsi.si_map_entry
             in
             let (mapexitst, mapexitct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_exit_stack x)
                  ~ctxt ~ccmaker bsi.si_map_exit
             in
             let (mapkeyst, mapkeyct) =
                generate_symstack
                  ~f:(fun x -> MSC_map_mapkey_stack x)
                  ~ctxt ~ccmaker bsi.si_map_mapkey
             in
             let (iterst, iterct) =
                generate_symstack
                  ~f:(fun x -> MSC_iter_stack x)
                  ~ctxt ~ccmaker bsi.si_iter
             in
             let balance_v : mich_v cc =
                MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_balance, ctxt)
                |> gen_custom_cc inst
             in
             let bc_balance_v : mich_v cc =
                MV_symbol (MT_mutez |> gen_custom_cc inst, MSC_bc_balance, ctxt)
                |> gen_custom_cc inst
             in
             let constraints_abp =
                ge_balance_amount_in_non_trx_entry_constraint
                  ~amount_v:eb_trx_image.ti_amount ~balance_v
                :: michv_maybe_mtznat_constraints ~v:eb_trx_image.ti_param
                @ [
                    mtz_comes_from_constraint ~mtz_v:eb_trx_image.ti_amount
                      ~from_v:balance_v;
                  ]
                @ amount_balance_mutez_constraints
                    ~amount_v:eb_trx_image.ti_amount ~balance_v ~bc_balance_v
             in
             ( {
                 si_mich = michst;
                 si_dip = dipst;
                 si_map_entry = mapentryst;
                 si_map_exit = mapexitst;
                 si_map_mapkey = mapkeyst;
                 si_iter = iterst;
                 si_balance = balance_v;
                 si_bc_balance = bc_balance_v;
                 si_param = eb_trx_image;
               },
               michct
               @ dipct
               @ mapentryct
               @ mapexitct
               @ mapkeyct
               @ iterct
               @ constraints_abp
             )
          in
          {
            ss_id = eb_ss_id;
            ss_start_mci = elsebr_mci;
            ss_block_mci = elsebr_mci;
            ss_start_si = eb_entry_si;
            ss_block_si = eb_entry_si;
            ss_param_history = blocked_state.ss_param_history;
            ss_constraints = eb_entry_constraints;
          }
       in
       (* 4. +. update ctxt_sr - increase sid-counter
          - becuase new sym-state constructed before.
       *)
       let ctxt_sr : se_result = ctxt_sr_sid_counter_incr ctxt_sr in
       (* RETURN *)
       {
         (* remember - current ctxt_sr contains tb_result in "3. +." *)
         ctxt_sr with
         sr_running = SSet.singleton eb_symstate;
         sr_blocked = SSet.add ctxt_sr.sr_blocked blocked_state;
       }
     )
   (* | MI_loop_left i -> TODO *)
   (* | MI_lambda (t1, t2, i) -> TODO *)
   (* | MI_exec -> TODO *)
   (* | MI_apply -> TODO *)
   (* | MI_dip_n (zn, i) -> TODO *)
   | MI_failwith ->
     (* 1. set block_mci
        2. enroll this sym_state to sr_terminated
     *)
     let bmci = { mci_loc = inst.cc_loc; mci_cutcat = MCC_trx_exit } in
     {
       se_result_empty with
       sr_terminated = SSet.singleton { ss with ss_block_mci = bmci };
     }
   (* | MI_cast t -> TODO *)
   | MI_rename ->
     update_top_1_bmstack ~f:(fun x -> [ { x with cc_anl = inst.cc_anl } ]) ss
     |> running_ss_to_sr ctxt_sr
   (* | MI_concat -> TODO *)
   (* | MI_slice -> TODO *)
   (* | MI_pack -> TODO *)
   (* | MI_unpack t -> TODO *)
   | MI_add -> (
     let add_gen_sr : mich_v * mich_f list -> se_result =
       fun (mv, csl) ->
       update_top_2_bmstack_and_constraint
         ~f:(fun _ -> ([ mv |> gen_custom_cc inst ], csl))
         ss
       |> running_ss_to_sr ctxt_sr
     in
     let (h, h2) = get_bmstack_2 ss in
     match ((typ_of_val h).cc_v, (typ_of_val h2).cc_v) with
     | (MT_nat, MT_int)       -> add_gen_sr (MV_add_nii (h, h2), [])
     | (MT_int, MT_nat)       -> add_gen_sr (MV_add_ini (h, h2), [])
     | (MT_int, MT_int)       -> add_gen_sr (MV_add_iii (h, h2), [])
     | (MT_nat, MT_nat)       ->
       add_gen_sr
         ( MV_add_nnn (h, h2),
           [ MF_nat_bound (MV_add_nnn (h, h2) |> gen_custom_cc inst) ]
         )
     | (MT_timestamp, MT_int) -> add_gen_sr (MV_add_tit (h, h2), [])
     | (MT_int, MT_timestamp) -> add_gen_sr (MV_add_itt (h, h2), [])
     | (MT_mutez, MT_mutez)   ->
       let nv = MV_add_mmm (h, h2) |> gen_custom_cc inst in
       let qstate : sym_state =
          {
            ss with
            ss_block_mci =
              {
                mci_loc = inst.cc_loc;
                mci_cutcat = MCC_query Q_mutez_add_no_overflow;
              };
          }
       in
       let rstate : sym_state =
          update_top_2_bmstack_and_constraint
            ~f:(fun _ -> ([ nv ], [ MF_mutez_bound nv ]))
            ss
       in
       {
         (running_ss_to_sr ctxt_sr rstate) with
         sr_queries = SSet.singleton qstate;
       }
     | _                      -> failwith "run_inst_i : MI_add : unexpected"
   )
   | MI_sub -> (
     let sub_gen_sr : mich_v -> se_result =
       fun mv ->
       update_top_2_bmstack_and_constraint
         ~f:(fun _ -> ([ mv |> gen_custom_cc inst ], []))
         ss
       |> running_ss_to_sr ctxt_sr
     in
     let (h, h2) = get_bmstack_2 ss in
     match ((typ_of_val h).cc_v, (typ_of_val h2).cc_v) with
     | (MT_nat, MT_nat) -> MV_sub_nni (h, h2) |> sub_gen_sr
     | (MT_nat, MT_int) -> MV_sub_nii (h, h2) |> sub_gen_sr
     | (MT_int, MT_nat) -> MV_sub_ini (h, h2) |> sub_gen_sr
     | (MT_int, MT_int) -> MV_sub_iii (h, h2) |> sub_gen_sr
     | (MT_timestamp, MT_timestamp) -> MV_sub_tti (h, h2) |> sub_gen_sr
     | (MT_timestamp, MT_int) -> MV_sub_tit (h, h2) |> sub_gen_sr
     | (MT_mutez, MT_mutez) ->
       let nv = MV_sub_mmm (h, h2) |> gen_custom_cc inst in
       let qstate : sym_state =
          {
            ss with
            ss_block_mci =
              {
                mci_loc = inst.cc_loc;
                mci_cutcat = MCC_query Q_mutez_sub_no_underflow;
              };
          }
       in
       let rstate : sym_state =
          update_top_2_bmstack_and_constraint
            ~f:(fun _ -> ([ nv ], [ MF_mutez_bound nv ]))
            ss
       in
       {
         (running_ss_to_sr ctxt_sr rstate) with
         sr_queries = SSet.singleton qstate;
       }
     | _ -> failwith "run_inst_i : MI_sub : unexpected"
   )
   | MI_mul -> (
     let mul_gen_sr : mich_v * mich_f list -> se_result =
       fun (mv, csl) ->
       update_top_2_bmstack_and_constraint
         ~f:(fun _ -> ([ mv |> gen_custom_cc inst ], csl))
         ss
       |> running_ss_to_sr ctxt_sr
     in
     let (h, h2) = get_bmstack_2 ss in
     match ((typ_of_val h).cc_v, (typ_of_val h2).cc_v) with
     | (MT_nat, MT_nat)   ->
       mul_gen_sr
         ( MV_mul_nnn (h, h2),
           [ MF_mutez_bound (MV_mul_nnn (h, h2) |> gen_custom_cc inst) ]
         )
     | (MT_nat, MT_int)   -> mul_gen_sr (MV_mul_nii (h, h2), [])
     | (MT_int, MT_nat)   -> mul_gen_sr (MV_mul_ini (h, h2), [])
     | (MT_int, MT_int)   -> mul_gen_sr (MV_mul_iii (h, h2), [])
     | (MT_mutez, MT_nat) ->
       let nv = MV_mul_mnm (h, h2) |> gen_custom_cc inst in
       let qstate : sym_state =
          {
            ss with
            ss_block_mci =
              {
                mci_loc = inst.cc_loc;
                mci_cutcat = MCC_query Q_mutez_mul_mnm_no_overflow;
              };
          }
       in
       let rstate : sym_state =
          update_top_2_bmstack_and_constraint
            ~f:(fun _ -> ([ nv ], [ MF_mutez_bound nv ]))
            ss
       in
       {
         (running_ss_to_sr ctxt_sr rstate) with
         sr_queries = SSet.singleton qstate;
       }
     | (MT_nat, MT_mutez) ->
       let nv = MV_mul_nmm (h, h2) |> gen_custom_cc inst in
       let qstate : sym_state =
          {
            ss with
            ss_block_mci =
              {
                mci_loc = inst.cc_loc;
                mci_cutcat = MCC_query Q_mutez_mul_nmm_no_overflow;
              };
          }
       in
       let rstate : sym_state =
          update_top_2_bmstack_and_constraint
            ~f:(fun _ -> ([ nv ], [ MF_mutez_bound nv ]))
            ss
       in
       {
         (running_ss_to_sr ctxt_sr rstate) with
         sr_queries = SSet.singleton qstate;
       }
     | _                  -> failwith "run_inst_i : MI_mul : unexpected"
   )
   | MI_ediv -> (
     let ediv_gen_sr : mich_v -> se_result =
       fun mv ->
       update_top_2_bmstack_and_constraint
         ~f:(fun _ -> ([ mv |> gen_custom_cc inst ], []))
         ss
       |> running_ss_to_sr ctxt_sr
     in
     let (h, h2) = get_bmstack_2 ss in
     match ((typ_of_val h).cc_v, (typ_of_val h2).cc_v) with
     | (MT_nat, MT_nat)     -> MV_ediv_nnnn (h, h2) |> ediv_gen_sr
     | (MT_nat, MT_int)     -> MV_ediv_niin (h, h2) |> ediv_gen_sr
     | (MT_int, MT_nat)     -> MV_ediv_inin (h, h2) |> ediv_gen_sr
     | (MT_int, MT_int)     -> MV_ediv_iiin (h, h2) |> ediv_gen_sr
     | (MT_mutez, MT_nat)   -> MV_ediv_mnmm (h, h2) |> ediv_gen_sr
     | (MT_mutez, MT_mutez) -> MV_ediv_mmnm (h, h2) |> ediv_gen_sr
     | _                    -> failwith "run_inst_i : MI_ediv : unexpected"
   )
   (* | MI_abs -> TODO *)
   | MI_isnat ->
     update_top_1_bmstack ~f:(fun x -> [ MV_isnat x |> gen_custom_cc inst ]) ss
     |> running_ss_to_sr ctxt_sr
   (* | MI_int -> TODO *)
   (* | MI_neg -> TODO *)
   (* | MI_lsl -> TODO *)
   (* | MI_lsr -> TODO *)
   (* | MI_or -> TODO *)
   (* | MI_and -> TODO *)
   (* | MI_xor -> TODO *)
   (* | MI_not -> TODO *)
   | MI_compare ->
     update_top_2_bmstack
       ~f:(fun (x, y) -> [ MV_compare (x, y) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_eq ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_eq_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_neq ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_neq_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_lt ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_lt_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_gt ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_gt_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_le ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_leq_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_ge ->
     update_top_1_bmstack
       ~f:(fun x ->
         [
           MV_geq_ib (x, MV_lit_int Bigint.zero |> gen_custom_cc inst)
           |> gen_custom_cc inst;
         ])
       ss
     |> running_ss_to_sr ctxt_sr
   (* | MI_self -> TODO *)
   | MI_contract t ->
     update_top_1_bmstack
       ~f:(fun x -> [ MV_contract_of_address (t, x) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_transfer_tokens ->
     update_top_3_bmstack
       ~f:(fun (x, y, z) ->
         [ MV_transfer_tokens (x, y, z) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_set_delegate ->
     update_top_1_bmstack
       ~f:(fun x -> [ MV_set_delegate x |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr ctxt_sr
   (* | MI_create_account -> TODO *)
   (* | MI_create_contract (t1, t2, i) -> TODO *)
   (* | MI_implicit_account -> TODO *)
   (* | MI_now -> TODO *)
   | MI_amount ->
     let amount_v = ss.ss_block_si.si_param.ti_amount in
     push_bmstack ss ~v:amount_v
     |> add_mtz_constraint_if_it_is ~tv:(MT_mutez |> gen_dummy_cc, amount_v)
     |> running_ss_to_sr ctxt_sr
   | MI_balance ->
     let balance_v = ss.ss_block_si.si_balance in
     push_bmstack ss ~v:balance_v
     |> add_mtz_constraint_if_it_is ~tv:(MT_mutez |> gen_dummy_cc, balance_v)
     |> running_ss_to_sr ctxt_sr
   (* | MI_check_signature -> TODO *)
   (* | MI_blake2b -> TODO *)
   (* | MI_sha256 -> TODO *)
   (* | MI_sha512 -> TODO *)
   (* | MI_hash_key -> TODO *)
   (* | MI_steps_to_quota -> TODO *)
   | MI_source ->
     let source_v = ss.ss_block_si.si_param.ti_source in
     push_bmstack ss ~v:source_v |> running_ss_to_sr ctxt_sr
   | MI_sender ->
     let sender_v = ss.ss_block_si.si_param.ti_sender in
     push_bmstack ss ~v:sender_v |> running_ss_to_sr ctxt_sr
   (* | MI_address -> TODO *)
   (* | MI_chain_id -> TODO *)
   | MI_unpair ->
     (* let _ = (* DEBUG *) print_endline "unpair enter" in *)
     update_top_1_bmstack_and_constraint
       ~f:(fun x ->
         let a_v = MV_car x |> gen_custom_cc inst in
         let d_v = MV_cdr x |> gen_custom_cc inst in
         let a_tv = (typ_of_val a_v, a_v) in
         let d_tv = (typ_of_val d_v, d_v) in
         ( [ a_v; d_v ],
           [
             mtz_constriant_if_it_is_or_true ~tv:a_tv;
             nat_constriant_if_it_is_or_true ~tv:a_tv;
             mtz_constriant_if_it_is_or_true ~tv:d_tv;
             nat_constriant_if_it_is_or_true ~tv:d_tv;
           ]
         ))
       ss
     |> running_ss_to_sr ctxt_sr
   | MI_micse_check i ->
     (* dealing with micse-check
         - bring running states from the result of "i", and convert them to queries.
         - se_result might be go wrong when any loop-like instructions
           (LOOP, LOOP_LEFT, ITER, MAP) are inserted in micse-check instruction.
     *)
     let micse_check_se_result : se_result = run_inst_i i (ctxt_sr, ss) in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr micse_check_se_result in
     {
       ctxt_sr with
       sr_running = SSet.singleton ss;
       (* If MI_micse_check allows loop-like instructions, it should be
          considered to add blocked-states in micse_check_se_result in return value.
       *)
       sr_blocked = SSet.empty;
       sr_queries =
         SSet.map micse_check_se_result.sr_running ~f:(fun rs ->
             {
               rs with
               ss_block_mci =
                 { mci_loc = inst.cc_loc; mci_cutcat = MCC_query Q_assertion };
             }
         );
       sr_terminated = SSet.empty;
     }
   | _ ->
     failwith
       ("run_inst_i : wildcard match triggered : "
       ^ (sexp_of_mich_i inst.cc_v |> SexpUtil.tz_cc_sexp_form |> Sexp.to_string)
       )
(* function run_inst_i end *)

let run_inst_entry :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
   let open Tz in
   fun (pt, st, c) ->
   let final_blocking : sym_state -> sym_state =
     fun ss ->
     let op_mtz_v : mich_v cc =
        MV_mtz_of_op_list
          (MV_car (List.hd_exn ss.ss_block_si.si_mich) |> gen_custom_cc c)
        |> gen_custom_cc c
     in
     let new_balance : mich_v cc =
        MV_sub_mmm (ss.ss_block_si.si_balance, op_mtz_v) |> gen_custom_cc c
     in
     let new_bc_balance : mich_v cc =
        MV_add_mmm (ss.ss_block_si.si_bc_balance, op_mtz_v) |> gen_custom_cc c
     in
     {
       (* If operation-mutez-subtraction policy turned on, we need to add balance-related constraint here. *)
       ss
       with
       ss_block_mci = { mci_loc = c.cc_loc; mci_cutcat = MCC_trx_exit };
       ss_block_si =
         {
           ss.ss_block_si with
           si_balance = new_balance;
           si_bc_balance = new_bc_balance;
         };
       ss_constraints =
         mtz_comes_from_constraint ~mtz_v:op_mtz_v
           ~from_v:ss.ss_block_si.si_balance
         :: amount_balance_mutez_constraints ~amount_v:op_mtz_v
              ~balance_v:new_balance ~bc_balance_v:new_bc_balance
         @ ss.ss_constraints;
     }
   in
   let result_raw = run_inst c (run_inst_initial_se_result (pt, st, c)) in
   (* let _ =
         (* DEBUG *)
         print_endline
           ("result_raw running = "
           ^ (SSet.length result_raw.sr_running |> string_of_int)
           ^ ", blocked = "
           ^ (SSet.length result_raw.sr_blocked |> string_of_int)
           )
      in *)
   {
     result_raw with
     sr_running = SSet.empty;
     sr_blocked =
       SSet.union
         (SSet.map result_raw.sr_blocked ~f:final_blocking)
         result_raw.sr_running;
   }
(* function run_inst_entry end *)