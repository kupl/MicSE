(* Se is a symbolic execution module based on Tz.sym_state definition *)

open! Core

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet = Core.Set.Make (Tz.SymState_cmp)
module MciSet = Core.Set.Make (Tz.MichCutInfo_cmp)
(* module SSet = Core.Set.Make(struct type t = Tz.sym_state [@@deriving sexp, compare, equal] end)
   module MciSet = Core.Set.Make(struct type t = Tz.mich_cut_info [@@deriving sexp, compare, equal] end) *)

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

let run_inst_initial_se_result :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
   let open Tz in
   fun (param_tcc, strg_tcc, code) ->
   (* sid_counter & sym_ctxt *)
   let scounter = 0
   and sctxt = [ 0 ] in
   (* mich_t cc values *)
   let cur_contract_tcc = MT_contract param_tcc |> gen_dummy_cc
   and addr_tcc = MT_address |> gen_dummy_cc
   and mutez_tcc = MT_mutez |> gen_dummy_cc
   and time_tcc = MT_timestamp |> gen_dummy_cc
   and paramstrg_tcc = MT_pair (param_tcc, strg_tcc) |> gen_dummy_cc in
   (* initial mich_cut_info *)
   let init_mci = { mci_loc = code.cc_loc; mci_cutcat = MCC_trx_entry } in
   (* beginning trx-image *)
   let beginning_ti : trx_image =
      {
        ti_contract =
          MV_symbol (cur_contract_tcc, MSC_contract, sctxt) |> gen_dummy_cc;
        ti_source = MV_symbol (addr_tcc, MSC_source, sctxt) |> gen_dummy_cc;
        ti_sender = MV_symbol (addr_tcc, MSC_sender, sctxt) |> gen_dummy_cc;
        ti_param = MV_symbol (param_tcc, MSC_param, sctxt) |> gen_dummy_cc;
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
          (let lit_total_mutez_amount =
              MV_lit_mutez (Bigint.of_int64 Int64.max_value) |> gen_dummy_cc
           in
           [
             (* 1. first stack's CAR is parameter-value *)
             MF_eq
               ( beginning_ti.ti_param,
                 MV_car (List.hd_exn beginning_si.si_mich) |> gen_dummy_cc
               );
             (* 2. amount, balance, and bc_balance are mutez values *)
             MF_mutez_bound beginning_ti.ti_amount;
             MF_mutez_bound beginning_si.si_balance;
             MF_mutez_bound beginning_si.si_bc_balance;
             (* 3. amount is less-or-equal than bc_balance *)
             MF_is_true
               (MV_leq_ib (beginning_ti.ti_amount, beginning_si.si_bc_balance)
               |> gen_dummy_cc
               );
             (* 4. (balance + bc_balance) is equal to total-mutez-amount *)
             MF_eq
               ( MV_add_mmm (beginning_si.si_balance, beginning_si.si_bc_balance)
                 |> gen_dummy_cc,
                 lit_total_mutez_amount
               );
           ]
          );
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

let rec run_inst : Tz.mich_i Tz.cc -> se_result -> se_result =
  fun inst sr ->
  SSet.fold sr.sr_running ~init:{ sr with sr_running = SSet.empty }
    ~f:(fun acc_sr ss -> se_result_pointwise_union (run_inst_i inst ss) acc_sr
  )

and run_inst_i : Tz.mich_i Tz.cc -> Tz.sym_state -> se_result =
  (fun _ -> failwith "run_inst_i")

let run_inst_entry :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
  (fun (pt, st, c) -> run_inst c (run_inst_initial_se_result (pt, st, c)))
