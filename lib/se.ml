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

(* let run_inst_initial_sym_image : (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc) -> Tz.sym_image =
     let open Tz in
     fun (param_t, strg_t) ->
     let i_sc = 0 in (* initial sid_counter *)
     let i_scx = [i_sc] in (* initial sym_ctxt *)
     let initial_ti : trx_image = {
       ti_contract = MV_symbol (MT_contract param_t, MSC_contract, i_scx);
       ti_source = MV_symbol ();
       ti_sender = MV_symbol ();
       ti_param = MV_symbol ();
       ti_amount = MV_symbol ();
       ti_time = MV_symbol ();
     } in
     {
     si_mich
   } *)

let run_inst_entry :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
  (* fun (param_t, strg_t, code) ->
     let initial_state : Tz.sym_state = {
          ss_id =
        } in
         run_inst code { se_result_empty with sr_running=(SSet.singleton initial_state);} *)
  (fun _ -> failwith "run_inst_entry")

let rec run_inst : Tz.mich_i Tz.cc -> se_result -> se_result =
  fun inst sr ->
  SSet.fold sr.sr_running ~init:{ sr with sr_running = SSet.empty }
    ~f:(fun acc_sr ss -> se_result_pointwise_union (run_inst_i inst ss) acc_sr
  )

and run_inst_i : Tz.mich_i Tz.cc -> Tz.sym_state -> se_result =
  (fun _ -> failwith "run_inst_i")
