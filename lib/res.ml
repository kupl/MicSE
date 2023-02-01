(* Result of MicSE *)

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_v Tz.cc *)
module MVSet = Set.Make (Tz.MichVCC_cmp)

(* Set of set of Tz.mich_f *)
module MFSSet = Set.Make (Tz.MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.qid *)
module QIDSet = Set.Make (Tz.QId_cmp)

(* Map of Tz.qid *)
module QIDMap = Map.Make (Tz.QId_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Inv.cand *)
module CSet = Set.Make (Inv.Cand_cmp)

(* Set of Inv.inv_map *)
module InvSet = Set.Make (Inv.InvMap_cmp)

(* Map of MState.summary *)
module SMYMap = Map.Make (MState.SMY_cmp)

(******************************************************************************)
(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
(******************************************************************************)

(* Flags **********************************************************************)

type prover_flag =
  | PF_p (* proved  *)
  | PF_u (* unknown *)
  | PF_f
(* failed - no invariant candidate combination left *)
[@@deriving sexp, compare, equal]

type refuter_flag =
  | RF_r (* refuted *)
  | RF_u (* unknown *)
  | RF_f
(* failed - no ppath left to check *)
[@@deriving sexp, compare, equal]

(* Partial Path ***************************************************************)

module PPath = struct
  type t = {
    pp_mstate : MState.t;
    pp_satisfiability : Smt.Solver.satisfiability option;
    pp_score : int list;
  }
  [@@deriving sexp, compare, equal]

  let t_of_ss : Tz.sym_state -> t =
    fun ss ->
    { pp_mstate = MState.init ss; pp_satisfiability = None; pp_score = [] }
  (* function t_of_ss end *)

  let satisfiability_fill :
      Smt.Ctx.t * Smt.Solver.t -> t -> t * Smt.Model.t option =
    fun (ctx, slvr) ppath ->
    match ppath.pp_satisfiability with
    | None   ->
      let (vc : Tz.mich_f) = Vc.gen_sp_from_ms ppath.pp_mstate Tz.MF_true in
      let (sat_result, mdl_opt) = Vc.check_sat ctx slvr vc in
      ({ ppath with pp_satisfiability = Some sat_result }, mdl_opt)
    | Some _ -> (ppath, None)

  let rec extract_ppath_from_first_trx : t -> t list =
     let open Tz in
     let proper_mcc : mich_cut_info -> bool =
       fun mci ->
       match mci.mci_cutcat with
       | MCC_trx_exit
       | MCC_query _ ->
         true
       | _ -> false
       (* inner-function proper_mcc end *)
     in
     fun ppath ->
     let (hd : Tz.sym_state) = MState.get_first_ss ppath.pp_mstate in
     if proper_mcc hd.ss_block_mci
     then [ ppath ]
     else (
       let (tl : MState.t) = MState.get_tail_ms ppath.pp_mstate in
       ppath :: extract_ppath_from_first_trx { ppath with pp_mstate = tl }
     )
  (* function extract_ppath_from_first_trx end *)
end

module PPSet = Set.Make (PPath)

(* Query Result ***************************************************************)

type qres = {
  qr_qid : Tz.qid;
  (* Overall Status *)
  qr_prv_flag : prover_flag;
  qr_rft_flag : refuter_flag;
  (* Query States *)
  qr_unk_qs : SSet.t;
  (* Partial Paths and Invariant Candidates *)
  (* debugging *) qr_validated_ppaths : PPath.t list;
  (* debugging *) qr_total_ppaths : (PPath.t * Smt.Solver.satisfiability) list;
  qr_last_picked_paths : PPSet.t;
  qr_exp_ppaths : PPSet.t;
  qr_prec_map : CSet.t SMYMap.t;
  qr_rft_ppath : (PPath.t * Smt.Model.t) option;
  (* Count expanding_ppaths *)
  qr_exp_cnt : int;
  (* Cooperation penalty *)
  qr_coop_penalty : int;
  qr_coop_stoptil : int;
}
[@@deriving sexp, compare, equal]

module QRes_cmp = struct
  type t = qres [@@deriving sexp, compare]
end

(* Result *********************************************************************)

type res = {
  r_qr_lst : qres list;
  r_inv : Inv.inv_map;
  r_cands : Inv.cand_map;
  r_idts : Inv.inductive_info;
  r_comb_cnt : int;
}
[@@deriving sexp, compare, equal]

(* Configuration **************************************************************)

type config = {
  (* Execution configuration *)
  cfg_timer : Utils.Time.t;
  cfg_memory : Utils.Memory.t;
  (* Environment for SMT solver *)
  cfg_smt_ctxt : Smt.Ctx.t;
  cfg_smt_slvr : Smt.Solver.t;
  (* Information from symbolic execution *)
  cfg_istate : Tz.sym_state;
  cfg_istrg : Tz.mich_v Tz.cc;
  cfg_se_res : Se.se_result;
  cfg_m_view : Se.SSGraph.mci_view;
  cfg_qid_set : QIDSet.t;
  cfg_trx_paths : MState.t list;
  cfg_query_paths : MState.t list QIDMap.t;
  (* Ingrdients for invariant synthesis *)
  cfg_imap : Igdt.igdts_map;
  (* Top-k setting *)
  cfg_ppath_k : int;
  cfg_cand_k : int;
  cfg_comb_k : int;
}
[@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Initialization                                                             *)
(******************************************************************************)
(******************************************************************************)

(* EXPERIMENT MODE *)
let exp_prover_no_benefit_mode : bool ref = ref false

let exp_prover_path_pool : PPSet.t QIDMap.t ref = ref QIDMap.empty

let exp_prover_compare_func : PPath.t -> PPath.t -> int =
  fun p1 p2 ->
  compare_int (MState.get_length p1.pp_mstate) (MState.get_length p2.pp_mstate)

(* EXPERIMENT MODE *)

let init_qres : Tz.qid -> SSet.t -> qres =
  fun qr_qid qr_unk_qs ->
  let exp_ppaths = PPSet.map qr_unk_qs ~f:(fun ss -> PPath.t_of_ss ss) in
  let _ =
     (* EXPERIMENT MODE *)
     if not !exp_prover_no_benefit_mode
     then ()
     else
       exp_prover_path_pool :=
         QIDMap.update !exp_prover_path_pool qr_qid ~f:(fun _ -> exp_ppaths)
     (* EXPERIMENT MODE *)
  in
  {
    qr_qid;
    qr_prv_flag = PF_u;
    qr_rft_flag = RF_u;
    qr_unk_qs;
    qr_validated_ppaths = [];
    qr_total_ppaths = [];
    qr_last_picked_paths = PPSet.empty;
    qr_exp_ppaths = exp_ppaths;
    qr_prec_map = SMYMap.empty;
    qr_rft_ppath = None;
    qr_exp_cnt = SSet.length qr_unk_qs;
    qr_coop_penalty = 0;
    qr_coop_stoptil = 0;
  }
(* function init_qres end *)

let init_res : config -> res =
   let open Se in
   let open Vc in
   fun {
         cfg_se_res;
         cfg_qid_set;
         cfg_imap;
         cfg_smt_ctxt;
         cfg_smt_slvr;
         cfg_istrg;
         cfg_istate;
         _;
       } ->
   let _ = Utils.Log.debug (fun m -> m "Res.init_res:acc_qsmap start") in
   let (acc_qsmap : SSet.t QIDMap.t) =
      SSet.fold cfg_se_res.sr_queries ~init:QIDMap.empty ~f:(fun acc_qsmap qs ->
          QIDMap.update acc_qsmap (TzUtil.qid_of_mci_exn qs.ss_block_mci)
            ~f:(function
          | Some s -> SSet.add s qs
          | None   -> SSet.singleton qs
          )
      )
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_res:acc_qsmap end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_res:r_qr_lst start") in
   let (r_qr_lst : qres list) =
      QIDMap.to_alist acc_qsmap
      |> List.map ~f:(fun (qr_qid, qr_unk_qs) -> init_qres qr_qid qr_unk_qs)
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_res:r_qr_lst end") in
   let _ =
      Utils.Log.debug (fun m ->
          m "Res.init_res : result value construction start"
      )
   in
   let result =
      {
        r_qr_lst;
        r_inv = Inv.gen_true_inv_map cfg_se_res;
        r_cands =
          Inv.gen_initial_cand_map
            ~is_cand_sat:(is_cand_sat cfg_smt_ctxt cfg_smt_slvr)
            ~do_cand_sat_istrg:
              (do_cand_sat_istrg cfg_smt_ctxt cfg_smt_slvr cfg_istrg cfg_istate)
            cfg_qid_set cfg_imap;
        r_idts = Inv.gen_initial_inductive_info_map cfg_se_res.sr_blocked;
        r_comb_cnt = 0;
      }
   in
   let _ =
      Utils.Log.debug (fun m -> m "Res.init_res : result value construction end")
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_res end") in
   result
(* function init_res end *)

let init_config :
    Tz.mich_i Tz.cc ->
    Tz.mich_v Tz.cc option ->
    Se.se_result ->
    Tz.sym_state ->
    config =
   let open Vc in
   fun cfg_code cfg_istrg_opt cfg_se_res cfg_istate ->
   let (mv_literal_set : MVSet.t) = TzUtil.scrap_code_literals cfg_code in
   (* Execution configuration *)
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_timer start") in
   let (cfg_timer : Utils.Time.t) =
      Utils.Time.create
        ~budget:!Utils.Argument.total_timeout
        () ~key_lst:[ "report" ]
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_timer end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_memory start") in
   let (cfg_memory : Utils.Memory.t) =
      Utils.Memory.create ~budget:!Utils.Argument.memory_bound ()
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_memory end") in
   (* Environment for SMT solver *)
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_smt_ctxt start") in
   let (cfg_smt_ctxt : Smt.Ctx.t) = Vc.gen_ctx () in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_smt_ctxt end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_smt_slvr start") in
   let (cfg_smt_slvr : Smt.Solver.t) = Vc.gen_solver cfg_smt_ctxt in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_smt_slvr end") in
   (* Information from symbolic execution *)
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_istrg start") in
   let (cfg_istrg : Tz.mich_v Tz.cc) =
      match cfg_istrg_opt with
      | Some v -> v
      | None   -> failwith "ExecFlow : config_base : cfg_istrg = None"
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_istrg end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_m_view start") in
   let (cfg_m_view : Se.SSGraph.mci_view) =
      Se.SSGraph.construct_mci_view ~basic_blocks:cfg_se_res.sr_blocked
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_m_view end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_qid_set start") in
   let (cfg_qid_set : QIDSet.t) =
      SSet.fold cfg_se_res.sr_queries ~init:QIDSet.empty
        ~f:(fun cfg_qid_set qs ->
          QIDSet.add cfg_qid_set (TzUtil.qid_of_mci_exn qs.ss_block_mci)
      )
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_qid_set end") in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_*_paths start") in
   let ( (cfg_trx_paths : MState.t list),
         (cfg_query_paths : MState.t list QIDMap.t)
       ) =
      MState.gen_trx_paths
        ~is_path_sat:(is_path_sat cfg_smt_ctxt cfg_smt_slvr)
        cfg_se_res.sr_queries cfg_m_view
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_*_paths end") in
   (* Ingrdients for invariant synthesis *)
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_imap start") in
   let (cfg_imap : Igdt.igdts_map) =
      Igdt.get_igdts_map cfg_se_res.sr_blocked cfg_istrg mv_literal_set
   in
   let _ = Utils.Log.debug (fun m -> m "Res.init_config:cfg_imap end") in
   {
     (* Execution configuration *)
     cfg_timer;
     cfg_memory;
     (* Environment for SMT solver *)
     cfg_smt_ctxt;
     cfg_smt_slvr;
     (* Information from symbolic execution *)
     cfg_istate;
     cfg_istrg;
     cfg_se_res;
     cfg_m_view;
     cfg_qid_set;
     cfg_trx_paths;
     cfg_query_paths;
     (* Ingrdients for invariant synthesis *)
     cfg_imap;
     (* Top-k setting *)
     cfg_ppath_k = 5;
     cfg_cand_k = 2;
     cfg_comb_k = 50;
   }
(* function init_config end *)

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

module QRSet = Set.Make (QRes_cmp)

type qres_classified = {
  (* proved           *)
  qrc_p : QRSet.t;
  (* refuted          *)
  qrc_r : QRSet.t;
  (* error case       *)
  qrc_err : QRSet.t;
  (* unknown-unknown  *)
  qrc_uu : QRSet.t;
  (* unknown-failed   *)
  qrc_uf : QRSet.t;
  (* failed-unknown   *)
  qrc_fu : QRSet.t;
  (* failed-failed    *)
  qrc_ff : QRSet.t;
}

let classify_qres : res -> qres_classified =
  fun res ->
  Core.List.fold res.r_qr_lst
    ~init:
      {
        qrc_p = QRSet.empty;
        qrc_r = QRSet.empty;
        qrc_err = QRSet.empty;
        qrc_uu = QRSet.empty;
        qrc_uf = QRSet.empty;
        qrc_fu = QRSet.empty;
        qrc_ff = QRSet.empty;
      } ~f:(fun acc qres ->
      match (qres.qr_prv_flag, qres.qr_rft_flag) with
      | (PF_p, RF_r) -> { acc with qrc_err = QRSet.add acc.qrc_err qres }
      | (PF_p, _)    -> { acc with qrc_p = QRSet.add acc.qrc_p qres }
      | (_, RF_r)    -> { acc with qrc_r = QRSet.add acc.qrc_r qres }
      | (PF_u, RF_u) -> { acc with qrc_uu = QRSet.add acc.qrc_uu qres }
      | (PF_u, RF_f) -> { acc with qrc_uf = QRSet.add acc.qrc_uf qres }
      | (PF_f, RF_u) -> { acc with qrc_fu = QRSet.add acc.qrc_fu qres }
      | (PF_f, RF_f) -> { acc with qrc_ff = QRSet.add acc.qrc_ff qres }
  )
(* function classify_qres end *)

let string_of_res_rough : config -> res -> string =
   let soi = string_of_int in
   fun cfg res ->
   let t =
      Utils.Time.string_of_elapsed_time_from_last_check cfg.cfg_timer
        ~key:"report"
   in
   let m = Utils.Memory.string_of_used_memory cfg.cfg_memory in
   let (p, r, u, f, c, e) =
      List.fold res.r_qr_lst ~init:(0, 0, 0, 0, 0, 0)
        ~f:(fun (p, r, u, f, c, e) qr ->
          let c = c + PPSet.length qr.qr_exp_ppaths in
          let e = e + qr.qr_exp_cnt in
          match (qr.qr_prv_flag, qr.qr_rft_flag) with
          (* WARNING : Be aware of match-with order *)
          | (PF_p, _) -> (p + 1, r, u, f, c, e)
          | (_, RF_r) -> (p, r + 1, u, f, c, e)
          | (PF_f, RF_f) -> (p, r, u, f + 1, c, e)
          | _ -> (p, r, u + 1, f, c, e)
      )
   in
   let (p, r, u, f, c, e) = (soi p, soi r, soi u, soi f, soi c, soi e) in
   let b = soi res.r_comb_cnt in
   let tstr = "Time = " ^ t in
   let mstr = "Memory = " ^ m in
   let prufstr = "P/R/U/F = " ^ String.concat ~sep:" / " [ p; r; u; f ] in
   let cstr = "expanding-ppath = " ^ c in
   let estr = "expanding-ppath-acc = " ^ e in
   let bstr = "combinations = " ^ b in
   String.concat ~sep:" , " [ tstr; mstr; prufstr; cstr; estr; bstr ]
(* function string_of_res_rough end *)

let string_of_res : config -> res -> string =
   let open Tz in
   let qres_info_str : qres -> string =
     fun qres ->
     Printf.sprintf "> Location:%s\n\tCategory:%s\n"
       (qres.qr_qid.qid_loc |> sexp_of_ccp_loc |> Sexp.to_string)
       (qres.qr_qid.qid_cat |> sexp_of_query_category |> Sexp.to_string)
     (* inner-function qres_info_str end *)
   in
   let qres_model_str : config -> qres -> string =
     fun cfg qres ->
     "\tRefuted Path:\n"
     ^ ( if Option.is_none qres.qr_rft_ppath
       then "\tErrored"
       else (
         let ((pp : PPath.t), (md : Smt.Model.t)) =
            Option.value_exn qres.qr_rft_ppath
         in
         let tz_mvcc_to_eval : sctx:sym_state_id -> mich_v cc -> string =
           fun ~sctx mv ->
           Vc.Encoder.cv_mvcc ~sctx cfg.cfg_smt_ctxt mv
           |> Smt.Model.eval md
           |> function
           | None   -> "None"
           | Some e -> Smt.Expr.to_string e
           (* inner-function tz_mvcc_to_eval end *)
         in
         let (ms : MState.t) = pp.pp_mstate in
         let (ts_lst : sym_state list) = MState.extract_trx_state ms in
         let (ts_0 : sym_state) = List.hd_exn ts_lst in
         let (balance_0 : string) =
            ts_0.ss_start_si.si_balance |> tz_mvcc_to_eval ~sctx:ts_0.ss_id
         in
         Printf.sprintf "\t\t- Initial Balance: %s\n" balance_0
         ^ (List.mapi ts_lst ~f:(fun idx ss ->
                let (param : string) =
                   ss.ss_start_si.si_param.ti_param
                   |> tz_mvcc_to_eval ~sctx:ss.ss_id
                   |> String.substr_replace_all ~pattern:"\n"
                        ~with_:"\n\t\t\t\t"
                in
                let (amount : string) =
                   ss.ss_start_si.si_param.ti_amount
                   |> tz_mvcc_to_eval ~sctx:ss.ss_id
                in
                Printf.sprintf
                  "\t\t- Transaction #%d:\n\t\t\tAmount:%s\n\t\t\tParameter:\n\t\t\t\t%s"
                  (idx + 1) amount param
            )
           |> String.concat ~sep:"\n"
           )
       )
       )
     ^ "\n"
     (* inner-function qres_model_str end *)
   in
   fun cfg res ->
   let (cres : qres_classified) = classify_qres res in
   let (tot_c : int) = Core.List.length res.r_qr_lst
   and (p_c : int) = QRSet.length cres.qrc_p
   and (r_c : int) = QRSet.length cres.qrc_r
   and (err_c : int) = QRSet.length cres.qrc_err
   and (uu_c : int) = QRSet.length cres.qrc_uu
   and (uf_c : int) = QRSet.length cres.qrc_uf
   and (fu_c : int) = QRSet.length cres.qrc_fu
   and (ff_c : int) = QRSet.length cres.qrc_ff in
   let (failed_c : int) = err_c + uu_c + uf_c + fu_c + ff_c in
   let (failed : QRSet.t) =
      List.fold ~f:QRSet.union ~init:cres.qrc_err
        [ cres.qrc_uu; cres.qrc_uf; cres.qrc_fu; cres.qrc_ff ]
   in
   let (head : string) = "=== Final Result ===" in
   let (conf : string) =
      Printf.sprintf "Time: %s\t\tMemory: %s"
        (Utils.Time.string_of_elapsed_time cfg.cfg_timer)
        (Utils.Memory.string_of_used_memory cfg.cfg_memory)
   in
   let (itvr : string) =
      Printf.sprintf "Combinations: %d" (res.r_comb_cnt + 1)
   in
   let (summ : string) =
      Printf.sprintf "#Total: %d\t\t#Proved: %d\t\t#Refuted: %d\t\t#Failed: %d"
        tot_c p_c r_c failed_c
   in
   let (finf : string) =
      Printf.sprintf "#Err: %d\t#UU: %d\t#UF: %d\t#FU: %d\t#FF: %d" err_c uu_c
        uf_c fu_c ff_c
   in
   let (prvd : string) =
      Printf.sprintf "<< Proved >>\n%s"
        (QRSet.to_list cres.qrc_p
        |> List.map ~f:qres_info_str
        |> String.concat ~sep:"\n"
        )
   in
   let (rftd : string) =
      Printf.sprintf "<< Refuted >>\n%s"
        (QRSet.to_list cres.qrc_r
        |> List.map ~f:(fun qres -> qres_info_str qres ^ qres_model_str cfg qres)
        |> String.concat ~sep:"\n"
        )
   in
   let (fail : string) =
      Printf.sprintf "<< Failed >>\n%s"
        (QRSet.to_list failed
        |> List.map ~f:qres_info_str
        |> String.concat ~sep:"\n"
        )
   in
   String.concat ~sep:"\n"
     [ ""; head; conf; itvr; summ; finf; prvd; rftd; fail ]
(* function string_of_res end *)

let find_precond : CSet.t SMYMap.t -> key:MState.summary -> CSet.t =
  fun pmap ~key ->
  SMYMap.find pmap key
  |> function
  | None      -> CSet.empty
  | Some cset -> cset
(* function find_precond end *)

let update_precond :
    CSet.t SMYMap.t -> key:MState.summary -> data:CSet.t -> CSet.t SMYMap.t =
  fun pmap ~key ~data ->
  SMYMap.update pmap key ~f:(function
  | None      -> data
  | Some cset -> CSet.union cset data
  )
(* function update_precond end *)
