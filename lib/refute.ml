open! Core
open Se
open MState
module MSSet = Core.Set.Make (MState)

let expand_ms : m_view:SSGraph.mci_view -> MState.t -> MSSet.t =
  fun ~m_view ms ->
  let open SSGraph in
  let { trx = trxset; ln = lnset; lb = lbset } =
     ss_view_pred ~m_view (get_first_ss ms)
  in
  SSet.union_list [ trxset; lnset; lbset ] |> MSSet.map ~f:(fun ss -> cons ss ms)

let expand_ms_multiple : m_view:Se.SSGraph.mci_view -> MSSet.t -> MSSet.t =
  fun ~m_view msset ->
  MSSet.fold msset ~init:MSSet.empty ~f:(fun accs ms ->
      MSSet.union (expand_ms ~m_view ms) accs
  )

let rec naive_run :
    timer:Utils.Time.t ->
    init_strg:Tz.mich_v Tz.cc ->
    smt_ctxt:Smt.Ctx.t ->
    smt_slvr:Smt.Solver.t ->
    invmap:Inv.inv_map ->
    m_view:Se.SSGraph.mci_view ->
    MSSet.t ->
    (Smt.Model.t * MState.t) option * MSSet.t =
  fun ~timer ~init_strg ~smt_ctxt ~smt_slvr ~invmap ~m_view msset ->
  if (* 1. If timeout or msset is empty, escape. *)
     Utils.Time.is_timeout timer || MSSet.length msset = 0
  then (None, msset)
  else (
    let _ =
       Utils.Log.debug (fun m ->
           m "Naive Refuter : time = %d , size = %d"
             (Utils.Time.read_elapsed_time timer)
             (MSSet.length msset)
       )
    in
    (* 2. For every total paths, check if the path is refutable *)
    let tp_result_model_opt : Smt.Model.t option ref = ref None in
    let tp_refutable : MState.t option =
       let open Tz in
       let open Smt.Solver in
       MSSet.find msset ~f:(fun ms ->
           (* It uses short-circuit conditional operator "&&" *)
           equal_mich_cut_category (get_first_ss ms).ss_start_mci.mci_cutcat
             MCC_trx_entry
           &&
           let fmla : mich_f =
              Vc.gen_query_vc_from_ms_with_init_strg invmap init_strg ms
              |> TzUtil.opt_mf
           in
           match Vc.check_sat smt_ctxt smt_slvr fmla with
           | (SAT, m_opt) ->
             tp_result_model_opt := m_opt;
             true
           | _            -> false
       )
       (* MSSet.fold msset ~init:None ~f:(fun acc ms ->) *)
    in
    match (!tp_result_model_opt, tp_refutable) with
    | (Some m, Some ms) -> (Some (m, ms), msset)
    | (None, Some _)    -> failwith "Refute.naive_run : unexpected"
    | _                 ->
      naive_run ~timer ~init_strg ~smt_ctxt ~smt_slvr ~invmap ~m_view
        (expand_ms_multiple ~m_view msset)
  )
