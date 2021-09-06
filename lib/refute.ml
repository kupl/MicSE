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
