module MSSet : module type of Core.Set.Make (MState)

module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

val expand_ms : m_view:Se.SSGraph.mci_view -> MState.t -> MSSet.t

val expand_ms_multiple : m_view:Se.SSGraph.mci_view -> MSSet.t -> MSSet.t

val naive_run_init_res : Se.se_result -> Res.res

val naive_run_qres_atomic_action : Res.config -> Res.res -> Res.qres -> Res.qres

val naive_run_res_escape_condition : Res.config -> Res.res -> bool

val naive_run_res_atomic_action : Res.config -> Res.res -> Res.res

val naive_run : Res.config -> Res.res -> Res.res
