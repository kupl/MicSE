module MSSet : module type of Core.Set.Make (MState)

val expand_ms : m_view:Se.SSGraph.mci_view -> MState.t -> MSSet.t

val expand_ms_multiple : m_view:Se.SSGraph.mci_view -> MSSet.t -> MSSet.t

val naive_run :
  timer:Utils.Time.t ->
  init_strg:Tz.mich_v Tz.cc ->
  smt_ctxt:Smt.Ctx.t ->
  smt_slvr:Smt.Solver.t ->
  invmap:Inv.inv_map ->
  m_view:Se.SSGraph.mci_view ->
  MSSet.t ->
  (Smt.Model.t * MState.t) option * MSSet.t
