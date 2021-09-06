module MSSet : module type of Core.Set.Make (MState)

val expand_ms : m_view:Se.SSGraph.mci_view -> MState.t -> MSSet.t

val expand_ms_multiple : m_view:Se.SSGraph.mci_view -> MSSet.t -> MSSet.t
