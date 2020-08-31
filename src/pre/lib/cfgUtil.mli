(*****************************************************************************)
(*****************************************************************************)
(* Optimization                                                              *)
(*****************************************************************************)
(*****************************************************************************)

(* simple optimization - remove meaningless skip node *)
(* meaningless skip node has only one in-degree and one out-degree and both edges are "Normal" *)
(* WARNING : this does not remove any vertex-information in Cfg.t *)
val remove_meaningless_skip_vertices : Cfg.t -> Cfg.t
val remove_meaningless_skip_vertices_fixpoint : Cfg.t -> Cfg.t

(* simple optimization - remove meaningless fail nodes *)
(* meaningless fail node has one predecessor and one successor, where the predecessor is fail node too. *)
val remove_meaningless_fail_vertices : Cfg.t -> Cfg.t
val remove_meaningless_fail_vertices_fixpoint : Cfg.t -> Cfg.t


(*****************************************************************************)
(*****************************************************************************)
(* Print                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

val cfg_to_dotformat : Cfg.t -> string
