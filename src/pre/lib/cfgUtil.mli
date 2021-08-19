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

(* simple optimization - remove Cfg_drop, Cfg_swap, Cfg_dig, and Cfg_dug *)
(* it can remove vertices only if the node has one predecessor and one successor. otherwise, it will replace the statememnt into Cfg_skip *)
val remove_simple_stack_operation_vertices : Cfg.t -> Cfg.t


(*****************************************************************************)
(*****************************************************************************)
(* Appeared Variable List in Expression                                      *)
(*****************************************************************************)
(*****************************************************************************)

(* it does not distinguishes variables in LHS and RHS of assign stmts. *)
val appeared_varlst_expr : Cfg.expr -> string list
val appeared_varlst_stmt : Cfg.stmt -> string list


(*****************************************************************************)
(*****************************************************************************)
(* Dot Graph                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module Dot : sig
  type t = {
    flow: string list;
    vi: string list;
  }

  val concat : ?sep:string -> string list -> string
  val make_label : ?style:string -> ?shape:string -> string -> string
  val fold_flow : Cfg.G.E.t -> string list -> string list
  val fold_vi : Cfg.t -> Cfg.G.V.t -> string list -> string list
  val of_cfg : Cfg.t -> t
  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Loop Unrolling                                                            *)
(*****************************************************************************)
(*****************************************************************************)

module LoopUnrolling : sig
  (* sugar *)
  module CPSet : module type of Core.Set.Poly
  module CPMap : module type of Core.Map.Poly

  (* "loopnest_info" : make loop-nesting information *)
  (* input : UNOPTIMIZED cfg. *)
  (* return : (vtx -> vtxlist) map. It means that the loop-vertex(key) contains other loop-vertices(value). *)
  (* WARNING : Current implementation only unroll loops in main function, not other lambda values. *)
  type lni_dfs_param = {
    lni_worklist :  Cfg.G.edge list;
    lni_acc_map : (Cfg.vertex, Cfg.vertex list) CPMap.t;
    lni_visited : Cfg.vertex CPSet.t;
  }
  val loopnest_info : Cfg.t -> (Cfg.vertex, Cfg.vertex CPSet.t) CPMap.t

  module DepG : Graph.Sig.P with type V.t=Cfg.V.t and type V.label=Cfg.V.t and type E.t=(Cfg.V.t * Cfg.V.t) and type E.label=unit
  type ts_dfs_param = {
    ts_graph : DepG.t;   (* loop-vertices dependency graph. "vtx1 -> vtx2" means that the loop vtx2 is in loop vtx1 *)
    ts_result : Cfg.vertex list;   (* topology-sorted list. most-nested loop vertex will be located at the head of this list. *)
    ts_unvisited : Cfg.vertex CPSet.t; (* the set of unvisited vertices, it helps ignoring visited vertices. *)
    ts_unvisited_top : Cfg.vertex CPSet.t; (* the set of unvisited vertices, which has no pred-vertices. *)
    ts_stack : Cfg.vertex list;    (* stack for Depth first search *)
  }
  val tplg_sort : (Cfg.vertex, Cfg.vertex CPSet.t) CPMap.t -> Cfg.vertex list


  module UnrollParam : sig
    (* environment *)
    type unroll_env = {
      env_entry_vtx : Cfg.vertex;  (* unrolled loop seq start from here. *)
      env_exit_vtx : Cfg.vertex;   (* unrolled loop seq ends here. *)
      env_vset : Cfg.vertex CPSet.t;  (* vertices in unrolled-graph *)
      env_eset : Cfg.G.edge CPSet.t;  (* edges in unrolled-graph *)
    }
    (* param-type *)
    type pt = {
      (* cfg : Cfg.t
        it is persistent data type, however, it will be updated for every new vertex&variable information created.
        For the aspect of clean data structure, it is useful to contain every unrolled loops and counters individually contained at unroll_body type,
        but it is convenient to update information at one place.
        In consequence, "counter" pulled out from "unroll_env" too, which supports monolithic cfg-construction.
      *)
      cfg : Cfg.t;   (* cfg-workspace. it is persistent data type, and it will be updated a lot at the end of "unroll" function evaluation. *)
      counter : Cfg.cfgcon_ctr;    (* counter for cfg-construction, IT SHOULD BE "Cfg.hardcopy_counter"-ED CFGCON_CTR FROM THE RETURN VALUE OF "Translator.adt_to_cfg_counter_included". *)
      unroll_num : int;    (* how many times to unroll every loop *)
      vtxrel : (Cfg.vertex, Cfg.vertex CPSet.t) CPMap.t;   (* relation between newly-created vertex and the vertices from original cfg. *)
      env : (Cfg.vertex, unroll_env) CPMap.t;    (* pre-unrolled loop-vertex's information *)
    }
    (* paramter for folding *)
    type unroll_fold_f_param = {
      ptval : pt;    (* unroll_param from "unroll" function. it should be updated *)
      target_vtx : Cfg.vertex;  (* target loop-vertex *)
      target_stmt : Cfg.stmt;   (* "uff_target_vtx"'s stmt. *)
      unroll_count : int;   (* how many unrolling performed for now, it should be start with 0, and increasing to (ptval.unroll_num + 1) *)
                            (* for example, N="ptval.unroll_num", C="unroll_count", 
                                if C > N, stop unrolling,
                                if C = 0, Don't append body, but the only entry and exit things (e.g. DROP instruction at the end) and make C <- 1.
                                if C <= N, append body once AT THE FRONT OF UFF_CUR_ENTRY, and make C <- C+1 .
                            *)
      acc_unroll_env : unroll_env;  (* current graph accumulator. but other cfg-related information are not belongs to this value.
                                        final acc_unroll_env will be added at ptval.env
                                    *)
    }
    type copy_unrolled_cfg_param = {
      cuc_ptval : pt;
      cuc_loopvtx : Cfg.vertex; (* loop to copy. it is the key value of pt.env *)
      cuc_entry : Cfg.vertex; (* graph starts here. i.e. copied vertices will be appended from here. *)
      cuc_exit : Cfg.vertex;  (* graph ends here. i.e. copied graph ends here. *)
    }
    type dfs_copy_cfg_param = {
      dcc_ptval : pt;
      (* every trace go through dcc_o_entry must go through dcc_o_exit too. *)
      dcc_o_entry : Cfg.vertex; (* original graph start-vertex *)
      dcc_o_exit : Cfg.vertex;  (* original graph exit-vertex *)
    }
    type dfs_copy_cfg_inner_param = {
      dcci_visited : Cfg.vertex CPSet.t;
      dcci_stack : Cfg.vertex list;
      (* entry, exit vertices and cfg are already given at the dfs_copy_cfg_param. *)
    }
  end (* module UnrollParam ends *)


  (* for detailed explanation, see cfgUtil.ml source code. *)
  val copy_unrolled_cfg : UnrollParam.copy_unrolled_cfg_param -> (UnrollParam.pt * (Cfg.vertex CPSet.t) * (Cfg.G.edge CPSet.t))
  val dfs_copy_cfg : UnrollParam.dfs_copy_cfg_param -> (UnrollParam.pt * (Cfg.vertex CPSet.t) * (Cfg.G.edge CPSet.t) * (Cfg.vertex * ((Cfg.vertex * Cfg.edge_label) CPSet.t)))
  val unroll_fold_f : UnrollParam.unroll_fold_f_param -> UnrollParam.unroll_fold_f_param

  (* RECOMMENDED. if you want to get UnrollParam.pt instead of only (Cfg & counter), please use "unroll" function instead of "run" function. *)
  val construct_pt : Cfg.t * Cfg.cfgcon_ctr * int -> UnrollParam.pt
  val unroll : UnrollParam.pt -> UnrollParam.pt

  (* "run (cfg, counter) n" unrolls cfg n times. Simplified version of "unroll" function. *)
  val run : (Cfg.t * Cfg.cfgcon_ctr * int) -> (Cfg.t * Cfg.cfgcon_ctr)
end (* module LoopUnrolling ends *)
