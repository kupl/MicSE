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
}
[@@deriving sexp, compare, equal]

let run_inst_entry :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
  (fun _ -> failwith "run_inst_entry")

let run_inst : Tz.mich_i Tz.cc -> se_result -> se_result =
  (fun _ -> failwith "run_inst")

let run_inst_i : Tz.mich_i Tz.cc -> se_result -> se_result =
  (fun _ -> failwith "run_inst_i")
