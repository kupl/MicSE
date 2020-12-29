(*
"create_params_and_storages_from_model" requires some implicit MicSE design decisions.
1. "Prover.Converter.create_rename_var" uses the prefix "#" to generate new symbol of existing variable.
2. It uses some magic-names from "Pre.Lib.Cfg.param_storage_name" and "Refuter.Extractor.trx_seq_param_name" and "Refuter.Extractor.trx_seq_storage_name"
3. It almost follows implementation of the function "Prover.Verifier.create_param_storage_from_model"
*)

(*
let create_params_and_storages_from_model : Prover.Lib.Smt.model -> Pre.Lib.Cfg.t -> (string * Prover.Lib.Smt.z_expr) list
= let open Pre.Lib in
  let open Prover.Lib in
  fun trx_length m cfg -> begin
  let param_storage_var = Cfg.param_storage_name in
  let param_storage_sort = Prover.Verifier.sort_of_typt (Pre.Lib.Cfg.CPMap.find_exn cfg.type_info param_storage_var) in
  let param_storage = Smt.read_var (Smt.create_symbol param_storage_var) param_storage_sort in
  let param, storage = ((Smt.read_pair_fst param_storage), (Smt.read_pair_snd param_storage)) in
  let param_expr_opt, storage_expr_opt = ((Smt.create_evaluation m param), (Smt.create_evaluation m storage)) in
  let paramstg_added : (string * Smt.z_expr) list = (
    match (param_expr_opt, storage_expr_opt) with
    | Some param_expr, Some storage_expr -> ["param", param_expr; "storage", storage_expr]
    | _, _ -> Stdlib.failwith "Refuter : Runner : create_params_and_storages_from_model : paramstg_added"
  ) in
  ()
end
*)

(* input
    - cfg
    - basicpaths (generated by Refuter.Extractor.get_concatenated_basicpaths)
    - queryfilter (function that removes unwanted queries)
*)
let collect_queries : PreLib.Cfg.t -> (Prover.Lib.Bp.lst) -> (Prover.Lib.Query.t list -> Prover.Lib.Query.t list) -> (Prover.Lib.Query.t list)
=fun cfg bplist queryfilter -> begin
  let entry_var, exit_var = ((bplist.entry.var |> Option.get), (bplist.exit.var |> Option.get)) in
  let whitelist : Pre.Lib.Cfg.ident -> bool = begin
    fun s -> 
      (s = Pre.Lib.Cfg.param_storage_name)
      || (Extractor.is_trx_seq_param_name s)
      || (Extractor.is_trx_seq_storage_name s)
      || (Extractor.is_trx_seq_operation_name s)
  end in
  let queryll : Prover.Lib.Query.t list list = List.map (fun bp -> Prover.Converter.convert ~whitelist_mem:whitelist bp cfg ~entry_var:entry_var ~exit_var:exit_var |> Stdlib.snd) bplist.bps in
  List.flatten (List.map queryfilter queryll)
end

(* It depends on newly created parameter/storage variable names in "Refuter.Extractor.concat_basicpath" *)
let get_param_stg_from_model : PreLib.Cfg.t -> ProverLib.Smt.ZModel.t option -> ((string * ProverLib.Smt.ZExpr.t) list) option
=fun cfg modelopt -> begin
  match modelopt with
  | None -> None
  | Some model -> (
      let rec append_template : (int -> string) -> int -> ((string * ProverLib.Smt.ZExpr.t) list) -> ((string * ProverLib.Smt.ZExpr.t) list)
      =fun vargen i_now acc_l -> begin
        try
          let varname = vargen i_now in
          let varexpr = varname |>
                        PreLib.Cfg.CPMap.find_exn cfg.type_info |>
                        Prover.Verifier.sort_of_typt |> 
                        ProverLib.Smt.ZExpr.create_var ~name:varname |>
                        ProverLib.Smt.ZModel.eval ~model:model |>
                        Option.get in
          append_template vargen (i_now + 1) ((varname, varexpr) :: acc_l)
        with
        | Core.Not_found_s _ -> acc_l (* Not_found from CPMap.find_exn *)
        | Stdlib.Invalid_argument _ -> Stdlib.failwith "Refuter : runner.ml : get_param_stg_from_model : append_template" (* Invalid_argument from Option.get *)
      end in
      Some ( 
        []
        |> append_template Extractor.trx_seq_param_name 0
        |> append_template Extractor.trx_seq_storage_name 0
      )
    )
end

(* Recieve one query(formula), and if refutable, return true and parameter/storages. else, return false and None. *)
(* this function repeats "Prover.Verifier.verify" function in refuter's way. *)
let refute_unit : PreLib.Cfg.t -> ProverLib.Query.t -> ProverLib.Smt.ZSolver.validity * ((string * ProverLib.Smt.ZExpr.t) list) option
=fun cfg query -> begin
  let open Prover in
  let open ProverLib in 
  let result, model_opt = Smt.ZSolver.check_validity [(query.query |> Verifier.smtexpr_of_vlangformula)] in
  (result, get_param_stg_from_model cfg model_opt)
end

let string_of_refute_result : ProverLib.Smt.ZSolver.validity * ((string * ProverLib.Smt.ZExpr.t) list) option -> string
=fun (b, ropt) -> begin
  let bodystr : string =
    match ropt with
    | None -> ""
    | Some lst -> List.fold_right (fun (varname, zexpr) accstr -> (varname ^ " : " ^ (ProverLib.Smt.ZExpr.to_string zexpr)) ^ "\n" ^ accstr) lst ""
  in (b |> ProverLib.Smt.ZSolver.string_of_validity) ^ ("{\n" ^ bodystr ^ "}")
end
