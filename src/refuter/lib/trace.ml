(* Transaction Traces *)

exception Error of string

(* Type for each transaction information *)
type trx = {
  index : int;
  storage : ProverLib.Smt.ZExpr.t;    (* storage of contract before the transaction *)
  parameter : ProverLib.Smt.ZExpr.t;  (* parameter value of the transaction *)
  sender : ProverLib.Smt.ZExpr.t;     (* sender address of the transaction *)
  source : ProverLib.Smt.ZExpr.t;     (* source address of the transaction *)
  amount : ProverLib.Smt.ZExpr.t;     (* amount of the transaction *)
  balance : ProverLib.Smt.ZExpr.t;    (* updated balance of contract by the transaction *)
}

(* Type for Transaction Trace *)
type t = {
  index : int;                                (* scenario index of trace *)
  query : Prover.VcGen.query_vc;              (* query information which violated by this trace *)
  length : int;                               (* length of transaction trace *)
  trx_list : trx list;                        (* transaction information of this trace *)
}

let gen : PreLib.Cfg.t -> int -> int -> Prover.VcGen.query_vc -> ProverLib.Smt.ZModel.t -> t
= let open PreLib in
  let open ProverLib in
  let open Prover in
  (* trx-length, scenario-index, query-information, smt-model *)
  fun cfg len idx qvc model -> begin
    (* Set helper function for getting SMT expression of value as syntax sugar *)
    let read_value : Smt.ZSort.t -> string -> Smt.ZExpr.t
    =fun sort name -> begin
      let var_expr = Smt.ZExpr.create_var sort ~name in
      Smt.ZModel.eval var_expr ~model:model 
      |> (function | Some ee -> ee | None -> (Error "gen.read_value: variable not found") |> Stdlib.raise)
    end in (* helper function read_value end *)
    (* Set the sorts *)
    let param_sort : Smt.ZSort.t = 
      Cfg.t_map_find 
        ~errtrace:("Refuter.Lib.Trace.gen : Some param : ps_vtyp")
        cfg.type_info
        Cfg.param_storage_name
      |> Mich.get_d
      |> Vlang.TypeUtil.ty_of_michtyp
      |> Vlang.TypeUtil.get_innertyp2
      |> Stdlib.fst 
      |> Verifier.smtsort_of_vlangtyp in
    let sender_sort : Smt.ZSort.t = Vlang.Ty.T_address |> Verifier.smtsort_of_vlangtyp in
    let source_sort : Smt.ZSort.t = Vlang.Ty.T_address |> Verifier.smtsort_of_vlangtyp in
    let amount_sort : Smt.ZSort.t = Vlang.Ty.T_mutez |> Verifier.smtsort_of_vlangtyp in
    let balance_sort : Smt.ZSort.t = Vlang.Ty.T_mutez |> Verifier.smtsort_of_vlangtyp in
    let storage_sort : Smt.ZSort.t =
      Cfg.t_map_find 
        ~errtrace:("Refuter.Lib.Trace.gen : Some stg : ps_vtyp")
        cfg.type_info
        Cfg.param_storage_name
      |> Mich.get_d
      |> Vlang.TypeUtil.ty_of_michtyp
      |> Vlang.TypeUtil.get_innertyp2
      |> Stdlib.snd 
      |> Verifier.smtsort_of_vlangtyp in
    (* Get Transactions' information *)
    let trx_list : trx list = Core.List.init len ~f:(fun trx_idx -> begin
        let stg_val : Smt.ZExpr.t = read_value storage_sort (GlVar.gen_storage trx_idx) in
        let param_val : Smt.ZExpr.t = read_value param_sort (GlVar.gen_param trx_idx) in
        let sender_val : Smt.ZExpr.t = read_value sender_sort (GlVar.gen_sender trx_idx) in
        let source_val : Smt.ZExpr.t = read_value source_sort (GlVar.gen_source trx_idx) in
        let amount_val : Smt.ZExpr.t = read_value amount_sort (GlVar.gen_amount trx_idx) in
        let balance_val : Smt.ZExpr.t = read_value balance_sort (GlVar.gen_balance trx_idx) in
        let transaction : trx = {
          index=trx_idx;
          storage=stg_val;
          parameter=param_val;
          sender=sender_val;
          source=source_val;
          amount=amount_val;
          balance=balance_val;
        } in
        transaction
      end) in
    (* Assemble the trace information *)
    let trace : t = { index=idx; query=qvc; length=len; trx_list=trx_list; } in
    trace
end (* function gen end *)

let to_string : t -> string
= let open ProverLib in
  (* transaction-trace *)
  fun trace -> begin
    let str = ("================ Scenario #" ^ (trace.index |> string_of_int) ^ "\n") in
    let str = str ^ ("======== Vertex: " ^ (trace.query.qvc_vtx |> string_of_int) ^ "\n") in
    let str = str ^ ("======== Category: " ^ (trace.query.qvc_cat |> Bp.JsonRep.of_query_category |> Yojson.Basic.pretty_to_string) ^ "\n") in
    let str = str ^ ("======== Formula: \n" ^ (trace.query.qvc_fml |> Vlang.Formula.to_string) ^ "\n") in
    let str = str ^ ("======== Transactions: " ^ "\n") in
    let str = str ^ ((
      trace.trx_list |> Core.List.fold_left ~init:"" ~f:(fun (s : string) (t: trx) -> begin
        let s = s ^ ("==== Trx #" ^ (t.index |> string_of_int) ^ "\n") in
        let s = s ^ ("== Storage: \n" ^ (t.storage |> Smt.ZExpr.to_string) ^ "\n") in
        let s = s ^ ("== Parameter: \n" ^ (t.parameter |> Smt.ZExpr.to_string) ^ "\n") in
        let s = s ^ ("== Sender: \n" ^ (t.sender |> Smt.ZExpr.to_string) ^ "\n") in
        let s = s ^ ("== Source: \n" ^ (t.source |> Smt.ZExpr.to_string) ^ "\n") in
        let s = s ^ ("== Amount: \n" ^ (t.amount |> Smt.ZExpr.to_string) ^ "\n") in
        let s = s ^ ("== Balance: \n" ^ (t.balance |> Smt.ZExpr.to_string) ^ "\n") in
        s
      end)
    ) ^ "\n") in
    str
end