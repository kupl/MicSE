
let instlst_of_cfgexpr : Pre.Lib.Cfg.expr -> ProverLib.Bp.inst list = fun _ -> [BI_skip] (* TODO. maybe moved to non-library module *)

let instlst_of_cfgstmt : Pre.Lib.Cfg.stmt -> ProverLib.Bp.inst list = fun _ -> [BI_skip] (* TODO. maybe moved to non-library module *)
