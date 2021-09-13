exception Error of string

let _ = 
  let (cfg, res) = Lib.ExecFlow.prover_naive_run None in
  let _ = Utils.Log.app (fun m -> m "%s" (Lib.Res.string_of_res cfg res)) in
  ()
