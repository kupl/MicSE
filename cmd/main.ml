exception Error of string

let _ =
   let _ = Utils.Log.app (fun m -> m "MicSE Start") in
   let (cfg, res) = Lib.ExecFlow.prover_refuter_toss None in
   let _ =
      Utils.Log.app (fun m ->
          m "Final-Report : %s" (Lib.Res.string_of_res_rough cfg res)
      )
   in
   ()
