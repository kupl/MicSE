exception Error of string

let _ =
   try
     let _ = Utils.Log.app (fun m -> m "MicSE Start") in
     let (cfg, res) = Lib.ExecFlow.prover_refuter_toss None in
     let _ =
        Utils.Log.app (fun m ->
            m "Final-Report : %s" (Lib.Res.string_of_res cfg res)
        )
     in
     ()
   with
   | exc ->
     Utils.Log.err (fun m ->
         m "%s\n%s" (exc |> Printexc.to_string) (Printexc.get_backtrace ())
     )
