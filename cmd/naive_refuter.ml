exception Error of string

let _ =
   try
     let (cfg, res) = Lib.ExecFlow.refuter_naive_run None in
     let _ = Utils.Log.app (fun m -> m "%s" (Lib.Res.string_of_res cfg res)) in
     ()
   with
   | exc ->
     Utils.Log.err (fun m ->
         m "%s\n%s" (exc |> Printexc.to_string) (Printexc.get_backtrace ())
     )
