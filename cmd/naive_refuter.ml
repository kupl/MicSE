exception Error of string

open Lib

let _ =
   try
    let _ = Utils.Log.app (fun m -> m "MicSE naive refuter Start") in
     let (cfg, res) = ExecFlow.refuter_naive_run None in
     let _ =
        Utils.Log.app (fun m ->
            m "Final-Report : %s" (Res.string_of_res cfg res)
        )
     in
     ()
   with
   | exc when Utils.Log.is_logger_created () ->
     Utils.Log.err (fun m ->
         m "%s\n%s" (exc |> Printexc.to_string) (Printexc.get_backtrace ())
     )
   | exc -> raise exc