(* Set Logger *)
Utils.Options.create_options ();
Utils.Log.create ();

(* Make Logs *)
Utils.Log.err (fun m -> m "ERROR %s" "TEST");
Utils.Log.warn (fun m -> m "WARNING %s" "TEST");
Utils.Log.info (fun m -> m "INFO LOG %s" "TEST");
Utils.Log.debug (fun m -> m "DEBUG LOG %s" "TEST");
Utils.Log.app (fun m -> m "APPLICATION OUTPUT %s" "TEST");