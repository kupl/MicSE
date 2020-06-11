let input_file = ref ""

let options =
  [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
  ]

let main : unit -> unit
=fun () -> begin
  let filepath = !input_file in
  let _ = Format.print_string "\n> Parse" in
  let ast = ProverLib.Adt.parse filepath in
  let _ = ProverLib.Adt.pp Format.std_formatter ast in
  ()
end

and activate_detector : string -> unit
=fun s -> begin
  match s with
  | _ -> invalid_arg "invalid option"
end

let _ = begin
  let usageMsg = "micse-main -input filename" in
  let _ = Arg.parse options activate_detector usageMsg in
  let _ = Printexc.record_backtrace true in
  try
    if !input_file <> ""
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc); prerr_endline (Printexc.get_backtrace())
end