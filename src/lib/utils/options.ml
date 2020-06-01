let input_file = ref ""

let activate_detector s =
  match s with
  | _ -> invalid_arg "invalid option"

let options =
  [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
  ]