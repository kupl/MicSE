(* Semantically, Operation.t should be defined at Cfg.operation type, 
    but because of dependency issue (Adt and Cfg's dependency cycle), Operation.t defined separately.
*)

type t = 
  | O_create_contract of Mich.program * string * string * string
  | O_transfer_tokens of string * string * string
  | O_set_delegate of string
  | O_create_account of string * string * string * string
