open Batteries

type loc = Unknown | Loc of int * int

type ident = string

type decl = ident

type typ = Tezla.Adt.typ

type expr = Tezla.Adt.expr

type stmt =
  | Cfg_assign of string * expr
  | Cfg_skip
  | Cfg_drop of string list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of string
  | Cfg_if_none of string
  | Cfg_if_left of string
  | Cfg_if_cons of string
  | Cfg_loop of string
  | Cfg_loop_left of string
  | Cfg_map of string
  | Cfg_iter of string
  | Cfg_failwith of string
  | Cfg_micse_check_entry
  | Cfg_micse_check_value of string
  

type t = { id : int; stmt : stmt }

let to_string n =
  let open Format in
  let open Tezla.Pp in
  let () =
    match n.stmt with
    | Cfg_assign (s, e) -> fprintf str_formatter "%s := %a" s expr e
    | Cfg_skip -> fprintf str_formatter "skip"
    | Cfg_drop l ->
        let print_list ppf =
        let pp_sep ppf _ = fprintf ppf "," in
        let pp_v = pp_print_text in
        pp_print_list ~pp_sep pp_v ppf
      in
      fprintf str_formatter "DROP %a" print_list l
    | Cfg_swap -> fprintf str_formatter "SWAP"
    | Cfg_dig -> fprintf str_formatter "DIG"
    | Cfg_dug -> fprintf str_formatter "DUG"
    | Cfg_if s -> fprintf str_formatter "IF %s" s
    | Cfg_if_none s -> fprintf str_formatter "IF_NONE %s" s
    | Cfg_if_left s -> fprintf str_formatter "IF_LEFT %s" s
    | Cfg_if_cons s -> fprintf str_formatter "IF_CONS %s" s
    | Cfg_loop s -> fprintf str_formatter "LOOP %s" s
    | Cfg_loop_left s -> fprintf str_formatter "LOOP_LEFT %s" s
    | Cfg_map s -> fprintf str_formatter "MAP %s" s
    | Cfg_iter s -> fprintf str_formatter "ITER %s" s
    | Cfg_failwith s -> fprintf str_formatter "FAILWITH %s" s
    | Cfg_micse_check_entry -> fprintf str_formatter "CHECK_ENTRY"
    | Cfg_micse_check_value s -> fprintf str_formatter "CHECK_VALUE %s" s
  in
  flush_str_formatter ()

let id_counter = ref (-1)

let next_id () =
  let () = id_counter := !id_counter + 1 in
  !id_counter

let create_node ?id stmt =
  let id = match id with
      None -> next_id ()
    | Some id -> id in
  { id; stmt }

