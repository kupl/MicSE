module S = Functional_stack

type elem = string * Adt.typ

type env = Failed | Stack of elem Functional_stack.t

let empty_env = Stack S.empty

let failed_env = Failed

let next_var var_counter =
  let () = var_counter := !var_counter + 1 in
  Printf.sprintf "v%s" (string_of_int !var_counter)

exception Stack_failed

let stack_or_failed = function Failed -> raise Stack_failed | Stack s -> s

let push x env =
  let s = stack_or_failed env in
  Stack (S.push x s)

let pop env =
  let s = stack_or_failed env in
  let x, s' = S.pop s in
  (x, Stack s')

let drop env =
  let s = stack_or_failed env in
  let _, s' = S.pop s in
  Stack s'

let peek env =
  let s = stack_or_failed env in
  S.peek s

let swap env =
  let s = stack_or_failed env in
  Stack (S.swap s)

let dig env n =
  let s = stack_or_failed env in
  Stack (S.dig s n)

let dug env n =
  let s = stack_or_failed env in
  Stack (S.dug s n)

let dip env n =
  if Z.(n = ~$0) then ([], env)
  else
    let rec aux (acc, env') n' =
      if Z.(n' = ~$0) then (acc, env')
      else
        let x, env'' = pop env' in
        let acc' = x :: acc in
        aux (acc', env'') Z.(n' - ~$1)
    in
    aux ([], env) n

let dup env =
  let x = peek env in
  let env' = push x env in
  (x, env')

(* let rename v = function (x, _) :: t -> (x, v) :: t | l -> l *)
