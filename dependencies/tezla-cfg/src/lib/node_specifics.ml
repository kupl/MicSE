open Batteries

let find_assignments blocks x =
  let open Cfg_node in
  let aux b acc =
    match b with
    | Cfg_assign (lv, _) when lv = x -> Set.add b acc
    | Cfg_assign _ | Cfg_dig | Cfg_drop _ | Cfg_failwith _ | Cfg_if_cons _
    | Cfg_if_left _ | Cfg_if_none _ | Cfg_skip | Cfg_swap | Cfg_dug
    | Cfg_if _ | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ ->
        acc
  in
  Set.fold aux blocks Set.empty
