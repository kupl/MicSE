(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Pre.Lib.Cfg.vertex

type formula = Vlang.t

type t = { id: vertex; formula: formula option }
and inv = t

let create_dummy_inv : vertex -> t
=fun vtx -> { id=vtx; formula=None }

let create_inv : vertex -> formula -> t
=fun vtx f -> { id=vtx; formula=Some f }

let string_of_inv : t -> string
=fun inv -> begin
  if Option.is_none inv.formula then (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": None"
  else (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": " ^ (Vlang.string_of_formula (Option.get inv.formula))
end


(*****************************************************************************)
(*****************************************************************************)
(* Invariant Map                                                             *)
(*****************************************************************************)
(*****************************************************************************)

module Map = struct
  module Vtx = struct
    module Key = struct
      type t = vertex
      let compare x y = Core.Int.compare x y
      let sexp_of_t s = Core.Int.sexp_of_t s
      let t_of_sexp x = Core.Int.t_of_sexp x
    end
  
    include Key
    include Core.Comparable.Make (Key)
  end

  module T = Vtx.Map

  type t = inv T.t

  let empty : t
  =T.empty

  let is_empty : t -> bool
  =fun m -> T.is_empty m

  let add : t -> key:vertex -> data:inv -> t
  =fun m ~key ~data -> T.add_exn m ~key:key ~data:data

  let find : t -> vertex -> inv
  =fun m key -> begin
    let data_opt = T.find m key in
    match data_opt with
    | None -> create_inv key (Vlang.create_formula_true)
    | Some data -> data
  end

  let mem : t -> vertex -> bool
  =fun m key -> T.mem m key
  
  let fold : t -> init:'a -> f:(key:vertex -> data:inv -> 'a -> 'a) -> 'a
  =fun m ~init ~f -> T.fold m ~init:init ~f:f

  let map : t -> f:(key:vertex -> data:inv -> 'a) -> 'a T.t
  =fun m ~f -> T.mapi m ~f:f
  
  let exists : t -> f:(key:vertex -> data:inv -> bool) -> bool
  =fun m ~f -> T.existsi m ~f:f

  let merge : t -> t -> t
  =fun m1 m2 -> begin
    let f : key:vertex -> [ `Both of inv * inv | `Left of inv | `Right of inv ] -> inv option
    =fun ~key dir -> begin
      match dir with
      | `Both (inv1, inv2) -> begin
          if key <> inv1.id || key <> inv2.id
          then raise (Failure "Inv.Map.merge: Error from the invariant id")
          else begin
            match (inv1.formula, inv2.formula) with
            | Some v1, Some v2 -> Some (create_inv key (Vlang.create_formula_and [v1; v2]))
            | Some _, None -> Some inv1
            | None, Some _ -> Some inv2
            | None, None -> None
          end
        end
      | `Left inv -> Some inv
      | `Right inv -> Some inv
    end in
    T.merge m1 m2 ~f:f
  end

  let to_string : t -> string
  =fun m -> begin
    "{\n" ^ 
    fold m ~init:"" ~f:(fun ~key ~data str -> (
      match data.formula with
      | None -> str
      | Some v -> begin
          str ^
          (string_of_int key) ^ ": " ^ (Vlang.string_of_formula v) ^ "\n"
        end
    )) ^
    "}"
  end
end