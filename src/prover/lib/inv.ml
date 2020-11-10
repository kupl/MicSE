(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Pre.Lib.Cfg.vertex

type formula = Vlang.t

type t = { id: vertex; formula: formula list }
and inv = t

let create : vtx:vertex -> t
=fun ~vtx -> { id=vtx; formula=[] }

let create_with_formulae : vtx:vertex -> fl:formula list -> t
=fun ~vtx ~fl -> { id=vtx; formula=fl }

let read_formula : inv:t -> formula
=fun ~inv -> begin
  if Core.List.is_empty (inv.formula)
  then Vlang.create_formula_true
  else Vlang.create_formula_and (inv.formula)
end

let update : inv:t -> f:formula -> t
=fun ~inv ~f -> { inv with formula=(f::inv.formula) }

let string_of_inv : t -> string
=fun inv -> begin
  if Core.List.is_empty (inv.formula)
  then (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": True"
  else (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": " ^
         (Core.String.concat ~sep:" && " (Core.List.map (inv.formula) ~f:Vlang.string_of_formula))
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
    | None -> create ~vtx:key
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
          else Some (create_with_formulae ~vtx:key ~fl:((inv1.formula)@(inv2.formula)))
        end
      | `Left inv -> Some inv
      | `Right inv -> Some inv
    end in
    T.merge m1 m2 ~f:f
  end

  let to_string : t -> string
  =fun m -> begin
    "{\n" ^ 
    fold m ~init:"" ~f:(fun ~key ~data str -> begin
      str ^
      (Pre.Lib.Cfg.string_of_vertex key) ^ " |-> " ^ (string_of_inv data) ^ "\n"
    end) ^
    "}"
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Worklist                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module WorkList = struct
  type t = Map.t list

  let empty : t
  =[]

  let is_empty : t -> bool
  =fun wl -> Core.List.is_empty wl

  let push : t -> Map.t -> t
  =fun wl m -> begin
    m::wl
  end

  let pop : t -> (Map.t * t)
  =fun wl -> begin
    let m_opt, wl'_opt = (Core.List.hd wl), (Core.List.tl wl) in
    match m_opt, wl'_opt with
    | Some m, Some wl' -> (m, wl')
    | _, _ -> raise (Failure "")
  end
end