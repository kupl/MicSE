type vertex = Pre.Lib.Cfg.vertex
type formula = Vlang.t


(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module T = struct
  module CPSet = Core.Set.Poly
  type t = { id: vertex; formula: formula CPSet.t }
  
  let create : vtx:vertex -> t
  =fun ~vtx -> { id=vtx; formula=CPSet.empty }

  let create_with_formulae : vtx:vertex -> fl:formula list -> t
  =fun ~vtx ~fl -> { id=vtx; formula=CPSet.of_list fl }

  let read_formula : t -> formula
  =fun inv -> begin
    if CPSet.is_empty (inv.formula)
    then Vlang.create_formula_true
    else Vlang.create_formula_and (CPSet.to_list (inv.formula))
  end

  let update : t -> f:formula -> t
  =fun inv ~f -> { inv with formula=(CPSet.add (inv.formula) f)}

  let order : inv1:t -> inv2:t -> bool (* inv1 <= inv2 ? true : false *)
  =fun ~inv1 ~inv2 -> CPSet.is_subset (inv1.formula) ~of_:(inv2.formula)

  let join : inv1:t -> inv2:t -> t
  =fun ~inv1 ~inv2 -> begin
    if inv1.id <> inv2.id
    then raise (Failure "Inv.T.join: Error from the invariant id")
    else { inv1 with formula=(CPSet.union (inv1.formula) (inv2.formula)) }
  end

  let to_string : t -> string
  =fun inv -> begin
    if CPSet.is_empty (inv.formula)
    then (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": True"
    else (Pre.Lib.Cfg.string_of_vertex inv.id) ^ ": " ^
         (Core.String.concat ~sep:" && " (Core.List.map (CPSet.to_list (inv.formula)) ~f:Vlang.string_of_formula))
  end
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

  module VtxMap = Vtx.Map

  type t = T.t VtxMap.t

  let empty : t
  =VtxMap.empty

  let is_empty : t -> bool
  =fun m -> VtxMap.is_empty m

  let add : t -> key:vertex -> data:T.t -> t
  =fun m ~key ~data -> VtxMap.add_exn m ~key:key ~data:data

  let find : t -> vertex -> T.t option
  =fun m key -> VtxMap.find m key

  let find_empty : t -> vertex -> T.t
  =fun m key -> begin
    let data_opt = VtxMap.find m key in
    match data_opt with
    | None -> T.create ~vtx:key
    | Some inv -> inv
  end

  let mem : t -> vertex -> bool
  =fun m key -> VtxMap.mem m key
  
  let fold : t -> init:'a -> f:(key:vertex -> data:T.t -> 'a -> 'a) -> 'a
  =fun m ~init ~f -> VtxMap.fold m ~init:init ~f:f

  let map : t -> f:(key:vertex -> data:T.t -> 'a) -> 'a VtxMap.t
  =fun m ~f -> VtxMap.mapi m ~f:f
  
  let exists : t -> f:(key:vertex -> data:T.t -> bool) -> bool
  =fun m ~f -> VtxMap.existsi m ~f:f

  let order : m1:t -> m2:t -> bool (* m1 <= m2 ? true : false *)
  =fun ~m1 ~m2 -> begin
    VtxMap.fold_right m2 ~init:true ~f:(fun ~key ~data b -> begin
      let data' = find_empty m1 key in
      b&&(T.order ~inv1:data' ~inv2:data)
    end)
  end

  let join : t -> t -> t
  =fun m1 m2 -> begin
    let f : key:vertex -> [ `Both of T.t * T.t | `Left of T.t | `Right of T.t ] -> T.t option
    =fun ~key dir -> begin
      let _ = key in
      match dir with
      | `Both (inv1, inv2) -> Some (T.join ~inv1:inv1 ~inv2:inv2)
      | `Left inv -> Some inv
      | `Right inv -> Some inv
    end in
    VtxMap.merge m1 m2 ~f:f
  end

  let to_string : t -> string
  =fun m -> begin
    "{\n" ^ 
    fold m ~init:"" ~f:(fun ~key ~data str -> begin
      str ^
      (Pre.Lib.Cfg.string_of_vertex key) ^ " |-> " ^ (T.to_string data) ^ "\n"
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
  module CPSet = Core.Set.Poly
  type t = {
    enable: Map.t CPSet.t;
    disable: Map.t CPSet.t;
  }

  let empty : t
  ={ enable=CPSet.empty; disable=CPSet.empty }

  let is_empty : t -> bool
  =fun w -> CPSet.is_empty (w.enable)

  let push : t -> Map.t -> t
  =fun w m -> begin
    if CPSet.mem (w.disable) m
    then w
    else { w with enable=(CPSet.add (w.enable) m) }
  end

  let push_list : t -> Map.t list -> t
  =fun w ml -> Core.List.fold_left ml ~init:w ~f:push

  let pop : t -> (Map.t * t)
  =fun w -> begin
    let m = CPSet.choose_exn (w.enable) in
    let w' = { enable=(CPSet.remove (w.enable) m); disable=(CPSet.add (w.disable) m) } in
    (m, w')
  end
  
  let map : t -> f:(Map.t -> Map.t) -> t
  =fun w ~f -> begin
    let el = CPSet.to_list (w.enable) in
    let mapped_el = Core.List.map el ~f:f in
    let w' = { w with enable=(CPSet.of_list mapped_el)} in
    w'
  end
end