(* Merge Two States *)

(* IMPORTANT NOTE
  - See "IMPORTANT NOTE" on top of the "Refute" module.
*)


(*****************************************************************************)
(*****************************************************************************)
(* Merge States in Intra-Transaction Situation                               *)
(*****************************************************************************)
(*****************************************************************************)

(* More explanation for "ms_iter_info" type.
  - We found three issues when implementing "intratrx-merge-state" function in an incremental way in the reverse direction.
    1. Length of input container & Input element membership properties in ITER instruction.
    2. Length of input container & Input element membership properties in MAP instruction.
    3. Length of output container & Output element membership properties in MAP instruction.
    (4. Length of input container and output container should be same)
  - To deal with above problems, we need to store output container & input, output elements in the type "ms_iter_info".
  - In our design, we does not need any input container information to hold.
  - If the query states located in ITER or MAP instruction, some problems might occured.
    1. No output container enrolled in "mii_map_accv".
    2. Inupt container can contain more elements than exposed iteration size in path.
  - So boolean flags are introduced to indicate whether the path includes the end of the iteration.
*)
type ms_iter_info = {
  (* iteration information for the function "intratrx-merge-state" *)
  mii_iter_iv : (Tz.mich_cut_info, Tz.mich_v Tz.cc list) Tz.PMap.t;  (* input-var-info for ITER instruction // MCC_lb_iter *)
  (* "mii_map_iov" : (single-ivar holder) * (single-ovar holder) * io-var-info for MAP instruction // MCC_lb_map *)
  mii_map_iov : (Tz.mich_cut_info, (Tz.mich_v Tz.cc option * Tz.mich_v Tz.cc option * ((Tz.mich_v Tz.cc * Tz.mich_v Tz.cc) list))) Tz.PMap.t; 
  mii_map_accv : (Tz.mich_cut_info, Tz.mich_v Tz.cc) Tz.PMap.t; (* result-var-info for MAP instruction // MCC_ln_map *)
  mii_iter_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for ITER instruction // MCC_ln_iter // If exists, then true, else false *)
  mii_map_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for MAP instruction // MCC_ln_map // If exists, then true, else false *)
}

let empty_ms_iter_info : ms_iter_info = {
  mii_iter_iv = Tz.PMap.empty;
  mii_map_iov = Tz.PMap.empty;
  mii_map_accv = Tz.PMap.empty;
  mii_iter_ef = Tz.PSet.empty;
  mii_map_ef = Tz.PSet.empty;
}

(* "intratrx_merge_state ss1 (ss2, mii)"
  Precondition: There should be no variable-name conflict between ss1 and ss2.
  Postcondition: (ss1 @ ss2, new-mii)
  Warning: It might emits tons of errors caused by List.tl and List.hd
*)
let intratrx_merge_state : Tz.sym_state -> (Tz.sym_state * ms_iter_info) -> (Tz.sym_state * ms_iter_info)
= let open Tz in
  let gdcc = gen_dummy_cc in (* sugar *)
  let stack_concat_tmpl : sym_state -> sym_state -> (mich_f list) -> sym_state
  = fun ss1 ss2 fl -> begin
    { ss1 with
      ss_block_mci=ss2.ss_block_mci;
      ss_symstack=ss2.ss_symstack;
      ss_constraints=(ss1.ss_constraints @ fl @ ss2.ss_constraints);
    }
  end in (* internal function stack_concat_tmpl end *)
  fun ss1 (ss2, mii) -> begin
  let ({mci_loc=ss1_b_loc; mci_cutcat=ss1_b_mcc;}, {mci_loc=ss2_e_loc; mci_cutcat=ss2_e_mcc;}) = (ss1.ss_block_mci, ss2.ss_entry_mci) in
  if ss1_b_loc <> ss2_e_loc then Stdlib.raise (Error (Stdlib.__LOC__)) else
  let (ss1bst, ss2est) = (ss1.ss_symstack, ss2.ss_entry_symstack) in
  match (ss1_b_mcc, ss2_e_mcc) with
  (*****************************************************************************)
  (* LOOP                                                                      *)
  (*****************************************************************************)
  | MCC_ln_loop, MCC_ln_loop ->
    (MF_not (MF_is_true (List.hd ss1bst))) :: (stack_eq_fmla (List.tl ss1bst) ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, mii))
  | MCC_ln_loop, MCC_lb_loop ->
    (MF_is_true (List.hd ss1bst)) :: (stack_eq_fmla (List.tl ss1bst) ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, mii))
  | MCC_lb_loop, MCC_ln_loop ->
    (MF_not (MF_is_true (List.hd ss1bst))) :: (stack_eq_fmla (List.tl ss1bst) ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, mii))
  | MCC_lb_loop, MCC_lb_loop -> 
    (MF_is_true (List.hd ss1bst)) :: (stack_eq_fmla (List.tl ss1bst) ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, mii))
  (*****************************************************************************)
  (* LOOP_LEFT                                                                 *)
  (*****************************************************************************)
  | MCC_ln_loopleft, MCC_ln_loopleft
  | MCC_ln_loopleft, MCC_lb_loopleft
  | MCC_lb_loopleft, MCC_ln_loopleft
  | MCC_lb_loopleft, MCC_lb_loopleft ->
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    let (hd1_lr, hd12) = (
      if (ss2_e_mcc = MCC_ln_loopleft)
      then (MF_not (MF_is_left hd1), MF_eq (MV_unlift_right hd1 |> gdcc, hd2))
      else (MF_is_left hd1, MF_eq (MV_unlift_left hd1 |> gdcc, hd2))
    ) in
    hd1_lr :: hd12 :: (stack_eq_fmla tl1 tl2)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, mii))
  (*****************************************************************************)
  (* MAP                                                                       *)
  (*****************************************************************************)
  | MCC_ln_map, MCC_ln_map ->
    (* Special Case - Empty Container *)
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    let (typ_hd1, typ_hd2) = (typ_of_val hd1, typ_of_val hd2) in
    let constraints : mich_f list =
      (match typ_hd1.cc_v, typ_hd2.cc_v with
      | MT_list _, MT_list _ -> (
          (* 1. input and output containers are nil *)
          let (f_inil, f_onil) = (MF_not (MF_is_cons hd1), MF_not (MF_is_cons hd2)) in
          (* 2. input and output container's sizes are 0 *)
          let (f_i0, f_o0) = (MF_eq (gdcc (MV_size_l hd1), gdcc (MV_lit_nat Z.zero)), MF_eq (gdcc (MV_size_l hd2), gdcc (MV_lit_nat Z.zero))) in
          [f_inil; f_onil; f_i0; f_o0]
        )
      | MT_map (kt1, vt1), MT_map (kt2, vt2) -> (
          (* 1. input and output containers are empty map *)
          let (f_iem, f_oem) = (MF_eq (hd1, gdcc (MV_empty_map (kt1, vt1))), MF_eq (hd2, gdcc (MV_empty_map (kt2, vt2)))) in
          (* 2. input and output container's sizes are 0 *)
          let (f_i0, f_o0) = (MF_eq (gdcc (MV_size_m hd1), gdcc (MV_lit_nat Z.zero)), MF_eq (gdcc (MV_size_m hd2), gdcc (MV_lit_nat Z.zero))) in
          [f_iem; f_oem; f_i0; f_o0]
        )
      | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
      )
    in
    let new_mii : ms_iter_info = {mii with mii_map_ef=(PSet.remove mii.mii_map_ef ss2.ss_entry_mci)} in
    constraints @ (stack_eq_fmla tl1 tl2)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, new_mii))

  | MCC_ln_map, MCC_lb_map ->
    (* Very Special Case - MAP instruction starts *)
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    let (typ_hd1, typ_hd2) = (typ_of_val hd1, typ_of_val hd2) in
    if (PSet.mem mii.mii_map_ef ss1.ss_block_mci |> Stdlib.not)
    (* CASE 1 : MAP instruction ending does not appeared before (special case) *)
    then (
      let ((map_iv, _(*map_ov*)), last_ielem) : (mich_v cc list * mich_v cc list) * mich_v cc = 
        (match PMap.find mii.mii_map_iov ss2.ss_entry_mci with
        | Some (Some iv, Some e, iovl) -> (List.split ((hd2, e) :: iovl), iv)
        | Some _ -> Stdlib.failwith Stdlib.__LOC__
        | None -> (([], []), hd2)
        )
      in
      let ielems = map_iv @ [last_ielem] in
      let elemlst_size : mich_v cc = gdcc (MV_lit_nat (Z.of_int (List.length ielems))) in
      let constraints : mich_f list = 
        (match typ_hd1.Tz.cc_v, typ_hd2.Tz.cc_v with
        | MT_list _, _ -> (
            (* 1. input-container contains input-elements *)
            (* No output container exists in this context *)
            let f_iccie : mich_f list = 
              List.fold_left 
                (fun (cur_l, fmla_l) ielem -> (gdcc (MV_tl_l cur_l), [MF_is_cons cur_l; MF_eq (gdcc (MV_hd_l cur_l), ielem)] @ fmla_l))
                (hd1, [])
                ielems
              |> Stdlib.snd
            in
            (* 2. (Size of input-container) >= input-elements *)
            let f_icsize : mich_f = 
              MF_eq ( gdcc (MV_compare (gdcc (MV_size_l hd1), elemlst_size)), gdcc (MV_lit_nat (Z.one)))
            in
            [MF_and f_iccie; f_icsize;]
          )
        | MT_map _, MT_pair _ -> (
            let ipl : (mich_v cc * mich_v cc) list = List.map (fun x -> (gdcc (MV_car x), gdcc (MV_cdr x))) ielems in
            let kpl : mich_v cc list = List.map (fun x -> Stdlib.fst x) ipl in
            (* 1. input-container contains input-elements *)
            let f_iccie : mich_f list = 
              List.fold_left
                (fun fmla_l (ielem_key, ielem_val) -> (MF_eq (gdcc (MV_get_xmoy (ielem_key, hd1)), gdcc (MV_some ielem_val))) :: fmla_l)
                []
                ipl
            in
            (* 2. (Size of input-container) >= input-elements *)
            let f_icsize : mich_f =
              MF_eq (gdcc (MV_compare (gdcc (MV_size_m hd1), elemlst_size)), gdcc (MV_lit_nat (Z.one)))
            in
            (* 3. key compare results *)
            let f_kcmp : mich_f list = 
              let rec foldf lst acc =
                (match lst with
                | [] -> acc
                | _ :: [] -> acc
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                )
              in
              foldf kpl []
            in
            [MF_and f_iccie; f_icsize; MF_and f_kcmp;]
          )
        | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
        )
      in
      let new_mii : ms_iter_info =
        (* reset iov and ef *)
        {mii with
          mii_map_iov=(PMap.remove mii.mii_map_iov ss2.ss_entry_mci);
          mii_map_ef=(PSet.remove mii.mii_map_ef ss1.ss_block_mci);
        }
      in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii))
    )
    (* CASE 2 : MAP instruction ending appeared before (normal case) *)
    else (
      let (map_iv, map_ov) : mich_v cc list * mich_v cc list = 
        (match PMap.find mii.mii_map_iov ss2.ss_entry_mci with
        | Some (None, Some e, iovl) -> (hd2, e) :: iovl
        | _ -> Stdlib.failwith Stdlib.__LOC__
        )
        |> List.split
      in
      let map_accv : mich_v cc = PMap.find mii.mii_map_accv ss1.ss_block_mci |> (function | Some e -> e | _ -> Stdlib.failwith Stdlib.__LOC__) in
      let map_ov_elt_typ : mich_t cc = typ_of_val (List.hd map_ov) in
      let map_c_size : mich_v cc = gdcc (MV_lit_nat (Z.of_int (List.length map_ov))) in
      let constraints : mich_f list = 
        (match typ_hd1.Tz.cc_v, typ_hd2.Tz.cc_v with
        | MT_list _, _ -> (
            (* 1. (input-container = inputs) && (output-container = outputs) *)
            let (f_ivs, f_ovs) = (MF_eq (hd1, gdcc (MV_lit_list (typ_hd2, map_iv))), MF_eq (map_accv, gdcc (MV_lit_list (map_ov_elt_typ, map_ov)))) in
            (* 2. Size of input-container and output-container *)
            let (f_icsize, f_ocsize) = (MF_eq (gdcc (MV_size_l hd1), map_c_size), MF_eq (gdcc (MV_size_l hd2), map_c_size)) in
            [f_ivs; f_ovs; f_icsize; f_ocsize]
          )
        | MT_map (kt, vt), MT_pair _ -> (
            let ipl : (mich_v cc * mich_v cc) list = List.map (fun x -> (gdcc (MV_car x), gdcc (MV_cdr x))) map_iv in
            let kpl : mich_v cc list = List.map (fun x -> Stdlib.fst x) ipl in
            let opl : (mich_v cc * mich_v cc) list = List.map2 (fun kv ov -> kv, ov) kpl map_ov in
            let imap : mich_v cc = gdcc (MV_lit_map (kt, vt, PMap.of_alist ipl |> (function | `Duplicate_key _ -> Stdlib.failwith Stdlib.__LOC__ | `Ok m -> m))) in
            let omap : mich_v cc = gdcc (MV_lit_map (kt, vt, PMap.of_alist opl |> (function | `Duplicate_key _ -> Stdlib.failwith Stdlib.__LOC__ | `Ok m -> m))) in    
            (* 1. (input-container = inputs) && (output-container = outputs) *)
            let (f_ivs, f_ovs) = (MF_eq (hd1, imap), MF_eq (map_accv, omap)) in
            (* 2. Size of input-container and output-container *)
            let (f_icsize, f_ocsize) = (MF_eq (gdcc (MV_size_m hd1), map_c_size), MF_eq (gdcc (MV_size_m hd2), map_c_size)) in
            (* 3. key compare results. *)
            let f_kcmp : mich_f list =
              let rec foldf lst acc = 
                (match lst with
                | [] -> acc 
                | _ :: [] -> acc
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                )
              in
              foldf kpl []
            in
            (* 4. let constraints = [...] *)
            [f_ivs; f_ovs; f_icsize; f_ocsize; MF_and f_kcmp;]
          )
        | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
        )
      in
      let new_mii : ms_iter_info =
        (* reset iov and accv and ef *) 
        {mii with 
          mii_map_accv=(PMap.remove mii.mii_map_accv ss1.ss_block_mci); 
          mii_map_iov=(PMap.remove mii.mii_map_iov ss2.ss_entry_mci);
          mii_map_ef=(PSet.remove mii.mii_map_ef ss1.ss_block_mci);
        }
      in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii)) 
    )

  | MCC_lb_map, MCC_ln_map ->
    (* Special Case - MAP instruction ends *)
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    (* let (typ_hd1, typ_hd2) = (Tz.typ_of_val hd1, Tz.typ_of_val hd2) in *)
    let new_mii : ms_iter_info = 
      (* 1. update mii_map_accv *)
      let updated_map_accv : (mich_cut_info, mich_v cc) PMap.t = 
        (PMap.update mii.mii_map_accv ss2.ss_entry_mci ~f:(function | None -> hd2 | _ -> Stdlib.failwith Stdlib.__LOC__)) 
      in
      (* 2. update mii_map_iov (only the first of the pair) *)
      let mii_map_iov_updated : (mich_cut_info, (mich_v cc option * mich_v cc option * ((mich_v cc * mich_v cc) list))) PMap.t = 
        (PMap.update mii.mii_map_iov ss1.ss_block_mci 
          ~f:(function | None -> (None, Some hd1, []) | _ -> Stdlib.failwith Stdlib.__LOC__)
        )
      in
      (* 3. udpate mii_map_ef *)
      let mii_map_ef_updated : mich_cut_info PSet.t = PSet.add mii.mii_map_ef ss2.ss_entry_mci in
      {mii with mii_map_accv=updated_map_accv; mii_map_iov=mii_map_iov_updated; mii_map_ef=mii_map_ef_updated;}
    in
    let constraints : mich_f list = [] in
    constraints @ (stack_eq_fmla tl1 tl2)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, new_mii))

  | MCC_lb_map, MCC_lb_map ->
    (* MAP loop continues *)
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    if (PSet.mem mii.mii_map_ef (ln_of_lb_exn ~debug:Stdlib.__LOC__ ss1.ss_block_mci) |> Stdlib.not)
    (* CASE 1 : MAP instruction ending does not appeared before (special case) *)
    then (
      let new_mii : ms_iter_info =
        (* 1. update mii_map_iov *)
        let mii_map_iov_updated : (mich_cut_info, (mich_v cc option * mich_v cc option * ((mich_v cc * mich_v cc) list))) PMap.t = 
          (PMap.update mii.mii_map_iov ss2.ss_entry_mci 
            ~f:(function | Some (ivopt, Some e, lst) -> (ivopt, Some hd1, (hd2, e) :: lst) 
                          | None -> (Some hd2, Some hd1, [])
                          | _ -> Stdlib.failwith Stdlib.__LOC__
            )
          )
        in
        {mii with mii_map_iov=mii_map_iov_updated;}
      in
      let constraints : mich_f list = [] in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii))
    )
    (* CASE 2 : MAP instruction ending appeared before (normal case) *)
    else (
      let new_mii : ms_iter_info = 
        (* 1. update mii_map_iov *)
        let mii_map_iov_updated : (mich_cut_info, (mich_v cc option * mich_v cc option * ((mich_v cc * mich_v cc) list))) PMap.t = 
          (PMap.update mii.mii_map_iov ss2.ss_entry_mci 
            ~f:(function | Some (ivopt, Some e, lst) -> (ivopt, Some hd1, (hd2, e) :: lst) | _ -> Stdlib.failwith Stdlib.__LOC__)
          )
        in
        {mii with mii_map_iov=mii_map_iov_updated;}
      in
      let constraints : mich_f list = [] in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii))
    )
  (*****************************************************************************)
  (* ITER                                                                      *)
  (*****************************************************************************)
  | MCC_ln_iter, MCC_ln_iter -> 
    (* Special Case - Empty Container *)
    let (hd1, tl1) = (List.hd ss1bst, List.tl ss1bst) in
    let typ_hd1 = typ_of_val hd1 in
    let constraints : mich_f list = 
      (match typ_hd1.cc_v with
      | MT_list _ -> (
          (* 1. input container is nil *)
          let f_inil = MF_not (MF_is_cons hd1) in
          (* 2. input container's size is 0 *)
          let f_i0 = MF_eq (gdcc (MV_size_l hd1), gdcc (MV_lit_nat Z.zero)) in
          [f_inil; f_i0;]
        )
      | MT_set _ -> (
          (* 1. input container is nil *)
          let f_ieset = MF_eq (hd1, gdcc (MV_empty_set typ_hd1)) in
          (* 2. input container's size is 0 *)
          let f_i0 = MF_eq (gdcc (MV_size_s hd1), gdcc (MV_lit_nat Z.zero)) in
          [f_ieset; f_i0;]
        )
      | MT_map (kt, vt) -> (
          (* 1. input container is nil *)
          let f_iemap = MF_eq (hd1, gdcc (MV_empty_map (kt, vt))) in
          (* 2. input container's size is 0 *)
          let f_i0 = MF_eq (gdcc (MV_size_m hd1), gdcc (MV_lit_nat Z.zero)) in
          [f_iemap; f_i0;]
        )
      | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
      )
    in
    let new_mii : ms_iter_info = mii in
    constraints @ (stack_eq_fmla tl1 ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, new_mii))
    
  | MCC_ln_iter, MCC_lb_iter -> 
    (* Very Special Case - ITER instruction starts *)
    let (hd1, tl1, hd2, tl2) = (List.hd ss1bst, List.tl ss1bst, List.hd ss2est, List.tl ss2est) in
    let typ_hd1 = typ_of_val hd1 in
    if (PSet.mem mii.mii_iter_ef ss1.ss_block_mci |> Stdlib.not)
    (* CASE 1 : ITER instruction ending does not appeared before (special case) *)
    then (
      let iter_iv : mich_v cc list = PMap.find mii.mii_iter_iv ss2.ss_entry_mci |> (function Some l -> (hd2 :: l) | None -> [hd2]) in
      let elemlst_size : mich_v cc = gdcc (MV_lit_nat (Z.of_int (List.length iter_iv))) in
      let constraints : mich_f list = 
        (match typ_hd1.Tz.cc_v with
        | MT_list _ -> (
            (* 1. input-container contains input-elements *)
            let f_iccie : mich_f list =
              List.fold_left 
                (fun (cur_l, fmla_l) ielem -> (gdcc (MV_tl_l cur_l), [MF_is_cons cur_l; MF_eq (gdcc (MV_hd_l cur_l), ielem)] @ fmla_l))
                (hd1, [])
                iter_iv
              |> Stdlib.snd
            in 
            (* (Size of input-container) >= input-elements *)
            let f_icsize : mich_f = 
              MF_eq ( gdcc (MV_compare (gdcc (MV_size_l hd1), elemlst_size)), gdcc (MV_lit_nat (Z.one)))
            in
            [MF_and f_iccie; f_icsize;]
          )
        | MT_set _ -> (
            (* 1. input-container contains input-elements *)
            let f_iccie : mich_f list = 
              List.fold_left
                (fun fmla_l ielem -> (MF_is_true (gdcc (MV_mem_xsb (ielem, hd1)))) :: fmla_l)
                []
                iter_iv
            in
            (* 2. (Size of input-container) >= input-elements *)
            let f_icsize : mich_f =
              MF_eq (gdcc (MV_compare (gdcc (MV_size_m hd1), elemlst_size)), gdcc (MV_lit_nat (Z.one)))
            in
            (* 3. key compare results *)
            let f_kcmp : mich_f list = 
              let rec foldf lst acc =
                (match lst with
                | [] -> acc
                | _ :: [] -> acc
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                )
              in
              foldf iter_iv []
            in
            [MF_and f_iccie; f_icsize; MF_and f_kcmp;]
          )
        | MT_map _ -> (
            let ipl : (mich_v cc * mich_v cc) list = List.map (fun x -> (gdcc (MV_car x), gdcc (MV_cdr x))) iter_iv in
            let kpl : mich_v cc list = List.map (fun x -> Stdlib.fst x) ipl in
            (* 1. input-container contains input-elements *)
            let f_iccie : mich_f list = 
              List.fold_left
                (fun fmla_l (ielem_key, ielem_val) -> (MF_eq (gdcc (MV_get_xmoy (ielem_key, hd1)), gdcc (MV_some ielem_val))) :: fmla_l)
                []
                ipl
            in
            (* 2. (Size of input-container) >= input-elements *)
            let f_icsize : mich_f =
              MF_eq (gdcc (MV_compare (gdcc (MV_size_m hd1), elemlst_size)), gdcc (MV_lit_nat (Z.one)))
            in
            (* 3. key compare results *)
            let f_kcmp : mich_f list = 
              let rec foldf lst acc =
                (match lst with
                | [] -> acc
                | _ :: [] -> acc
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                )
              in
              foldf kpl []
            in
            [MF_and f_iccie; f_icsize; MF_and f_kcmp;]
          )
        | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
        )
      in
      let new_mii : ms_iter_info =
        (* reset iv and ef *) 
        {mii with mii_iter_iv=(PMap.remove mii.mii_iter_iv ss2.ss_entry_mci); mii_iter_ef=(PSet.remove mii.mii_iter_ef ss1.ss_block_mci);}
      in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii)) 
    )
    (* CASE 2 : ITER instruction ending appeared before (normal case) *)
    else (
      let iter_iv : mich_v cc list = PMap.find mii.mii_iter_iv ss2.ss_entry_mci |> (function Some l -> (hd2 :: l) | _ -> Stdlib.failwith Stdlib.__LOC__) in
      let iter_c_size : mich_v cc = gdcc (MV_lit_nat (Z.of_int (List.length iter_iv))) in
      let constraints : mich_f list = 
        (match typ_hd1.Tz.cc_v with
        | MT_list elt -> (
            (* 1. input-container = inputs *)
            let f_ivs = MF_eq (hd1, gdcc (MV_lit_list (elt, iter_iv))) in
            (* 2. Size of input-container *)
            let f_icsize = MF_eq (gdcc (MV_size_l hd1), iter_c_size) in
            [f_ivs; f_icsize;]
          )
        | MT_set elt -> (
            (* 1. input-container = inputs *)
            let f_ivs = MF_eq (hd1, gdcc (MV_lit_set (elt, PSet.of_list iter_iv))) in
            (* 2. Size of input-container *)
            let f_icsize = MF_eq (gdcc (MV_size_s hd1), iter_c_size) in
            (* 3. key compare results *)
            let f_kcmp : mich_f list =
              let rec foldf lst acc = 
                (match lst with 
                | [] -> acc 
                | _ :: [] -> acc 
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                ) 
              in
              foldf iter_iv []
            in
            [f_ivs; f_icsize; MF_and f_kcmp;]
          )
        | MT_map (kt, vt) -> (
            let ipl : (mich_v cc * mich_v cc) list = List.map (fun x -> (gdcc (MV_car x), gdcc (MV_cdr x))) iter_iv in
            let kpl : mich_v cc list = List.map (fun x -> Stdlib.fst x) ipl in
            let imap : mich_v cc = gdcc (MV_lit_map (kt, vt, PMap.of_alist ipl |> (function | `Duplicate_key _ -> Stdlib.failwith Stdlib.__LOC__ | `Ok m -> m))) in    
            (* 1. input-container = inputs *)
            let f_ivs = MF_eq (hd1, imap) in
            (* 2. Size of input-container and output-container *)
            let f_icsize = MF_eq (gdcc (MV_size_m hd1), iter_c_size) in
            (* 3. key compare results *)
            let f_kcmp : mich_f list =
              let rec foldf lst acc = 
                (match lst with
                | [] -> acc 
                | _ :: [] -> acc
                | h1 :: h2 :: tl -> foldf (h2 :: tl) ((MF_eq (gdcc (MV_compare (h1, h2)), gdcc (MV_lit_int (Z.minus_one)))) :: acc)
                )
              in
              foldf kpl []
            in
            (* 4. let constraints = [...] *)
            [f_ivs; f_icsize; MF_and f_kcmp;]
          )
        | _ -> Error Stdlib.__LOC__ |> Stdlib.raise
        )
      in
      let new_mii : ms_iter_info =
        (* reset iv and ef *) 
        {mii with mii_iter_iv=(PMap.remove mii.mii_iter_iv ss2.ss_entry_mci); mii_iter_ef=(PSet.remove mii.mii_iter_ef ss1.ss_block_mci);}
      in
      constraints @ (stack_eq_fmla tl1 tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii)) 
    )

  | MCC_lb_iter, MCC_ln_iter -> 
    (* ITER instruction ends *)
    let mii_iter_iv_updated = PMap.update mii.mii_iter_iv ss1.ss_block_mci ~f:(function | None -> [] | Some _ -> Stdlib.failwith Stdlib.__LOC__) in
    let mii_iter_ef_updated = PSet.add mii.mii_iter_ef ss2.ss_entry_mci in
    let new_mii = {mii with mii_iter_iv=mii_iter_iv_updated; mii_iter_ef=mii_iter_ef_updated;} in
    (stack_eq_fmla ss1bst ss2est)
    |> stack_concat_tmpl ss1 ss2
    |> (fun ss -> (ss, new_mii))

  | MCC_lb_iter, MCC_lb_iter -> 
    (* ITER loop continues *)
    let (hd2, tl2) = (List.hd ss2est, List.tl ss2est) in
    if (PSet.mem mii.mii_iter_ef (ln_of_lb_exn ss1.ss_block_mci ~debug:Stdlib.__LOC__) |> Stdlib.not)
    (* CASE 1 : ITER instruction ending does not appeared before (special case) *)
    then (
      let new_mii : ms_iter_info = 
        (* 1. update mii_iter_iv *)
        let mii_iter_iv_updated : (mich_cut_info, mich_v cc list) PMap.t = 
          (PMap.update mii.mii_iter_iv ss2.ss_entry_mci 
            ~f:(function | Some lst -> hd2 :: lst | None -> [hd2])
          )
        in
        {mii with mii_iter_iv=mii_iter_iv_updated;}
      in
      let constraints : mich_f list = [] in
      constraints @ (stack_eq_fmla ss1bst tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii))
    )
    (* CASE 2 : ITER instruction ending appeared before (normal case) *)
    else (
      let new_mii : ms_iter_info = 
        (* 1. update mii_iter_iv *)
        let mii_iter_iv_updated : (mich_cut_info, mich_v cc list) PMap.t = 
          (PMap.update mii.mii_iter_iv ss2.ss_entry_mci ~f:(function | Some lst -> hd2 :: lst | _ -> Stdlib.failwith Stdlib.__LOC__))
        in
        {mii with mii_iter_iv=mii_iter_iv_updated;}
      in
      let constraints : mich_f list = [] in
      constraints @ (stack_eq_fmla ss1bst tl2)
      |> stack_concat_tmpl ss1 ss2
      |> (fun ss -> (ss, new_mii))
    )

  | _ -> Stdlib.raise (Error (Stdlib.__LOC__))
end (* function intratrx_merge_state end *)

