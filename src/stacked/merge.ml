(* Merge Two States *)

(* IMPORTANT NOTE
  Every concepts in "Se" module were designed with forward symbolic execution in mind.
  So, well-constructed "Se.sym_state" and "Se.state_set" contains 
  various Tezos system abstraction such as fixed/dynamic blockchain status
  and internal operation queues (though queue will be changed into stack for later
  Tezos version).
  
  However, the "Refute" module uses state-merging (module "Merge") to construct
  symbolic-executed results from back to forward, it is impossible to form soundly
  blockchain-specific abstractions when merging two different transaction states.

  Therefore, "Merge" and "Refute" modules will not strictly follows the Tezos
  abstraction defined in "Se".
   
  Instead, they will consider following Tezos/Michelson abstractions only:
  [ Refute Target Properties ]
  - "Tz.ss_entry_mci", "Tz.ss_entry_symstack", "Tz.ss_block_mci", "Tz.ss_symstack", "Tz.ss_constraints"
  - "Se.query_category"
  - "Jc.Rcfv" to track non-michelson contexts
  - "Jc.Stvn" to avoid variable name conflicts when merging two states
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


(*****************************************************************************)
(*****************************************************************************)
(* Merge States in Inter-Transaction Situation                               *)
(*****************************************************************************)
(*****************************************************************************)

(* "ms" : Merged State Type 
  ms_state    : merged state
  ms_te_count : count transaction-entered
  ms_le_count : count loop-entered (initialized when the state leaves current transaction)
  ms_le_stack : count loop-entered using stack. It is useful to restrict the number of loop unrolling.
  ms_iinfo    : iteration information for intratrx-merge.
  ms_qcopt    : query-category if exists
*)
  type ms = {
    ms_state    : Tz.sym_state;
    ms_te_count : int;
    ms_le_count : (Tz.mich_cut_info, int) Tz.PMap.t;
    ms_le_stack : (Tz.mich_cut_info * int) list;
    ms_iinfo    : ms_iter_info;
    ms_querycat : Se.query_category option;
  }

let intertrx_merge_state : Tz.sym_state -> Tz.sym_state -> Tz.sym_state
= let open Tz in
  fun ss ms -> begin
  let ss_exit_strg : mich_v cc = MV_cdr (List.hd ss.ss_symstack) |> gen_dummy_cc in
  let ms_entry_strg : mich_v cc = MV_cdr (List.hd ms.ss_entry_symstack) |> gen_dummy_cc in
  let fl : mich_f list = [MF_eq (ss_exit_strg, ms_entry_strg)] in
  { ss with
    ss_block_mci=ms.ss_block_mci;
    ss_symstack=ms.ss_symstack;
    ss_constraints=(ss.ss_constraints @ fl @ ms.ss_constraints);
  }
end (* function intertrx_merge_state end *)


(*****************************************************************************)
(*****************************************************************************)
(* Expand states (merging / unrolling restriction and renaming considered)   *)
(*****************************************************************************)
(*****************************************************************************)

type expand_param = {
  ep_bss : (Tz.mich_cut_info, Tz.sym_state Tz.PSet.t) Tz.PMap.t;  (* blocked-states. key-mci should be symstate's blocked-mci *)
  ep_uloop_lim : int; (* loop-unrolling-numbers in [1, ep_uloop_lim] are allowed. Negative value for no-limit *)
  ep_utrx_lim : int;  (* trx-unrolling-numbers in [1, ep_utrx_lim] are allowed. Negative value for no-limit *)
}


(*****************************************************************************)
(* Set Structured Variable Names to sym_state                                *)
(*****************************************************************************)

(* "set_stvn_ss" modifies variables following "Refute Target Properties",
  - ("Tz.ss_entry_mci")
  - "Tz.ss_entry_symstack"
  - ("Tz.ss_block_mci")
  - "Tz.ss_symstack"
  - "Tz.ss_constraints"
*)

let set_stvn_ss : (int option * int option) -> Tz.sym_state -> Tz.sym_state
= let open Tz in
  fun (tnopt, lnopt) ss -> begin
  let stvn_s : mich_v cc -> mich_v cc = Se.set_stvn_mv (tnopt, lnopt) in
  let stvn_f : mich_f -> mich_f = Se.set_stvn_mf (tnopt, lnopt) in
  {ss with
    ss_entry_symstack = List.map stvn_s ss.ss_entry_symstack;
    ss_symstack = List.map stvn_s ss.ss_symstack;
    ss_constraints = List.map stvn_f ss.ss_constraints;
  }
end (* function set_stvn_ss end *)


(*****************************************************************************)
(* Expand                                                                    *)
(*****************************************************************************)

(* "expand_i" 
  - filter state-set by trx, loop unrolling limits.
  - rename state
  - update merged-state (ms)
*)
let expand_i : expand_param -> (Tz.sym_state Tz.PSet.t) -> ms -> (ms Tz.PSet.t)
= let open Tz in
  fun ep sset ms -> begin
  let ms_en_mci = ms.ms_state.ss_entry_mci in
  PSet.fold sset ~init:PSet.empty
    ~f:(fun accs ss ->
      (* TODO : There are no filtering logic *)
      let ss_en_mci = ss.ss_block_mci in
      let (ssmcc, mmmcc) = (ss_en_mci.mci_cutcat, ms_en_mci.mci_cutcat) in
      match (is_ln_mcc ssmcc, is_lb_mcc ssmcc, is_ln_mcc mmmcc, is_lb_mcc mmmcc) with
      | (true, false, true, false) -> (
          (* LN -> LN *)
          let ss_trx_num = ms.ms_te_count in
          (* note : cur_loop_num : "[] -> 0" for query-loop case *)
          let ss_loop_num = (match ms.ms_le_stack with | [] -> 0 | (_, hdv) :: _ -> hdv) in
          (* No loop-unroll limitation checking is needed in this case *)
          let ss' = set_stvn_ss (Some ss_trx_num, Some ss_loop_num) ss in
          let (ms', iinfo') = intratrx_merge_state ss' (ms.ms_state, ms.ms_iinfo) in
          {ms with ms_state=(ms'); ms_iinfo=(iinfo');}
          |> PSet.add accs
        )
      | (true, false, false, true) -> (
          (* LN -> LB *)
          let ss_trx_num = ms.ms_te_count in
          (* note : cur_loop_num : "_ -> 0" for query-loop case *)
          let ss_loop_num = (match ms.ms_le_stack with | _ :: (_, hdv) :: _ -> hdv | _ -> 0) in
          (* No loop-unroll limitation checking is needed in this case *)
          let ss' = set_stvn_ss (Some ss_trx_num, Some ss_loop_num) ss in
          let (ms', iinfo') = intratrx_merge_state ss' (ms.ms_state, ms.ms_iinfo) in
          let le_stack' = (match ms.ms_le_stack with | [] -> [] | _ :: tl -> tl) in
          {ms with ms_state=(ms'); ms_iinfo=(iinfo'); ms_le_stack=(le_stack');}
          |> PSet.add accs
        )
      | (false, true, true, false) -> (
          (* LB -> LN *)
          let ss_trx_num = ms.ms_te_count in
          let ss_loop_count = PMap.update ms.ms_le_count ss.ss_block_mci ~f:(function | None -> 1 | Some n -> (n+1)) in
          let ss_loop_num = pmap_find_dft ms.ms_le_count ss.ss_block_mci ~default:0 in
          (* I'll not put any loop-unroll limitation checking in this case too. Query itself might be located in the loop *)
          let ss' = set_stvn_ss (Some ss_trx_num, Some ss_loop_num) ss in
          let (ms', iinfo') = intratrx_merge_state ss' (ms.ms_state, ms.ms_iinfo) in
          let le_stack' = (ss.ss_block_mci, 1) :: ms.ms_le_stack in
          {ms with ms_state=(ms'); ms_iinfo=(iinfo'); ms_le_count=ss_loop_count; ms_le_stack=(le_stack');}
          |> PSet.add accs
        )
      | (false, true, false, true) -> (
          (* LB -> LB *)
          let ss_trx_num = ms.ms_te_count in
          let ss_loop_count = PMap.update ms.ms_le_count ss.ss_block_mci ~f:(function | None -> 1 | Some n -> (n+1)) in
          let ss_loop_num = pmap_find_dft ms.ms_le_count ss.ss_block_mci ~default:0 in
          (* loop-unroll limitation checking here *)
          if (0 <= ep.ep_utrx_lim && ep.ep_uloop_lim < ss_loop_num) then accs else
          let ss' = set_stvn_ss (Some ss_trx_num, Some ss_loop_num) ss in
          let (ms', iinfo') = intratrx_merge_state ss' (ms.ms_state, ms.ms_iinfo) in
          let le_stack' = (match ms.ms_le_stack with | [] -> [(ss.ss_block_mci, 1)] | (bmci, n) :: tl -> (bmci, (n+1)) :: tl) in
          {ms with ms_state=(ms'); ms_iinfo=(iinfo'); ms_le_count=ss_loop_count; ms_le_stack=(le_stack');}
          |> PSet.add accs
        )
      | (false, false, false, false) -> (
          (* TRX-EXIT -> TRX-ENTRY *)
          let ss_trx_num = ms.ms_te_count + 1 in
          (* trx-unroll limitation checking here *)
          if (0 <= ep.ep_utrx_lim && ep.ep_utrx_lim < ss_trx_num) then accs else
          let ss' = set_stvn_ss (Some ss_trx_num, Some 0) ss in
          let ms' = intertrx_merge_state ss' ms.ms_state in
          { ms_state    = (ms');
            ms_te_count = ss_trx_num;
            ms_le_count = ms.ms_le_count;
            ms_le_stack = [];
            ms_iinfo    = empty_ms_iter_info;
            ms_querycat = ms.ms_querycat;
          }
          |> PSet.add accs
        )
      | _ -> Stdlib.failwith Stdlib.__LOC__
    )
end (* function expand_i end *)

(* "expand" 
  - filters state-set by mich-cut-info.
*)
let expand : expand_param -> (ms Tz.PSet.t) -> (ms Tz.PSet.t)
= let open Tz in
  fun ep msset -> begin
  PSet.fold msset ~init:PSet.empty 
    ~f:(fun accs ms ->
      let ms_en_mci = ms.ms_state.ss_entry_mci in
      let sset : sym_state PSet.t =
        if ms_en_mci.mci_cutcat = MCC_trx_entry 
        then (
          let tex_mci = {ms_en_mci with mci_cutcat=MCC_trx_exit;} in
          pmap_find_dft ep.ep_bss tex_mci ~default:PSet.empty
        )
        else (
          let lb_mci = lb_of_ln_mci ms_en_mci |> Option.value ~default:ms_en_mci in
          let ln_mci = ln_of_lb_mci ms_en_mci |> Option.value ~default:ms_en_mci in
          PSet.union
            (pmap_find_dft ep.ep_bss lb_mci ~default:PSet.empty)
            (pmap_find_dft ep.ep_bss ln_mci ~default:PSet.empty)
        )
      in
      PSet.union (expand_i ep sset ms) accs
    )
end (* function expand end *)