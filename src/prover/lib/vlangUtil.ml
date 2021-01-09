(* Naive Formula Optimizations *)
module NaiveOpt = struct
  open Vlang.Formula

  (* "and_flatten" removes nested-AND form. It will iterate every subformula. *)
  let rec and_flatten : Vlang.t -> Vlang.t
  =(function
    | VF_and t -> VF_and (List.fold_left (fun acc f -> acc @ (and_flatten_i f)) [] t)
    | VF_or t -> VF_or (List.map and_flatten t)
    | VF_not f -> VF_not (and_flatten f)
    | VF_imply (t1,t2) -> VF_imply (and_flatten t1, and_flatten t2)
    | _ as f -> f
  )
  and and_flatten_i : Vlang.t -> Vlang.t list
  =(function | VF_and t -> List.fold_left (fun acc f -> acc @ (and_flatten_i f)) [] t | f -> [and_flatten f])

  (* "or_flatten" removes nested-OR form. It will iterate every subformula *)
  let rec or_flatten : Vlang.t -> Vlang.t
  =(function
    | VF_and t -> VF_and (List.map or_flatten t)
    | VF_or t -> VF_or (List.fold_left (fun acc f -> acc @ (or_flatten_i f)) [] t)
    | VF_not f -> VF_not (or_flatten f)
    | VF_imply (t1,t2) -> VF_imply (or_flatten t1, or_flatten t2)
    | _ as f -> f
  )
  and or_flatten_i : Vlang.t -> Vlang.t list
  =(function | VF_or t -> List.fold_left (fun acc f -> acc @ (or_flatten_i f)) [] t | f -> [or_flatten f])

  (* Testcase for "and_flatten" *)
  (*
      (* 1. You can copy and paste below testcase to ocaml REPL. *)
      (* 2. Check if the codes below and above are same (except type definition) *)

      type t = 
      | VF_and of t list
      | VF_or of t list
      | VF_not of t
      | VF_imply of t * t
      | HI
      ;;

      let rec and_flatten : t -> t
      =(function
        | VF_and t -> VF_and (List.fold_left (fun acc f ->  acc @ (and_flatten_i f)) [] t)
        | VF_or t -> VF_or (List.map and_flatten t)
        | VF_not f -> VF_not (and_flatten f)
        | VF_imply (t1,t2) -> VF_imply (and_flatten t1, and_flatten t2)
        | _ as f -> f
      )
      and and_flatten_i : t -> t list
      =(function | VF_and t -> List.fold_left (fun acc f -> acc @ (and_flatten_i f)) [] t | f -> [and_flatten f])
      ;;

      (* RUN *)
      and_flatten (
        VF_and [
          VF_and [
            VF_or [
              VF_and [HI; HI];
              HI; 
              VF_and [HI; VF_and [HI; HI]]; 
              VF_imply (VF_and [HI; HI;], HI)
            ]; 
            VF_or [HI; HI; VF_and [HI; HI]]
          ]; 
          VF_imply (HI, VF_and [HI; HI]);
          VF_and [VF_and [HI; HI]; HI];
          VF_and [HI; HI];
        ]
      )
      ;;
  *)

  (* NaiveOpt.opt optimizes formula based on the existence of constant boolean values, 
    such as VF_true and VF_false *)
  let rec opt : Vlang.t -> Vlang.t
  =fun fmla -> begin
    match fmla with
    | VF_and l -> opt_andlst l
    | VF_or l -> opt_orlst l
    | VF_not f -> (
      let ofmla = opt f in
      match opt f with
      | VF_true -> VF_false
      | VF_false -> VF_true
      | _ -> VF_not ofmla
      )
    | VF_imply (f1, f2) -> (
      let of1, of2 = opt f1, opt f2 in
      match of1, of2 with
      | VF_true, _ -> of2
      | VF_false, _ -> VF_true
      | _, VF_true -> VF_true
      | _, VF_false -> VF_not of1 (* "VF_not of1" has been carefully checked and concluded that no further optimization required. *)
      | _ -> VF_imply (of1, of2)
    )
    | _ -> fmla
  end (* function opt end *)
  and opt_andlst : Vlang.t list -> Vlang.t
  =fun flst -> begin
    let oflst = List.map opt flst in
    (* If "VF_false" exists, entire formula will be substituted to "VF_false" *)
    if List.exists (function VF_false -> true | _ -> false) oflst then VF_false else
    (* If "VF_true" exists, remove it *)
    let oflst' = List.filter (function VF_true -> false | _ -> true) oflst in
    (* be aware of empty-list cases *)
    (match oflst' with
    | [] -> VF_true
    | f :: [] -> f
    | _ -> VF_and oflst'
    )
  end (* function opt_andlst end *)
  and opt_orlst : Vlang.t list -> Vlang.t
  =fun flst -> begin
    let oflst = List.map opt flst in
    (* If "VF_true" exists, entire formula will be "VF_trues" *)
    if List.exists (function VF_true -> true | _ -> false) oflst then VF_true else
    (* If "VF_false" exists, remove it *)
    let oflst' = List.filter (function VF_false -> false | _ -> true) oflst in
    (* be aware of empty-list cases *)
    (match oflst' with
    | [] -> VF_false
    | f :: [] -> f
    | _ -> VF_or oflst'
    )
  end (* function opt_orlst end *)
  

  (* "run" is a user-level interface for NaiveOpt module *)
  let run : Vlang.t -> Vlang.t
  =fun fmla -> begin
    (* 1. and_flatten & or_flatten *)
    let flattend_fmla = fmla |> and_flatten |> or_flatten in
    opt flattend_fmla
  end (* function NaiveOpt.run end *)

end (* module NaiveOpt end *)