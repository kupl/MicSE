let open ProverLib.Smt in

let _ = Utils.Options.z3_time_budget := 300 in

(* Sort Generation *)
let mutez_sort = ZMutez.sort () in
let mutez_list_sort = ZList.create_sort ~content_sort:mutez_sort in

let mutez_form = fun () -> begin
  (* Expressions *)
  let sd = ZExpr.create_var mutez_sort ~name:"s_d" in
  let sd' = ZExpr.create_var mutez_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var mutez_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var mutez_list_sort ~name:"s_f1'" in
  (* let sf2 = ZExpr.create_var mutez_list_sort ~name:"s_f2" in *)
  let c = ZExpr.create_var mutez_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var mutez_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var mutez_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var mutez_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZList.read_tail sf1' in
  let hd_sf1' = ZList.read_head sf1' in

  (* Formulas *)
  let pre_inv = (
    let sum1 = ZMutez.create_add sd' sigma_sf1' in
    let noof1 = ZMutez.check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let fcons = (
    ZList.is_cons sf1') in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let sub = ZMutez.create_sub sigma_sf1' hd_sf1' in
    ZMutez.create_eq sigma_sf1 sub) in
  
  let fssf1nouf1 = (
    ZMutez.check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let add = ZMutez.create_add sd' hd_sf1' in
    ZMutez.create_eq sd add) in
  
  let fsd1noof1 = (
    ZMutez.check_add_no_overflow sd' hd_sf1') in

  let post_inv = (
    let sum1 = ZMutez.create_add sd sigma_sf1 in
    let noof1 = ZMutez.check_add_no_overflow sd sigma_sf1 in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd; fsd1noof1] in
  let f1c = ZFormula.create_and [post_inv] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in

let z3_list_form = fun () -> begin
  let z3_mutez_list_sort = Z3.Z3List.mk_sort (ZCtx.read ()) (ZSym.create "MutezList") mutez_sort in
  let z3_get_is_cons_decl = Z3.Z3List.get_is_cons_decl z3_mutez_list_sort in
  let z3_get_tail_decl = Z3.Z3List.get_tail_decl z3_mutez_list_sort in
  let z3_get_head_decl = Z3.Z3List.get_head_decl z3_mutez_list_sort in

  (* Expressions *)
  let sd = ZExpr.create_var mutez_sort ~name:"s_d" in
  let sd' = ZExpr.create_var mutez_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var z3_mutez_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var z3_mutez_list_sort ~name:"s_f1'" in
  let c = ZExpr.create_var mutez_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var mutez_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var mutez_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var mutez_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZFunc.apply z3_get_tail_decl ~params:[sf1'] in
  let hd_sf1' = ZFunc.apply z3_get_head_decl ~params:[sf1'] in

  (* Formulas *)
  let pre_inv = (
    let sum1 = ZMutez.create_add sd' sigma_sf1' in
    let noof1 = ZMutez.check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let fcons = (
    ZFunc.apply z3_get_is_cons_decl ~params:[sf1']) in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let sub = ZMutez.create_sub sigma_sf1' hd_sf1' in
    ZMutez.create_eq sigma_sf1 sub) in
  
  let fssf1nouf1 = (
    ZMutez.check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let add = ZMutez.create_add sd' hd_sf1' in
    ZMutez.create_eq sd add) in
  
  let fsd1noof1 = (
    ZMutez.check_add_no_overflow sd' hd_sf1') in

  let post_inv = (
    let sum1 = ZMutez.create_add sd sigma_sf1 in
    let noof1 = ZMutez.check_add_no_overflow sd sigma_sf1 in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd; fsd1noof1] in
  let f1c = ZFormula.create_and [post_inv] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in

let int_form = fun () -> begin
  let int_sort = ZInt.sort () in
  let int_list_sort = ZList.create_sort ~content_sort:int_sort in
  let mutez_bound = fun e -> (
    let mutez_upper = ZInt.of_zarith (Z.of_string "9223372036854775808") in
    let upbound = ZInt.create_le (ZInt.zero_ ()) e in
    let lobound = ZInt.create_lt e mutez_upper in
    ZFormula.create_and [upbound; lobound]) in
  let check_add_no_overflow = fun e1 e2 -> (
    let mutez_upper = ZInt.of_zarith (Z.of_string "9223372036854775808") in
    let add = ZInt.create_add [e1; e2] in
    ZInt.create_lt add mutez_upper) in
  let check_sub_no_underflow = fun e1 e2 -> (
    let sub = ZInt.create_sub [e1; e2] in
    ZInt.create_le (ZInt.zero_ ()) sub) in

  (* Expressions *)
  let sd = ZExpr.create_var int_sort ~name:"s_d" in
  let sd' = ZExpr.create_var int_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var int_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var int_list_sort ~name:"s_f1'" in
  let c = ZExpr.create_var int_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var int_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var int_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var int_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZList.read_tail sf1' in
  let hd_sf1' = ZList.read_head sf1' in

  (* Formulas *)
  let pre_inv = (
    let bound1 = mutez_bound sd' in
    let bound2 = mutez_bound sigma_sf1' in
    let bound3 = mutez_bound sigma_sf2 in
    let sum1 = ZInt.create_add [sd'; sigma_sf1'] in
    let noof1 = check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZInt.create_add [sum1; sigma_sf2] in
    let noof2 = check_add_no_overflow sum1 sigma_sf2 in
    let f = ZInt.create_eq sum2 c in
    ZFormula.create_and [bound1; bound2; bound3; noof1; noof2; f]) in

  let fcons = (
    ZList.is_cons sf1') in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let bound = mutez_bound sigma_sf1 in
    let sub = ZInt.create_sub [sigma_sf1'; hd_sf1'] in
    let f = ZInt.create_eq sigma_sf1 sub in
    ZFormula.create_and [bound; f]) in
  
  let fssf1nouf1 = (
    check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let bound = mutez_bound sd in
    let add = ZInt.create_add [sd'; hd_sf1'] in
    let f = ZInt.create_eq sd add in
    ZFormula.create_and [bound; f]) in
  
  let fsd1noof1 = (
    check_add_no_overflow sd' hd_sf1') in

  let post_inv = (
    let sum1 = ZInt.create_add [sd; sigma_sf1] in
    let noof1 = check_add_no_overflow sd sigma_sf1 in
    let sum2 = ZInt.create_add [sum1; sigma_sf2] in
    let noof2 = check_add_no_overflow sum1 sigma_sf2 in
    let f = ZInt.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd; fsd1noof1] in
  let f1c = ZFormula.create_and [post_inv] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in



let mutez_query = fun () -> begin
  (* Expressions *)
  let sd = ZExpr.create_var mutez_sort ~name:"s_d" in
  let sd' = ZExpr.create_var mutez_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var mutez_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var mutez_list_sort ~name:"s_f1'" in
  (* let sf2 = ZExpr.create_var mutez_list_sort ~name:"s_f2" in *)
  let c = ZExpr.create_var mutez_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var mutez_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var mutez_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var mutez_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZList.read_tail sf1' in
  let hd_sf1' = ZList.read_head sf1' in

  (* Formulas *)
  let pre_inv = (
    let sum1 = ZMutez.create_add sd' sigma_sf1' in
    let noof1 = ZMutez.check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let fcons = (
    ZList.is_cons sf1') in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let sub = ZMutez.create_sub sigma_sf1' hd_sf1' in
    ZMutez.create_eq sigma_sf1 sub) in
  
  let fssf1nouf1 = (
    ZMutez.check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let add = ZMutez.create_add sd' hd_sf1' in
    ZMutez.create_eq sd add) in
  
  let fsd1noof1 = (
    ZMutez.check_add_no_overflow sd' hd_sf1') in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd] in
  let f1c = ZFormula.create_and [fsd1noof1] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in

let z3_list_query = fun () -> begin
  let z3_mutez_list_sort = Z3.Z3List.mk_sort (ZCtx.read ()) (ZSym.create "MutezList") mutez_sort in
  let z3_get_is_cons_decl = Z3.Z3List.get_is_cons_decl z3_mutez_list_sort in
  let z3_get_tail_decl = Z3.Z3List.get_tail_decl z3_mutez_list_sort in
  let z3_get_head_decl = Z3.Z3List.get_head_decl z3_mutez_list_sort in

  (* Expressions *)
  let sd = ZExpr.create_var mutez_sort ~name:"s_d" in
  let sd' = ZExpr.create_var mutez_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var z3_mutez_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var z3_mutez_list_sort ~name:"s_f1'" in
  let c = ZExpr.create_var mutez_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var mutez_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var mutez_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var mutez_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZFunc.apply z3_get_tail_decl ~params:[sf1'] in
  let hd_sf1' = ZFunc.apply z3_get_head_decl ~params:[sf1'] in

  (* Formulas *)
  let pre_inv = (
    let sum1 = ZMutez.create_add sd' sigma_sf1' in
    let noof1 = ZMutez.check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZMutez.create_add sum1 sigma_sf2 in
    let noof2 = ZMutez.check_add_no_overflow sum1 sigma_sf2 in
    let f = ZMutez.create_eq sum2 c in
    ZFormula.create_and [noof1; noof2; f]) in

  let fcons = (
    ZFunc.apply z3_get_is_cons_decl ~params:[sf1']) in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let sub = ZMutez.create_sub sigma_sf1' hd_sf1' in
    ZMutez.create_eq sigma_sf1 sub) in
  
  let fssf1nouf1 = (
    ZMutez.check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let add = ZMutez.create_add sd' hd_sf1' in
    ZMutez.create_eq sd add) in
  
  let fsd1noof1 = (
    ZMutez.check_add_no_overflow sd' hd_sf1') in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd] in
  let f1c = ZFormula.create_and [fsd1noof1] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in

let int_query = fun () -> begin
  let int_sort = ZInt.sort () in
  let int_list_sort = ZList.create_sort ~content_sort:int_sort in
  let mutez_bound = fun e -> (
    let mutez_upper = ZInt.of_zarith (Z.of_string "9223372036854775808") in
    let upbound = ZInt.create_le (ZInt.zero_ ()) e in
    let lobound = ZInt.create_lt e mutez_upper in
    ZFormula.create_and [upbound; lobound]) in
  let check_add_no_overflow = fun e1 e2 -> (
    let mutez_upper = ZInt.of_zarith (Z.of_string "9223372036854775808") in
    let add = ZInt.create_add [e1; e2] in
    ZInt.create_lt add mutez_upper) in
  let check_sub_no_underflow = fun e1 e2 -> (
    let sub = ZInt.create_sub [e1; e2] in
    ZInt.create_le (ZInt.zero_ ()) sub) in

  (* Expressions *)
  let sd = ZExpr.create_var int_sort ~name:"s_d" in
  let sd' = ZExpr.create_var int_sort ~name:"s_d'" in
  let sf1 = ZExpr.create_var int_list_sort ~name:"s_f1" in
  let sf1' = ZExpr.create_var int_list_sort ~name:"s_f1'" in
  let c = ZExpr.create_var int_sort ~name:"c" in

  let sigma_sf1 = ZExpr.create_var int_sort ~name:"r_s_f1" in
  let sigma_sf2 = ZExpr.create_var int_sort ~name:"r_s_f2" in
  let sigma_sf1' = ZExpr.create_var int_sort ~name:"r_s_f1'" in

  let tl_sf1' = ZList.read_tail sf1' in
  let hd_sf1' = ZList.read_head sf1' in

  (* Formulas *)
  let pre_inv = (
    let bound1 = mutez_bound sd' in
    let bound2 = mutez_bound sigma_sf1' in
    let bound3 = mutez_bound sigma_sf2 in
    let sum1 = ZInt.create_add [sd'; sigma_sf1'] in
    let noof1 = check_add_no_overflow sd' sigma_sf1' in
    let sum2 = ZInt.create_add [sum1; sigma_sf2] in
    let noof2 = check_add_no_overflow sum1 sigma_sf2 in
    let f = ZInt.create_eq sum2 c in
    ZFormula.create_and [bound1; bound2; bound3; noof1; noof2; f]) in

  let fcons = (
    ZList.is_cons sf1') in

  let fsf1 = (
    ZList.create_eq sf1 tl_sf1') in

  let fssf1 = (
    let bound = mutez_bound sigma_sf1 in
    let sub = ZInt.create_sub [sigma_sf1'; hd_sf1'] in
    let f = ZInt.create_eq sigma_sf1 sub in
    ZFormula.create_and [bound; f]) in
  
  let fssf1nouf1 = (
    check_sub_no_underflow sigma_sf1' hd_sf1') in

  let fsd = (
    let bound = mutez_bound sd in
    let add = ZInt.create_add [sd'; hd_sf1'] in
    let f = ZInt.create_eq sd add in
    ZFormula.create_and [bound; f]) in
  
  let fsd1noof1 = (
    check_add_no_overflow sd' hd_sf1') in

  let f1p = ZFormula.create_and [pre_inv; fcons; fsf1; fssf1; fssf1nouf1; fsd] in
  let f1c = ZFormula.create_and [fsd1noof1] in
  let f1 = ZFormula.create_imply f1p f1c in

  (* Formula Addition to Solver *)
  let validity, model = ZSolver.check_validity [f1] in
  let _ = print_endline (validity |> ZSolver.string_of_validity) in
  if validity |> ZSolver.is_invalid then print_endline (model |> Option.get |> ZModel.to_string) else ()
end in




let get_time_exec = fun f -> begin
  let timer = Utils.Timer.create ~budget:300 in
  let _ = f () in
  let _ = print_endline ("----- elaped time:" ^ (timer |> Utils.Timer.read_interval |> Utils.Timer.string_of_time)) in
  ()
end in



get_time_exec mutez_form;
get_time_exec z3_list_form;
get_time_exec int_form;

get_time_exec mutez_query;
get_time_exec z3_list_query;
get_time_exec int_query;