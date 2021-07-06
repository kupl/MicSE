open Domainslib

type job = F | S
type info = Info | Wrong

let num_jobs = 2
let pool = Task.setup_pool ~num_additional_domains:num_jobs

let mc = Chan.make_unbounded ()
let fc = Chan.make_unbounded ()
let sc = Chan.make_unbounded ()


let first (nf, ns) = (
  let rec get_all_msg acc = (
    let msg = Chan.recv_poll fc in
    if Option.is_none msg then acc else (
      get_all_msg (acc + (match Option.get msg with Info -> 1 | _ -> Failure "" |> Stdlib.raise)))
    ) in
  let _ = Unix.sleep 5 in
  let ns' = ns - (get_all_msg 0) in
  let _ = Chan.send mc F in
  if (nf = 0) then (nf, ns') else (nf - 1, ns')
  )

let second (nf, ns) = (
  let rec get_all_msg acc = (
    let msg = Chan.recv_poll sc in
    if Option.is_none msg then acc else (
      get_all_msg (acc + (match Option.get msg with Info -> 1 | _ -> Failure "" |> Stdlib.raise)))
    ) in
  let _ = Unix.sleep 2 in
  let nf' = nf - (get_all_msg 0) in
  let _ = Chan.send mc S in
  if (ns = 0) then (nf', ns) else (nf', ns - 1)
  )


let call_f pair = Task.async pool (fun _ -> first pair)
let call_s pair = Task.async pool (fun _ -> second pair)


let _ = (
  (* 1. Initialize Input Arguments *)
  Utils.Options.create_options ();
  (* 2. Initialize Logger *)
  Utils.Log.create ();

  let rec loop pf ps cur_res = (
    Utils.Log.app (fun m -> m "Manager Loop Start: (%d, %d)" (fst cur_res) (snd cur_res));
    if cur_res = (0, 0) then () else
    let done_proc = Chan.recv mc in
    match done_proc with
    | F -> (
      Utils.Log.app (fun m -> m "First job is done");
      let fres = Task.await pool pf in
      let _ = Chan.send sc Info in
      let pf' = if fst fres > 0 then call_f (fst fres, snd cur_res) else pf in
      loop pf' ps fres
    )
    | S -> (
      Utils.Log.app (fun m -> m "Second job is done");
      let sres = Task.await pool ps in
      let _ = Chan.send fc Info in
      let ps' = if snd sres > 0 then call_s (fst cur_res, snd sres) else ps in
      loop pf ps' sres
    )  
  ) in

  Utils.Log.app (fun m -> m "Start");
  let init_pair = (4, 10) in
  let proc_f = call_f init_pair in
  let proc_s = call_s init_pair in
  let _ = loop proc_f proc_s init_pair in
  Utils.Log.app (fun m -> m "Done")
)

