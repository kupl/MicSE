(* Execute pre program *)
let _ = print_endline ("- Execute pre program") in
let rec pre i = if i < 5000000000 then pre (i+1) else () in
let _ = pre 0 in
let _ = print_endline ("- End of pre program") in

(* Make timer object *)
let timer = Utils.Timer.create () ~budget:3 in

(* Print start time after process launched *)
let _ = print_endline ("- Timer Start At: " ^ (Utils.Timer.string_of_curr_time ())) in

(* Check timeout *)
let _ = if Utils.Timer.is_timeout timer then print_endline "TIMEOUT" else print_endline "NOT_TIMEOUT" in

(* Execute program what we want to check timed out *)
let _ = print_endline ("- Execute main program") in
let rec main i = if i < 10000000000 then main (i+1) else () in
let _ = main 0 in
let _ = print_endline ("- End of main program") in

(* Check timeout *)
let _ = if Utils.Timer.is_timeout timer then print_endline "TIMEOUT" else print_endline "NOT_TIMEOUT" in

(* Print interval time between now and point when timer set *)
let _ = print_endline ("- Interval: " ^ (Utils.Timer.string_of_elapsed_time timer)) in

(* Print current execution time *)
let _ = print_endline ("- Finish At: " ^ (Utils.Timer.string_of_curr_time ())) in

()