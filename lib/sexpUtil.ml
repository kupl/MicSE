open Core

let rec tz_cc_sexp_form : Sexp.t -> Sexp.t =
   let open Sexp in
   function
   | List
       [
         List [ Atom "cc_loc"; Atom "<opaque>" ];
         List [ Atom "cc_anl"; Atom "<opaque>" ];
         List [ Atom "cc_v"; body ];
       ] ->
     tz_cc_sexp_form body
   | Atom _ as s -> s
   | List sl -> List (List.map ~f:tz_cc_sexp_form sl)
