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

let rec tz_remove_ctx_i_ctx_v : Sexp.t -> Sexp.t =
   let open Sexp in
   function
   | List [ List [ Atom "ctx_i"; ctx ]; List [ Atom "ctx_v"; body ] ] -> (
     match tz_remove_ctx_i_ctx_v body with
     | List l      -> List (ctx :: l)
     | Atom _ as s -> List [ ctx; s ]
   )
   | Atom _ as s -> s
   | List sl -> List (List.map ~f:tz_remove_ctx_i_ctx_v sl)

let to_string : Sexp.t -> string =
  fun sexp ->
  sexp |> tz_cc_sexp_form |> tz_remove_ctx_i_ctx_v |> Sexp.to_string
