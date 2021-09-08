(* Memory Manager *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type typ =
    | Self
    | Children
    | Both

  type t = {
    typ : typ;
    base : int;
    memoryout : bool;
    budget : int option;
  }

  let kB : int = 1024

  let mB : int = 1024 * 1024

  let gB : int = 1024 * 1024 * 1024

  let unit_size : int option =
     match Core.Unix.open_process_in "uname" |> input_line with
     | "Darwin" -> Some 1 (* Byte *)
     | "Linux"  -> Some kB (* Kilo-Byte *)
     | _        -> None
  (* Cannot restrict memory *)

  (* Current Maximum Memory Usage in Bytes *)
  let cur_max_memory : typ -> int =
     let module Rusage = Core.Unix.Resource_usage in
     fun typ ->
     if Option.is_none unit_size
     then 0
     else
       ((match typ with
        | Self     -> Rusage.get `Self
        | Children -> Rusage.get `Children
        | Both     -> Rusage.add (Rusage.get `Self) (Rusage.get `Children))
       |> Rusage.maxrss
       |> Int64.to_int
       )
       * Option.get unit_size

  let create : ?budget:int -> ?typ:typ -> unit -> t =
    fun ?(budget = -1) ?(typ = Both) () ->
    {
      typ;
      base = cur_max_memory typ;
      memoryout = false;
      budget = (if budget < 0 then None else Some (budget * gB));
    }

  let read_memory_usage : t -> int =
    (fun memory -> cur_max_memory memory.typ - memory.base)
  (* function read_memory_usage end *)

  let read_memory_remain : t -> int =
    fun memory ->
    if Option.is_none memory.budget
    then 0
    else Stdlib.max (Option.get memory.budget - read_memory_usage memory) 0
  (* function read_memory_remain end *)

  let read_is_memoryout : t -> bool =
    fun memory ->
    if Option.is_none memory.budget
    then false
    else read_memory_usage memory >= Option.get memory.budget
  (* function read_is_memoryout end *)

  let update_memoryout : t -> t =
    fun memory ->
    if read_is_memoryout memory
    then { memory with memoryout = true }
    else memory
  (* function update_memoryout end *)
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref

type typ = Setting.typ

type memory = int (* in GigaBytes *)

(* Current maximum usage of memory in this execution *)
let memory_curr : ?typ:typ -> unit -> memory =
  (fun ?(typ = Both) () -> Setting.cur_max_memory typ / Setting.gB)

(* Current maximum usage of memory in this execution *)
let string_of_curr_memory : ?typ:typ -> unit -> string =
  fun ?(typ = Both) () ->
  ((Setting.cur_max_memory typ |> Float.of_int) /. (Setting.gB |> Float.of_int)
  |> string_of_float
  )
  ^ " GB"

(* Create memory restriction in gigabytes *)
let create : ?budget:int -> ?typ:typ -> unit -> t =
  fun ?(budget = -1) ?(typ = Both) () ->
  Stdlib.ref (Setting.create ~budget ~typ ())
(* function create end *)

let read_used_memory : t -> memory =
  (fun memory -> Setting.read_memory_usage !memory / Setting.gB)

let read_remaining : t -> memory =
  (fun memory -> Setting.read_memory_remain !memory / Setting.gB)

let check_memoryout : t -> unit =
  fun memory ->
  if Setting.read_is_memoryout !memory
  then memory := Setting.update_memoryout !memory
  else ()

let is_memoryout : t -> bool =
  fun memory ->
  let _ = check_memoryout memory in
  !memory.memoryout

let string_of_used_memory : t -> string =
  fun memory ->
  ((Setting.read_memory_usage !memory |> Float.of_int)
   /. (Setting.gB |> Float.of_int)
  |> string_of_float
  )
  ^ " GB"

let sexp_of_t : t -> Core.Sexp.t =
   let open Core in
   fun memory ->
   List [ Atom "budget"; sexp_of_option sexp_of_int !memory.budget ]

let t_of_sexp : Core.Sexp.t -> t =
   let open Core in
   fun sexp ->
   match sexp with
   | List [ Atom "budget"; budget ] ->
     let (budget_opt : float option) = option_of_sexp float_of_sexp budget in
     create ()
       ~budget:(Option.value budget_opt ~default:Float.zero |> int_of_float)
   | _ -> Failure "" |> raise

let compare : t -> t -> int =
   let open Core in
   fun memory1 memory2 ->
   compare_option compare_int !memory1.budget !memory2.budget

let equal : t -> t -> bool =
   let open Core in
   fun memory1 memory2 ->
   equal_option equal_int !memory1.budget !memory2.budget
