(* Memory Manager *)

open! Core

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
  [@@deriving sexp, compare, equal]

  type t = {
    typ : typ;
    created_at : int64;
    expired_at : int64 option;
  }
  [@@deriving sexp, compare, equal]

  let (kB : int64) = Int64.of_int 1024

  let (mB : int64) = Int64.of_int (1024 * 1024)

  let (gB : int64) = Int64.of_int (1024 * 1024 * 1024)

  let unit_size : int64 option =
     match Core.Unix.open_process_in "uname" |> In_channel.input_line with
     | Some "Darwin" -> Some Int64.one (* Byte *)
     | Some "Linux"  -> Some kB (* Kilo-Byte *)
     | _             -> None

  let cur_max_memory : typ -> int64 =
     let module Rusage = Core.Unix.Resource_usage in
     fun typ ->
     if Option.is_none unit_size
     then Int64.zero
     else
       Int64.( * )
         ((match typ with
          | Self     -> Rusage.get `Self
          | Children -> Rusage.get `Children
          | Both     -> Rusage.add (Rusage.get `Self) (Rusage.get `Children))
         |> Rusage.maxrss
         )
         (Option.value unit_size ~default:Int64.one)
  (* function cur_max_memory end *)

  let create : ?budget:int -> ?typ:typ -> unit -> t =
    fun ?(budget = -1) ?(typ = Both) () ->
    let (now : int64) = cur_max_memory typ in
    {
      typ;
      created_at = now;
      expired_at =
        ( if budget < 0
        then None
        else
          Int64.( * ) gB (Int64.of_int budget) |> Int64.( + ) now |> Option.some
        );
    }
  (* function create end *)

  let read_memory_used : t -> int64 =
    (fun memory -> Int64.( - ) (cur_max_memory memory.typ) memory.created_at)
  (* function read_memory_used end *)

  let read_memory_remain : t -> int64 =
    fun memory ->
    let (now : int64) = cur_max_memory memory.typ in
    let (expired : int64) = Option.value memory.expired_at ~default:now in
    Int64.max (Int64.( - ) expired now) Int64.zero
  (* function read_memory_remain end *)

  let read_is_memoryout : t -> bool =
    fun memory ->
    let (now : int64) = cur_max_memory memory.typ in
    let (expired : int64) = Option.value memory.expired_at ~default:now in
    Int64.( > ) expired now
  (* function read_is_memoryout end *)
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t ref [@@deriving sexp, compare, equal]

type typ = Setting.typ

type memory = int64 (* in GigaBytes *)

let memory_curr : ?typ:typ -> unit -> memory =
  (fun ?(typ = Both) () -> Int64.( / ) (Setting.cur_max_memory typ) Setting.gB)
(* function memory_curr end *)

let string_of_curr_memory : ?typ:typ -> unit -> string =
  fun ?(typ = Both) () ->
  ((Setting.cur_max_memory typ |> Float.of_int64)
   /. (Setting.gB |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6
  )
  ^ " GB"
(* function string_of_curr_memory end *)

let create : ?budget:int -> ?typ:typ -> unit -> t =
  fun ?(budget = -1) ?(typ = Both) () ->
  Stdlib.ref (Setting.create ~budget ~typ ())
(* function create end *)

let read_used_memory : t -> memory =
  (fun memory -> Int64.( / ) (Setting.read_memory_used !memory) Setting.gB)
(* function read_used_memory end *)

let read_remaining_memory : t -> memory =
  (fun memory -> Int64.( / ) (Setting.read_memory_remain !memory) Setting.gB)
(* function read_remaining_memory end *)

let is_memoryout : t -> bool = (fun memory -> Setting.read_is_memoryout !memory)
(* function is_memoryout end *)

let string_of_used_memory : t -> string =
  fun memory ->
  ((Setting.read_memory_used !memory |> Float.of_int64)
   /. (Setting.gB |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6
  )
  ^ " GB"
(* function string_of_used_memory end *)

let string_of_remaining_memory : t -> string =
  fun memory ->
  ((Setting.read_memory_remain !memory |> Float.of_int64)
   /. (Setting.gB |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6
  )
  ^ " GB"
(* function string_of_remaining_memory end *)
