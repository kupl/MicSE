type 'a t = 'a list

exception Empty

exception Unsufficient_length

let empty = []

let push = List.cons

let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

let drop = function [] -> raise Empty | _ :: tl -> tl

let peek = function [] -> raise Empty | hd :: _ -> hd

let swap = function
  | hd_1 :: hd_2 :: tl -> hd_2 :: hd_1 :: tl
  | _ :: _ -> raise Unsufficient_length
  | [] -> raise Empty

let dig s n =
  if Z.(n = ~$0) then s
  else if Z.(n = ~$1) then swap s
  else
    let rec aux (l_h, l_t) n =
      match l_t with
      | h :: t when Z.(n = ~$0) -> ((h :: l_h) @ t, [])
      | h :: t -> aux (l_h @ [ h ], t) Z.(n - one)
      | [] -> ([], [])
    in
    fst (aux ([], s) n)

let dug s n =
  if Z.(n = ~$0) then s
  else if Z.(n = ~$1) then swap s
  else
    match s with
    | [] -> s
    | top :: t ->
        let rec aux (l_h, l_t) n' =
          match l_t with
          | [] -> (top :: l_t, [])
          | _ when Z.(n' = ~$0) -> (l_h @ (top :: l_t), [])
          | h :: t -> aux (l_h @ [ h ], t) Z.(n' - ~$1)
        in
        fst (aux ([], t) n)

let map = List.map

let map2 = List.map2

let find = List.find
