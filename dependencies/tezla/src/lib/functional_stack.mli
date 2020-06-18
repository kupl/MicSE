type 'a t = 'a list

val empty : 'a t

val push : 'a -> 'a t -> 'a t

val pop : 'a t -> 'a * 'a t

val drop : 'a t -> 'a t

val peek : 'a t -> 'a

val swap : 'a t -> 'a t

val dig : 'a t -> Z.t -> 'a t

val dug : 'a t -> Z.t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val find : ('a -> bool) -> 'a t -> 'a
