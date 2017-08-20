type 'a t

val of_list : 'a list -> default:'a -> 'a t

val constant : default:'a -> 'a t

val e_i : int -> zero:'a -> one:'a -> 'a t

val nth_exn : 'a t -> int -> 'a

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
