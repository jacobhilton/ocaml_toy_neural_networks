type 'a t

val of_list : 'a list -> default:'a -> 'a t

val constant : default:'a -> 'a t

val e_i : int -> zero:'a -> one:'a -> 'a t

val nth_exn : 'a t -> int -> 'a

val split_n : 'a t -> int -> 'a list * 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val zip : 'a t -> 'b t -> ('a * 'b) t

val fold
  :  'a t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> f_default:('accum -> 'a -> 'accum)
  -> 'accum

val transpose : 'a t t -> 'a t t
