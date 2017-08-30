type 'a t =
  | Element of 'a
  | List of 'a t Infinite_list.t

val return : 'a -> 'a t

val element_exn : 'a t -> 'a

val list_exn : 'a t -> 'a t Infinite_list.t

val list2_exn : 'a t -> 'a t Infinite_list.t Infinite_list.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val lift : f:('a -> 'b Infinite_list.t) -> 'a t -> 'b t

val flatten_exn : f:('a t Infinite_list.t -> 'b) -> 'a t -> 'b t
