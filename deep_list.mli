module Constructors : sig
  type z = private Z

  type 'a s = private S of 'a

  type _ nat =
    | Zero : unit -> z nat
    | Succ : 'n nat -> 'n s nat
end

open Constructors

type ('a, _) t =
  | Element : 'a -> ('a, z) t
  | List : ('a, 'n) t Infinite_list.t -> ('a, 'n s) t

val depth : (_, 'n) t -> 'n nat

val return : 'n nat -> 'a -> ('a, 'n) t

val element : ('a, z) t -> 'a

val list : ('a, 'n s) t -> ('a, 'n) t Infinite_list.t

val map : ('a, 'n) t -> f:('a -> 'b) -> ('b, 'n) t

val map2 : ('a, 'n) t -> ('b, 'n) t -> f:('a -> 'b -> 'c) -> ('c, 'n) t

val lift : ('a, 'n) t -> f:('a -> 'b Infinite_list.t) -> ('b, 'n s) t

val flatten : ('a, 'n s) t -> f:('a Infinite_list.t -> 'b) -> ('b, 'n) t
