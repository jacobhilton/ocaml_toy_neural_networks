module Constructors = struct
  type z = private Z

  type 'n s = private S of 'n

  type _ nat =
    | Zero : unit -> z nat
    | Succ : 'n nat -> 'n s nat
end

open Constructors

type ('a, _) t =
  | Element : 'a -> ('a, z) t
  | List : ('a, 'n) t Infinite_list.t -> ('a, 'n s) t

let rec depth : type el . (_, el) t -> el nat = function
  | Element _ -> Zero ()
  | List l -> Succ (depth (Infinite_list.default l))

let rec return : type el . el nat -> 'a -> ('a, el) t =
  fun depth default ->
    match depth with
    | Zero () -> Element default
    | Succ n -> List (Infinite_list.constant ~default:(return n default))

let element (Element x) = x

let list (List l) = l

let rec map : type el . ('a, el) t -> f:('a -> 'b) -> ('b, el) t =
  fun t ~f ->
    match t with
    | Element e -> Element (f e)
    | List l -> List (Infinite_list.map l ~f:(map ~f))

let rec map2 : type el . ('a, el) t -> ('b, el) t -> f:('a -> 'b -> 'c) -> ('c, el) t =
  fun t1 t2 ~f ->
    match t1, t2 with
    | Element e1, Element e2 -> Element (f e1 e2)
    | List l1, List l2 -> List (Infinite_list.map2 l1 l2 ~f:(map2 ~f))

let rec lift : type el . ('a, el) t -> f:('a -> 'b Infinite_list.t) -> ('b, el s) t =
  fun t ~f ->
    match t with
    | Element e -> List (Infinite_list.map (f e) ~f:(fun y -> Element y))
    | List l -> List (Infinite_list.map l ~f:(lift ~f))

let rec flatten : type el . ('a, el s) t -> f:('a Infinite_list.t -> 'b) -> ('b, el) t =
  fun t ~f ->
    match t with
    | List l ->
      match Infinite_list.default l with
      | Element _ -> Element (f (Infinite_list.map l ~f:(fun (Element x) -> x)))
      | List _ -> List (Infinite_list.map l ~f:(flatten ~f))
