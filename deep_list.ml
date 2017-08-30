open Core

type 'a t =
  | Element of 'a
  | List of 'a t Infinite_list.t

let return e = Element e

let element_exn = function
  | Element e -> e
  | List _ -> failwith "Deep_list.element_exn called on a List"

let list_exn = function
  | Element _ -> failwith "Deep_list.list_exn called on an Element"
  | List l -> l

let list2_exn t = Infinite_list.map (list_exn t) ~f:list_exn

let rec map ~f = function
  | Element e -> Element (f e)
  | List l -> List (Infinite_list.map l ~f:(map ~f))

let rec lift ~f = function
  | Element e -> List (Infinite_list.map (f e) ~f:(fun y -> Element y))
  | List l -> List (Infinite_list.map l ~f:(lift ~f))

let flatten_exn ~f = function
  | Element _ -> failwith "Deep_list.flatten_exn called on an Element"
  | List l -> Element (f l)
