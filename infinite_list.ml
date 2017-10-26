open Core

type 'a t =
  { list : 'a list
  ; default : 'a
  }

let of_list list ~default =
  { list
  ; default
  }

let constant ~default =
  of_list [] ~default

let e_i i ~zero ~one =
  { list = List.init (i + 1) ~f:(fun j -> if Int.equal i j then one else zero)
  ; default = zero
  }

let nth_exn { list; default } n =
  match Int.sign n with
  | Neg -> failwithf "Infinite_list.nth_exn %i called" n ()
  | Zero | Pos -> Option.value (List.nth list n) ~default

let default t = t.default

let split_n { list; default } n =
  let diff = Int.(-) (List.length list) n in
  match Int.sign diff with
  | Zero -> list, constant ~default
  | Pos ->
    let list1, list2 = List.split_n list n in
    list1, { list = list2; default }
  | Neg -> list @ (List.init (Int.abs diff) ~f:(Fn.const default)), constant ~default

let map { list; default } ~f =
  { list = List.map list ~f
  ; default = f default
  }

let map2 { list = list1; default = default1 } { list = list2; default = default2 } ~f =
  let list1, list2 =
    let diff = Int.(-) (List.length list1) (List.length list2) in
    match Int.sign diff with
    | Zero -> list1, list2
    | Pos -> list1, list2 @ (List.init diff ~f:(Fn.const default2))
    | Neg -> list1 @ (List.init (Int.abs diff) ~f:(Fn.const default1)), list2
  in
  { list = List.map2_exn list1 list2 ~f
  ; default = f default1 default2
  }

let zip = map2 ~f:(fun x1 x2 -> (x1, x2))

let fold { list; default } ~init ~f ~f_default =
  f_default (List.fold list ~init ~f) default

let transpose t =
  let f acc s = Int.max acc (List.length s.list) in
  let max_length = fold t ~init:0 ~f ~f_default:f in
  { list = List.init max_length ~f:(fun n -> map t ~f:(fun s -> nth_exn s n))
  ; default = map t ~f:(fun s -> nth_exn s max_length)
  }
