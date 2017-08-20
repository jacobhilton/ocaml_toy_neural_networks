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
