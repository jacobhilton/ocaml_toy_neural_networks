open Core

module Autodiff = Autodiff.Make(Float)

module Matrix_numeric = Matrix.Numeric(Float)

let equal ~robust x1 x2 =
  if robust then
    Int.equal (Float.robustly_compare x1 x2) 0
  else
    Float.equal x1 x2

let find_root ?(robust=false) ?(step_size=1.) ?iterations ?(init=0.) f =
  let d_f = Autodiff.OneD.d f in
  let x = ref init in
  let f_of_x = ref (Autodiff.OneD.eval f !x) in
  let k = ref 0 in
  let converged = ref false in
  while
    (not !converged) &&
    match iterations with
    | Some n -> Int.(!k < n)
    | None -> true
  do
    let new_x = !x -. step_size *. !f_of_x /. (Autodiff.OneD.eval d_f !x) in
    let new_f_of_x = Autodiff.OneD.eval f new_x in
    if Float.is_nan new_f_of_x || Float.is_inf new_f_of_x then
      converged := true
    else
      begin
        if equal ~robust !f_of_x new_f_of_x then converged := true;
        x := new_x;
        f_of_x := new_f_of_x;
        k := Int.(!k + 1)
      end
  done;
  !x

let _find_root_rubbish ?(robust=false) ?(step_size=1.) ?iterations
    ?(init=(Infinite_list.constant ~default:0.)) f =
  let grad_f = Autodiff.grad f in
  let x = ref init in
  let f_of_x = ref (Autodiff.eval f !x) in
  let k = ref 0 in
  let converged = ref false in
  while
    (not !converged) &&
    match iterations with
    | Some n -> Int.(!k < n)
    | None -> true
  do
    let new_x =
      Infinite_list.map2 !x grad_f ~f:(fun x_i grad_f_i ->
        x_i -. step_size *. !f_of_x /. (Autodiff.eval grad_f_i !x)
      )
    in
    let new_f_of_x = Autodiff.eval f new_x in
    if Float.is_nan new_f_of_x || Float.is_inf new_f_of_x then
      converged := true
    else
      begin
        if equal ~robust !f_of_x new_f_of_x then converged := true;
        x := new_x;
        f_of_x := new_f_of_x;
        k := Int.(!k + 1)
      end
  done;
  !x
