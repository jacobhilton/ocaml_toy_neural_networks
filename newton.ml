open Core

module Autodiff = Autodiff.Float

module Matrix_numeric = Matrix.Float

let equal ~robust x1 x2 =
  if robust then
    Int.equal (Float.robustly_compare x1 x2) 0
  else
    Float.equal x1 x2

let find_root ?(robust=false) ?(step_size=1.) ?iterations ?(init=0.) f =
  let d_f = Autodiff.Univar.d f in
  let x = ref init in
  let f_of_x = ref (Autodiff.Univar.eval f !x) in
  let k = ref 0 in
  let converged = ref false in
  while
    (not !converged) &&
    match iterations with
    | Some n -> Int.(!k < n)
    | None -> true
  do
    let new_x = !x -. step_size *. !f_of_x /. (Autodiff.Univar.eval d_f !x) in
    let new_f_of_x = Autodiff.Univar.eval f new_x in
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

let find_min ?(robust=false) ?(step_size=1.) ?iterations
    ?(init=(Infinite_list.constant ~default:0.)) ~dim f =
  let grad_f_inf = Autodiff.grad f in
  let hessian_f =
    Infinite_list.map grad_f_inf ~f:Autodiff.grad
    |> Matrix.of_infinite_matrix ~dimx:dim ~dimy:dim
  in
  let grad_f =
    Infinite_list.split_n grad_f_inf dim |> fst
    |> Matrix.column_vector_of_list
  in
  let x = ref (Infinite_list.split_n init dim |> fst) in
  let f_of_x = ref (Autodiff.eval' f !x) in
  let k = ref 0 in
  let converged = ref false in
  while
    (not !converged) &&
    match iterations with
    | Some n -> Int.(!k < n)
    | None -> true
  do
    let eval_at_x g = Autodiff.eval' g !x in
    let grad_f_of_x = Matrix.map grad_f ~f:eval_at_x in
    let hessian_f_of_x = Matrix.map hessian_f ~f:eval_at_x in
    match Matrix_numeric.solve' ~robust hessian_f_of_x ~vector:grad_f_of_x with
    | None -> converged := true
    | Some solution ->
      let minus_delta_x = Matrix.to_matrix solution |> List.concat in
      let new_x =
        List.map2_exn !x minus_delta_x ~f:(fun x_i minus_delta_x_i ->
          x_i -. step_size *. minus_delta_x_i
        )
      in
      let new_f_of_x = Autodiff.eval' f new_x in
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
