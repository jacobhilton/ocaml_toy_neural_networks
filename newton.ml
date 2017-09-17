open Core

module Status = struct
  type t =
    | Converged
    | Failed
    | Iterating
  [@@deriving sexp]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

let equal ~robust x1 x2 =
  if robust then
    Int.equal (Float.robustly_compare x1 x2) 0
  else
    Float.equal x1 x2

let find_root ?(robust=false) ?(step_size=1.) ?iterations ?(init=0.) f =
  let d_f = Autodiff.Float.Univar.d f in
  let x = ref init in
  let f_of_x = ref (Autodiff.Float.Univar.eval f !x) in
  let k = ref 0 in
  let status = ref Status.Iterating in
  while
    match !status with
    | Converged | Failed -> false
    | Iterating ->
      match iterations with
      | Some n -> Int.(!k < n)
      | None -> true
  do
    let new_x = !x -. step_size *. !f_of_x /. (Autodiff.Float.Univar.eval d_f !x) in
    let new_f_of_x = Autodiff.Float.Univar.eval f new_x in
    if Float.is_nan new_f_of_x || Float.is_inf new_f_of_x then
      status := Failed
    else
      begin
        if equal ~robust !f_of_x new_f_of_x then status := Converged;
        x := new_x;
        f_of_x := new_f_of_x;
        k := Int.(!k + 1)
      end
  done;
  !x, !status

let find_stationary ?(robust=false) ?(step_size=1.) ?iterations
    ?(init=(Infinite_list.constant ~default:0.)) ~dim f =
  let grad_f_inf = Autodiff.Float.grad f in
  let hessian_f =
    Infinite_list.map grad_f_inf ~f:Autodiff.Float.grad
    |> Matrix.of_infinite_matrix ~dimx:dim ~dimy:dim
  in
  let grad_f =
    Infinite_list.split_n grad_f_inf dim |> fst
    |> Matrix.column_vector_of_list
  in
  let x = ref (Infinite_list.split_n init dim |> fst) in
  let f_of_x = ref (Autodiff.Float.eval' f !x) in
  let k = ref 0 in
  let status = ref Status.Iterating in
  while
    match !status with
    | Converged | Failed -> false
    | Iterating ->
      match iterations with
      | Some n -> Int.(!k < n)
      | None -> true
  do
    let eval_at_x g = Autodiff.Float.eval' g !x in
    let grad_f_of_x = Matrix.map grad_f ~f:eval_at_x in
    let hessian_f_of_x = Matrix.map hessian_f ~f:eval_at_x in
    match Matrix.Float.solve' ~robust hessian_f_of_x ~vector:grad_f_of_x with
    | None -> status := Failed
    | Some solution ->
      let minus_delta_x = Matrix.to_matrix solution |> List.concat in
      let new_x =
        List.map2_exn !x minus_delta_x ~f:(fun x_i minus_delta_x_i ->
          x_i -. step_size *. minus_delta_x_i
        )
      in
      let new_f_of_x = Autodiff.Float.eval' f new_x in
      if Float.is_nan new_f_of_x || Float.is_inf new_f_of_x then
        status := Failed
      else
        begin
          if equal ~robust !f_of_x new_f_of_x then status := Converged;
          x := new_x;
          f_of_x := new_f_of_x;
          k := Int.(!k + 1)
        end
  done;
  !x, !status

let find_minimum ?(robust=false) ~step_size ?iterations
    ?(init=(Infinite_list.constant ~default:0.)) ~dim f =
  let grad_f_inf = Autodiff.Float.grad f in
  let grad_f =
    Infinite_list.split_n grad_f_inf dim |> fst
  in
  let x = ref (Infinite_list.split_n init dim |> fst) in
  let f_of_x = ref (Autodiff.Float.eval' f !x) in
  let k = ref 0 in
  let status = ref Status.Iterating in
  while
    match !status with
    | Converged | Failed -> false
    | Iterating ->
      match iterations with
      | Some n -> Int.(!k < n)
      | None -> true
  do
    let eval_at_x g = Autodiff.Float.eval' g !x in
    let grad_f_of_x = List.map grad_f ~f:eval_at_x in
    let new_x =
      List.map2_exn !x grad_f_of_x ~f:(fun x_i grad_f_of_x_i ->
        x_i -. step_size *. grad_f_of_x_i
      )
    in
    let new_f_of_x = Autodiff.Float.eval' f new_x in
    if Float.is_nan new_f_of_x || Float.is_inf new_f_of_x then
      status := Failed
    else
      begin
        if equal ~robust !f_of_x new_f_of_x then status := Converged;
        x := new_x;
        f_of_x := new_f_of_x;
        k := Int.(!k + 1)
      end
  done;
  !x, !status
