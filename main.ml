open Core

let main ~numbers:_ =
  let module Autodiff = Autodiff.Make(Float) in
  let f = Autodiff.((int_pow (sin x_0) 2 + cos x_1 * exp x_2)) in
  let d = Autodiff.grad f in
  let d0 = Infinite_list.nth_exn d 0 in
  let d1 = Infinite_list.nth_exn d 1 in
  let d2 = Infinite_list.nth_exn d 2 in
  let x = [12.; 11.; 10.] in
  Autodiff.(printf "%f %f %f\n" (eval' d0 x) (eval' d1 x) (eval' d2 x));
  Float.(printf "%f %f %f" (sin 24.) (-. (sin 11.) *. (exp 10.)) ((cos 11.) *. (exp 10.)));
  let module M = Matrix.Make(Float) in
  let magic =
    [ [17.; 24.; 1.; 8.; 15.]
    ; [23.; 5.; 7.; 14.; 16.]
    ; [4.; 6.; 13.; 20.; 22.]
    ; [10.; 12.; 19.; 21.; 3.]
    ; [11.; 18.; 25.; 2.; 9.]
    ]
    |> List.map ~f:(Infinite_list.of_list ~default:0.)
    |> Infinite_list.of_list ~default:(Infinite_list.constant ~default:0.)
    |> M.of_infinite_matrix ~dimx:5 ~dimy:5
  in
  let to_sexp m =
    M.to_matrix m
    |> List.sexp_of_t (List.sexp_of_t Float.sexp_of_t)
  in
  let _lu_test =
    match M.plu magic with
    | None -> false
    | Some (p, l, u) ->
      let m = M.Exn.(p * l * u) in
      Sexp.equal (to_sexp magic) (to_sexp m)
  in
  ()

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"demo"
    [%map_open
      let numbers = flag "numbers" (optional string) ~doc:"S list of numbers separated by commas"
      in
      fun () ->
        main ~numbers
    ]
  |> Command.run
