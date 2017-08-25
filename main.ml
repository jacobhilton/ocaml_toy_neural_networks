open Core

let main ~numbers:_ =
  let module M = Matrix.Numeric(Float) in
  let _magic =
    [ [17.; 24.; 1.; 8.; 15.]
    ; [23.; 5.; 7.; 14.; 16.]
    ; [4.; 6.; 13.; 20.; 22.]
    ; [10.; 12.; 19.; 21.; 3.]
    ; [11.; 18.; 25.; 2.; 9.]
    ]
    |> List.map ~f:(Infinite_list.of_list ~default:0.)
    |> Infinite_list.of_list ~default:(Infinite_list.constant ~default:0.)
    |> Matrix.of_infinite_matrix ~dimx:5 ~dimy:5
  in
  let _to_sexp m =
    Matrix.to_matrix m
    |> List.sexp_of_t (List.sexp_of_t Float.sexp_of_t)
  in
  let module Autodiff = Autodiff.Make(Float) in
  let _f = Autodiff.(sin ((c 1.) + x_0 + x_1)) in
  let _f = Autodiff.((exp ((c 3.) * x_0 * x_1)) + (c 4.) * (int_pow x_0 3)) in
  let f = Autodiff.(x_0 * x_1) in
  let root = Newton.find_root ~iterations:100 f in
  match Infinite_list.split_n root 2 |> fst with
  | a :: b :: [] -> printf "root: %f %f" a b
  | _ -> ()

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
