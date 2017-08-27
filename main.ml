open Core

let main ~numbers:_ =
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
  let _f = Autodiff.Float.(int_pow x_0 2 + int_pow x_1 2 + (x_0 - (c 2.)) * (x_1 - (c 5.))) in
  let f = Autodiff.Float.(zero - (int_pow (x_0 - (c 2.)) 2 + int_pow (x_1 - (c 3.)) 2)) in
  let a = Newton.find_stationary ~dim:2 ~iterations:100 f in
  printf "%s %f %f" (Newton.Status.to_string (snd a)) (List.nth_exn (fst a) 0) (List.nth_exn (fst a) 1);
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
