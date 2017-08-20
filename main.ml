open Core

let main ~numbers:_ =
  let module Autodiff = Autodiff.Make(Float) in
  let f = Autodiff.((int_pow (sin x_0) 2 + cos x_1 * exp x_2)) in
  let d = Autodiff.d f in
  let d0 = Infinite_list.nth_exn d 0 in
  let d1 = Infinite_list.nth_exn d 1 in
  let d2 = Infinite_list.nth_exn d 2 in
  let x = [12.; 11.; 10.] in
  Autodiff.(printf "%f %f %f\n" (eval' d0 x) (eval' d1 x) (eval' d2 x));
  Float.(printf "%f %f %f" (sin 24.) (-. (sin 11.) *. (exp 10.)) ((cos 11.) *. (exp 10.)));
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
