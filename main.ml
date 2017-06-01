open Core

let main ~numbers:_ =
  let module Autodiff = Autodiff.Make(Float) in
  let myfun = Autodiff.((int_pow (sin x) 2 * cos x + c 3. * cos x) ** (c 4. * cos x + sin x)) in
  let y = 12. in
  printf "f(%f) = %f. f''(%f) = %s" y Autodiff.(eval myfun y) y (Float.to_string_round_trippable Autodiff.(eval (d (d myfun)) y));
  let aha = Autodiff.(d (d (relu x))) in
  Autodiff.(printf "%f %f %f %f %f %f %s" (eval (step x) 0.) (eval (step x) (-0.5)) (eval (step x) 0.5) (eval aha 0.) (eval aha 1.) (eval aha (-1.)) (Float.to_string_round_trippable (eval (step x) (-0.5))))

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"cook eggs"
    [%map_open
      let numbers = flag "numbers" (optional string) ~doc:"S list of numbers separated by commas"
      in
      fun () ->
        main ~numbers
    ]
  |> Command.run
