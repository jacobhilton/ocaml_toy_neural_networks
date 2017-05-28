open Core

let main ~numbers:_ =
  let module Autodiff = Autodiff.Make(Float) in
  let myfun = Autodiff.((int_pow (sin id) 2 * cos id + const 3. * cos id) ** (const 4. * cos id + sin id)) in
  let x = 12. in
  printf "f(%f) = %f. f''(%f) = %s" x Autodiff.(eval myfun x) x (Float.to_string_round_trippable Autodiff.(eval (diff (diff myfun)) x))

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
