open Core

let main ~numbers:_ =
  let module Autodiff = Autodiff.Make(Float) in
  let myfun = Autodiff.(sin id * sin id * cos id + const 3. * cos id) in
  let x = 12. in
  printf "f(%f) = %f. f''(%f) = %f" x Autodiff.(eval myfun x) x Autodiff.(eval (diff (diff myfun)) x)

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
