open Core

let main ~numbers =
  let numbers_l = String.split numbers ~on:',' in
  let numbers_i = List.map numbers_l ~f:Int.of_string in
  let sum_of_squares = List.fold numbers_i ~init:0 ~f:(fun acc n -> acc + n * n) in
  printf "%i" sum_of_squares

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"cook eggs"
    [%map_open
      let numbers = flag "numbers" (required string) ~doc:"S list of numbers separated by commas"
      in
      fun () ->
        main ~numbers
    ]
  |> Command.run
