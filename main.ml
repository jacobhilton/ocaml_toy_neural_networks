open Core
open Async

let () =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"cook eggs"
    [%map_open
      let num_eggs =
        flag "num-eggs" (required int) ~doc:"COUNT cook this many eggs"
      in
      fun () ->
        (* TODO: implement egg-cooking in ocaml *)
        failwith "no eggs today"
    ]
  |> Command.run

(* Trivial change *)
