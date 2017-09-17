open Core
open Async

let main ~m ~regularization ~iterations =
  Reader.load_sexp_exn "data.sexp" [%of_sexp: (float list * float list) list]
  >>= fun data ->
  Random.self_init ();
  let shuffled_data =
    List.map data ~f:(fun l -> (Random.float 1., l))
    |> List.sort ~cmp:(fun (a1, _) (a2, _) -> Float.compare a1 a2)
    |> List.map ~f:snd
  in
  let training_data, non_training_data = List.split_n shuffled_data m in
  let testing_data = List.split_n non_training_data m |> fst in
  let network = Neural_network.create_full_exn [400; 10] in
  match
    Neural_network.train_parameters ?regularization ~iterations network
      ~inputs_and_answers:training_data
  with
  | _, Newton.Status.Failed -> failwith "Newton's method failed to converge"
  | trained_parameters, _ ->
    let trained_network =
      Neural_network.output ~trained_parameters network
      |> Staged.unstage
    in
    let argmax l =
      List.foldi l ~init:(-1, None) ~f:(fun index (acc_index, acc_value_option) value ->
        match acc_value_option with
        | None -> (index, Some value)
        | Some acc_value ->
          if Float.(value > acc_value) then (index, Some value) else (acc_index, Some acc_value))
      |> fst
    in
    let test_results =
      List.map testing_data ~f:(fun (input, answer) ->
        let output = trained_network input in
        if Int.equal (argmax output) (argmax answer) then 1. else 0.)
      |> List.fold ~init:0. ~f:(+.)
      |> (fun sum -> sum /. (Float.of_int (List.length testing_data)))
    in
    printf "%f\n" test_results;
    Deferred.unit

let () =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"main"
    [%map_open
      let m = flag "m" (required int) ~doc:"N number of examples from the dataset to train and test on"
      and regularization = flag "l" (optional float) ~doc:"x regularization paramter"
      and iterations = flag "i" (required int) ~doc:"N number of iterations for Newton's method"
      in
      fun () ->
        main ~m ~regularization ~iterations
    ]
  |> Command.run
