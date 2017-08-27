open Core

(* Todo: make a more general version of [Neural_network.create_exn], and incoporate [Autodiff.Float.compose] somehow. *)

let main ~ignored:_ =
  let g = Autodiff.Float.(compose' (x_0 / x_1) [log x_0 * x_1; x_0 + x_1]) in
  let dgs = Infinite_list.map (Autodiff.Float.grad g) ~f:(fun dg -> Autodiff.Float.(eval' dg [2.; 3.])) in
  printf "compose_check %f %f\n" (Infinite_list.nth_exn dgs 0) (Infinite_list.nth_exn dgs 1);
  let f = Autodiff.Float.(zero - (int_pow (x_0 - (c 2.)) 2 + int_pow (x_1 - (c 3.)) 2)) in
  let a = Newton.find_stationary ~dim:2 ~iterations:100 f in
  printf "%s %f %f\n" (Newton.Status.to_string (snd a)) (List.nth_exn (fst a) 0) (List.nth_exn (fst a) 1);
  let nn = Neural_network.create_exn [2; 3; 1] in
  let out = nn.Neural_network.output [3.; 4.] in
  printf "%s\n" (Sexp.to_string_hum (List.sexp_of_t Neural_network.Parameter.sexp_of_t nn.Neural_network.parameters));
  let res = Autodiff.Float.eval' out [-0.5;-0.1;0.2;-0.18;0.32;0.21;-0.25;0.1;0.02;0.22;-0.18;0.23;-0.3] in
  let sigmoid = Autodiff.Float.Univar.(eval (sigmoid x)) in
  let res2 = sigmoid (-.0.5 -. 0.1 *. sigmoid (0.32+.0.21*.3.-.0.25*.4.) +.0.2 *. sigmoid (0.1+.0.02*.3.+.0.22*.4.) -. 0.18 *. sigmoid (-.0.18+.0.23*.3.-.0.3*.4.)) in
  printf "nn %f %f\n" res res2;
  ()

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"main"
    [%map_open
      let ignored = flag "ignored" no_arg ~doc:" ignored"
      in
      fun () ->
        main ~ignored
    ]
  |> Command.run
