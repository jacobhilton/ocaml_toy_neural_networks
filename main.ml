open Core

let main ~ignored:_ =
  let g = Autodiff.Float.(compose' (x_0 / x_1) [log x_0 * x_1; x_0 + x_1]) in
  let dgs = Infinite_list.map (Autodiff.Float.grad g) ~f:(fun dg -> Autodiff.Float.(eval' dg [2.; 3.])) in
  printf "compose_check %f %f\n" (Infinite_list.nth_exn dgs 0) (Infinite_list.nth_exn dgs 1);
  let f = Autodiff.Float.(zero - (int_pow (x_0 - (c 2.)) 2 + int_pow (x_1 - (c 3.)) 2)) in
  let a = Newton.find_stationary ~dim:2 ~iterations:100 f in
  printf "%s %f %f\n" (Newton.Status.to_string (snd a)) (List.nth_exn (fst a) 0) (List.nth_exn (fst a) 1);
  let nn = Neural_network.create_full_exn [2; 3; 1] in
  let layers = List.map nn.Neural_network.layers ~f:Neural_network.Layer.to_size in
  printf "%s\n" "layers";
  List.iter layers ~f:(fun layer -> printf "%i, " layer);
  let out = nn.Neural_network.parameterized_output [3.; 4.] in
  printf "%s\n" (Sexp.to_string_hum (List.sexp_of_t Neural_network.Parameter.sexp_of_t nn.Neural_network.parameters));
  let res = Autodiff.Float.eval' (List.hd_exn out) [0.32;0.21;-0.25;0.1;0.02;0.22;-0.18;0.23;-0.3;-0.5;-0.1;0.2;-0.18] in
  let sigmoid = Autodiff.Float.Univar.(eval (sigmoid x)) in
  let res2 = sigmoid (-.0.5 -. 0.1 *. sigmoid (0.32+.0.21*.3.-.0.25*.4.) +.0.2 *. sigmoid (0.1+.0.02*.3.+.0.22*.4.) -. 0.18 *. sigmoid (-.0.18+.0.23*.3.-.0.3*.4.)) in
  printf "nn %f %f\n" res res2;
  let res3 =
    Autodiff.Float.(compose_list'' [[sin (x_i 0) + cos (x_i 1)];[x_i 0 * x_i 1; int_pow (x_i 1) 2];[c 3.; exp x_0]])
    |> List.hd_exn
    |> Autodiff.Float.grad
    |> (fun l -> Infinite_list.nth_exn l 0)
    |> (fun t -> Autodiff.Float.eval' t [1.2])
  in
  printf "compose_list %f\n" res3;
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
