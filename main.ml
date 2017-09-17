open Core

module Boolean_function = struct
  type t =
    | Var of int
    | True
    | False
    | Not of t
    | Equal of t * t
    | And of t * t
    | Or of t * t
  [@@deriving sexp]

  let arg = Command.Arg_type.create (fun s -> t_of_sexp (Sexp.of_string s))

  let rec max_index = function
    | Var i -> if Int.(i < 0) then failwith "The supplied boolean function has a negative index" else i
    | True -> -1
    | False -> -1
    | Not s -> max_index s
    | Equal (s1, s2) | And (s1, s2) | Or (s1, s2) -> Int.max (max_index s1) (max_index s2)

  let rec eval t l =
    match t with
    | Var i ->
      begin
        match List.nth l i with
        | None -> failwith "Boolean_function.eval called with a variable index that is out of bounds"
        | Some b -> b
      end
    | True -> true
    | False -> false
    | Not s -> not (eval s l)
    | Equal (s1, s2) -> Bool.equal (eval s1 l) (eval s2 l)
    | And (s1, s2) -> (eval s1 l) && (eval s2 l)
    | Or (s1, s2) -> (eval s1 l) || (eval s2 l)
end

let main ~boolean_function ~hidden_layers ~regularization ~init_epsilon ~method_ ~iterations =
  let boolean_function_arity = Int.(Boolean_function.max_index boolean_function + 1) in
  let inputs =
    List.init (Int.pow 2 boolean_function_arity) ~f:(fun input_number ->
      List.init boolean_function_arity ~f:(fun input_number_bit ->
        Int.((input_number / (pow 2 input_number_bit)) % 2))
      |> List.rev)
  in
  let inputs_and_answers =
    List.map inputs ~f:(fun input ->
      ( List.map input ~f:Float.of_int
      , Boolean_function.eval boolean_function (List.map input ~f:(Int.equal 1))
        |> Bool.to_int
        |> Float.of_int
        |> List.return
      ))
  in
  printf "Data:\n";
  let string_of_float_list l = [%sexp_of:float list] l |> Sexp.to_string in
  List.iter inputs_and_answers ~f:(fun (input, answer) ->
    printf "Input: %s. Answer: %s.\n" (string_of_float_list input) (string_of_float_list answer));
  let hidden_layer_sizes = List.init hidden_layers ~f:(fun _ -> boolean_function_arity) in
  let layer_sizes = [boolean_function_arity] @ hidden_layer_sizes @ [1] in
  printf "Creating a neural network with these numbers of nodes in its layers: %s.\n"
    ([%sexp_of:int list] layer_sizes |> Sexp.to_string);
  let network = Neural_network.create_full_exn layer_sizes in
  printf "Training neural network on the dataset...\n";
  Random.self_init ();
  match
    Neural_network.train_parameters ~regularization ~init_epsilon ?method_ ~iterations
      network ~inputs_and_answers
  with
  | _, Newton.Status.Failed -> failwith "Minimization method failed to converge"
  | trained_parameters, _ ->
    printf "Trained parameter values:\n";
    List.iter (List.zip_exn network.Neural_network.parameters trained_parameters)
      ~f:(fun (parameter, value) ->
        printf "Parameter: %s. Value: %f.\n"
          (Neural_network.Parameter.sexp_of_t parameter |> Sexp.to_string) value);
    let trained_network =
      Neural_network.output ~trained_parameters network
      |> Staged.unstage
    in
    printf "Testing the trained network on the training set:\n";
    let test_results =
      List.map inputs_and_answers ~f:(fun (input, answer) ->
        let output = trained_network input in
        printf "Input: %s. Answer: %s. Output: %s.\n" (string_of_float_list input)
          (string_of_float_list answer) (string_of_float_list output);
        if Float.equal (List.hd_exn output) 0.5 then 0. else begin
          if
            Bool.equal Float.(List.hd_exn answer > 0.5) Float.(List.hd_exn output > 0.5)
          then 1. else 0.
        end)
      |> List.fold ~init:0. ~f:(+.)
      |> (fun sum -> sum /. (Float.of_int (List.length inputs_and_answers)))
    in
    printf "Success rate: %s.\n" (Percent.of_mult test_results |> Percent.to_string);
    ()

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"Create, train and test a logistic neural network to evaluate on a boolean function.\n\
              The boolean function should be specified as a sexp, e.g. '(And (Or (Var 0) (Var 1)) (Var 2))'"
    [%map_open
      let boolean_function =
        anon ("boolean_function" %: Boolean_function.arg)
      and hidden_layers =
        flag "hidden-layers" (optional_with_default 0 int)
          ~doc:"N number of hidden layers in the neural network\n\
               default: 0"
      and regularization =
        flag "regularization" (optional_with_default 0.01 float)
          ~doc:"f regularization parameter\n\
               default: 0.01"
      and init_epsilon =
        flag "init-epsilon" (optional_with_default 1. float)
          ~doc:"f initialize parameters uniformly at random in the range [-f, f)\n\
                default: 1"
      and method_ =
        flag "method" (optional Neural_network.Method.arg)
          ~doc:"SEXP minimization method, either 'Newton' or 'Gradient_descent_with_step_size f'\n\
                default: Newton for networks with no hidden layers, Gradent_descent_with_step_size 0.5 for networks with at least one hidden layer"
      and iterations =
        flag "iterations" (optional_with_default 100 int)
          ~doc:"N maximum number of iterations for the minimization method\n\
               default: 100"
      in
      fun () ->
        main ~boolean_function ~hidden_layers ~regularization ~init_epsilon ~method_ ~iterations
    ]
  |> Command.run
