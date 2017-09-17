open Core

module Layer : sig
  type t

  val of_size : int -> t
  val to_size : t -> int
end = struct
  type t = int

  let of_size i = i
  let to_size i = i
end

module Parameter = struct
  module Node_from = struct
    type t =
      | Bias
      | Index of int
    [@@deriving sexp,compare,hash]
  end

  module Node_to = struct
    type t =
      | Index of int
    [@@deriving sexp,compare,hash]
  end

  module T = struct
    type t =
      { layer_from_index : int
      ; node_from : Node_from.t
      ; node_to : Node_to.t
      } [@@deriving sexp,compare,hash]
  end

  include T
  include Hashable.Make(T)
end

type t =
  { layers : Layer.t list
  ; parameters : Parameter.t list
  ; index_of_parameter : Parameter.t -> int option
  ; parameterized_output : float list -> Autodiff.Float.t list
  }

let create_exn ?(activation=Autodiff.Float.Univar.(sigmoid x)) connections =
  let layers =
    List.map2_exn ([] :: connections) (connections @ [[]])
      ~f:(fun connections_into connections_out_of ->
        let node_indices =
          (List.map connections_into ~f:snd) @ (List.map connections_out_of ~f:fst)
        in
        let max_node_index =
          List.fold node_indices ~init:0 ~f:(fun acc index ->
            if Int.(index < 0) then
              failwith "Neural_network.create_exn called with a negative index"
            else
              Int.max acc index)
        in
        Layer.of_size Int.(max_node_index + 1))
  in
  let parameters =
    List.mapi (List.zip_exn (List.tl_exn layers) connections)
      ~f:(fun layer_from_index (layer_to, connections_for_layer) ->
        let connections_for_layer_deduped =
          List.dedup ~compare:(fun (a1, b1) (a2, b2) ->
            if Int.equal a1 a2 then Int.compare b1 b2 else Int.compare a1 a2)
            connections_for_layer
        in
        List.init (Layer.to_size layer_to) ~f:(fun node_to_index ->
          let bias_parameter =
            { Parameter.layer_from_index
            ; node_from = Bias
            ; node_to = Index node_to_index
            }
          in
          let indexed_parameters =
            List.filter_map connections_for_layer_deduped
              ~f:(fun (node_from_index, node_to_index') ->
                if Int.equal node_to_index node_to_index' then
                  Some
                    ( `Node_from_index node_from_index
                    , { Parameter.layer_from_index
                      ; node_from = Index node_from_index
                      ; node_to = Index node_to_index
                      }
                    )
                else
                  None)
          in
          `Bias bias_parameter, `Indexed indexed_parameters))
  in
  let flattened_parameters =
    List.map parameters
      ~f:(List.map ~f:(fun (`Bias p, `Indexed ps) -> p :: List.map ps ~f:snd))
    |> List.concat
    |> List.concat
  in
  let index_of_parameter =
    Memo.general ~hashable:Parameter.hashable (fun p1 ->
      List.findi flattened_parameters ~f:(fun _ p2 ->
        Int.equal (Parameter.compare p1 p2) 0)
      |> Option.map ~f:fst)
  in
  let autodiff_offset = List.length flattened_parameters in
  let parameter_autodiffs = List.init autodiff_offset ~f:Autodiff.Float.x_i in
  let layers_autodiffs =
    List.map parameters ~f:(fun layer_parameters ->
      let layer_autodiffs_without_parameters =
        List.map layer_parameters
          ~f:(fun (`Bias bias_parameter, `Indexed indexed_parameters) ->
            let autodiff_of_parameter p =
              Autodiff.Float.x_i (Option.value_exn (index_of_parameter p))
            in
            List.fold indexed_parameters
              ~init:(autodiff_of_parameter bias_parameter)
              ~f:(fun acc (`Node_from_index node_from_index, indexed_parameter) ->
                Autodiff.Float.(
                  acc + (
                    autodiff_of_parameter indexed_parameter *
                    x_i Int.(autodiff_offset + node_from_index))))
            |> Autodiff.Float.compose_univar activation)
      in
      parameter_autodiffs @ layer_autodiffs_without_parameters)
  in
  let parameterized_output input =
    let input_layer_size = Layer.to_size (List.hd_exn layers) in
    if Int.equal (List.length input) input_layer_size then
      let input_autodiffs = List.map input ~f:Autodiff.Float.c in
      let output_autodiffs =
        Autodiff.Float.compose_list''
          (List.rev ((parameter_autodiffs @ input_autodiffs) :: layers_autodiffs))
      in
      List.split_n output_autodiffs autodiff_offset |> snd
    else
      failwithf "Neural network input has length %i, expected length %i"
        (List.length input) input_layer_size ()
  in
  { layers
  ; parameters = flattened_parameters
  ; index_of_parameter
  ; parameterized_output
  }

let create_full_exn ?activation = function
  | [] -> failwith "Neural_network.create_full_exn called without any layers"
  | input_layer_size :: layer_sizes_after_input ->
    let layer_sizes_from_and_to =
      match List.rev layer_sizes_after_input with
      | [] -> []
      | output_layer_size :: hidden_layer_sizes_backwards ->
        let hidden_layer_sizes = List.rev hidden_layer_sizes_backwards in
        List.zip_exn (input_layer_size :: hidden_layer_sizes)
          (hidden_layer_sizes @ [output_layer_size])
    in
    let connections =
      List.map layer_sizes_from_and_to ~f:(fun (layer_size_from, layer_size_to) ->
        List.init layer_size_from ~f:(fun node_from_index ->
          List.init layer_size_to ~f:(fun node_to_index ->
            (node_from_index, node_to_index)))
        |> List.concat)
    in
    create_exn ?activation connections

let train_parameters
    ?(cost_of_output_and_answer =
      Autodiff.Float.(zero - x_1 * log x_0 - (one - x_1) * log (one - x_0)))
    ?(regularization=1.) ?init_epsilon ?robust ?step_size ?iterations t ~inputs_and_answers =
  let cost_of_errors =
    List.map inputs_and_answers ~f:(fun (input, answer) ->
      List.map2_exn (t.parameterized_output input) answer ~f:(fun output_i answer_i ->
        Autodiff.Float.compose' cost_of_output_and_answer
          [output_i; Autodiff.Float.c answer_i]))
    |> List.concat
    |> List.fold ~init:Autodiff.Float.zero ~f:Autodiff.Float.(+)
  in
  let cost_of_parameters =
    List.mapi t.parameters ~f:(fun i parameter ->
      match parameter.Parameter.node_from with
      | Bias -> Autodiff.Float.zero
      | Index _ -> Autodiff.Float.(int_pow (x_i i) 2))
    |> List.fold ~init:Autodiff.Float.zero ~f:Autodiff.Float.(+)
  in
  let cost =
    Autodiff.Float.(
      scale
        (cost_of_errors + (scale cost_of_parameters (regularization /. 2.)))
        (1. /. (Float.of_int (List.length inputs_and_answers))))
  in
  let dim = List.length t.parameters in
  let init =
    Option.map init_epsilon ~f:(fun epsilon ->
      List.init dim ~f:(fun _ -> Random.float (2. *. epsilon) -. epsilon)
      |> Infinite_list.of_list ~default:0.)
  in
  Newton.find_stationary ?robust ?step_size ?iterations ?init ~dim cost

let output t ~trained_parameters =
  Staged.stage (fun input ->
    t.parameterized_output input
    |> List.map ~f:(fun output -> Autodiff.Float.eval' output trained_parameters))
