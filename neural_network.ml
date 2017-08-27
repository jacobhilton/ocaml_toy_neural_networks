open Core

module Layer = struct
  type t = Nodes of int

  let of_int i = Nodes i
  let to_int (Nodes i) = i
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
  ; output : float list -> Autodiff.Float.t
  }
        
let create_exn ?(activation=Autodiff.Float.Univar.(sigmoid x)) layers_int =
  match List.map layers_int ~f:Layer.of_int with
  | [] | _ :: [] -> failwith "Neural_network.create_exn called with too few layers"
  | input_layer :: layers_after_input as layers ->
    begin
      match List.rev layers_after_input with
      | Layer.Nodes 1 :: hidden_layers_backwards ->
        let layers_from_and_to_backwards =
          List.zip_exn (hidden_layers_backwards @ [input_layer])
            (Layer.Nodes 1 :: hidden_layers_backwards)
        in
        let parameters_backwards =
          List.mapi layers_from_and_to_backwards
            ~f:(fun layer_from_index_backwards (layer_from, layer_to) ->
              let layer_from_index =
                Int.(List.length layers_from_and_to_backwards - 1 - layer_from_index_backwards)
              in
              List.init (Layer.to_int layer_to) ~f:(fun node_to_index ->
                let bias_parameter =
                  { Parameter.layer_from_index
                  ; node_from = Bias
                  ; node_to = Index node_to_index
                  }
                in
                let indexed_parameters =
                  List.init (Layer.to_int layer_from) ~f:(fun node_from_index ->
                    { Parameter.layer_from_index
                    ; node_from = Index node_from_index
                    ; node_to = Index node_to_index
                    })
                in
                `Bias bias_parameter, `Indexed indexed_parameters))
        in
        let flattened_parameters =
          List.map parameters_backwards
            ~f:(List.map ~f:(fun (`Bias p, `Indexed ps) -> p :: ps))
          |> List.rev
          |> List.concat
          |> List.concat
        in
        let index_of_parameter =
          Memo.general ~hashable:Parameter.hashable (fun p1 ->
            List.findi flattened_parameters ~f:(fun _ p2 ->
              Int.equal (Parameter.compare p1 p2) 0)
            |> Option.map ~f:fst)
        in
        let output_autodiff =
          List.fold parameters_backwards ~init:Fn.id
            ~f:(fun output_of_last_layers layer_parameters ->
              fun inputs ->
                let output_of_this_layer =
                List.map layer_parameters
                  ~f:(fun (`Bias bias_parameter, `Indexed indexed_parameters) ->
                    let autodiff_of_parameter p =
                      Autodiff.Float.x_i (Option.value_exn (index_of_parameter p))
                    in
                    List.foldi indexed_parameters
                      ~init:(autodiff_of_parameter bias_parameter)
                      ~f:(fun node_from_index acc indexed_parameter ->
                        Autodiff.Float.(
                          acc + autodiff_of_parameter indexed_parameter * List.nth_exn inputs node_from_index))
                    |> Autodiff.Float.compose_univar activation)
                in
                output_of_last_layers output_of_this_layer)
        in
        let output input =
          if Int.equal (List.length input) (Layer.to_int input_layer) then
            output_autodiff (List.map input ~f:(fun x -> Autodiff.Float.c x))
            |> List.hd_exn
          else
            failwithf "Neural network input has length %i, expected length %i"
              (List.length input) (Layer.to_int input_layer) ()
        in
        { layers
        ; parameters = flattened_parameters
        ; index_of_parameter
        ; output
        }
      | _ :: _ | [] ->
        failwith "Neural_network.create_exn called without a final layer with exactly one node"
    end
