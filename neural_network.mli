module Layer : sig
  type t

  val of_size : int -> t
  val to_size : t -> int
end

module Parameter : sig
  module Node_from : sig
    type t =
      | Bias
      | Index of int
    [@@deriving sexp,compare,hash]
  end

  module Node_to : sig
    type t =
      | Index of int
    [@@deriving sexp,compare,hash]
  end

  type t =
    { layer_from_index : int
    ; node_from : Node_from.t
    ; node_to : Node_to.t
    } [@@deriving sexp,compare,hash]

  include Core.Hashable.S with type t := t
end

type t =
  { layers : Layer.t list
  ; parameters : Parameter.t list
  ; index_of_parameter : Parameter.t -> int option
  ; output : Autodiff.Float.t list -> Autodiff.Float.t list
  }

val create_exn
  :  ?activation:Autodiff.Float.Univar.t
  -> (int * int) list list
  -> t

val create_full_exn
  :  ?activation:Autodiff.Float.Univar.t
  -> int list
  -> t
