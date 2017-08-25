open Core

module type S = sig
  type t

  val zero : t
  val one : t
  val two : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val scale : t -> float -> t
  val int_pow : t -> Int.t -> t
  val (/) : t -> t -> t
  val exp : t -> t
  val log : t -> t
  val ( ** ) : t -> t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val abs : t -> t
  val step : t -> t
  val relu : t -> t
  val softplus : t -> t
  val sigmoid : t -> t
end

module Make (Floatlike : Floatlike.For_autodiff) : sig
  module OneD : sig
    type t

    val eval : t -> Floatlike.t -> Floatlike.t

    val d : t -> t

    val c : Floatlike.t -> t

    val x : t

    val compose : t -> t -> t

    include S with type t := t
  end

  type t

  val eval : t -> Floatlike.t Infinite_list.t -> Floatlike.t

  val eval' : t -> Floatlike.t list -> Floatlike.t

  val grad : t -> t Infinite_list.t

  val c : Floatlike.t -> t

  val x_i : int -> t

  val x_0 : t

  val x_1 : t

  val x_2 : t

  val compose : OneD.t -> t -> t

  include S with type t := t
end
