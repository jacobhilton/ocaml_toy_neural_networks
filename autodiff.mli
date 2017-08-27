module type S = sig
  type t
  type floatlike

  val c : floatlike -> t
  val zero : t
  val one : t
  val two : t
  val scale : t -> floatlike -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val int_pow : t -> int -> t
  val (/) : t -> t -> t
  val pow : t -> floatlike -> t
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
  module Univar : sig
    type t

    val eval : t -> Floatlike.t -> Floatlike.t

    val d : t -> t

    val x : t

    val compose : t -> t -> t

    include S with type t := t and type floatlike := Floatlike.t
  end

  type t

  val eval : t -> Floatlike.t Infinite_list.t -> Floatlike.t

  val eval' : t -> Floatlike.t list -> Floatlike.t

  val grad : t -> t Infinite_list.t

  val x_i : int -> t

  val x_0 : t

  val x_1 : t

  val x_2 : t

  val compose : Univar.t -> t -> t

  include S with type t := t and type floatlike := Floatlike.t
end
