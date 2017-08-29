module type Common = sig
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

module type S = sig
  type floatlike

  module Univar : sig
    type t

    val eval : t -> floatlike -> floatlike

    val d : t -> t

    val x : t

    val compose : t -> t -> t

    include Common with type t := t and type floatlike := floatlike
  end

  type t

  val eval : t -> floatlike Infinite_list.t -> floatlike

  val eval' : t -> floatlike list -> floatlike

  val grad : t -> t Infinite_list.t

  val x_i : int -> t

  val x_0 : t

  val x_1 : t

  val x_2 : t

  val compose_univar : Univar.t -> t -> t

  val compose : t -> t Infinite_list.t -> t

  val compose' : t -> t list -> t

  include Common with type t := t and type floatlike := floatlike

  module Multidim : sig
    type unidim = t

    type t

    val eval : t -> floatlike Infinite_list.t -> floatlike Infinite_list.t

    val jacobian : t -> unidim Infinite_list.t Infinite_list.t

    val of_unidims : unidim Infinite_list.t -> t

    val of_unidims' : unidim list -> t

    val of_unidim : dim:int -> unidim -> t

    val nth_unidim : t -> int -> unidim

    val to_unidims : dim:int -> t -> unidim list

    val c : dim:int -> floatlike -> t

    val zero : t

    val x : dim:int -> t

    val scale : t -> floatlike -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val map : dim:int -> t -> f:(unidim -> unidim) -> t

    val map2 : dim:int -> t -> t -> f:(unidim -> unidim -> unidim) -> t

    val compose_univar : dim:int -> Univar.t -> t -> t
  end
end

