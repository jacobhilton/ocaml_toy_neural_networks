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

  val compose_list : t -> t Infinite_list.t list -> t

  val compose_list' : t -> t list list -> t

  val compose_list'' : t list list -> t list

  include Common with type t := t and type floatlike := floatlike
end
