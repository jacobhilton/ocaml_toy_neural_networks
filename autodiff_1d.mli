module Make (Floatlike : sig
    type t
    val zero : t
    val one : t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val scale : t -> float -> t
    val int_pow : t -> int -> t
    val exp : t -> t
    val log : t -> t
    val sin : t -> t
    val cos : t -> t
    val abs : t -> t
  end) : sig
  type t

  val eval : t -> Floatlike.t -> Floatlike.t

  val d : t -> t

  val c : Floatlike.t -> t

  val zero : t

  val one : t

  val two : t

  val x : t

  val (+) : t -> t -> t

  val (-) : t -> t -> t

  val ( * ) : t -> t -> t

  val compose : t -> t -> t

  val scale : t -> float -> t

  val int_pow : t -> int -> t

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
