open Core

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
  end) : sig
  type t

  val diff : t -> t

  val eval : t -> Floatlike.t -> Floatlike.t

  val const : Floatlike.t -> t

  val id : t

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
end
