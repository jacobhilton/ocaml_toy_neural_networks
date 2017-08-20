module type Basic = sig
  type t
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val equal : t -> t -> bool
end

module type Extended = sig
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
end
