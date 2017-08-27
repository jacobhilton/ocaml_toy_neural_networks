module Float = struct
  include Core.Float

  let ( ** ) = ( ** )
end

module type For_matrix = sig
  type t
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val equal : t -> t -> bool
  val robustly_compare : t -> t -> int
  val (>) : t -> t -> bool
  val abs : t -> t
end

module type For_autodiff = sig
  type t
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val int_pow : t -> int -> t
  val of_int : int -> t
  val ( ** ) : t -> t -> t
  val exp : t -> t
  val log : t -> t
  val sin : t -> t
  val cos : t -> t
  val abs : t -> t
end
